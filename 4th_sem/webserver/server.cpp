#include <iostream>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h> 
#include <netdb.h>
#include <unistd.h>
#include <string>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <vector>

using namespace std;

#define WHITE          "\x1b[38m"
#define CYAN           "\x1b[36m"
#define RED            "\x1b[31m"
#define BLUE           "\x1b[38;5;63m"
#define GREEN          "\x1b[32m"
#define PURPLE         "\x1b[38;2;190;82;125m"
#define COLORENDS      "\x1b[0m"
#define BALD           "\x1b[1m"

#define PORTNUMBER 8080;

/* 
TODO :
 exceptions, HttpHeader, HttpRequest, HttpResponse, HttpServer, HttpClient, Connect for client and the whole client(rn I use my browser to test program), cgi(fork and ...) if RequestHeader asked
*/

void Check(int num, const char* str){
    if (num < 0) perror(str);
}

void ColorText(string str1, string str2) {
    cout << BALD BLUE << str1 << PURPLE << str2 << COLORENDS << endl;
}

class SocketAddress {
    struct sockaddr_in addr;
public:
    SocketAddress() {
        addr.sin_family = AF_INET;
        addr.sin_port = htons(8080); 
        addr.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
    }
    SocketAddress(const char* ip, short port){
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port); 
        addr.sin_addr.s_addr = inet_addr(ip);
    }
    SocketAddress(unsigned int ip, short port) {
        addr.sin_family = AF_INET;
        addr.sin_port = htons(port);
        addr.sin_addr.s_addr = htonl(ip);
    }
    struct sockaddr* GetAddr() const { return (sockaddr *)&addr;}
    int GetLen() const { return sizeof(addr);}
    
    ~SocketAddress() {}
};


class Socket{
protected:
    int sd;
    explicit Socket(int _sd) : sd(_sd) {}
public:
    Socket() {
        sd = socket(AF_INET, SOCK_STREAM, 0);
        Check(sd, "socket");
    }
    int GetSd() const { return sd;}
    void Shutdown() { shutdown(sd, 2);}
    ~Socket() { close(sd);}
};

class ServerSocket: public Socket {
public:
    void Bind(const SocketAddress& sock) {
        Check(bind(sd, sock.GetAddr(), sock.GetLen()), "bind");
    }
    void Listen(int backlog){
        Check(listen(sd, backlog), "listen");
    }
    int Accept(SocketAddress& sock) {
        size_t len = sizeof(sock);
        int acc = accept(sd, sock.GetAddr(), (socklen_t *)&len);
        Check(acc, "accept");
        return acc;
    }
};

class ConnectedSocket: public Socket {
public:
    ConnectedSocket() : Socket() {}
    explicit ConnectedSocket(int cd) : Socket(cd) {}
    void Write(const string& str) {
        ColorText("Server sends: ", str);
        send(sd, str.c_str(), str.length(), 0);
    }
    void WriteFile(int fd){
        string str;
        str += "\nContent-length: ";
        char c;
        int len = 0;
        while(read(fd, &c, 1)) len++;
        lseek(fd, 0, 0);

        ColorText("Content-length: ", to_string(len));

        str += to_string(len) + "\n\n";

        int n = str.length();
        char* buf = (char*) malloc(sizeof(char) * (n + 1));
        strcpy(buf, str.c_str());
        len = strlen(buf);

        send(sd, buf, len, 0);
        free(buf);

        int buflen = 1024;
        char bufer[buflen];
        while((len = read(fd, bufer, buflen)) > 0){
            send(sd, bufer, len, 0);
            //cout << buf2 << endl;
        }
       
    }
    void Write(const vector<uint8_t>& bytes);
    void Read(string& str) {
        int buflen = 1024;
        char buf[buflen];
        int req = recv(sd, buf, buflen, 0);
        Check(req, "read");
        str = buf;
    }
    void Read(vector<uint8_t>& bytes);
};

vector<string> SplitLines(string s) {
    string meta = "\r\n", token;
    int start = 0, end, len = meta.length();
    vector<string> res;
    while ((end = s.find(meta, start)) > 0) {
        token = s.substr(start, end - start);
        start = end + len;
        res.push_back(token);
    }
    res.push_back(s.substr(start));
    return res;
}

string GetPath(string from) {
    int i = 4, j = 0;
    char* w = (char*)malloc(_PC_PATH_MAX);
    while (from[i] != ' ') {
        w[j] = from[i];
        j++; 
        i++;
    }
    w[j] = '\0';
    string res = "index/";
    res += w;
    free(w);
    return res;
}

void ProcessConnection(int cd, const SocketAddress& clAddr) {
    ConnectedSocket cs(cd);
    string request;
    cs.Read(request);
    vector<string> lines = SplitLines(request);
    string path = GetPath(lines[0]);
    ColorText("Path: ", path);
    int fd = open(path.c_str(), O_RDONLY);
    int fl = 0;
    if (fd < 0) {
        cs.Write("HTTP/1.1 404 NotFound");
        fl = 1;
        fd = open("index/404.html", O_RDONLY);
    } 
    if (path == "index//") {
        fd = open("index/index.html", O_RDONLY);
        if (fd < 0) {
            cs.Write("HTTP/1.1 404 NotFound");
            fl = 1;
            fd = open("index/404.html", O_RDONLY);
        }
    }
    
    if (fl == 0) cs.Write("HTTP/1.1 200 MyServer");
    cs.WriteFile(fd);
    
    cs.Shutdown();
    close(fd);
}

const int BACKLOG = 5;

void RunServer() {
    SocketAddress serverAddr;
    ServerSocket ss;
    ss.Bind(serverAddr);
    ss.Listen(BACKLOG);
    ColorText("Please connect to ", "127.0.0.1:8080");
    while(1) {
        SocketAddress clAddr;
        
        int cd = ss.Accept(clAddr);
        ProcessConnection(cd, clAddr);
    }
}
//----------------------------------------------------------
class ClientSocket: public ConnectedSocket {
public:
    ClientSocket() : ConnectedSocket() {}
    void Connect(const SocketAddress& serverAddr) {
        Check(connect(sd, serverAddr.GetAddr(), serverAddr.GetLen()), "connect");
    }
};

class HttpHeader {
    string name;
    string value;
public:
    HttpHeader(const string& n, const string& v) : name(n), value(v) {}
    static HttpHeader* ParseHeader(const string& line);
};

class HttpRequest {
    vector<string> _lines;
public:
    HttpRequest() {}
    //const string& ToString() {};
};
class HttpResponse {};
class HttpServer {};
class HttpClient {};

void ClientConnection() {
    ClientSocket s;
    SocketAddress saddr("127.0.0.1", 8080);
    s.Connect(saddr);

    HttpRequest rq;
    //make req
    //s.Write(rq.ToString());
    HttpResponse resp;
    string strResponse;
    s.Read(strResponse);
    vector<string> lines = SplitLines(strResponse);
    //parse response...
    // lines[0] - response header
    // lines[1]... - httpheader
    // empty line or body of the response
    s.Shutdown();
}

int main(){
    RunServer();
    return 0;
}
