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
#include <iterator>

using namespace std;

#define WHITE          "\x1b[38m"
#define CYAN           "\x1b[36m"
#define RED            "\x1b[31m"
#define BLUE           "\x1b[38;5;63m"
#define GREEN          "\x1b[32m"
#define PURPLE         "\x1b[38;2;190;82;125m"
#define COLORENDS      "\x1b[0m"
#define BALD           "\x1b[1m"

void Check(int num, const char* str){
    if (num < 0) perror(str);
}

void ColorText(string str1, string str2) {
    cout << BALD BLUE << str1 << PURPLE << str2 << COLORENDS << endl;
}

const int PORTNUMBER = 8080;

class SocketAddress {
    struct sockaddr_in addr;
public:
    SocketAddress() {
        addr.sin_family = AF_INET;
        addr.sin_port = htons(PORTNUMBER); 
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

vector<uint8_t> ToVect(int fd) {
    vector<uint8_t> vu; char c;
    while(read(fd, &c, 1)) vu.push_back(c);
    return vu;
}

class ConnectedSocket: public Socket {
public:
    ConnectedSocket() : Socket() {}
    explicit ConnectedSocket(int cd) : Socket(cd) {}
    void Write(const string& str) {
        send(sd, str.c_str(), str.length(), 0);
    }
    void Write(const vector<uint8_t>& bytes) {
        send(sd, bytes.data(), bytes.size(), 0);
    }
    void Read(string& str) {
        int buflen = 1024;
        char buf[buflen];
        int req = recv(sd, buf, buflen, 0);
        Check(req, "read");
        str = buf;
    }
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
    int i = 4; // GET /...
    string res = "index/";
    if (from[i] == ' ') i++; // if HEAD
    while (from[i] != ' ') res += from[i++];
    return res;
}

void ProcessConnection(int cd, const SocketAddress& clAddr) {
    ConnectedSocket cs(cd);

    string request;
    cs.Read(request);
    vector<string> lines = SplitLines(request);
    ColorText("Client sent: ", lines[0]);
    string path = GetPath(lines[0]);
    ColorText("Path: ", path);

    int fd;
    if (path == "index//") { fd = open("index/index.html", O_RDONLY);}
    else { fd = open(path.c_str(), O_RDONLY);}
    if (fd < 0) {
        ColorText("Server sends: ", "HTTP/1.1 404 Not Found");
        cs.Write("HTTP/1.1 404 Not Found");
        fd = open("index/404.html", O_RDONLY);
        Check(fd, "fd404");
    } else {
        ColorText("Server sends: ", "HTTP/1.1 200 OK");
        cs.Write("HTTP/1.1 200 OK");
    }
    vector<uint8_t> vect = ToVect(fd);
    string str;
    str += "\r\nVersion: HTTP/1.1\r\nContent-length: " + to_string(vect.size()) + "\r\n\r\n";
    ColorText("Version: ", "HTTP/1.1");
    ColorText("Content-length: ", to_string(vect.size()));
    cs.Write(str);
    cs.Write(vect);
    cs.Shutdown();
    close(fd);
}

const int BACKLOG = 5;

void RunServer() {
    SocketAddress serverAddr;
    ServerSocket ss;
    ss.Bind(serverAddr);
    ss.Listen(BACKLOG);
    ColorText("\nPlease connect to ", "127.0.0.1:8080\n");
    while(1) {
        SocketAddress clAddr;
        ProcessConnection(ss.Accept(clAddr), clAddr);
        cout << BALD GREEN << "---------" << COLORENDS << endl;
    }
}
int main(){
    RunServer();
    return 0;
}
