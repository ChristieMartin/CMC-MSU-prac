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


class ConnectedSocket: public Socket {
public:
    ConnectedSocket() = default;
    explicit ConnectedSocket(int cd) : Socket(cd) {}
    void Write(const string& str) {
        ColorText("Sending: ", str);
        send(sd, str.c_str(), str.length(), 0);
    }
    void WriteFile(int fd){
        string str;
        str += "\nVersion: HTTP/1.1\nContent-length: ";
        char c;
        int len = 0;
        while(read(fd, &c, 1)) len++;
        lseek(fd, 0, 0);
        ColorText("Version: ", "HTTP/1.1");
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
        }
    }
    void Read(string& str) {
        int buflen = 4096;
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


//----------------------------------------------------------
class ClientSocket: public ConnectedSocket {
public:
    //ClientSocket() : ConnectedSocket() {}
    void Connect(const SocketAddress& serverAddr) {
        Check(connect(sd, serverAddr.GetAddr(), serverAddr.GetLen()), "connect");
    }
};

class HttpHeader {
    string name;
    string value;
public:
    HttpHeader() = default;
    HttpHeader(const string& n, const string& v) : name(n), value(v) {}
    HttpHeader(const HttpHeader& copy) {
        name = copy.name;
        value = copy.value;
    }
    string ToString() const {
        string temp = name + " " + value;
        return temp;
    }
    static HttpHeader ParseHeader(const string& line){
        int i = 0;
        string name, value;
        if (!line.empty()){
            while (line[i] != ' ') {
                name += line[i];
                i++;
            }
            name += '\0';
            
            while (i < line.size()) {
                value += line[i];
                i++;
            }
            value += '\0';
            
        } else {
            name = " "; value = " ";
        }
        
        HttpHeader temp(name, value);
        return temp;
    }
    int GetValue() const {
        return atoi(value.c_str());
    }
};

class HttpRequest {
    vector<string> _lines;
public:
    HttpRequest() {
        _lines = {"GET /123.txt HTTP/1.1"};
    }
    string ToString() const {
        string res;
        for (int i = 0; i < _lines.size(); i++) res += _lines[i];
        return res;
    };
};

class HttpResponse {
    HttpHeader response;
    HttpHeader* other;
    string body;
    int len;
public:
    HttpResponse(vector<string> lines) {
        response = HttpHeader::ParseHeader(lines[0]);
        other = new HttpHeader[lines.size() - 1];
        int i;
        int n = lines.size();
        for (i = 1; i < lines.size(); i++) {
            other[i - 1] = HttpHeader::ParseHeader(lines[i]);
            if ((lines[i]).empty()) {
                body = lines[i + 1];
                break;
            }
        }
        len = i;
    }
    void PrintResponse() const {
        cout << response.ToString() << endl;
        int j = 0;
        while (j < len) {
            cout << (other[j]).ToString() << endl;
            j++;
        }
        cout << body << endl;
    }
    ~HttpResponse() {
        delete[] other;
    }
};

void ClientConnection() {
    ClientSocket cs;
    SocketAddress saddr;
    cs.Connect(saddr);
    HttpRequest rq;
    string req = rq.ToString();
    cs.Write(req);
    vector<string> lines;
    string response;
    string temp;
    for (int i = 0; i < 3; i++){
        cs.Read(response);
        temp += response;
    }
    lines = SplitLines(temp);
    HttpResponse resp(lines);
    resp.PrintResponse();
    cs.Shutdown();
}


int main(){
    ClientConnection();
    return 0;
}
