#include <iostream>
#include <cstring>

using namespace std;

#define WHITE          "\x1b[38m"
#define CYAN           "\x1b[36m"
#define RED            "\x1b[31m"
#define BLUE           "\x1b[38;5;63m"
#define GREEN          "\x1b[32m"
#define PURPLE         "\x1b[38;2;190;82;125m"
#define COLORENDS      "\x1b[0m"

#define BALD           "\x1b[1m"

#define MAXSIZE 20

#define HLP 87
#define SHW 88

#define ADR 99
#define ORG 90
#define STR 91
#define HNM 92
#define ZIP 93

#define RADR 199
#define RORG 190
#define RSTR 191
#define RHNM 192
#define RZIP 193

#define ERR 101
#define EXT 102

void printErr() {
    cout << BALD RED "Wrong command, type help for command list" COLORENDS << endl;
}

char* getWord(char* w, char end) {
    char ch;
    int i = 0;
    while (((ch = getchar()) != end) && (ch != '\n')) {
        w[i] = ch;
        i++;
    }
    w[i] = '\0';
    return w;
}

class Address {
    string orgname;
    string street;
    string housenum;
    string zipcode;
public:
    
    Address(string s) : orgname(s) {}
    Address(string a, string b, string c, string d) : orgname(a), street(b), housenum(c), zipcode(d) {}

    void showAddr() {
        if (!orgname.empty()) cout << '\t' << '\t' << "< " << orgname << " >" << endl;
        if (!street.empty() || !housenum.empty() || !zipcode.empty()) {
            cout << "\t  " << street << " " << housenum << " " << zipcode << endl;
        } else cout << "Empty address\n";
    }
    void change(string a, const int num);
    void remove(const int num);

};

void Address::change(string a, const int num) {
        switch (num){
        case ORG:
            orgname = a;
            break;
        case STR:
            street = a;
            break;
        case HNM:
            housenum = a;
            break;
        case ZIP:
            zipcode = a;
            break;
        }
    }

void Address::remove(const int num) {
        switch (num){
        case RORG:
            if (!orgname.empty()) orgname.clear();
            break;
        case RSTR:
            if (!street.empty()) street.clear();
            break;
        case RHNM:
            if (!housenum.empty()) housenum.clear();
            break;
        case RZIP:
            if (!zipcode.empty()) zipcode.clear();
            break;
        default:
            break;
        }
    }

int getCommand(char* w) {
    if (!strcmp(w, "help")) {
        return HLP;
    } if (!strcmp(w, "show")) {
        return SHW;
    } if (!strcmp(w, "changeAddr")) {
        return ADR;
    } if (!strcmp(w, "changeName")) {
        return ORG;
    } if (!strcmp(w, "changeStreet")) {
        return STR;
    } if (!strcmp(w, "changeHouseNumber")) {
        return HNM;
    } if (!strcmp(w, "changeZip")) {
        return ZIP;
    } if (!strcmp(w, "removeAddr")) {
        return RADR;
    } if (!strcmp(w, "removeName")) {
        return RORG;
    } if (!strcmp(w, "removeStreet")) {
        return RSTR;
    } if (!strcmp(w, "removeHouseNumber")) {
        return RHNM;
    } if (!strcmp(w, "removeZip")) {
        return RZIP;
    } if (!strcmp(w, "exit")) {
        return EXT;
    }   return ERR;
}

void listCommands() {
    cout << BALD GREEN "help" COLORENDS << " - list of commands" << endl;
    cout << BALD GREEN "show" COLORENDS << " - print organisation name and address" << endl;
    cout << BALD GREEN "changeName" COLORENDS << " - change organisation's name" << endl;
    cout << BALD GREEN "changeAddr" COLORENDS << " - change organisation's address" << endl;
    cout << BALD GREEN "changeStreet" COLORENDS << " - change organisation's street" << endl;
    cout << BALD GREEN "changeHouseNumber" COLORENDS << " - change organisation's house number" << endl;
    cout << BALD GREEN "changeZip" COLORENDS << " - change organisation's zip code" << endl;
    cout << BALD GREEN "removeName" COLORENDS << " - remove organisation's zip code" << endl;
    cout << BALD GREEN "removeAddr" COLORENDS << " - remove organisation's address" << endl;
    cout << BALD GREEN "removeStreet" COLORENDS << " - remove organisation's street" << endl;
    cout << BALD GREEN "removeHouseNumber" COLORENDS << " - remove organisation's house number" << endl;
    cout << BALD GREEN "removeZip" COLORENDS << " - remove organisation's zip code" << endl;
    cout << BALD GREEN "exit" COLORENDS << " - to exit the program" << endl;
}

int main(){
    Address b("Google", "Pushkin st.", "19", "123321");
    cout << BALD BLUE "Hello!" << endl << BALD BLUE "Welcome to my program" << endl << BALD BLUE "For menu please type " << BALD GREEN "help" COLORENDS << endl;
    cout << BALD PURPLE "> " COLORENDS;
    char* w = new char[255];
    int n;
    while(strcmp(getWord(w, ' '), "exit") != 0) {
        switch((n = getCommand(w))){
            case(HLP) :
                listCommands();
                break;
            case(SHW) :
                b.showAddr();
                break;
            case(ADR) :
                cout << BALD BLUE "Name: " COLORENDS;
                b.change(getWord(w, '\n'), ORG);
                cout << BALD BLUE "Street: " COLORENDS;
                b.change(getWord(w, '\n'), STR);
                cout << BALD BLUE "House number: " COLORENDS;
                b.change(getWord(w, '\n'), HNM);
                cout << BALD BLUE "ZIP code: " COLORENDS;
                b.change(getWord(w, '\n'), ZIP);
                cout << "\n";
                b.showAddr();
                cout << "\n";
                break;
            case(ORG) : case (STR) : case(HNM) : case(ZIP) :
                b.change(getWord(w, '\n'), n);
                break;
            case(RORG) : case (RSTR) : case(RHNM) : case(RZIP) :
                b.remove(n);
                break;
            case(RADR):
                b.remove(RORG); b.remove(RSTR); b.remove(RHNM); b.remove(RZIP); 
                break;
            case(ERR) :
                printErr();
                break;
        }
        cout << BALD PURPLE "> " COLORENDS;
    }

    delete[] w;

    return 0;
}
