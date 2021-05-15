#include <iostream>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include <sys/stat.h>
#include <vector>
#include <cctype>
#include <stack>
#include <algorithm>
#include <fstream>
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

ifstream ff;

void Error(string str1, string str2) {
    cout << BALD RED << str1 << " " << str2 << COLORENDS << endl;
}

void ColorText(string str1, string str2) {
    cout << BALD BLUE << str1 << PURPLE << " -- " << GREEN << str2 << COLORENDS << endl;
}

/* enum */

enum LexType {
    LNULL,
    // 01
    BOOL, BREAK, CONTINUE, DO, ELSE, FALSE, FOR, FUNCTION, GETENV, IF, IN,
    // 12   
    NAN, NUMBER, NULLPTR, OBJECT, RETURN, STRING, TRUE, TYPEOF,
    // 20
    UNDEFINED, VAR, WHILE, WRITE, INT,
    // 25
    FIN,
    // 26
    SEMICOLON, COMMA, COLON, DOT, LPAREN, RPAREN, LQPAREN, RQPAREN, BEGIN, END,
    // 35
    EQ, DEQ, TEQ, LSS, GTR, PLUS, PLUSEQ, DPLUS, MINUS, MINUS_EQ, DMINUS,
    // 45
    TIMES, TIMES_EQ, TIMES_SLASH, SLASH, SLASH_EQ, SLASH_TIMES, DSLASH, PERCENT, PERCENT_EQ,
    // 55
    LEQ, NOT, NEQ, NDEQ, GEQ, OR, DPIPE, AND, DAMP,
    // 64
    NUMB,
    // 65
    STR_CONST,
    // 66
    ID,
    // 67
    PLABEL, PADDRESS, PGO, PFGO, PLEFTINC, PRIGHTINC,  PLEFTDEC, PRIGHTDEC, PWRITE, PGETENV
    // 77                                                    
};

enum state{ H, IDENT, NUM, COM, COM2, COM3, SLSH, EEQ, EEQ2, PLUSS, MINUSS, AMP, PIPE, QUOTE };

const char* words[] = { "", "Boolean", "break", "continue", "do", "else", "false", "for", "function", "getenv", "if", "in", "NaN", "Number", "null", "Object", "return", "String", "true", "typeof", "undefined", "var", "while", "write", NULL };

const char* tokens[] = { "@", ";", ",", ":", ".", "(", ")", "[", "]", "{", "}", "=", "==", "===", "<", ">", "+", "+=", "++", "-", "-=", "--", "*", "*=", "*/", "/", "/=", "/*", "//", "%", "%=", "<=", "!", "!=", "!==", ">=", "|", "||", "&", "&&", NULL };

/* Ident */

class Ident {
    string name;
    LexType type;
    int value;

    bool assign;
    bool declare;
public:
    Ident(): assign(false), declare(false) {}
    Ident(const string n) : name(n), assign(false), declare(false) {}
    bool operator==(const string& s) const { return name == s; }

    string GetStr() const { return name;}
    LexType GetType() const { return type;}
    int GetValue() const { return value;}
    bool GetAssign() const { return assign;}
    bool GetDeclare() const { return declare;}

    void SetType(LexType t) { type = t;}
    void SetValue(int v) { value = v;}
    void SetAssign() { assign = true;}
    void SetDeclare() { declare = true;}
    void SetString(string str) { name = str;}
};

vector<Ident> Table;

int PlaceInTable(const string& buf){
    vector<Ident>::iterator k;
    if ((k = find(Table.begin(), Table.end(), buf)) != Table.end()) return k - Table.begin();
    Table.push_back(Ident(buf));
    return Table.size() - 1;
}

/* Lex */

class Lex {
    LexType type;
    int val;
    string str;
public:
    Lex(LexType t = LNULL, int v = 0, string s = "") : type(t), val(v), str(s) {}
    LexType GetType() const { return type;}
    int GetValue() const { return val;}
    string GetStr() const { return str;}
    void Print();

    void SetType(LexType ntype) { type = ntype;}
    void SetValue(int nval) {val = nval;}
    void SetString(string nstr) {str = nstr;}
};

int SetState(string& buf, state& st, char ch, int& digit) {
    if (cin.eof()) { return 1;}
    if (!isspace(ch)) {
        if(ch == '@') { return 1;}
        if (isalpha(ch)) {
            buf.push_back(ch);
            st = IDENT;
        } else if (isdigit(ch)) {
            digit = ch - '0';
            st = NUM;
        } else switch(ch) {
            case '/':
                buf.push_back(ch);
                st = SLSH;
                break;
            case '!': case '=':
                buf.push_back(ch);
                st = EEQ;
                break;
            case '*': case '<': case '>': case '%':
                buf.push_back(ch);
                st = EEQ2;
                break;
            case '+':
                buf.push_back(ch);
                st = PLUSS;
                break;
            case '-':
                buf.push_back(ch);
                st = MINUSS;
                break;
            case '&':
                buf.push_back(ch);
                st = AMP;
                break;
            case '|':
                buf.push_back(ch);
                st = PIPE;
                break;
            case '#':
                st = COM3;
                break;
            case '"':
                st = QUOTE;
                break;
            case '@':
                return 1;
            default:
                return 2;
        }
    }
    return 0;
} 

/* Scanner */

class Scanner {
    char ch;
    bool flag = true;
    void gc() {  cin.read(&ch, 1);}
    int FindString(string str, const char** list);
public:
    Scanner() = default;
    Scanner(const char* name){
        ff.open(name);
        if (!ff) {
            cout << "File not found\n";
            exit(1);
        }
        cin.rdbuf(ff.rdbuf());
    }
    Lex GetLex();
};

int Scanner::FindString(string str, const char** list) {
    int i = 0;
    while (list[i]) {
        if (str == list[i]) return i;
        i++;
    }
    return 0;
}

Lex Scanner::GetLex() {
    string buf, res;
    int digit, t, k;
    state CurState = H;
    while (1) {
        if (flag) gc(); else flag = true;
        switch (CurState) {
            case H:
                k = SetState(buf, CurState, ch, digit);
                if (k == 2){
                    buf.push_back(ch);
                    if ((t = FindString(buf, tokens))){
                        return Lex((LexType)(FIN + t), t);
                    } else throw ch;
                }
                if (k == 1) return Lex(FIN);
                break;
            case IDENT:
                if (isdigit(ch) || isalpha(ch)) { 
                    buf.push_back(ch);
                } else {
                    flag = false;
                    if ((t = FindString(buf, words)) != 0) {
                        return Lex((LexType)t, t);
                    } else {
                        t = PlaceInTable(buf);
                        return Lex(ID, t);
                    }
                }
                break;
            case NUM:
                if (isdigit(ch)) { digit = digit * 10 + (ch - '0');}
                else if (isalpha(ch)) { throw ch;}
                else {
                    flag = false;
                    return Lex(NUMB, digit);
                }
                break;
            case SLSH:
                if (ch == '*'){
                    buf.pop_back();
                    CurState = COM;
                }
                else if (ch == '='){
                    buf.push_back(ch);
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                else if (ch == '/'){
                    buf.pop_back();
                    CurState = COM3;
                }
                else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case COM:
                if (ch == '*'){
                    CurState = COM2;
                } else if (ch == '@' || std::cin.eof()) CurState = H;
                    
                break;
            case COM2:
                if (ch == '/') {
                    CurState = H;
                } else if (ch == '@' || std::cin.eof()) {
                    CurState = H;
                } else
                    CurState = COM;
                break;
            case COM3:
                if (ch == '\n')
                    CurState = H;
                else if (ch == '@' || std::cin.eof()) CurState = H;
                break;
            case EEQ:
                if (ch == '='){
                    buf.push_back(ch);
                    CurState = EEQ2;
                } else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case EEQ2:
                if (ch == '=') {
                    buf.push_back(ch);
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                } else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case PLUSS:
                if (ch == '=' || ch == '+') {
                    buf.push_back(ch);
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                } else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case MINUSS:
                if (ch == '=' || ch == '-') {
                    buf.push_back(ch);
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                } else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case AMP:
                if (ch == '&') {
                    buf.push_back(ch);
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                } else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case PIPE:
                if (ch == '|') {
                    buf.push_back(ch);
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                } else {
                    flag = false;
                    t = FindString(buf, tokens);
                    return Lex((LexType)(FIN + t), t);
                }
                break;
            case QUOTE:
                if (ch == '"') {
                    res = "";
                    for (int i = 0; i < buf.size(); i++) res += buf[i];
                    return Lex(STR_CONST, 0, res);
                } else if (cin.eof()) throw ch;
                buf.push_back(ch);
                break;
        }
    }
}

void Lex::Print() { 
        string t;
        bool fl = true;
        if (type <= WRITE)
            t = words[type];
        else if (type >= FIN && type <= DAMP)
            t = tokens[type - FIN];
        else switch(type) {
            case NUMB:
                t = "number";
                break;
            case STR_CONST:
                t = "string const";
                fl = false;
                break;
            case ID:
                t = Table[val].GetStr();
                break;
            case PLABEL:
                t = "Label";
                break;
            case PADDRESS:
                t = "Addr";
                break;
            case PGO:
                t = "!";
                break;
            case PFGO:
                t = "!F";
                break;
            case PLEFTINC:
                t = "+#";
                break;
            case PRIGHTINC:
                t = "#+";
                break;
            case PLEFTDEC:
                t = "-#";
                break;
            case PRIGHTDEC:
                t = "#-";
                break;
            case PWRITE:
                t = "write";
                break;
            case PGETENV:
                t = "getenv";
                break;
            default:
                throw type;
                break;
        }
        if (fl) ColorText(t, to_string(val)); else ColorText(t, str);
    }

template <class A, class B>
void Pop(A& st, B& a){
    a = st.top();
    st.pop();
}

/* Poliz */

class Poliz {
    vector<Lex> p;
    int index;
public:
    Poliz(int max = 1000) : index(0) { p.resize(max);}
    void PutLex(Lex l) { 
        p[index] = l; 
        ++index;
    }
    void PutLex(Lex l, int pl) { p[pl] = l;}
    void blank() { index++;}
    void PopBack() { p.pop_back();}
    int GetIndex() { return index;}
    Lex& operator[] (int idx);
    void Print() { 
        for (int i = 0; i < index; ++i) {
            cout << i << " ";
            p[i].Print(); 
        }
    }
};

Lex& Poliz::operator[](int idx){
    if (idx > p.size()) throw "Poliz: out of range";
    else if (idx > index) throw "Poliz: indefinite";
    else return p[idx];
}

/* Parser */

class Parser {
    Scanner s;

    Lex curLex;
    LexType curType;
    int curVal;
    string curStr;

    stack<int> StInt;
    stack<LexType> StLex;
    stack <int> StLabelCon;
	stack <int> StLabelBr;

    void GetL();

    void B();
    void S();
    void S_IF();
    void S_WHILE();
    void S_DO();
    void S_FOR();
    void S_FOR_P();
    void S_ID();
    void S_CONTINUE();
    void S_BREAK();
    void S_WRITE();
    void S_GETENV();
    void S_CHECKBREAK();
    void S_1();
    void S_2(int tmp);
    void E();
    void E1();
    void ED(LexType l);
    void ED2(LexType l);
    void T();
    void F();
    bool TF(bool b);
    void D();
    void D_EQ();
    void G(LexType l);
    void J(bool b);
    void dec(LexType type, int i);

    void CheckId(LexType l);
    void CheckOp();
    void CheckNot();
    void EqType(LexType& type);
    void EqBool();
public:
    Poliz prog;
    Parser(): s() {}
    Parser(const char* name) : s(name), prog() {}
    void Analyze();
};

void Parser::GetL() {
    curLex = s.GetLex();
    curType = curLex.GetType();
    curVal = curLex.GetValue();
    curStr = curLex.GetStr();
    curLex.Print();
}

void Parser::CheckId(LexType l) {
    if (Table[curVal].GetDeclare()) StLex.push (Table[curVal].GetType());
    else throw "Parser: not declared";
    prog.PutLex(Lex(l, curVal));
}

void Parser::CheckNot() {
    if (StLex.top() == BOOL) prog.PutLex(Lex(NOT));
    else throw "Parser: wrong type in not";
}

void Parser::CheckOp() {
    LexType t1, t2, op;
    LexType it = INT, bl = BOOL;
    Pop(StLex, t2);
    Pop(StLex, op);
    Pop(StLex, t1);
    if (op == PLUS || op == MINUS || op == TIMES || op == SLASH || op == PERCENT) bl = INT;
    if (op == OR || op == AND) it = BOOL;
    if (t1 == t2  &&  t1 == it) StLex.push(bl);
    else throw "Parser: wrong types";
    prog.PutLex(Lex(op));
}

void Parser::EqBool() {
    if (StLex.top() != BOOL) throw "Parser: expression is not boolean";
    StLex.pop();
}

void Parser::EqType(LexType & type) {
    Pop(StLex, type);
    if (type == UNDEFINED) throw "Parser: wrong types in =";
}

void Parser::Analyze() {
    GetL();
    S();
    if (curType != FIN) throw curLex;
    cout << BALD RED << "------------\n" << COLORENDS;
    cout << BALD GREEN << "Poliz:\n" << COLORENDS;
    prog.Print();
}

void Parser::B() {
    GetL();
    if (curType == BEGIN){
        GetL();
        S();
        if (curType == END) GetL();
        else throw curType;
    } else throw curType;
}

void Parser::S() {
    switch (curType){
    case IF:
        S_IF();
        break;
    case WHILE:
        S_WHILE();
        break;
    case DO:
        S_DO();
        break;
    case FOR:
        S_FOR();
        break;
    case ID:
        S_ID();
        break;
    case CONTINUE:
        S_CONTINUE();
        break;
    case BREAK:
        S_BREAK();
        break;
    case VAR:
        D();
        break;
    case NUMB:
        StLex.push(NUMB);
        prog.PutLex(Lex(curType));
        E();
        GetL();
        break;
    case WRITE:
        S_WRITE();
        GetL();
        break;
    case GETENV:
        S_GETENV();
        break;
    case DPLUS: case DMINUS:
        E();
        GetL();
        break;
    case FIN: case END:
        return;
    }
    S();
}

void Parser::S_IF() {
    int pl0, pl1;
    GetL();
    if (curType != LPAREN) throw curLex;
    GetL();
    E();
    pl0 = prog.GetIndex();
    G(PFGO);
    if (curType == RPAREN) {
		B();
        pl1 = prog.GetIndex();
        G(PGO);
        if (curType == ELSE){
            prog[pl0] = Lex(PLABEL, prog.GetIndex());
            B();
            prog[pl1] = Lex(PLABEL, prog.GetIndex());
        } else {
            prog.PopBack();
			prog.PopBack();
			prog[pl0] = Lex(PLABEL, prog.GetIndex());
        }
    } else throw curLex;
}

void Parser::S_WHILE() {
    int pl0, pl1;
    pl0 = prog.GetIndex();
    StLabelCon.push(pl0);
    GetL();
    if (curType != LPAREN) throw curLex;
    GetL();
    E();
    EqBool();
    pl1 = prog.GetIndex();
    G(PFGO);
    if (curType == RPAREN) {
        B();
        prog.PutLex(Lex(PLABEL, pl0));
        prog.PutLex(Lex(PGO));
        prog[pl1] = Lex(PLABEL, prog.GetIndex());
    } else throw curLex;
    S_CHECKBREAK();
}

void Parser::S_DO() {
    int pl0, pl1;
    pl0 = prog.GetIndex();
    B();
    StLabelCon.push(prog.GetIndex());
    if (curType == WHILE) {
        GetL();
        if (curType == LPAREN) {
            GetL();
            E();
            EqBool();
            pl1 = prog.GetIndex() + 4;
            prog.PutLex(Lex(PLABEL, pl1));
            prog.PutLex(Lex(PFGO));
            prog.PutLex(Lex(PLABEL, pl0));
            prog.PutLex(Lex(PGO));
            if (curType != RPAREN) throw curLex;
            GetL();
            GetL();
        } else throw curLex;
    } else throw curLex;
    S_CHECKBREAK();
}

void Parser::S_FOR() {
    int pl0, pl1;
    GetL();
    if (curType == LPAREN) {
        S_FOR_P();
        pl0 = prog.GetIndex();
        if (curType == SEMICOLON) {
            GetL();
            E();
            G(PFGO);
            G(PGO);
            pl1 = prog.GetIndex();
            StLabelCon.push(prog.GetIndex());
            if (curType == SEMICOLON) {
                S_FOR_P();
                prog.PutLex(Lex(PLABEL, pl0));
                prog.PutLex(Lex(PGO));
                prog[pl1 - 2] = Lex(PLABEL, prog.GetIndex());
                if (curType == RPAREN) {
                    B();
                    prog.PutLex(Lex(PLABEL, pl1));
                    prog.PutLex(Lex(PGO));
                    prog[pl1 - 4] = Lex(PLABEL, prog.GetIndex());
                } else throw curLex;
            } else throw curLex;
        }
        S_CHECKBREAK();
    } else throw curLex;
}

void Parser::S_FOR_P() {
    GetL();
    int tmp = curLex.GetValue();
	CheckId(PADDRESS);
    GetL();
	if (curType == EQ || curType == MINUS_EQ || curType == PLUSEQ || curType == TIMES_EQ || curType == PERCENT_EQ || curType == SLASH_EQ) {
        S_2(tmp);
	} else if (curType == DPLUS) {
        prog.PutLex(Lex(PRIGHTINC));
		GetL();
		if (curType != SEMICOLON && curType != RPAREN) throw curLex;
		if (curType != RPAREN) GetL();
	} else if (curType == DMINUS) {
        prog.PutLex(Lex(PRIGHTDEC));
		GetL();
		if (curType != SEMICOLON && curType != RPAREN) throw curLex;
		if (curType != RPAREN) GetL();
	} else throw curLex;
}

void Parser::S_CHECKBREAK() {
    if (!StLabelBr.empty()){
        prog[StLabelBr.top()] = Lex(PLABEL, prog.GetIndex());
        StLabelBr.pop();
    }
}

void Parser::S_ID() {
    int tmp = curLex.GetValue();
    CheckId(PADDRESS);
    GetL();
    if (curType == EQ || curType == MINUS_EQ || curType == PLUSEQ || curType == TIMES_EQ || curType == PERCENT_EQ || curType == SLASH_EQ) {
        S_2(tmp);
        if (curType == SEMICOLON) GetL();
        else if (curType == FIN) return;
        else throw curLex;
    } else if (curType == DPLUS) {
        prog.PutLex(Lex(PRIGHTINC));
        S_1();
    } else if (curType == DMINUS) {
        prog.PutLex(Lex(PRIGHTDEC));
        S_1();
    } else throw curLex;
}

void Parser::S_CONTINUE() {
    if (StLabelCon.empty()) throw curLex;
    int curTop = StLabelCon.top();
    StLabelCon.pop();
    prog.PutLex(Lex(PLABEL, curTop));
    prog.PutLex(Lex(PGO));
    S_1();
}

void Parser::S_BREAK() {
    StLabelBr.push(prog.GetIndex());
    G(PGO);
    S_1();
}

void Parser::S_1() {
    GetL();
    if (curType != SEMICOLON && curType != FIN) throw curLex;
    GetL();
}

void Parser::S_2(int tmp) {
    LexType newVal;
    GetL();
	E();
	EqType(newVal);
    Table[tmp].SetType(newVal);
    prog.PutLex(Lex(EQ));
}

void Parser::S_WRITE() {
    GetL();
    if (curType != LPAREN) throw curLex;
    GetL();
    E();
    if (curType != RPAREN) throw curLex;
    GetL();
    if (curType != SEMICOLON) throw curLex;
    prog.PutLex(Lex(PWRITE));
}

void Parser::S_GETENV() {
    GetL();
    if (curType != LPAREN) throw curLex;
    GetL();
    if (curType == STR_CONST) {
        prog.PutLex(Lex(STR_CONST, 0, curStr));
        GetL();
    } else throw curLex;

    if (curType == RPAREN) {
        GetL();
        prog.PutLex(Lex(PGETENV));
    } else throw curLex;
    GetL();
}

void Parser::E() {
    if (curType == DPLUS){
        ED(PLEFTINC);
	} else if (curType == DMINUS) {
        ED(PLEFTDEC);
    } else {
        E1();
        while (curType == EQ || curType == LSS || curType == GTR || curType == LEQ || curType == GEQ || curType  == NEQ || curType == DMINUS || curType == DPLUS || curType == DEQ){
			LexType tmp = curType;
            StLex.push(curType);
            if (!(curType == DMINUS || curType == DPLUS)) {
                GetL();
                E1();
                prog.PutLex(Lex(tmp));
            } else {
                if (curType == DMINUS) ED2(PRIGHTDEC);
                if (curType == DPLUS)  ED2(PRIGHTINC);
            }
		}
    }
}

void Parser::E1() { F(); J(0); J(1);}

void Parser::ED(LexType l) {
    GetL();
    int lvIndex = curVal;
    if (curType == ID) CheckId(PADDRESS);
    else throw curLex;
    prog.PutLex(Lex(l));
    GetL();
}

void Parser::ED2(LexType l) {
    prog[prog.GetIndex() - 1].SetType(PADDRESS);
    GetL();
    prog.PutLex(Lex(l));
}

void Parser::F() {
    switch (curType){
    case ID:
        CheckId(ID);
        GetL();
        break;
    case NUMB:
        StLex.push(NUMB);
        prog.PutLex(curLex);
        GetL();
        break;
    case TRUE:
        StLex.push(BOOL);
        prog.PutLex(Lex(TRUE, 1));
        GetL();
        break;
    case FALSE:
        StLex.push(BOOL);
        prog.PutLex(Lex(FALSE, 0));
        GetL();
        break;
    case STR_CONST:
        StLex.push(STR_CONST);
        prog.PutLex(curLex);
        GetL();
        break;
    case NOT:
        GetL();
        F();
        break;
    case LPAREN:
        GetL();
        E();
        if (curType == RPAREN) GetL();
        else throw curLex;
    default:
        throw curLex;
    }   
}

void Parser::D() {
    do D_EQ(); while (curType == COMMA);
    if (curType != SEMICOLON && curType != FIN) throw curLex;
    if (curType == SEMICOLON) GetL(); else throw curLex;
}

void Parser::D_EQ() {
    GetL();
    if (curType != ID) throw curLex;   
    StInt.push(curVal);
    int lvIndex = curVal;
    GetL();
    if (curType == EQ) {
        prog.PutLex(Lex(PADDRESS, lvIndex));
        LexType i;
        LexType tmp = curType;
        GetL();
        E();
        Pop(StLex, i);
        dec(i, lvIndex);
        prog.PutLex(Lex(tmp));
    } else dec(UNDEFINED, lvIndex);
}

void Parser::G(LexType l) {
    prog.blank();
    prog.PutLex(Lex(l));
}

void Parser::J(bool b) {
    while (TF(b)) {
        LexType tmp = curType;
        StLex.push(curType);
        GetL();
        F();
        if(b) J(0);
        prog.PutLex(Lex(tmp));
    }
}

bool Parser::TF(bool b) {
    if (b) return (curType == PLUS || curType == MINUS || curType == DPIPE || curType == COMMA);
    else return (curType == TIMES || curType == SLASH || curType == DAMP || curType == PERCENT);
}

void Parser::dec(LexType type, int i) {
    if (Table[i].GetDeclare()) throw "Parser: declared twice";
    Table[i].SetDeclare();
    Table[i].SetType(type);
}

void Print(vector<string> vect) {
    for (int i = 0; i < vect.size(); i++) cout << vect[i];
}

/* Executer */

class Executer {
    Lex elem;

    stack<int> IncSt;
    stack<int> DecSt;

    bool SetBools(Lex& lex1, Lex& lex2);
    bool SetBoolsI(int tmp, Lex& lex2);
    void DecIncSt();

    vector<string> ToPrint;
public:
    void Execute(Poliz& prog);
};

void Executer::Execute(Poliz& prog) {
    stack<Lex> args;
    int i, j, k, index = 0, size = prog.GetIndex(), tmp;
    string str;
    Lex lex1, lex2;
    LexType tmp1, tmp2;
    int IncDec;
    while(index < size) {
        elem = prog[index];
        cout << index << BALD GREEN << " ok" << COLORENDS << endl;
        switch (elem.GetType()){
        case TRUE: case FALSE: case NUMB: case PADDRESS: case PLABEL: ;
        case STR_CONST:
            args.push(elem);
            break;
        case ID:
            i = elem.GetValue();
            if (!Table[i].GetAssign()) throw "Executer: indefinite identifier";
            args.push(Lex(Table[i].GetType(), Table[i].GetValue(), Table[i].GetStr()));
            break;
        case PGO: 
            Pop(args, lex1);
            index = lex1.GetValue() - 1;
            break;
        case PFGO: 
            Pop(args, lex1);
            Pop(args, lex2);
            if (lex2.GetType() != TRUE && lex2.GetType() != FALSE) throw "Executer: FGO error";
            if (lex2.GetType() == FALSE) index = lex1.GetValue() - 1;
            break;
        case PLUS: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            tmp1 = lex1.GetType();
            tmp2 = lex2.GetType();
            if (tmp1 == STR_CONST && tmp2 == STR_CONST) {
                str = lex1.GetStr() + lex2.GetStr();
                lex1.SetString(str);
            } else if (tmp1 == NUMB && tmp2 == NUMB) {
                tmp = lex1.GetValue() + lex2.GetValue();
                lex1.SetValue(tmp);
            } else if (tmp1 == NUMB && tmp2 == STR_CONST) {
                str = to_string(lex1.GetValue()) + lex2.GetStr();
                lex1.SetType(STR_CONST);
                lex1.SetString(str);
            } else if (tmp1 == STR_CONST && tmp2 == NUMB) {
                str = lex1.GetStr() + to_string(lex2.GetValue());
                lex1.SetString(str);
            } else throw "Executer: PLUS error";
            args.push(lex1);
            break;
        case PLUSEQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                cout << lex1.GetType() << " " << lex2.GetType() << endl;
                if(!SetBoolsI(tmp, lex2))
                if (Table[tmp].GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                    Table[tmp].SetString(Table[tmp].GetStr() + lex2.GetStr());
                } else if (lex1.GetType() == PADDRESS && lex2.GetType() == NUMB) {
                    Table[tmp].SetValue(Table[tmp].GetValue() + lex2.GetValue());
                } else throw "Executer: PLUS EQ error 1";
            } else throw "Executer: PLUS EQ error 2";
            DecIncSt();
            break;
        case TIMES: 
            Pop(args, lex1);
            Pop(args, lex2);
            SetBools(lex1, lex2);
            tmp1 = lex1.GetType();
            tmp2 = lex2.GetType();
            if (tmp1 == NUMB && tmp2 == NUMB) {
                tmp = lex1.GetValue() * lex2.GetValue();
                lex1.SetValue(tmp);
            } else throw "Executer: TIMES error";
            args.push(lex1);
            break;
        case TIMES_EQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if(!SetBoolsI(tmp, lex2))
                if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                    Table[tmp].SetValue(Table[tmp].GetValue() * lex2.GetValue());
                } else throw "Executer: TIMES EQ error";
            } else throw "Executer: TIMES EQ error";
            DecIncSt();
            break;
        case MINUS: 
            Pop(args, lex1);
            Pop(args, lex2);
            SetBools(lex1, lex2);
            if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                tmp = lex2.GetValue() - lex1.GetValue();
                lex1.SetValue(tmp);
            } else throw "Executer: MINUS error";
            args.push(lex1);
            break;
        case MINUS_EQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if(!SetBoolsI(tmp, lex2))
                if (lex1.GetType() == PADDRESS && lex2.GetType() == NUMB) {
                    Table[tmp].SetValue(Table[tmp].GetValue() - lex2.GetValue());
                } else throw "Executer: MINUS EQ error";
            } else throw "Executer: MINUS EQ error";
            DecIncSt();
            break;
        case SLASH: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if(lex2.GetValue() == 0 ) throw "Executer: Divide by zero";
                if (lex2.GetType()) {
                    tmp = lex1.GetValue() / lex2.GetValue();
                    lex1.SetValue(tmp);
                }
            } else throw "Executer: SLASH error";
            args.push(lex1);
            break;
        case SLASH_EQ:
            Pop(args, lex2);
            Pop(args, lex1);
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if(!SetBoolsI(tmp, lex2))
                if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                    if(lex2.GetValue() == 0 ) throw "Executer: Divide by zero";
                    if (!lex1.GetValue()) {
                        Table[tmp].SetValue(Table[tmp].GetValue() / lex2.GetValue());
                    }
                } else throw "Executer: SLASH EQ error";
            } else throw "Executer: SLASH EQ error";
            DecIncSt();
            break;
        case PERCENT:
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if(lex2.GetValue() == 0 ) throw "Executer: Divide by zero";
                if (!lex2.GetType()) {
                    tmp = lex1.GetValue() % lex2.GetValue();
                    lex1.SetValue(tmp);
                } 
            } else throw "Executer: PERCENT error";
            args.push(lex1);
            break;
        case PERCENT_EQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if(!SetBoolsI(tmp, lex2))
                if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                    if(lex2.GetValue() == 0 ) throw "Executer: Divide by zero";
                    if (!lex1.GetValue()) {
                        Table[tmp].SetValue(Table[tmp].GetValue() % lex2.GetValue());
                    } else throw "Executer: Divide by zero";
                } else throw "Executer: Percent EQ error";
            } else throw "Executer: Percent EQ error";
            DecIncSt();
            break;
        case EQ: 
            Pop(args, lex1);
            Pop(args, lex2);
            tmp = lex2.GetValue();
            if (lex2.GetType() == NUMB || lex2.GetType() == STR_CONST) {
                args.push(lex1); 
            } else if (lex1.GetType() == STR_CONST) {
                Table[tmp].SetString(lex1.GetStr());
                Table[tmp].SetType(STR_CONST);
            } else if (lex1.GetType() == NUMB) {
                Table[tmp].SetValue(lex1.GetValue());
                Table[tmp].SetType(NUMB);
            } else if (lex1.GetType() == TRUE)  Table[tmp].SetType(TRUE);
              else if (lex1.GetType() == FALSE) Table[tmp].SetType(FALSE);
            Table[tmp].SetAssign();
            DecIncSt();
            break;
        case PLEFTINC: 
            Pop(args, lex1);
            if (lex1.GetType() != PADDRESS) throw "Executer: LEFT INC error";
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if (Table[tmp].GetType() == NUMB) {
                    k = Table[tmp].GetValue();
                    Table[tmp].SetValue(k + 1);
                    args.push(Lex(NUMB, k + 1, elem.GetStr()));
                    IncSt.push(tmp);
                }
            } else throw "Executer: LEFT INC error";
            break;
        case PRIGHTINC: 
            Pop(args, lex1);
            if (lex1.GetType() != PADDRESS) throw "Executer: RIGHT INC error";
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if (Table[tmp].GetType() == NUMB) {
                    k = Table[tmp].GetValue();
                    args.push(Lex(NUMB, k, elem.GetStr()));
                    Table[tmp].SetValue(k + 1);
                }
            } else throw "Executer: RIGHT INC error";
            break;
        case PLEFTDEC:
            Pop(args, lex1);
            if (lex1.GetType() != PADDRESS) throw "Executer: LEFT DEC error";
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if (Table[tmp].GetType() == NUMB) {
                    k = Table[tmp].GetValue();
                    Table[tmp].SetValue(k - 1);
                    DecSt.push(tmp);
                    args.push(Lex(NUMB, k - 1, elem.GetStr()));
                }
            } else throw "Executer: LEFT DEC error";
            break;
        case PRIGHTDEC:
            Pop(args, lex1);
            if (lex1.GetType() != PADDRESS) throw "Executer: RIGHT DEC error";
            tmp = lex1.GetValue();
            if (Table[tmp].GetAssign()) {
                if (Table[tmp].GetType() == NUMB) {
                    args.push(Lex(NUMB, k = Table[tmp].GetValue(), elem.GetStr()));
                    Table[tmp].SetValue(k - 1);
                }
            } else throw "Executer: RIGHT DEC error";
            break;
        case NOT:
            Pop(args, lex1);
            if (lex1.GetType() == STR_CONST) throw "Executer: NOT Error";
            if (lex1.GetType() == NUMB) {
                tmp = lex1.GetValue();
                if (tmp == 1) lex1.SetType(FALSE);
                else if (tmp == 0) lex1.SetType(TRUE);
            } else if (lex1.GetType() == PADDRESS || lex1.GetType() == PLABEL) throw "Executer: NOT Error";
            if (lex1.GetType() == TRUE) lex1.SetType(FALSE);
            if (lex1.GetType() == FALSE) lex1.SetType(TRUE);
            args.push(lex1);
            break;
        case DPIPE:
            Pop(args, lex1);
            Pop(args, lex2);
            if (lex1.GetType() == NUMB) {
                if (lex1.GetValue() > 0) tmp = 1; else tmp = 0;
            } else if (lex1.GetType() == TRUE) tmp = 1;
            else if (lex1.GetType() == FALSE) tmp = 0;
            else throw "OR Error";
            if (lex2.GetType() == NUMB) {
                if (lex2.GetValue() > 0) tmp++;
            } else if (lex2.GetType() == TRUE) tmp++;
            else if (lex2.GetType() != FALSE) throw "Executer: OR Error";
            if (j == 0) lex1.SetType(FALSE);
            else lex1.SetType(TRUE);
            args.push(lex1);
            break;
        case DAMP: 
            Pop(args, lex1);
            Pop(args, lex2);
            if (lex1.GetType() == NUMB) {
                if (lex1.GetValue() > 0) tmp = 1; else tmp = 0;
            } else if (lex1.GetType() == TRUE) tmp = 1;
            else if (lex1.GetType() == FALSE) tmp = 0;
            else throw "Executer: AND Error";
            if (lex2.GetType() == NUMB) {
                if (lex2.GetValue() == 0) tmp = 0;
            } else if (lex2.GetType() == TRUE) tmp = 1;
              else if (lex2.GetType() == FALSE) tmp = 0;
              else throw "Executer: AND Error";
            if (tmp == 0) lex1.SetType(FALSE);
            else lex1.SetType(TRUE);
            args.push(lex1);
            break;
        case DEQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            if(!SetBools(lex1, lex2)) {
                if (lex1.GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                    if (lex1.GetStr() == lex2.GetStr()) lex1.SetType(TRUE);
                    else lex1.SetType(FALSE);
                } else if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                    if (lex1.GetValue() == lex2.GetValue()) lex1.SetType(TRUE);
                    else lex1.SetType(FALSE);
                } else throw "Executer: EQ error";
            }
            args.push(lex1);
            break;
        case LSS: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                if (lex1.GetStr() < lex2.GetStr()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if (lex1.GetValue() < lex2.GetValue()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else throw "Executer: LESS error";
            args.push(lex1);
            break;
        case GTR: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                if (lex1.GetStr() > lex2.GetStr()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if (lex1.GetValue() > lex2.GetValue()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else throw "Executer: GREATER error";
            args.push(lex1);
            break;
        case LEQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                if (lex1.GetStr() <= lex2.GetStr()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if (lex1.GetValue() <= lex2.GetValue()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else throw "Executer: LESS EQ error";
            args.push(lex1);
            break;
        case GEQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                if (lex1.GetStr() >= lex2.GetStr()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if (lex1.GetValue() >= lex2.GetValue()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else throw "Executer: GREATER EQ error";
            args.push(lex1);
            break;
        case NEQ: 
            Pop(args, lex2);
            Pop(args, lex1);
            SetBools(lex1, lex2);
            if (lex1.GetType() == STR_CONST && lex2.GetType() == STR_CONST) {
                if (lex1.GetStr() != lex2.GetStr()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else if (lex1.GetType() == NUMB && lex2.GetType() == NUMB) {
                if (lex1.GetValue() != lex2.GetValue()) lex1.SetType(TRUE);
                else lex1.SetType(FALSE);
            } else throw "Executer: NOT EQ error";
            args.push(lex1);
            break;
        case PWRITE: 
            Pop(args, lex1);
            if (lex1.GetType() == PADDRESS) {
                if (!(Table[lex1.GetValue()].GetStr().empty())) 
                    ToPrint.push_back(Table[lex1.GetValue()].GetStr());
                else ToPrint.push_back(to_string(Table[lex1.GetValue()].GetValue()));
            } else if (lex1.GetType() == STR_CONST) ToPrint.push_back(lex1.GetStr());
            else if (lex1.GetType() == TRUE) ToPrint.push_back("true");
            else if (lex1.GetType() == FALSE) ToPrint.push_back("false");
            else ToPrint.push_back(to_string(lex1.GetValue()));
            break;
        case PGETENV: 
            Pop(args, lex1);
            ToPrint.push_back(getenv(lex1.GetStr().c_str()));
            break;
        default:
            throw "Executer: unexpected element in POLIZ";
        }
        index++;
    }
    cout << BALD RED << "------------" << COLORENDS;
    cout << endl << BALD GREEN << "Printing:" << COLORENDS << endl;
    Print(ToPrint);
}

bool Executer::SetBools(Lex& lex1, Lex& lex2) {
    LexType tmp1, tmp2;
    tmp1 = lex1.GetType();
    tmp2 = lex2.GetType();
    if (tmp1 == TRUE) {
        lex1.SetValue(1);
        lex1.SetType(NUMB);
    } else if (tmp1 == FALSE) {
        lex1.SetValue(0);
        lex1.SetType(NUMB);
    } else if (tmp2 == TRUE) {
        lex2.SetValue(1);
        lex2.SetType(NUMB);
    } else if (tmp2 == FALSE) {
        lex2.SetValue(0);
        lex2.SetType(NUMB);
    } else return 0;
    return 1;
}

bool Executer::SetBoolsI(int tmp, Lex& lex2) {
    LexType tmp1, tmp2;
    tmp1 = Table[tmp].GetType();
    tmp2 = lex2.GetType();
    if (tmp1 == TRUE) {
        Table[tmp].SetValue(1);
        Table[tmp].SetType(NUMB);
    } else if (tmp1 == FALSE) {
        Table[tmp].SetValue(0);
        Table[tmp].SetType(NUMB);
    } else if (tmp2 == TRUE) {
        lex2.SetValue(1);
        lex2.SetType(NUMB);
    } else if (tmp2 == FALSE) {
        lex2.SetValue(0);
        lex2.SetType(NUMB);
    } else return 0;
    return 1;
}

void Executer::DecIncSt() {
    int IncDec;
    while (!IncSt.empty()) {
        IncDec = IncSt.top();
        IncSt.pop();
        Table[IncDec].SetValue(Table[IncDec].GetValue() + 1);
    }
    while (!DecSt.empty()) {
        IncDec = DecSt.top();
        DecSt.pop();
        Table[IncDec].SetValue(Table[IncDec].GetValue() - 1);
    }
}

/* Translator */

class Translator {
    Parser p;
    Executer E;
public:
    Translator() : p() {}
    Translator(const char* name) : p(name) {};
    void Translate ();
};

void Translator::Translate() {
    p.Analyze();
    cout << BALD RED << "------------\n" << COLORENDS;
    cout << BALD GREEN << "Executing: " << COLORENDS << endl;
    E.Execute(p.prog);
    cout << BALD RED << "\n------------" << COLORENDS;
    cout << endl << BALD GREEN << "All Good!!!" << COLORENDS << endl;
}

int main(int argc, char** argv, char** env) {
    //cout << env[3];
    cout << BALD RED << "\n------------\n" << COLORENDS;
    cout << BALD GREEN << "Lexemes:\n" << COLORENDS;
    Translator t;
    if (argc == 2) t = Translator(argv[1]);
    else if (argc == 1) t = Translator();

    try { t.Translate();}
    catch (char ch){
        string s;
        s.push_back(ch);
        Error("Wrong lexem", s);
        return 1;
    }
    catch (LexType type) {
        Error("Error type", to_string(type));
        return 2;
    }
    catch (Lex l) {
        Error("Wrong lexem", "");
        l.Print();
        return 3;
    }
    catch (const char* str) {
        Error("Error: ", str);
        return 4;
    }
    cout << BALD RED << "------------\n\n" << COLORENDS;
    return 0;
}
