#include <iostream>
using namespace std;

class M {
    int* k;
public:
    M() {
        k = new int;
        *k = rand();
    }
    M(int n) {
        k = new int;
        *k = n;
    }
    ~M() {
        delete k;
    }
    friend bool operator==(const M &m, const M &n);
    int& operator()(const M &m){
        if(*this == m) {
            return *k;
        }
        M temp(0);
        return *(temp.Get());
    }
    int* Get() const {
        return k;
    }
};

bool operator ==(const M &m, const M &n) {
    return (&m == &n);
}

int main(){
    cout << "Creating M m(3) and M x(4)\n";
    M m(3);
    M x(4);
    cout << "m(m) = " << m(m) << endl;
    cout << "'m(m) = 9' \n";
    m(m) = 9;
    cout << "m(x) = "<< m(x) << endl;
    cout << "'m(x) = 2' \n";
    m(x) = 2;
    cout << "m(m) = " << m(m) << endl;
    cout << "x(x) = " << x(x) << endl;


    cout << endl;
    return 0;
}
