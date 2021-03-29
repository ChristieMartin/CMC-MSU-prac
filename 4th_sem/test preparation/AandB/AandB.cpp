#include <iostream>
using namespace std;

class A;

class B {
public:
    B() {}
    ~B() {}
    
    int operator[](const A& a){
        return 75;
    }
    int operator() (const A& a){
        return 200;
    }
};

class A {
public:
    A() {}
    ~A() {}
    int operator[](const B& b) {
        return 75;
    }
    int operator() () {
        return 50;
    }
    int operator() (const B& b) {
        return 100;
    }
    int operator() (const B& b1, const B& b2) {
        return 150;
    }
    int operator() (const A& a1, const A& a2, const A& a3) {
        throw A();
    }
    int operator--() {
        return 500;
    }
    int operator--(int) {
        return 501;
    }
};



int main() {
    A a;
    B b;
    cout << "a[b] = " << a[b] << endl;
    cout << "b[a] = " << b[a] << endl;
    cout << "a() = " << a() << endl;
    cout << "a(b) = " << a(b) << endl;
    cout << "a(b, b) = " << a(b, b) << endl;
    cout << "b(a) = " << b(a) << endl;
    try {
        cout << "a(a, a, a) = " << a(a, a, a) << endl;
    } catch (A) {
        cout << "exteption type A caught\n";
    }
    cout << "--a = " << --a << endl;
    cout << "a-- = " << a-- << endl;
    return 0;
}
