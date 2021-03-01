#include <iostream>

using namespace std;

class A {
    int n;
public:
    A() : n(7) {}
    A(int k) : n(k) {}
    int get() {
        return n;
    }
    const A& operator*=(const A& b){
        n *= b.n;
        return *this;
    }
};

int main(){
    A a1(5), a2 = 4, a3;
    a2 *= a1 *= 3;
    cout << a1.get() << ' ' << a2.get() << ' ' << a3.get() << endl;
    return 0;
}