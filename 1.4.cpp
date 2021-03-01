#include <iostream>

using namespace std;

class A {
    int n;
public:
    A(const int k) {
        n = k;
    }
    int get() {
        return n;
    }
    const A& operator*=(const A& b) {
        n *= b.n;
        return *this;
    }
};

int main(){
    A a1 (5), a2 = 3;
    a1 *= 10;
    a2 *= a1 *= 2;
    cout << a1.get() << " " << a2.get() << endl;
    return 0;
}
