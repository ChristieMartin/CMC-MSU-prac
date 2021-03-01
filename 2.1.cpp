#include <iostream>

using namespace std;


class Cls {
    int i;
    void operator=(const Cls& c){};
public:
    Cls() { i = 1; }
};

void f(Cls * p, Cls * q) {
    *p = *q;
}

int main(int argc, char const *argv[])
{
    return 0;
}
