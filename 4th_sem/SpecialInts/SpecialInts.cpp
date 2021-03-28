#include <iostream>
using namespace std;

#define WHITE          "\x1b[38m"
#define CYAN           "\x1b[36m"
#define RED            "\x1b[31m"
#define BLUE           "\x1b[38;5;63m"
#define GREEN          "\x1b[32m"
#define PURPLE         "\x1b[38;2;190;82;125m"
#define COLORENDS      "\x1b[0m"

#define BALD           "\x1b[1m"

class BadRange {
    int index;
public:
    BadRange(int i): index(i) {}
    int Get() const { return index;}
};

class SpecialInts {
    int* M;
    int size, maxind;
public:
    SpecialInts() : maxind(0), size(8) {
        M = new int[size];
    }
    SpecialInts(int len) : maxind(0), size(len) {
        M = new int[size];
    }
    ~SpecialInts() {
        delete[] M;
    }
    int& operator[](int index);
    int Len() const { return maxind;}
    SpecialInts& operator,(int num);

private:
    void AddSize();
};

int& SpecialInts::operator[](int index) {
    if (index < 0 || index >= maxind) throw BadRange(index);
    return M[index];
}

SpecialInts& SpecialInts::operator,(int num) {
    if (maxind == size) AddSize();
    M[maxind] = num;
    maxind++;
    return *this;
}

void SpecialInts::AddSize() {
    int *temp;
    temp = new int[size * 2];
    for(int i = 0; i < maxind; i++) temp[i] = M[i];
    delete[] M;
    temp = M;
    size *= 2;
}

int main(){
    SpecialInts m;
    m, 5, 6;
    try {
        cout << "m[0] = " << m[0] << endl;
        cout << "m[1] = " << m[1] << endl;
        cout << "Len() : " << m.Len() << endl;
        cout << "m[2] = " << m[2] << endl;
    }
    catch(const BadRange &b) {
        cout << BALD RED << "BadRange: " << COLORENDS << b.Get() << endl;
    }
    return 0;
}
