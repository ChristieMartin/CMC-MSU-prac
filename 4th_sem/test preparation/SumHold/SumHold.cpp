#include <iostream>
using namespace std;

class SumHold {
    int sumAccept;
    int sumReject;
    static int sumAll;
public:
    SumHold() : sumAccept(0), sumReject(0) {}
    ~SumHold() {}
    SumHold& operator<<(int n) {
        sumAll += n;
        if (Acceptable(n)){
            sumAccept += n;
        } else 
            sumReject += n;
        return *this;
    }
    int Get() const{
        return sumAccept;
    }
    int GetRejected() const{
        return sumReject;
    }

    static int GetAll(){
        return sumAll;
    }

    virtual bool Acceptable(int n) const {
        return true;
    }
};

int SumHold::sumAll = 0;

class SumHoldSmall : public SumHold {
public:
    SumHoldSmall() : SumHold() {}
    ~SumHoldSmall() {}
    virtual bool Acceptable(int n) const override {
        return ((n <= 10) && (n >= -10));
    }
};

int main() {
    cout << "Creating sh and sh2 of SumHold\n\n";
    SumHold sh;
    SumHold sh2;
    cout << "sh << 10 << 20 << 50\n";
    sh << 10 << 20 << 50;
    cout << "sh.Get() = "<< sh.Get() << endl << endl;

    cout << "sh2 << 20 << 100\n";
    sh2 << 20 << 100;
    cout << "sh2.Get() = "<< sh2.Get() << endl << endl;

    cout << "GetAll() = " << SumHold::GetAll() << endl << endl;

    cout << "sh.GetRejected() = " << sh.GetRejected() << endl;
    cout << "sh2.GetRejected() = "  << sh2.GetRejected() << endl << endl;

    cout << "Creating sh3 of SumHoldSmall\n\n";
    SumHoldSmall sh3;
    cout << "sh3 << 3 << 5 << 2 << 10 << 20 << 30\n\n";
    sh3 << 3 << 5 << 2 << 10 << 20 << 30;
    cout << "sh3.Get() = " << sh3.Get() << endl;
    cout << "sh3.GetRejected() = " << sh3.GetRejected() << endl;
    cout << "GetAll() = " << SumHoldSmall::GetAll() << endl;

}
