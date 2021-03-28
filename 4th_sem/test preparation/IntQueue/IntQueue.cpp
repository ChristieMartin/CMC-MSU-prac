#include <iostream>
using namespace std;

struct myList {
    int n;
    myList *next;
};

class QueueEmpty {};

class IntQueue {
    myList *l;
    int am;
public:
    IntQueue() {
        l = NULL;
        am = 0;
    }

    virtual bool Check(int k) const {
        return true;
    }

    IntQueue& operator<<=(int k) {
        if (Check(k)){
            am++;
            myList *temp = new myList;
            temp -> n = k;
            temp -> next = NULL;
            if (l == NULL) {
                l = temp;
            } else {
                myList *temp2 = l;
                while (temp2 -> next != NULL) temp2 = temp2 -> next;
                temp2 -> next = temp;
            }
        }
        return *this;
    }
    void operator>>=(int& k) {
        if (!l) throw QueueEmpty();
        k = l -> n; 
        myList *temp = l -> next;
        delete l;
        l = temp;
        am--;
    }
    int Amount() const {
        return am;
    }
    ~IntQueue() {
        while (l) {
            myList *temp = l -> next;
            delete l;
            l = temp;
        }
    }    
};

class EvenFilter : public IntQueue {
public:
    EvenFilter() : IntQueue() {}
    virtual bool Check(int k) const override {
        return !(k % 2);
    }
};

int main() {
    cout << "IntQueue q; int num;\n";
    IntQueue q;
    int num;
    cout << "q <<= 2; q <<= 3; q <<= 4;\n";
    q <<= 2;
    q <<= 3;
    q <<= 4;
    cout << "q >>= num;\n";
    q >>= num;
    cout << "num = " << num;
    cout << endl;
    cout << "q.Amount() = " << q.Amount();
    cout << endl;
    cout << "q >>= num;\n";
    q >>= num;
    cout << "num = " << num;
    cout << endl;
    cout << "q.Amount() = " << q.Amount();
    cout << endl << endl;
    cout << "EvenFilter e;\n";
    EvenFilter e;
    
    cout << "e <<= 2; e <<= 3; e <<= 4;\n";
    e <<= 2;
    e <<= 3;
    e <<= 4;
    cout << "e.Amount() = " << e.Amount() << endl;
    try {
        cout << "\nTrying to extract more numbers to get exception\n";
        cout << "q >>= num;\n";
        q >>= num;
        cout << "q >>= num;\n";
        q >>= num;
    } catch (QueueEmpty) {
        cout << "QueueEmpty() exception catched";
    }
    cout << endl;
    return 0;
}
