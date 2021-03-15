#include<iostream>
using namespace std;

#define WHITE          "\x1b[38m"
#define CYAN           "\x1b[36m"
#define RED            "\x1b[31m"
#define BLUE           "\x1b[38;5;63m"
#define GREEN          "\x1b[32m"
#define PURPLE         "\x1b[38;2;190;82;125m"
#define COLORENDS      "\x1b[0m"

#define BALD           "\x1b[1m"


class Matrix {
    int** T;
    int m;
    int n;
    static int count;
public:
    Matrix(): m(0), n(0), T(NULL) { count++;}
    Matrix(int m1, int n1) {
        if ((m1 > 0) && (n1 > 0)){
            m = m1; n = n1;
            T = (int**) new int*[m];
            for(int i = 0; i < m; i++){
                T[i] = (int*) new int[n];
            }
            for (int i = 0; i < m; i++){
                for (int j = 0; j < n; j++){
                    T[i][j] = 0;
                }
            }
            count++;
        } else {
            cout << BALD RED "dimentions must be positive numbers\n" COLORENDS;
            exit(1);
        }
    }
    Matrix(const Matrix& a) { // copy constructor
        m = a.m; n = a.n;
        T = (int**) new int*[m];
        for (int i = 0; i < m; i++) {
            T[i] = (int *) new int[n];
        } 
        for (int i = 0; i < m; i++){
            for (int j = 0; j < n; j++) {
                T[i][j] = a.T[i][j];
            }
        }
        count++;
    }
    
    Matrix& operator=(const Matrix& a) { //assigment
        if (n > 0) for(int i = 0; i < m; i++) delete[] T[i];
        if (m > 0) delete[] T;
        m = a.m; n = a.n;
        T = (int**) new int*[m];
        for (int i = 0; i < m; i++) {
            T[i] = (int *) new int[n];
        }
        for (int i = 0; i < m; i++){
            for (int j = 0; j < n; j++) {
                T[i][j] = a.T[i][j];
            }
        }
        return *this;
    }

    Matrix operator+(const Matrix& a) {
        if (a.n != n || a.m != m) {
            cout << BALD RED "Cannot summ\n" COLORENDS;
            exit(2);
        }
        Matrix t(m, n);
        for (int i = 0; i < m; i++){
            for (int j = 0; j < n; j++){
                t.T[i][j] = T[i][j] + a.T[i][j];
            }
        }
        return t;
    }

    Matrix operator*(const Matrix& a) {
        if (n != a.m) {
            cout << BALD RED "Cannot multiply\n" COLORENDS;
            exit(2);
        }
        Matrix c(m, a.n);
        for (int i = 0; i < c.m; i++){
            for (int j = 0; j < c.n; j++) {
                for (int k = 0; k < m; k++){
                    c.T[i][j] += T[i][k] * a.T[k][j];
                }
            }
        }
        return c;
    }

    Matrix operator*(int z){
        Matrix c(m, n);
        for (int i = 0; i < c.m; i++){
            for (int j = 0; j < c.n; j++) {
                c.T[i][j] = z * T[i][j];
            }
        }
        return c;
    }

    friend Matrix operator*(const Matrix& a, int z) {
        return a*z;
    }

    int& operator()(int index1, int index2) {
        if ((index1 < m) && (index2 < n)) {
        return T[index1][index2];
        } else {
            cout << BALD RED "Index out of bounds\n" COLORENDS;
            exit(1);
        }
    }

    static int GetCount(){ //static
        return count;
    }

    friend ostream& operator<< (ostream &out, const Matrix &a) { // operator <<
            for(int i = 0; i < a.m; i++){
            for(int j = 0; j < a.n; j++){
                out << BALD GREEN << a.T[i][j] << COLORENDS << " ";  
            }
            out << endl;
        }
        out << endl;
        return out;
    }

    ~Matrix() { //destructor
        if (n > 0) for (int i =0; i < m; i++) delete[] T[i];
        if (m > 0) delete[] T;

        count--;
    }
};

int Matrix::count = 0;

int main(){
    
    cout << BALD BLUE "Enter m and n (m colums and n lines) for matrix a (enter n = m or multiplication won't work in my example)\n" COLORENDS;
    int n, m;
    cout << "m = "; cin >> m;
    cout << "n = "; cin >> n;
    Matrix a(n, m);
    cout << BALD BLUE "Enter a's numbers\n" COLORENDS;
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++){
            printf("a[%d][%d] = ", i, j);
            cin >> a(i, j);
        }
    }
    cout << BALD BLUE "a printed two times using << operator:\n" COLORENDS;
    cout << a << a;
    Matrix b;
    cout << BALD BLUE "Second matrix b that equals a + a \n" COLORENDS;
    b = a + a;
    cout << b;
    Matrix c;
    cout << BALD BLUE "Third matrix c that equals a * b \n" COLORENDS;
    c = a * b;
    cout << c;
    cout << BALD BLUE "Matrix a multiplied by 4 (a = a * 4) \n" COLORENDS;
    a = a * 4;
    cout << a;
    cout << BALD BLUE "Matrix b multiplied by 2 (b = 2 * b) \n" COLORENDS;
    
    b = b * 2;
    cout << b;
    
    cout << BALD BLUE "Static method that shows how much matrices created \n" COLORENDS;
    cout << "count = " << Matrix::GetCount();
    cout << endl;
    return 0;
}
