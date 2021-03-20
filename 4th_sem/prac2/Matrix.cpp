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

void Error(string err, int num) {
    cout << BALD RED << err << COLORENDS << endl;
    exit(num);
}

class Matrix {
protected:
    int** T;
    int m, n;
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
        } else { Error("Dimentions must be positive numbers", 2);}
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

    ~Matrix() { //destructor
        if (n > 0) for (int i =0; i < m; i++) delete[] T[i];
        if (m > 0) delete[] T;
        count--;
    }
    
    int& operator()(int index1, int index2) {
        if ((index1 < m) && (index2 < n)) {
        return T[index1][index2];
        } else { Error("Index out of bounds", 3); exit(3);}

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


    static int GetCount(){ return count;}

    virtual bool isSquare() = 0;
};

int Matrix::count = 0;

class SqrMatrix: public Matrix {
public:
    SqrMatrix() : Matrix() {}
    SqrMatrix(int n) : Matrix(n, n) {}

    bool isSquare() override { return true;}

    SqrMatrix operator+(const SqrMatrix& a) {
        if (a.n != n) { Error("Cannot summ", 4);}
        SqrMatrix c(n);
        for (int i = 0; i < n; i++){
            for (int j = 0; j < n; j++){
                c.T[i][j] = T[i][j] + a.T[i][j];
            }
        }
        return c;
    }

    SqrMatrix operator*(const SqrMatrix& a) {
        SqrMatrix c(n);
        for (int i = 0; i < n; i++){
            for (int j = 0; j < n; j++) {
                for (int k = 0; k < n; k++){
                    c.T[i][j] += T[i][k] * a.T[k][j];
                }
            }
        }
        return c;
    }


    SqrMatrix operator*(const int z){
        SqrMatrix c(n);
        for (int i = 0; i < n; i++){
            for (int j = 0; j < n; j++) {
                c.T[i][j] = z * T[i][j];
            }
        }
        return c;
    }
    
    //friend SqrMatrix operator*(const SqrMatrix& a, int z) {};
    int GetDim() const{ return n;}
    
};

SqrMatrix operator*(const int z, SqrMatrix& a) { return a * z;}

class MatrixMN: public Matrix {
public:
    MatrixMN() : Matrix() {}
    MatrixMN(int m, int n) : Matrix(m, n) {}

    bool isSquare() override { return false;}

    MatrixMN operator+(const MatrixMN& a) {
        if (a.n != n || a.m != m) { Error("Cannot summ", 4);}
        MatrixMN c(m, n);
        for (int i = 0; i < m; i++){
            for (int j = 0; j < n; j++){
                c.T[i][j] = T[i][j] + a.T[i][j];
            }
        }
        return c;
    }

    MatrixMN operator*(const MatrixMN& a) {
        if (n != a.m) { Error("Cannot multiply", 5);}
        MatrixMN c(m, a.n);
        for (int i = 0; i < c.m; i++){
            for (int j = 0; j < c.n; j++) {
                for (int k = 0; k < m; k++){
                    c.T[i][j] += T[i][k] * a.T[k][j];
                }
            }
        }
        return c;
    }


    MatrixMN operator*(int z){
        MatrixMN c(m, n);
        for (int i = 0; i < c.m; i++){
            for (int j = 0; j < c.n; j++) {
                c.T[i][j] = z * T[i][j];
            }
        }
        return c;
    }

};

MatrixMN operator*(const int z, MatrixMN& a) { return a * z;}


int main(){
    
    int n = 2;
    int m = 3;
    SqrMatrix A(n), B(n);
    MatrixMN C(n, m);
    A(0, 0) = A(0, 1) = A(1, 0) = A(1, 1) = 1;
    B(0, 0) = B(0, 1) = B(1, 0) = B(1, 1) = 3;
    C(0, 0) = 1; C(0, 1) = 2; C(0, 2) = 3;
    C(1, 0) = 4; C(1, 1) = 5; C(1, 2) = 6;

    cout << BALD BLUE "Matrix C 3 x 2 printed using printf and ():\n" COLORENDS;

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < m; j++){
            printf("C[%d][%d] = ", i, j);
            cout << C(i, j) << endl;
        }
    }
    cout << BALD BLUE "Matrix C 3 x 2 printed using cout and <<:\n" COLORENDS;
    cout << C;
    cout << BALD BLUE "Square matrix A 2 x 2 printed using cout and <<:\n" COLORENDS;
    cout << A;
    cout << BALD BLUE "A * A :\n" COLORENDS << A * A;
    cout << BALD BLUE "Matrix B :\n" COLORENDS << B;
    cout << BALD BLUE "A * B :\n" COLORENDS << A * B;
    cout << BALD BLUE "A * 10 :\n" COLORENDS << A * 10;
    cout << BALD BLUE "10 * A :\n" COLORENDS << 10 * A;
    cout << BALD BLUE "C * 3 :\n" COLORENDS << C * 3;
    cout << BALD BLUE "4 * C :\n" COLORENDS << 4 * C;
    cout << BALD BLUE "Virtual bool method isSquare() that shows if the matrix square or not: A - " COLORENDS << A.isSquare() << BALD BLUE " C - " COLORENDS << C.isSquare() << endl;
    cout << BALD BLUE "Static valuable count that shows how much matrices created : " COLORENDS << Matrix::GetCount() << endl;

    return 0;
}
