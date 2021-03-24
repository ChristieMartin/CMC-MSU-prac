#include <iostream>
using namespace std;


class BadSegment {};

class BadAddition {
    float a1, b1, a2, b2;
public:
    BadAddition(float a, float b, float aa, float bb) : a1(a), b1(b), a2(aa), b2(bb) {}
    float GetA1() const { return a1;}
    float GetB1() const { return b1;}
    float GetA2() const { return a2;}
    float GetB2() const { return b2;}

};

class FloatPair {
    float x, y;
public:
    FloatPair(float a, float b) : x(a), y(b) {}
    float getX() const { return x;}
    float getY() const { return y;}

    virtual float Measure() const = 0;
};

class Segment: public FloatPair {
public:
    Segment(float a, float b) : FloatPair(a, b) {
        if (a > b) throw BadSegment();
    }
    virtual float Measure() const { return (this -> getY()) - (this -> getX());}
    Segment operator+(const Segment &seg) {
        if ((this -> getX() > seg.getY()) || (this -> getY() < seg.getX())) {
            throw BadAddition(this -> getX(), this -> getY(), seg.getX(), seg.getY());
        }
        Segment tempseg(min(this -> getX(), seg.getX()), max(this -> getY(), seg.getY()));
        return tempseg;
    }
};

int main(){
    try{
        Segment f(1, 2), g(0.5, 5), h(2.5, 6.5); 
        printf("%3.3f, %3.3f, %3.3f\n", (f + g).Measure(), (g + h).Measure(), (f + g + h).Measure()); 
        printf("%3.3f\n", (f + h).Measure());
    }
    catch(const BadAddition &bad) {
        printf("Bad addition: [%3.3f; %3.3f]+[%3.3f;%3.3f]\n", bad.GetA1(), bad.GetB1(), bad.GetA2(), bad.GetB2());
    }
    catch(BadSegment b) { 
        printf("Bad segment\n"); 
    }
    return 0;
}
