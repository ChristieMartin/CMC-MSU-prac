#! asdf

var a;
var b;

for (a = 0; a < 10; a++){
    write(" a = "); write(a);
    b = a;
    if (b == 3) {
        break;
    }
    write(" for ");
}

write(" ended ");
