
void g(int i);

/* introduce indirection to x */
int x;

#define A(y) y + y
void main() {
    int y = A(x);
    g(y);
}