
struct a {
    int b;
    int c;
};

int main() {
    a x;
    int y;
    y = x.b;
    y = x.c;
    x.b = y;
    x.c = y;
    return 0;
}