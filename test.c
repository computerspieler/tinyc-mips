int test;


int square(int x)
{
	return x*x;
}

void print_string(int* s)
{
	__mips {
		"lw $a0, 0($a0)\n"
		"li $v0, 4\n"
		"syscall"
	};
}

void print_int(int x)
{
	__mips {
		"lw $a0, 0($a0)\n"
		"li $v0, 1\n"
		"syscall"
	};

	print_string("\n");
	return;
}

int read_int()
{
	int x;

	__mips {
		"or $s0, $zero, $fp\n"
		"li $v0, 5\n"
		"syscall\n"
		"sw $v0, 0($s0)\n"
	};

	return x;
}

void main3(int x) {
	print_string("\nEst ce que x est plus grand que 10 ? ");
	print_string(x > 10 ? "Oui\n" : "Non\n");
}

void func4(int x, ...)
{
	int *start = __varargs_start;
	
	print_int(*start); start = start + 1;
	print_int(*start); start = start + 1;
	print_int(*start); start = start + 1;
	print_int(*start);
}

int fib(int n) {
	if(n<=1)
		return n;
    return fib(n-1) + fib(n-2);
}
int syr(int n) {
	print_int(n);
	if(n==1) return 0;
	if(n%2) return syr(3*n+1);
	return syr(n/2);
}

void func5()
{
	int i = -10;
	while(i = i + 1)
	;
}

void main() {
	int x;

	print_string("x?\n");
	x = read_int();
	print_string("Value of x*x: ");
	print_int(square(x));

	func5();

	main3(x);
	func4(x, 1, 2, 3, 4);

	int v ;
	v = fib(10);
	print_int(v);
	print_int (syr(v));

	return;
}

int main2() {
    int a = 0;
    int b = 0;
    a && (b = 5);
    return b;
}
