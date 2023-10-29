int test;

void non_existant_function(int x, ...);
void print_string(void* s);

int square(int x)
{
	return x*x;
}

void print_int(int x)
{
	__asm (
		"lw $a0, 0($a0)\n"
		"li $v0, 1\n"
		"syscall"
	);

	print_string("\n");
	return;
}

void print_string(void* s)
{
	__asm (
		"lw $a0, 0($a0)\n"
		"li $v0, 4\n"
		"syscall"
	);
}

int read_int()
{
	int x;

	__asm (
		"li $v0, 5\n"
		"syscall\n"
		"sw $v0, -4($fp)\n"
	);

	return x;
}

void main3(int x) {
	print_string("Est ce que x est plus grand que 10 ? ");
	print_string(x > 10 ? "Oui\n" : "Non\n");
}

void func4(int x, ...)
{
	int *start = __varargs_start;
	
	// Affiche les 4 valeurs dans le varargs
    print_int(*start); start = start + 1;
    print_int(*start); start = start + 1;
    print_int(*start); start = start + 1;
    print_int(*start);
}

int fib(int n) {
	int x, y, z;
	
	x = 0;
	y = 1;

	while(n > 0) {
		z = x+y;
		x=y;
		y=z;
		n = n - 1;
	}

	return x;
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

void* sbrk(int n)
{
	void *output;

	__asm (
		// On met la valeur de n dans le registre a0
		"lw $a0, 0($a0)\n"
		
		"li $v0, 9\n"
		"syscall\n"
		"sw $v0, -4($fp)\n"
	);

	return output;
}

int main() {
    int x;
	int *y = 0;

	print_string("x?\n");
	x = read_int();
	print_string("Value of x*x: ");
	print_int(square(x));

	func5();

	y = sbrk(10 * sizeof(int));
	
	y[0] =  0;
	y[1] = 10;
	y[2] = 20;
	y[3] = 30;
	y[4] = 40;

	main3(x);
	func4(x, 1, 2, 3, 4);

	int v ;
	v = fib(x);
	print_string("Fib(x)=");
	print_int(v);
	print_string("syr(x)=\n");
	print_int(syr(x));

	print_string ("The size in byte of int: ");
	print_int (sizeof(int));
	print_string ("The size in byte of a pointer: ");
	print_int (sizeof(void*));
	print_string ("The size of the variable y: ");
	print_int (sizeof(y));
	print_int(10*y[4]);

	for(int a = 0; a < 10; a = a+1)
		print_int(a);

	{
		int z;
		z = 8;
	}

	x = y;
	print_int(x);
	x = &y[0];
	print_int(x);

	return 1;
}

int main2() {
    int a = 0;
    int b = 0;
    a && (b = 5);
    return b;
}
