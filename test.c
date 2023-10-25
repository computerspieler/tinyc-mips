int test;

int square(int x)
{
	return x*x;
}

void print_int(int x)
{
	__mips {
		"lw $a0, 0($a0)\n"
		"li $v0, 1\n"
		"syscall"
	};

	return;
}

void print_string(int* s)
{
	__mips {
		"lw $a0, 0($a0)\n"
		"li $v0, 4\n"
		"syscall"
	};

	return;
}

void main() {
	int a;
	int* b;
	int** c;
	int*** d;
	int**** e;

//s
/*d
d
*/

	b = &a;
	c = &b;
	d = &c;
	e = &d;

	__mips { "\n" };
	
	d = &*d + 1;
	
	print_string("Value of 5*5: ");
	print_int(square(5));

	return;
}

