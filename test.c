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

	{
		return ;
	}

	return;
}

void print_string(int* s)
{
	__mips {
		"lw $a0, 0($a0)\n"
		"li $v0, 4\n"
		"syscall"
	};
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

void main() {
	int x;

	print_string("x?\n");
	x = read_int();
	print_string("Value of x*x: ");
	print_int(square(x));

	return;
}

