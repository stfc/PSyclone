#include <stdio.h>

int python_caller(int *n)
{
	printf("Python caller: n=%d\n", *n);
	return *n+1;
}