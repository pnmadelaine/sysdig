#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

int print(unsigned long int a){
	printf("%lu\n", a);
}

int main(int argc, char* argv[]){
	int _n = 50000000;
	// if (argc > 1) {
	// 	_n = (long int) argv[1];
	// } else {
	// 	_n = -1;
	// }

