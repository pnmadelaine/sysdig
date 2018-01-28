#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/types.h>
#include <time.h>

void* threadSimulator (int* a);
void* threadPrinter (int* a);

int print(unsigned long int a){
	printf("%lu\n", a);
}

int f(unsigned long int t0, unsigned long int t1, unsigned long int t2, unsigned long int t3, unsigned long int t5, unsigned long int t6){
	printf("%c[2K\r", 27);
	printf("%02lu:%02lu:%02lu, %02lu/%02lu/%04lu", t2, t1, t0, t3, t5, t6);
	fflush(stdout);
}

int main(){
