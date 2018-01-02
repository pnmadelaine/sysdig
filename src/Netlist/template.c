#include <stdio.h>
#include <stdlib.h>

int printbl(int *t) {
	int length = sizeof(t) / sizeof(int);
	for (int i = 0; i < length; ++i)
	{
		printf("%d", t[i]);
	}
	printf("\n");
}
int* eslice(int* t, int i, int j) {
	int length = sizeof(t) / sizeof(int);
	if (i >= 0 && j < length && i <= j) {
		int newlength = j - i + 1;
		int* newt = NULL;
		newt = malloc(sizeof(int) * newlength);
		for (int k = 0; i < newlength; ++k)
		{
			newt[k] = t[k+i];
		}
		printbl(newt);
		return newt;
	}
}
int* econcat(int* a, int* b) {
	int sza = sizeof(a);
	int szb = sizeof(b);
	int sz = sza + szb;
	int lengtha = sza / sizeof(int);
	int lengthb = szb / sizeof(int);
	int* newt = NULL;
	newt = malloc(sz);
	for (int k = 0; k < lengtha; ++k)
	{
		newt[k] = a[k];
	}
	for (int k = 0; k < lengthb; ++k)
	{
		newt[k+lengtha] = b[k];
	}
	return newt;
}
int* eselect(int* t, int i) {
	int lengtha = sizeof(t) / sizeof(int);
	if (i >= 0 && i < lengtha) {
		int* newt = NULL;
		newt = malloc(sizeof(int));
		newt[0] = t[i];
		return newt;
	}
}
int* emux(int* a, int* b, int* c) {
	if (a[0]) {
		return b;
	} else {
		return c;
	}
}
int* enot(int* t) {
	int sz = sizeof(t);
	int* newt = NULL;
	newt = malloc(sz);
	int length = sz / sizeof(int);
	for (int k = 0; k < length; ++k)
	{
		newt[k] = ~t[k];
	}
	return newt;
}
int* eand(int* a, int* b) {
	int sza = sizeof(a);
	int szb = sizeof(b);
	if (sza == szb) {
		int length = sza / sizeof(int);
		int* newt = NULL;
		newt = malloc(sza);
		for (int k = 0; k < length; ++k)
		{
			newt[k] = a[k] & b[k];
		}
		return newt;
	}
}
int* eor(int* a, int* b) {
	int sza = sizeof(a);
	int szb = sizeof(b);
	if (sza == szb) {
		int length = sza / sizeof(int);
		int* newt = NULL;
		newt = malloc(sza);
		for (int k = 0; k < length; ++k)
		{
			newt[k] = a[k] | b[k];
		}
		return newt;
	}
}
int* exor(int* a, int* b) {
	int sza = sizeof(a);
	int szb = sizeof(b);
	if (sza == szb) {
		int length = sza / sizeof(int);
		int* newt = NULL;
		newt = malloc(sza);
		for (int k = 0; k < length; ++k)
		{
			newt[k] = a[k] ^ b[k];
		}
		return newt;
	}
}
int* enand(int* a, int* b) {
	int sza = sizeof(a);
	int szb = sizeof(b);
	if (sza == szb) {
		int length = sza / sizeof(int);
		int* newt = NULL;
		newt = malloc(sza);
		for (int k = 0; k < length; ++k)
		{
			newt[k] = ~ (a[k] & b[k]);
		}
		return newt;
	}
}
