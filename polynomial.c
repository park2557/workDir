#include <stdio.h>
#include <stdlib.h>
#define COMPARE(x,y) (((x)<(y)) ? -1 : ((x)==(y)) ? 0 : 1)

#define MAX_TERMS 100

typedef struct {
	float coef;
	int expon;
} polynomial;

polynomial terms[MAX_TERMS] = {
	{3, 50}, {2, 3}, {-3, 2}, {1, 1}, {-5, 0}, 	// a식의 항들. 0~4
	{2, 4}, {3, 3}, {3, 2}, {5, 1}, {-1, 0}		// b식의 항들. 5~9
} ;

// polynomial terms[MAX_TERMS] = {
// 	{3, 50}, {2, 3}, {-3, 2}, // {1, 1}, {-5, 0}, 	// a식의 항들. 0~4
// 	{2, 4}, {3, 3}, {3, 2} //, {5, 1}, {-1, 0}		// b식의 항들. 5~9
// } ;
int avail = 0;

void attach(float coefficient,  int exponent) 
{
    // 다항식에 새로운 항을 추가하는 함수
    if (avail  >=  MAX_TERMS) {
        fprintf(stderr, "Too many terms in the polynomial \n.");
        exit(1);
	}
    terms[avail].coef = coefficient;
    terms[avail++].expon = exponent;	// avail은 여기에서 증가됨.
}

void padd(int startA, int finishA, int startB, int finishB, int *startD, int *finishD)	// d = a + b. 그런데 startd와 finishd는 왜 포인터일까?
{
	float coefficient;
	*startD = avail;	// avail은 terms[]에서 비어있는 공간의 색인
    while (startA <= finishA && startB <= finishB)
		switch (COMPARE(terms[startA].expon, terms[startB].expon)){
    		case -1: // a.expon < b.expon
              	attach(terms[startB].coef, terms[startB].expon);
               	startB++;  break;   // startb를 증가시키는 이유?
			case 0: // equal exponents
             	coefficient = terms[startA].coef + terms[startB].coef;
               	if (coefficient) 
				    attach(coefficient, terms[startA].expon);
               	startA++;  startB++;  // starta와 startb를 모두 증가
				break;
			case 1: //  a.expon > b expon
				attach(terms[startA].coef, terms[startA].expon);
				startA++;	// starta만 증가
		}

 		// a의 나머지 항들을 d에 모두 추가. 항이 없을 경우?
    	for( ; startA <= finishA; startA++ )
			attach(terms[startA].coef, terms[startA].expon);

    	// b의 나머지 항들을 d에 모두 추가.
    	for( ; startB <= finishB; startB++)
        	attach(terms[startB].coef, terms[startB].expon);       
    	*finishD = avail-1;		// avail-1에는 뭐가 들어 있을까?
}

void printPoly(char *p, int s, int f)
{
	printf("Poly %s : ", p);
	for (int i = s; i <= f; i++)
	{
		if (i == s)
			printf("%dx^%d", (int)terms[i].coef, terms[i].expon);
		else if (terms[i].coef > 0)
		{
			if (terms[i].expon == 0)
				printf(" + %d", (int)terms[i].coef);
			else if (terms[i].expon == 1)
				printf(" + %dx", (int)terms[i].coef);
			else
				printf(" + %dx^%d", (int)terms[i].coef, terms[i].expon);
		}
		else 
		{
			if (terms[i].expon == 0)
				printf(" - %d", (int)terms[i].coef*-1);
			else if (terms[i].expon == 1)
				printf(" - %dx", (int)terms[i].coef*-1);
			else
				printf(" - %dx^%d", (int)terms[i].coef*-1, terms[i].expon);
		}
	}
	printf("\n");
}

int main()
{
	int startA = 0, finishA = 4, startB = 5, finishB = 9;
	// int startA = 0, finishA = 2, startB = 3, finishB = 5;
	int startD, finishD; 
	avail = finishB+1;
	
	printPoly("A", startA, finishA);		// a식 출력
	printPoly("B", startB, finishB);		// b식 출력
	
	padd(startA, finishA, startB, finishB, &startD, &finishD);
	printPoly("D", startD, finishD);		// b식 출력
}