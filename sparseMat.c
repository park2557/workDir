#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#define MAX_COL  50         /*  maximum number of column + 1 */
#define MAX_TERMS 101
#define COMPARE(x, y) (((x)<(y)) ? -1 : ((x)==(y)) ? 0 : 1)

typedef struct {
	int	row;
	int	col;
	int	value;
} term;
typedef enum shape {SPARSE, MATRIX} printShape;

void printMatrix(term t[], printShape shape, char *title)
{
	int n = t[0].value;
	int i, len;
	char *line = "---------------", tLine[32];
	
	len = strlen(line);
	sprintf(tLine, "%s: %s", title, line);
	
	if (shape == SPARSE)	// 희소행렬 형태로 출력
	{
		printf("%.*s\n", len, tLine);
		for (i=0; i<=n; i++)
			printf("%2d | %d, %d, %d\n", i, t[i].row, t[i].col, t[i].value);
		printf("%s\n", line);
	}
	else if (shape == MATRIX)  // 일반 행렬 형태로 출력
	{
		int j, p=1;
		printf("%s:\n", title);
		for (i=0; i<t[0].row; i++)
		{
			for (j=0; j<t[0].col; j++)
				if (t[p].row == i && t[p].col == j)
					printf("%4d", t[p++].value);
				else
					printf("%4d", 0);
			printf("\n");
		}
		printf("\n");
	}
}

void fast_transpose(term a[], term b[])
{
/*  the transpose of a is placed in b */ 
	int  row_terms[MAX_COL], starting_pos[MAX_COL];
   	int  i, j, num_cols = a[0].col, num_terms = a[0].value;
   	b[0].row = num_cols; b[0].col = a[0].row;
   	b[0].value = num_terms;
   	if (num_terms > 0)     /*  nonzero matrix  */
	{
		for (i = 0; i < num_cols; i++)
			row_terms[i] = 0;
		for (i = 1; i <= num_terms; i++)
			row_terms[a[i].col]++;
		starting_pos[0] = 1;
		for (i = 1; i < num_cols; i++)
			starting_pos[i] = starting_pos[i-1] + row_terms[i-1];
		for (i = 1; i <= num_terms; i++)
		{
			j = starting_pos[a[i].col]++;
			b[j].row = a[i].col;
			b[j].col = a[i].row;
			b[j].value = a[i].value;
		}
	}
}

void storesum(term d[], int *totald, int row, int column, int *sum)
{
	/* if *sum != 0, then it along with its row and column
	position is stored as the *totald+1 entry in d */
	if (*sum)
		if (*totald < MAX_TERMS)
		{
			d[++*totald].row = row;
			d[*totald].col = column;
			d[*totald].value = *sum;
			*sum = 0;
		}
		else 
		{
			fprintf(stderr, "Numbers of terms in product exceeds %d\n", MAX_TERMS);
			return;
		}
}

void mMult(term a[], term b[], term d[]) 
{	// multiply two sparse matrics
	int i, j, column, totald = 0;
	int rows_a = a[0].row, cols_a = a[0].col, totala = a[0].value;
	int cols_b = b[0].col, totalb = b[0].value;
	int row_begin = 1, row = a[1].row, sum = 0;
	term newB[MAX_TERMS];
	if (cols_a != b[0].row) {
		fprintf(stderr, "Incompatible matricses for MMult\n"); 
		exit(0);
	}
	fast_transpose(b, newB);

	/* set boundary condition */
	a[totala+1].row = rows_a;
	newB[totalb+1].row = cols_b;
	newB[totalb+1].col = 0; 

	for (i = 1; i <= totala; ) 
	{
		column = newB[1].row;
		for (j = 1; j <= totalb+1; ) 
		{
			// multiply a's row by b's column
			if (a[i].row != row) 
			{
				storesum(d,&totald,row,column,&sum);
				i = row_begin;
				for (; newB[j].row == column; j++) 
					;
				column = newB[j].row;
			}
			else if (newB[j].row != column) 
			{
				storesum(d,&totald,row,column,&sum);
				i = row_begin;
				column = newB[j].row;
			}
			else switch (COMPARE(a[i].col, newB[j].col))
			{
				case -1: /* go to next term in a */
					i++;	
					break;
				case 0: 
				/* add terms, go to next term in a and b */
					sum += (a[i++].value * newB[j++].value); 
					break;
				case 1: /* advance to next term in b */
					j++;
			}
		} // end of J <= totalb+1
		for (; a[i].row == row; i++) 
			;
		row_begin = i;	
		row = a[i].row;
	} /* end of for I <= totala */
	d[0].row = rows_a;
	d[0].col = cols_b; 
	d[0].value = totald;
}

void mAdd(term a[], term b[], term d[])
{	// multiply two sparse matrics
	int i = 1, j = 1, sum = 0, totald = 0;
	
	if (a[0].row != b[0].row || a[0].col != b[0].col)
	{
		fprintf(stderr, "Incompatible matricses for mAdd\n"); 
		exit(0);
	}
	while(i <= a[0].value && j <= b[0].value)
		switch(COMPARE(a[i].row, b[j].row))
		{
			case -1: 
				sum = a[i].value;
				storesum(d, &totald, a[i].row, a[i].col, &sum);
				i++; break;
			case  0:
				switch(COMPARE(a[i].col, b[j].col))
				{
					case -1:
						sum = a[i].value;
						storesum(d, &totald, a[i].row, a[i].col, &sum);
						i++; break;
					case  0:
						sum = a[i].value + b[j].value;
						if (sum)
							storesum(d, &totald, a[i].row, a[i].col, &sum);
						i++; j++; break;
					case  1:
						sum = b[j].value;
						storesum(d, &totald, b[j].row, b[j].col, &sum);
						j++;
				}
				break;
			case  1:
				sum = b[j].value;
				storesum(d, &totald, b[j].row, b[j].col, &sum);
				j++;			
		}
	for ( ; i <= a[0].value; i++)
		storesum(d, &totald, a[i].row, a[i].col, &a[i].value);
	for ( ; j <= b[0].value; j++)
		storesum(d, &totald, b[j].row, b[j].col, &b[j].value);
	d[0].row = a[0].row;
	d[0].col = a[0].col; 
	d[0].value = totald;
}

void main()
{
	term d[MAX_TERMS];
	term a[MAX_TERMS] = 
			   {{6,6,8},
				{0,0,15},
				{0,3,22},
				{0,5,-15},
				{1,1,11},
				{1,2,3},
				{2,3,-6},
				{4,0,91},
				{5,2,28}};
	term b[MAX_TERMS] = 
			   {{6,2,5},
				{0,0,1},
				{1,1,1},
				{2,0,3},
				{3,1,1},
				{4,0,2}};

	printf("\n***** Sparse Matrix Multiplication *****\n");
	/* Debug */printMatrix(a, SPARSE, "A");		 // SPARSE: 희소 행렬 형태로 출력
	/* Debug */printMatrix(b, SPARSE, "B");
	mMult(a, b, d);
	/* Debug */printMatrix(d, SPARSE, "D=AxB");
	/* Debug */printMatrix(d, MATRIX, "D=AxB");	 // MATRIX: 일반 행렬 형태로 출력
	
	// printf("\n***** Sparse Matrix Addition *****\n");
	// fast_transpose(a, b);	// b행렬을 a를 전치하여 생성
	// /* Debug */printMatrix(a, SPARSE, "A");
	// /* Debug */printMatrix(b, SPARSE, "B=A.T");  
	// mAdd(a, b, d);
	// /* Debug */printMatrix(d, SPARSE, "D=A+B");
	// /* Debug */printMatrix(d, MATRIX, "D=A+B"); 
}