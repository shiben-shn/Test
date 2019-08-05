#include <stdio.h>
#include <stdlib.h>
#include <error.h>
#include <asm-generic/errno-base.h>

int *queens;
int recursion = 0, level = 0, success = 0;
char *space;

void PrintBoard(int n)
	{
  int i, j;
  for(i = 0; i < n; i++)
	  {
		for(j = 0; j < n; j++)
 	  if(queens[j] == i) printf(" Q ");
	 	else if((i+j)&1) printf(" / "); else printf(" . ");
		printf("\n");
  	}
	}

int Safe(int r, int c, int n)
	{
  int i, j;
	if(queens[c] < n) return 0;
  for(i = 0; i < c; i++)
		if(queens[i] == r)	return 0;
		else if(abs(i-c) == abs(queens[i]-r)) return 0;
  return 1;
	}

#define Place(r,c,n) queens[c]=r
#define Remove(r,c,n) queens[c]=n

int NQueens(int n, int N)
	{
  int i;
  ++level;
  ++recursion;
  printf("%sCall %d Recursion Level %d\n",space+N+1-level,recursion,level);
	if(!(0 < n && n <= N)) error(-1,0,"Stack overflow?");
	for(i = 0; i < N; i++)
		if(Safe(i,N-n,N))
		{
		Place(i,N-n,N);
		printf("%sPlaced queen %d at %d %d\n",space+N+1-level,N+1-n,i,N-n);
		if(n == 1)
			{
			printf("\n\n\nSolution No. %d!\n",++success);
			PrintBoard(N);
			/* break; : if satisfied with the first solution*/
			Remove(i,N-n,N);
			printf("%sTo find more, Removed queen %d from %d %d\n",space+N+1-level,N+1-n,i,N-n);
			continue; /* !! to find more solutions! */
			}
		if(NQueens(n-1,N)) break;
		else
			{
			Remove(i,N-n,N);
			printf("%sRemoved queen %d from %d %d\n",space+N+1-level,N+1-n,i,N-n);
			}
		}
	if(i == N) printf("%sFound no suitable place for queen %d\n",space+N+1-level,N+1-n);
	printf("%sReturning from level %d\n",space+N+1-level,level);
	--level;
	return (i < N);
	}

int main(int argc, char *argv[])
	{
	int i, j, N;
	if(argc > 1)
		{
		N = atoi(argv[1]);
		if(N < 4 || N > 20) N = 4;
		}
	else N = 4;	
	queens = (int *)malloc(N*sizeof(int));
	space = (char *)malloc((N+1)*sizeof(char));
	if(queens == NULL)
		error(-1,ENOMEM,"Memory shortage");
	if(space == NULL)
		error(-1,ENOMEM,"Memory shortage");
	for(i = 0; i < N; i++)
		{
		queens[i] = N;
		space[i] = ' ';
		}
	space[N] = '\0';	
  NQueens(N,N);
  return 0;
	}
