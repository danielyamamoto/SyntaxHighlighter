#include <stdio.h>
#include <stdlib.h>
int IsPrime(int x);

int IsPrime(int x)
{
  // Determine si x es un numero primo, si la funcion devuelve 0, significa que no es un numero primo, si devuelve 1, significa que es un numero primo
	
  int i,count=0;
    
    for(i=2;i<x;i++)
    {
		if(x%i==0)
        {
			count++;
            break;
        }
			
    }
    if(count==0)
    {
		return 1;
    }
	else
    {
		return 0;	
    }
}

int main()
{	
    int i,x,y,sum=0;
    printf("Input n:");
    scanf("%d",&x);
         if (x> 0) // Juicio x es mayor que 0
	{
     // 1 no es un numero primo, calcule de 2 an
		 para (i = 2; i <= x; i ++) 
		{	
			y=IsPrime(i);
			if(y==1)
            {
				sum=sum+i;
            }
		}
        printf("sum=%d\n",sum);	
	}
         else // menor que 0
    {
		printf("sum=%d\n",sum);
    }
    
	system("pause");
	return 0;
}