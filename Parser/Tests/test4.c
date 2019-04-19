// Testing recursive calls
#include <stdio.h>
 
int sum (int num)
{
    if (num != 0)
    {
        return (num % 10 + sum (num / 10));
    }
    else
    {
       return 0;
    }
}
 
int main()
{
    int num, result;

    result = sum(num);
	
    return 0;
}
 