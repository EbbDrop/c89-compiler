//output:
//2
//3
//5
//7
//11
//13
//17
//19
//23
//29
//31
//37

int main()
{
   int n;
   int i = 3;
   int count;
   int c;

   // number of prime numbers
   n = 12;

   if ( n >= 1 )
   {
      printf(2);
   }

   count = 2;
   while(count <= n){
      c = 2;
      while(c <= i - 1) {
         if ( i%c == 0 ){
            break;
         }
	c++;
      }
      if(c == i){
         printf(i);
         count++;
      }
      i++;
   }
}
