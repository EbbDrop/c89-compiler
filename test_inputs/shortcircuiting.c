int a = 3;
printf(a);

(a = 0) && (a = 255);

// should print 0
printf(a);

(a = 2) || (a = 255);

// should print 2
printf(a);
