#Exam 1
a=5:14 #created sequence of 10 numbers starting at 5
length(a) # length=10
a[c(1,7)] #extracted the 1st and 7th values from object
b=5;c=11 #assigned extracted values a new object
c>b #greater than
b<c #less than
b>=c # greater than or equal to
a[c(2,6,9)] #extrcted the 2nd, 6th and9th values
x=6;y=10;z=13 #assigned extracted values a new object
((z+x)*(z+y))/2
10*(x-y)
# the 'R' operator for not is "!"
c%/%b # integer division of (11/5) 5 is in 11 twice, and you disregard the remainder so =2
c%%b # modulus of (11/5) this gives you the remainder,of 11/5 =2.2, which is rounded up to the nearest whole number =1
b^c # b(5) raised to the power of c(11)= 48828125

"%sal%"=function(r,t) {2*r+5*t}
4%sal%6 # working example of operator "sal"

55->p # created object with L to R operator
d=3;k=4;l=5 # created 3 new objects with numbers assigned in single line of code
d;k;l # print values of d, k and l with one line of code
0/0 #"NaN "is displayes because 0/0 is not a number
55^123445 #"Inf" because the value is too large to compute
-55^123445#"Inf" because the value is too large to compute
ser=c(1,2,3,4)
length(ser)=5
ser # created a vector "ser" and set the length to =5, when there are only 4 values, so when you ask R to show "ser" there is an NA for the 5th value

