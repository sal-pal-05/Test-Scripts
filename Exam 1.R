#Exam 1

#Operators

a=5:14 #created sequence of 10 numbers starting at 5
length(a) # length=10
b=a[1];c=a[7] #extracted the 1st and 7th values from object and assigned the extracted values each a new object
b;c 
c>b #greater than=TRUE
b<c #less than=TRUE
b>=c # greater than or equal to=FALSE
x=a[2];y=a[6];z=a[9] #extrcted the 2nd, 6th and9th values and assigned the extracted values each a new object
x;y;z 
((z+x)*(z+y))/2
10*(x-y)
# the 'R' operator for not is "!"
c%/%b # integer division of (11/5) 5 is in 11 twice, and you disregard the remainder so =2
c%%b # modulus of (11/5) this gives you the remainder,of 11/5 =2.2, which is rounded up to the nearest whole number =1
b^c # b(5) raised to the power of c(11)= 48828125

"%sal%"=function(r,t) {2*r+5*t} # created operator "%sal%"
4%sal%6 # working example of operator "sal"

55->p # created object with L to R assignment operator
d=3;k=4;l=5 # created 3 new objects with numbers assigned in single line of code
d;k;l # print values of d, k and l with one line of code
0/0 #"NaN "is displayes because 0/0 is not a number
55^123445 #"Inf" because the value is too large to compute
-55^123445#"Inf" because the value is too large to compute
ser=c(1,2,3,4)
length(ser)=5
ser # created a vector "ser" and set the length to =5, when there are only 4 values, so when you ask R to show "ser" there is an NA for the 5th value


#conditional Statements

animals=function (x){
  if (x=="a")
    "alligator"
  else if (x=="b")
    "buck"
  else if (x=="c")
    "carocodile"
  else "tiger"
}
animals(x="a")
animals(x="b")
animals(x="c")
animals(x="y")


#Object Data Types

letters=as.factor(c('a','b','c','d')) #created factor data type (with 4 levels)
class(letters) # reveals the type of data of"letters"=factor
str(letters) # reveals the number of levels of the factor"letters"=4 levels
char=as.character(c("ant","dog","toy","yam"))
class(char)# reveals the type of data of"char"=character
int=as.integer(c(1, 2, 3, 4))# reveals the type of data of"int"=integer
class(int)
sa=as.double(c(3,4,5)) # created double-numeric
typeof(sa) #reveals data type as"double"
str(letters)

#Data Storage Structures

arr.1=array(data=1:24, dim= c(4, 3, 2)) # create array with (4, 3 ,2)
arr.1

matr= matrix (data= 1:10, nrow=5, ncol=2) # created matrix with 5 rows, and 2 columns
matr
# the array is 3D where as the matrix is 2D, so the data in an array can be split-u into multiple "chunks"
mat4=matr[4,1] # created an object"mat4' that extracts the 4th value from the 1st column in the matrix "matr"
mat4

arr.1[2,2,2] #extracted 2nd value from 2nd column, from 2nd level in my array

#Working With Data Sets

load(file="test1_data.Rdata") #load data frame
nrow(d) #reveal the number of rows in the dataset=503441
ncol(d)#reveal the number of columns in the dataset=18
class(d$transect.id) # reveal the data type of only the "transect.id" column="character"
str(d) # numeric (num), factor (Factor), integer(int), POSIXct, and character

d$tow=as.factor(d$tow) # changed tow field from character to factor
class(d$tow) # reveal data type of "tow"="factor"

d$haul=as.integer(d$haul)# changed "haul" field from numeric to integer
class(d$haul) # reveal data type of "haul"="integer"
d$sw.density=NULL # removed the "sw.density" column from the data frame
str(d)





