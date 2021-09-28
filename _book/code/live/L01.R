# note: r vs rstudio, rstudio panes

# comments
# (Rstudio) cmd+shift+c  [windows users: cmd == ctrl?]

# basic math operators (+ - * / ^ %%)

2*2
3^2
8 %% 3

# basic function syntax (place, name args)

4 ^ (1/2)

sqrt(x = 4)

log(8, 2)
log( x = 8, base = 2)

# (Rstudio) tab completion

# (Rstudio) argument suggestion (base 3 log)

log(x = 8, base = 2)

# suggestion: if function unfamiliar/ambiguous use named arguments.

# help
?log
??log

# syntax, errors
execute
execute()

# environment
ls()

# variables (assignment)
x = 4

# variable types
a = 3
b = "3"
mode(a)
mode(b)
typeof(a)
typeof(b)
str(a)
str(b)
a*2
b*2
as.numeric(b)
as.numeric(b)*2
as.numeric('three')

# peculiar types from peculiar math / data
3/0
Inf/Inf
0/0
NA
sqrt(-2) 
# if you want complex numbers:  sqrt(as.complex(-2))
# but you probably dont

# logical comparisons
x = 3
x == 3
x > 3
x >= 3
x < 3
x <= 3
x != 3

(x <= 3)*4
TRUE*4
FALSE*4
'TRUE'*4


# boolean operators
x > 3
!(x > 3)
x > 3 | x == 3  
x > 3 & x == 3
# | and ||  & and && for vectors.
c(T, F) | c(F, F)
c(F, T) || c(F, F)

# vectors
a = c(8, 6, 7, 5, 3, 0, 9)  
# ...don't override "c"
b = 8675309
str(a)
str(b)
length(a)
length(b)

# vectors: indexing, assigning, growing, deleting
a[1]
a[c(1:3, 5)]
a[-1]
a
a[1] = 4
a
a = c(a, 100)
a
a = a[-length(a)]
a
a[100] = 100
a

# vector building functions
seq(1, 10)
1:10
seq(from = 1, to = 10, by = 1)
seq(1, 10, by = 1/3)

rep(1:3, 2)
rep(1:3, times = 2)
rep(1:3, each = 2)

letters
LETTERS

# vectors, logic
a = c(8, 6, 7, 5, 3, 0, 9)  
a >= 6
a[!(a >= 6)]
which(a >= 6)
((a %% 2) == 0)

((a %% 2) == 0) & (a >= 6)
((a %% 2) == 0) | (a >= 6)

# double logical operators...
((a %% 2) == 0) && (a >= 6)
((a %% 2) == 0) || (a >= 6)
# to avoid confusion: avoid || and && (may be helpful for checking if statements)

# vectors are of the same type, and R "helps" you with this:
a.char.vector = c(8, 6, 7, 5, "3", 0, 9)
a.char.vector
sum(a.char.vector)
# can be fixed...
a = as.numeric(a.char.vector)
sum(a)
# but may yield NAs, and unintended behavior
a.char.vector = c(8, 6, 7, 5, "w3", 0, 9, "three", "3w", "3e3")
a = as.numeric(a.char.vector)
# if you get the NA coercion warning, figure out why
is.na(a)
a.char.vector[is.na(a)]
# much more on varieties of this in data cleaning.

# default R behavior when reading data is even more prone to weirdness:
read.vector = as.factor(c(8, 6, 7, 5, "3", 0, 9))
as.numeric(read.vector)
#... oops, wtf?
as.numeric(as.character(read.vector))
# better.  convert to characters first, unless you want factor numbers

# warning... you could do something stupid if you try just a bit.
a = c('cat', 'dog', 'squirrel', 'dog', 'cat', 'cat')
mean(a)
a = as.factor(a)
mean(a)
a = as.numeric(a)
mean(a)

# factors
b = "3"
b2factor = as.factor(b)
b2factor2numeric = as.numeric(b2factor)
b2factor2character2numeric = as.numeric(as.character(b2factor))




# vector implicit expansion
a = c(1, 2, 3, 1, 2, 3)
a == c(1, 2)
# wtf?
c(1, 2)
a
rep(c(1, 2), 3)
#.... dont do this.
# also... what was the intent?
# perhaps... %in%?
a %in% c(1, 2)
# this happens with all operators...
c(1, 2, 3, 1, 2, 3) * c(1, 2)
# my suggestion: do not use it intentionally (rep stuff yourself, for clarity)!


# aggregation functions
a = c(8, 6, 7, 5, 3, 0, 9)  
a >= 6
any(a >= 6)
all(a >= 6)
sum(a)
mean(a)

# influence of weird types:
a = c(1, 2, 3, 4, 3/0)
sum(a)
a = c(1, 2, 3, 4, 0/0)
mean(a)
a = c(1, 2, 3, 4, NA)
sum(a)
sum(a, na.rm = TRUE)

# functions
fx = function(x) {
  return(x^2)
}
fx
fx(4)

# return explicitly to avoid confusion
fx = function(x){ x^2 } # allowed, concise
# variable scope
fx = function(x){
  a = 2
  x = x+a
  return(x^2)
}
a = 3
x = 4
fx(x)
a
x

# global variables show up in functions... 
a = 3
x = 4
fx = function(x){
  print(sprintf('a=%d, x=%d', a, x))
  a = 2
  print(sprintf('a=%d, x=%d', a, x))
  x = x+a
  print(sprintf('a=%d, x=%d', a, x))
  return(x^2)
}
print(sprintf('a=%d, x=%d', a, x))
fx(5)
# to make life easy:
# - don't rely on global variables inside functions (sometimes very handy, so minimize this at least)
# - don't try to set global variables in functions (<<- will do it though)


# loops
for(i in 1:10) {
  print(i^2)
}
# while(){} also available

# conditionals
x = 3
if(x != 5){
  print(x)
} else {
  print('meow')
}

for(i in 1:10){
  if(i != 5){
    print(i)
  } else {
    print('meow')
  }
}

# fizz buzz?
for(i in 1:15){
  if((i %% 3 ==0) & (i %% 5 == 0)){
   print('fizzbuzz') 
  } else if(i %% 5 == 0){
    print('buzz')
  } else if(i%%3 == 0){
    print('fizz')
  } else {
    print(i)
  }
}


# vector operations are fast, loops are slow
n = 1:1000000
start.time = proc.time()
for(i in 1:length(n)){
  n[i] = n[i]^2
}
proc.time() - start.time

start.time = proc.time()
n = n^2
proc.time() - start.time

# ... but don't worry about optimization until you need to.




# lists
a = list(1:3, letters[1:4])
str(a)
x <- a[1]
y <- a[[1]]
names(a)
names(a) <- c('x', 'y')
a
a = list(x = 1:3, 
         y = letters[1:4])
a$x
a[['x']]


# data frames
a = data.frame(x = 1:3, y = letters[1:4])
a = data.frame(x = 1:3, y = letters[1:3])
a
a$x
a[[1]]
a[['x']]
a[1]
a[1,]
a[,1]
a[,'x']
a[1,2]
a[1,'y']
a$y[1]
# oy!
# tomorrow: an opinionated approach from tidyverse that yields more readable code.

# data frame building:
expand.grid(suit = c('diamonds', 'clubs', 'hearts', 'spades'), value = c(2:10, 'jack', 'queen', 'king', 'ace'))

# built in dataset...
mtcars
str(mtcars)
summary(mtcars)
hist(mtcars$mpg)
plot(mtcars$mpg, mtcars$hp)
# we will use ggplot, but these are useful for quick plotting.

# tibbles from tidyverse -- slightly handier data.frames

# reading data, working with the file system.
# download... vulstats.ucsd.edu/data/fifa-2019.csv
# but where is it?
getwd()
setwd()

fifa = read.csv('../../fifa-2019.csv')
fifa = read.csv('~/Downloads/fifa-2019.csv')
fifa = read.csv('http://vulstats.ucsd.edu/data/fifa-2019.csv')
# apologies to those on windows... \\
# for homeworks, files to be loaded will be in the same directory, to avoid path separator confusion
# also:
.Platform$file.sep
file.path('..', '..', 'data', 'fifa-2019.csv')

# read.csv has annoying defaults.  tidyverse read_csv is better.
fifa.tibble = readr::read_csv('http://vulstats.ucsd.edu/data/fifa-2019.csv')
# :: indicates function inside library.

