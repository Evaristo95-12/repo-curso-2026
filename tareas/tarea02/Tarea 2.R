#r syntax
## ejercicio 1
### Insert the missing part of the code below to output "Hello World!"
"Hello word! "
## ejercicio 2
### In R, you can also use a function to output code. Fill in the missing part to output "Hello World!"
print("Hello word! ")
## ejercicio 3
###Comments in R are written with a special character. Insert the missing part
### La respuesta en numeral, la escribo porque no se como ponerla sin que la tome como inicio de comentario


# R Variable
## ejercicio 1
### Create a variable named carName and assign the value Volvo to it.
carName <- "Volvo"
## ejercicio 2
### Create a variable named maxSpeed and assign the value 120 to it.
maxSpeed <- 120
## ejercicio 3
### Use the correct function to combine the text "Hello" with the txt variable, to output "Hello World!"
txt <- "World!"
paste("Hello", txt)
## ejercicio 4 
###Display the sum of 5 + 10, using two variables: x and y
x <- 5
y <- 10
x+y
## ejercicio 5
### Assign the value "Orange" to multiple variables in one line:
fruit1 <- fruit2 <- fruit1 <- "Orange"


# R Data Types
## ejercicio 1
### What data type is myVar?
myVar <- 30
### Numeric
## ejercicio 2
### Which function can be used to check the data type of a variable?
x <- 10.5
class(x)


# R Math
## ejercicio 1
### Use the correct function to find the lowest number in a set.
min(5, 10, 15)
## ejercicio 2
### Use the correct function to find the square root of the number 16.
sqrt(16)


#R Strings
## ejercicio 1
### Create a str variable with the value "Hello"
str <- "Hello"
## ejercicio 2
### Use the correct function to find the number of characters in the str variable:
str <- "Hello World!"
nchar(str)
## ejercicio 3
### Use the correct function to check if the character "H" is present in the str variable:
str <- "Hello World!"
grepl("H", str)
## ejercicio 4
### Use a function to combine (concatenate) the two strings:
str1 <- "Hello"
str2 <- "World"
paste(str1,str2)



#R Blooleans
## ejercicio 1
### What is the output of the following code?
10 < 9
### False
## ejercicio 2
###Fill in the missing part to compare the two variables:
a <- 10
b <- 9
a > b
## ejercicio 3
### Fill in the missing part to print the value TRUE:
a <- 10
b <- 9
a > b ### true


#R Operators
## ejercicio 1
### Multiply 10 with 5:
10*5
## ejercicio 2
###Divide 10 by 5:
10/5
## ejercicio 3
### Use the "equal" operator to compare two values:
5==5



#R if...else
## ejercicio 1
### Print "Hello World" if a is greater than b.
a <- 50
b <- 10
if (a>b) {print("Hello World")}
## ejercicio 2
###Print "Hello World" if a is equal to b
a <- 50
b <- 50
if (a==b) {print("Hello World")}
## ejercicio 3
### Print "Yes" if a is equal to b, otherwise print "No"
a <- 50
b <- 50
if (a==b) {print("yes")}
ifelse(print("no"))

#R Loops
## ejercicio 1
### Print i as long as i is less than 6.
i <- 1
while (i < 6) 
{print(i)
  i <- i + 1
}
## ejercicio 2
### Use the correct keyword to exit the loop if i is equal to 4.
i <- 1
while (i < 6) {
  print(i)
  i <- i + 1
  if (i == 4) {
    
    break
  }
}
## ejercicio 3 consultar
### Use the correct keyword to skip the value of 3 in a loop
i <- 0
while (i < 6) {
  i <- i + 1
  if (i == 3) {
    Next
  }
  print(i)
}
## ejercicio 4
### Use the correct keyword to iterate over a sequence of numbers.
for (x in 1:10) {
  print(x)
}


#R Functions
## ejercicio 1 consultar
###Insert the missing parts to create a function with the name my_function.

my_function <- function() {print("Hello World!")}

my_function()

## ejercicio 2
###Insert the missing part to call my_function
my_function <- function() {
  print("Hello World!")
}

my_function()
## ejercicio 3
###Add a fname argument to the function.
my_function <- function(fname) {  paste(fname)}
## ejercicio 4
###Insert the missing part to return a result (15).
my_function <- function(x) {
  
  return (5 * x)
}

print(my_function(3))


#R Data STructure
## ejercicio 1 
### Insert the missing parts to create a vector of strings.
fruits <- c("banana", "apple", "orange")
## ejercicio 2 
### Insert the missing parts to find out how many items a vector has.
fruits <- c("banana", "apple", "orange")
length(fruits)
## ejercicio 3 
### Insert the missing parts to create a list of strings.
thislist <- list("apple", "banana", "cherry")
## ejercicio 4
### Insert the missing parts to create a matrix of strings
thismatrix <-   matrix(c("apple", "banana", "cherry", "orange"), nrow = 2, ncol = 2)
## ejercicio 5 
### Insert the missing parts to create an array with two dimensions.
thisarray <- c(1:24)
multiarray <-  array(thisarray,dim= c(4, 3, 2))
## ejercicio 6 
### Insert the missing part to create a data frame.
Data_Frame <- data.frame(    Training = c("Strength", "Stamina", "Other"),
                             Pulse = c(100, 150, 120),
                             Duration = c(60, 30, 45)
)
## ejercicio 7 
### Insert the missing part to create a factor.
music_genre <- factor(c("Jazz", "Rock", "Classic", "Classic", "Pop", "Jazz", "Rock", "Jazz"))

