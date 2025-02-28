# 01-customize a code snippets

# https://blog.devgenius.io/how-to-automatically-create-headers-in-r-scripts-be69152ac23f

# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This scribes how to use R packages and 
#          how to write effective R code.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-03-05
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

####################################################
# 02-built-in R function
####################################################
# A: elementary calculation
10-(2+3) 
(3*8)/(2*3)
log(12)
sqrt(121)

# replicate something many times

rep("words",4)
rep(x=1, times=5)

# make a sequence of numbers

seq(1,5,1) # a sequence of 1-5, with a step size of 1.
seq(from=100, to=10, by= -10)
rep(seq(1,3,1),times=2) # repeat the seq of 1-3 for 2 times 

################################################
# B: Operators of R
# https://www.datamentor.io/r-programming/operator
# Assignment Operators
x <- 5
x
x <- 9
x
10 -> x
x

x <- 10-(2+3) 
x

# Operation on Vectors
x <- c(2, 8, 3)
y <- c(6, 4, 1)
x + y
x > y

# a mismatch in length
x <- c(2, 1, 8, 3)
y <- c(9, 4)
x + y
x - 1
x + c(1, 2, 3)

# Arithmetic Operators
x <- 5
y <- 16
x + y
x - y
x * y
y / x
y ^ x
y %/% x # Integer Division (floor)
y %% x # Remainder from division

floor(8.9)
ceiling(8.9)
round(8.4)
round(8.5)
round(8.6)

# Relational Operators
x <- 5
y <- 16
x < y
x > y
x <= 5
y >= 20
y == 16
x != 5

# Logical Operators

# OR operator (|)
x <- c(TRUE, TRUE, FALSE, FALSE)
y <- c(TRUE, FALSE, TRUE, FALSE)
!x
x | y

# AND operator (&)
x <- c(TRUE, TRUE, FALSE, FALSE)
y <- c(TRUE, FALSE, TRUE, FALSE)
x & y

###################################################
# C: plot operations
?plot.function

x <- c(1,2,3,4,5)
y<- c(1,4,9,16,25)
plot(x, y)

# shape of the markers (pch)
# pch = 0-25, 0 is for a square, 1 is for a circle, 
# 3 is for a triangle, 4 is for a cross and so on.
plot(x, y, 
     pch =4)

# Size of the markers (cex)
plot(x, y, pch =0, 
     cex = 0.5)

# Color of the markers (col)
colors()
plot(x, y, pch =0, cex = 1.5, 
     col="black")

# Connecting points with lines (type)
# type = "l", "o", "p", "b"
plot(x, y, pch =0, cex = 1.5, col="black", 
     type="b")

# Varying the lines (lty, lwd)
plot(x, y, pch =0, cex = 1.5, col="red", type="l",
     lwd=2,lty=1)

# Adding graphic information
plot(x, y, pch =0, cex = 1.5, col="red", type="l",lwd=2,lty=1,
     main="This is a graph",col.main='blue') # main and its col

plot(x, y, pch =0, cex = 1.5, col="red", type="l",lwd=2,lty=1, 
     main="This is a graph",col.main='blue',
     xlab="temperature",ylab="body size") # x and y labels

plot(x, y, pch =0, cex = 1.5, col="red", type="l",lwd=2,lty=1, 
     main="This is a graph",col.main='blue',
     xlab="temperature",ylab="body size") 

legend('topleft',inset=0.05,"body size",lty=3,col='green',lwd=4)

# Overlaying Graphs
x=seq(2,10,0.1)
y1=x^2
y2=x^3
plot(x,y1,type='l',col='red')
lines(x,y2,col='green')
legend('bottomright',inset=0.05,c("Squares","Cubes"),lty=1,col=c("red","green"),title="Graph type")

# Adding Lines to a Plot
x=seq(2,10,0.1)
y1=x^2
y2=x^3
plot(x,y1,type='l',col='red')
lines(x,y2,col='green')
legend('bottomright',inset=0.05,c("Squares","Cubes"),lty=1,col=c("red","green"),title="Graph type")

abline(a=4,b=5,col='blue') # a= slope and b=intercept
abline(h=c(4,6,8),col="dark green",lty=2)
abline(v=c(4,6,8),col="dark green",lty=2)

##################################################
# 03-user R packages or libraries
##################################################

# finding and selecting packages
# install.packages("packagefinder", dependencies = TRUE)

library(packagefinder)
findPackage("community ecology") 

# A: from CRAN
# Install packages by IDE or using install.packages()

# install.packages('readr')
# install.packages(c('readr', 'ggplot2', 'tidyr'))

# B: from GitHub

# install.packages('devtools')
# devtools::install_github('rstudio/shiny')

# C: from special Repositories
# install.packages('furrr',
#                  repos='http://cran.us.r-project.org',
#                  dependencies=TRUE)

# D: from Zip files
# Installing R Packages from Zip Files by IDE or
# install.packages('C:/Users/User/Downloads/abc_2.1.zip',
#                  repos=NULL, type='source')


# using pak to install R packages

# first install pak package
# install.packages("pak")
# install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

# Install packages from CRAN or Bioconductor
# pak::pkg_install("clusterProfiler")

# Install packages from github
# pak::pkg_install("tomwenseleers/export")

# help yourself about using packages

# Helping yourself
help(package="tidyverse")

# Vignettes Demonstrations
vignette("tidyverse")
browseVignettes(package="tidyverse")
demo(package="tidyverse")

# Searching for Help
apropos("^tidyverse")
ls("package:tidyverse")
help.search("^tidyverse")

##################################################
# 04-using self-defined functions
##################################################
# https://rpubs.com/NateByers/functions

# Writing functions
myMean <- function(x){
  total_count_of_values <- length(x)
  total_sum_of_values <- sum(x)
  average_of_values <- total_sum_of_values/total_count_of_values
  average_of_values
}

my_vector <- c(1, 3, 5, 2, 6, 9, 0)
vector_mean <- myMean(x = my_vector)
vector_mean

# For loop

my_list <- list(c(1, 5, 9, 3), 1:10, c(23, 42))
my_averages <- c()
for(i in c(1, 2, 3)){
  my_averages[i] <- myMean(my_list[[i]])
}
my_averages


# The apply() family
mtcars <- mtcars[, c("mpg", "drat", "wt")]
mtcars_max <- apply(mtcars, 
                    MARGIN = 2, # applying the max() function to each column
                    FUN = max, 
                    na.rm = TRUE # na.rm is being passed to the max() function
)
mtcars_max

#Creating a function without an argument
new.function <- function(){
  for(i in 1:5){
    print(i^2)
  }
}

new.function()

# Creating a function with argments
new.function <- function(a){
  for(i in 1:a){
    b <- i^2
    print(b)
  }
} 
# Calling the function supplying arguments
new.function(10)

source("src/coldiss.R")

########################################
# 05-using gptstudio for help
########################################
# install.packages("pak")
pak::pak("usethis")
# pak::pak("MichelNivard/gptstudio")
usethis::edit_r_environ()
Sys.getenv("OPENAI_API_KEY")
gptstudio:::gptstudio_chat()

