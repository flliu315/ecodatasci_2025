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
# 01-The basic R and operation
####################################################
# A) starting RStudio in the two ways

# typing codes after > in R console and hitting to run

10-(2+3) 
(3*8)/(2*3)

# open a new file for typing and running codes in editor

10-(2+3) 
(3*8)/(2*3)

# B) creating R variables using = or <- assignment
# https://www.datacamp.com/doc/r/variables

# naming variables as arguments to functions for dynamic 
# operation and also store functions outputs

x <- 5
x
x <- 9
x
10 -> x
x

x <- 10-(2+3) 
x

# C) performing operations on variables and values
# https://www.datacamp.com/doc/r/operators

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


# D) Almost everything in R is done through functions

# a) Built-in Functions in R
# https://www.datacamp.com/doc/r/functions

# Numeric Functions
abs(12)
log(12)
sqrt(121)
exp(15)
floor(8.9)
ceiling(8.9)
round(8.4)

# Character Functions

x <- "abcdef"
substr(x , 2, 4)
text_vector <- c("DataScience", "datascience", "DATA", "science", 
                 "Science")
grep("science", text_vector, ignore.case = TRUE)

strsplit("abc", "")

paste("y",1:3,sep="")

x <- "abcdef"
toupper(x)

# Statistical Functions

mean(x, trim=0,na.rm= FALSE )
sum(x)
range(x)

# Other Functions

seq(from , to , by)
rep(x , ntimes)

# b) using self-defined functions
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

# Creating a function without an argument
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

# E) Many useful R function come in packages

# finding and selecting packages
# install.packages("packagefinder", dependencies = TRUE)

library(packagefinder)
findPackage("community ecology") 

# a) from CRAN
# Install packages by IDE or using install.packages()

# install.packages('readr')
# install.packages(c('readr', 'ggplot2', 'tidyr'))

# b) from GitHub

# install.packages('devtools')
# devtools::install_github('rstudio/shiny')

# c) from special Repositories
# install.packages('furrr',
#                  repos='http://cran.us.r-project.org',
#                  dependencies=TRUE)

# d) from Zip files
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

########################################
# 02-Best Practices for Writing R Code
########################################
# https://swcarpentry.github.io/r-novice-inflammation/06-best-practices-R.html
# https://www.r-bloggers.com/2024/06/writing-r-code-the-good-way/

# A) A well-organized directory structure helps navigating 
# the project efficiently. It separates data, scripts, and 
# results, making it easier to locate and manage files

# project/
#   ├── data/
#   ├── scripts/
#   └── results/

# B) Writing R scripts in an editor
# To start writing a new R script in RStudio, 
# click File – New File – R Script

# C) Customizing Snippets for tracking
# Starting with an annotated description of who write the
# code and what the code does for track when you have to 
# look at or change it in the future
# https://blog.devgenius.io/how-to-automatically-create-headers-in-r-scripts-be69152ac23f

# D) Naming variables and avoiding Reserved Names
# descriptive names improves readability and avoid using 
# reserved names such as c, T, or F as variable names to 
# prevent conflicts with built-in functions and constants

# data_analysis.R
# Data Analysis.R
vec <- c(1, 2, 3)

# E) import into R and export using relative path

input_file <- "data/data.csv" 
output_file <- "data/results.csv"

# F) Assignment Using <- Not =
# The assignment operator <- is preferred over = for clarity
# and consistency in R code. Using proper spacing, especially 
# near operators, enhances code readability

result <- a + b

# G) annotating and marking code using # or #- to set off 
#  code sections or separate the function definitions. If 
#  creating only one or a few custom functions in script, 
#  put them toward the top of code. If many functions, put
#  them all in their own .R file and source those files

# H) Proper indentation and spacing make code more readable 
#  and maintainable

# if (condition) {
#   do_something()
# }

# I) Pipes are widely used for streamline code by chaining 
#  operations in a readable manner

library(dplyr)
data %>%
  filter(x > 1) %>%
  summarise(mean_y = mean(y))

# K) using gptstudio for help

# install.packages("pak")
pak::pak("usethis")
# pak::pak("MichelNivard/gptstudio")
usethis::edit_r_environ()
Sys.getenv("OPENAI_API_KEY")
gptstudio:::gptstudio_chat()
