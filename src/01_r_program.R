# --------------------------------------------
# Script Name: Basic R (object-oriented programming)
# Purpose: This provide an overview of RStudio and show
#          how to do some basic operations on R console, 
#          including installing and loading packages.

# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-03-05
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# https://bookdown.org/manishpatwal/bookdown-demo/list-in-r.html

# 01-starting RStudio and configure its appearance

# 02-introduce of RStudio panes and their functions

# 03-basic some operations by typing on R console

# getwd() # current working directory
# setwd() # set working directory
# list.files() or dir()

# Airthmetic Operations
2+2  # Addition
2-2  # Substraction
2*2 # Multiplication
2/2 # Division
3^2 # Square
sqrt(2)
5%/%2 # Integer Division
5%%2 # Modulus

# Relational Operators 
# assign a value to a variable using “<-”

x <- 5 #  assigned x value of 5
y <- 7 # assigned y value of 7
x > y

# Logical Operations

!TRUE # "!" is Logical NOT
TRUE & FALSE # "&" is Logical AND
TRUE | FALSE # “|” is for Logical OR

# 04-Almost everything in R is done through functions

# A) Built-in Functions in R
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

# B) installing and loading packages for many functions

# finding and selecting packages
# install.packages("packagefinder", dependencies = TRUE)

library(packagefinder)
findPackage("community ecology") 

# --from CRAN
# Install packages by IDE or using install.packages()

# install.packages('readr')
# install.packages(c('readr', 'ggplot2', 'tidyr'))

# --from GitHub

# install.packages('devtools')
# devtools::install_github('rstudio/shiny')

# --from special Repositories
# install.packages('furrr',
#                  repos='http://cran.us.r-project.org',
#                  dependencies=TRUE)

# --from Zip files
# Installing R Packages from Zip Files by IDE or
# install.packages('C:/Users/User/Downloads/abc_2.1.zip',
#                  repos=NULL, type='source')


# --using pak to install R packages

# first install pak package
# install.packages("pak")
# install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

# Install packages from CRAN or Bioconductor
# pak::pkg_install("clusterProfiler")

# Install packages from github
# pak::pkg_install("tomwenseleers/export")

# C) using self-defined functions
# # https://rpubs.com/NateByers/functions
# 
# # Writing functions
# myMean <- function(x){
#   total_count_of_values <- length(x)
#   total_sum_of_values <- sum(x)
#   average_of_values <- total_sum_of_values/total_count_of_values
#   average_of_values
# }
# 
# my_vector <- c(1, 3, 5, 2, 6, 9, 0)
# vector_mean <- myMean(x = my_vector)
# vector_mean
# 

# 05-installing gptstudio for help

# install.packages("pak")
pak::pak("usethis")
# pak::pak("MichelNivard/gptstudio")
usethis::edit_r_environ()
Sys.getenv("OPENAI_API_KEY")
gptstudio:::gptstudio_chat()
