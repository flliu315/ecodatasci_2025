# --------------------------------------------
# Script Name: Basic R
# Purpose: This section introduces data types and structures
#          used in R, and basic data manipulation. The main
#          websites refereed are
#          
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-15
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# 01- data types
# https://www.geeksforgeeks.org/r-data-types/

# numeric, integer, logical, character, complex

# Double

x <- 5.23 # Assign a decimal value to x

is.numeric(x) # check the data type
is.integer(x)
is.double(x)

class(x) # check the data type
typeof(x)
mode(x)

# Integer

x <- 5L 

# Logical

x <- c(TRUE, TRUE, FALSE)

# Character

x <- "elevated"
is.character(x)
is.numeric(x)

# complex

x <- 1 + 2i
class(1 + 2i)

# 02- data objects
# https://www.geeksforgeeks.org/r-objects/

# A) Vectors/scalars

c(1,4,7)
Num_variable <- c(1,4,7) # assign this to a variable for future use
print(Num_variable)
(Num_variable <- c(1,4,7)) # assign and print(s) 

# Basic information 
length(Num_variable)  # How many elements
typeof(Num_variable)  # Of which type
is.vector(Num_variable) # Data structure:
is.list(Num_variable)
names(Num_variable)
str(Num_variable) 

a <- 100
is.vector(a)
length(a)

# Accessing elements
Num_variable[2]
Num_variable[-2]
which(Num_variable == "4") # know elements for location

# Indexing by subset()

(v <- c(1:2, NA, 4:6, NA, 8:10))
v[v > 5] # missing/NA values are preserved
subset(v, v > 5)  # missing/NA values are lost 

# B) Rectangular objects

# a) matrices/Arrays

(m0 <- matrix(data = 1:9, nrow = 3, byrow = TRUE))

x <- 1:3 # Creating 3 vectors
y <- 4:6
z <- 7:9

(m1 <- rbind(x, y, z)) 

# matrix attributions

mode(m0)
typeof(m0)
length(m0)

is.vector(m0)
is.matrix(m0)
dim(m0)

# Indexing matrices

m0[2, 3] #  rows, or columns of matrices
m0 > 5  # returns a matrix of logical values
sum(m0) # Computations with matrices
max(m0)
mean(m0)
colSums(m0)
rowSums(m0)
t(m0)

# b) data frames/tibbles

name   <- c("Adam", "Bertha", "Cecily", "Dora", "Eve", "Nero", "Zeno")
gender <- c("male", "female", "female", "female", "female", "male", "male")
age    <- c(21, 23, 22, 19, 21, 18, 24)
height <- c(165, 170, 168, 172, 158, 185, 182)

df <- data.frame(name, gender, age, height, 
                 stringsAsFactors = TRUE)
df  

is.matrix(df)
is.data.frame(df)
dim(df)

tb <- tibble::as_tibble(df) # Turn to a tibble tb 
dim(tb) 

# work with df

df[5, 3]  # numeric indexing 
df[6, ]  
df[ , 4] 

names(df)    
names(df)[4] 

df$gender  
df$age 

df$gender == "male"
sum(df$gender == "male") 

df$age < 21
df$age[df$age < 21] 
df$name[df$age < 21]

subset(df, age > 20) # Subsetting rectangular tables
subset(df, gender == "male")
subset(df, age > 20 | gender == "male")

df[age > 20, ]
df[gender == "male", ]
df[age > 20 | gender == "male", ] 
df[age > 20 & gender == "male", ]

# c) categories and factors

df <- data.frame(name, gender, age, height, 
                 stringsAsFactors = FALSE) # the default (as of R 4.0.0)
df
df$gender
is.character(df$gender)  
is.factor(df$gender)
all.equal(df$gender, gender) 

df <- data.frame(name, gender, age, height, 
                 stringsAsFactors = TRUE)

df$gender  
is.factor(df$gender)
typeof(df$gender)
unclass(df$gender)

df$gender <- as.factor(df$gender) # convert to a "factor"
df$gender

# d) Lists

l_1 <- list(1, 2, 3) # 3 elements (all numeric scalars)
l_1

l_2 <- list(1, c(2, 3))  # 2 elements (different lengths)
l_2

l_3 <- list(1, "B", 3)
l_3

# Inspecting lists

is.list(l_3)  # a list
is.list(1:3)  # a vector
is.list("A")
str(l_3)

# Accessing list elements

l_2[2] 
l_2[[2]] 

x <- list(1:3)
x[[1]][3] # The 3rd element at the first position

# C) conversion among objects

df$gender <- as.factor(df$gender) 
df$gender

# convert matrix to data.frame
matrix_data=matrix(c(1,2,3,4,5,6,7,8), nrow=4) # default byrow=FALSE
print(matrix_data) 
class(matrix_data)
dataframe_data=as.data.frame(matrix_data) # convert the matrix into dataframe
print(dataframe_data)
class(dataframe_data)

dataframe_data1 <- data.frame(a = 1:3, b = letters[10:12],
                              c = seq(as.Date("2004-01-01"), by = "week", 
                                      length.out = 3),
                              stringsAsFactors = TRUE)
dataframe_data1 
class(dataframe_data1)

matrix_data1 <- data.matrix(dataframe_data1[1:2]) # column
class(dataframe_data1[1:2])

matrix_data1
class(matrix_data1)

matrix_data2 <- data.matrix(dataframe_data1)
matrix_data2

# convert dataframe to array

df1 <- data.frame(x = 1:5, y = 5:1)
df1

df2 <- data.frame(x = 11:15,y = 15:11)
df2


# 03-function objects

sum(c(1, 2))
sum(1, 2, 3, NA, na.rm = TRUE)
paste0("hell", "o ", "world", "!")
substr(x = "television", start = 5, stop = 10)
?substr

?filter  # 2 different packages 

# Indicating the filter() function of a given package: 
?stats::filter
?dplyr::filter

library(ds4psy)  # load the package
plot_fn()
plot_fn(x = 1)
plot_fn(x = 7)

plot_fn(x = 5, y = 1)
plot_fn(x = 5, y = 5, A = TRUE, B = FALSE, C = TRUE, 
        D = FALSE, E = FALSE, F = FALSE,  g = "black")


# 04-assigning an object with a variable, i.e., naming

# case sensitive 
o <- 10  # assign/set o to 10
O <-  5  # assign/set O to  5

o * O  
o * 0  # Note: O differs from 0
o / O
O / 0

length(o) # Object shape
dim(o)

mode(o) # Object type

# Avoid using spaces inside variables 

# naming tea_pot instead of tea pot


#####################################################
# 05-import data from a file stored on a web site
#####################################################

# A) using download.file() or readr::read_csv()
# https://www.davidzeleny.net/anadat-r/doku.php/en:data:doubs

# Set the base URL for the datasets

base_url <- "https://raw.githubusercontent.com/zdealveindy/anadat-r/master/data/"

datasets <- c("DoubsSpe.csv","DoubsEnv.csv","DoubsSpa.csv")  # List of datasets 

# Download each dataset
for(dataset in datasets) {
  full_url <- paste0(base_url, dataset) # full URL for each file
  dest_file <- file.path("data/rbasic_data", dataset) # the destination
  download.file(full_url, destfile = dest_file, mode = "wb") # Download

  cat("Downloaded:", dataset, "\n") # Print a message for complete
}

# B) loading data from R package

# data()
# data(doubs, package = "ade4")

DoubsEnv <- readr::read_csv("data/rbasic_data/DoubsEnv.csv")
DoubsEnv

# Checking the dataset

str(DoubsEnv)
tibble::glimpse(DoubsEnv)


library("tidyverse")  # load the tidyverse packages, incl. dplyr
surveys <- read_csv("data/dat_type/portal_data_joined.csv") #  from tidyverse
str(surveys) # check the data

# select(), filter(), mutate(), and pipe
select(surveys, plot_id, species_id, weight) # from dplyr
filter(surveys, year == 1995)

# use pipes for manipulating data
surveys_sml <- surveys %>% # |>
  filter(weight < 5) %>%
  select(species_id, sex, weight)
surveys_sml

# use mutate() to create a new column
surveys %>%
  filter(!is.na(weight)) %>%
  mutate(weight_kg = weight / 1000) %>%
  head()

# Split-apply-combine data analysis
surveys %>%
  group_by(sex) %>%
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys %>%
  filter(!is.na(weight)) %>%
  group_by(sex, species_id) %>%
  summarize(mean_weight = mean(weight))

surveys %>%
  group_by(sex) %>%
  tally()

# # Reshaping with gather and spread
# download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
#               dest="data/dat_type/surveys_wide.csv")
surveys_wide <- read.csv("data/dat_type/surveys_wide.csv")
head(surveys_wide)

surveys_gather <- surveys_wide %>%
  gather(key = species_abbrev, value = count, -c(month, day, year, plot_id))
str(surveys_gather)
head(surveys_gather)

surveys_long <- surveys_wide %>%
  gather(key = species_abbrev, value = count, -(month:plot_id))
str(surveys_long)
# reverse the gather() operation
spread(surveys_long,key=species_abbrev,value=count)

surveys_gw <- surveys %>%
  filter(!is.na(weight)) %>%
  group_by(genus, plot_id) %>%
  summarize(mean_weight = mean(weight))

str(surveys_gw)

# Exporting data
surveys_complete <- surveys %>%
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex

species_counts <- surveys_complete %>% # Extract the most common species_id
  group_by(species_id) %>%
  tally() %>%
  filter(n >= 50)

surveys_complete <- surveys_complete %>% # Only keep the most common species
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, "results/surveys_complete.csv")

###############################################
# 05-Data visualization with ggplot2
###############################################

surveys_complete <- read_csv("results/surveys_complete.csv")

# Plotting scatter plot
ggplot(data = surveys_complete)
ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) # define aes
ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point() # dot plots

# Assign plot to a variable and then add plot
surveys_plot <- ggplot(data = surveys_complete, 
                       aes(x = weight, y = hindfoot_length))
surveys_plot + 
  geom_point()

# for large data set using hexagonal binning
library(hexbin)
surveys_plot +
  geom_hex()

ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1) # add transparency

ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, 
             color = "blue") #  add colors

ggplot(data = surveys_complete, 
       aes(x = weight, y = hindfoot_length)) +
  geom_point(alpha = 0.1, 
             aes(color = species_id)) #  color each species

# Plotting boxplot
ggplot(data = surveys_complete, 
       aes(x = species_id, y = weight)) +
  geom_boxplot()

ggplot(data = surveys_complete, 
       aes(x = species_id, y = weight)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato") # add points  

# Plotting time series data
yearly_counts <- surveys_complete %>%
  group_by(year, species_id) %>%
  tally()

ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() # draw a line for all species

ggplot(data = yearly_counts, aes(x = year, y = n, group = species_id)) +
  geom_line() # draw a line for each species 

ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line() # add colors

# split one plot into multiple plots based on a factor
ggplot(data = yearly_counts, aes(x = year, y = n)) +
  geom_line() +
  facet_wrap(~ species_id)

yearly_sex_counts <- surveys_complete %>%
  group_by(year, species_id, sex) %>%
  tally()
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) # split the line in each plot by the sex 

# Usually plots with white background
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  theme_bw() +
  theme(panel.grid = element_blank())

# Customization with aes and title
ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw()

ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(text=element_text(size = 16)) # modifying font size

grey_theme <- theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
                    axis.text.y = element_text(colour = "grey20", size = 12),
                    text = element_text(size = 16)) #  created a theme
ggplot(surveys_complete, aes(x = species_id, y = hindfoot_length)) +
  geom_boxplot() +
  grey_theme

# Arranging and exporting plots
library(gridExtra)

spp_weight_boxplot <- ggplot(data = surveys_complete, aes(x = species_id, y = weight)) +
  geom_boxplot() +
  xlab("Species") + ylab("Weight (g)") +
  scale_y_log10()

spp_count_plot <- ggplot(data = yearly_counts, aes(x = year, y = n, color = species_id)) +
  geom_line() + 
  xlab("Year") + ylab("Abundance")

grid.arrange(spp_weight_boxplot, # combine into a single figure
             spp_count_plot, 
             ncol = 2, widths = c(4, 6))

my_plot <- ggplot(data = yearly_sex_counts, aes(x = year, y = n, color = sex)) +
  geom_line() +
  facet_wrap(~ species_id) +
  labs(title = "Observed species in time",
       x = "Year of observation",
       y = "Number of species") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12, angle = 90, hjust = 0.5, vjust = 0.5),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text=element_text(size = 16))
ggsave("results/name_of_file.png", my_plot, width = 15, height = 10)

## This also works for grid.arrange() plots
combo_plot <- grid.arrange(spp_weight_boxplot, spp_count_plot, ncol = 2, widths = c(4, 6))
ggsave("results/combo_plot_abun_weight.png", combo_plot, width = 10, dpi = 300)
