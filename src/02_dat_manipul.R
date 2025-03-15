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

# 01-value or data types

# double, integer, logical, character, datetime
# https://www.geeksforgeeks.org/r-data-types/

# Double

x <- 5.23
is.numeric(x)
is.integer(x)
is.double(x)

# Integer

x <- 5L 

# Logical

x <- c(TRUE, TRUE, FALSE)

# Character

x <- "elevated"
is.character(x)
is.numeric(x)

# Datetime

x <- Sys.time()

# 02-creating objects and assigning variables
# https://www.geeksforgeeks.org/r-objects/

# R objects include vectors, list, dataframe, matrix, array
# Create vectors 

c(1,4,7)
Num_variable <- c(1,4,7) # assign this to a variable for future use

# Create list 
ls <- list(c(1, 2, 3, 4), list("a", "b", "c")) 

# Create matrix 
x <- c(1, 2, 3, 4, 5, 6) 
mat <- matrix(x, nrow = 2) 

# Create vector 
s <- c("spring", "autumn", "winter", "summer",  
       "spring", "autumn") 
factor(s)

# Create 3-dimensional array 
arr <- array(c(1, 2, 3), dim = c(3, 3, 3)) 

# Create data frame of vectors
x <- 1:5
y <- LETTERS[1:5] 
z <- c("Albert", "Bob", "Charlie", "Denver", "Elie") 

df <- data.frame(x, y, z) 


# 03-conversion among data structures

# convert matrix to data.frame
matrix_data=matrix(c(1,2,3,4,5,6,7,8), nrow=4) # default byrow=FALSE

print(matrix_data) 

class(matrix_data)

dataframe_data=as.data.frame(matrix_data) # convert the matrix into dataframe

print(dataframe_data)

class(dataframe_data)

# convert data frame to matrix

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

Array1 <- array(data = c(unlist(df1),  unlist(df2)),
                dim = c(5, 2, 2),
                dimnames = list(rownames(df1),
                                colnames(df1)))
Array1                                  

# library("plotKML") # Vector to raster 

#####################################################
# 04-operations for data file 
#####################################################

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

########################################
# 06-Best Practices for Writing R Code
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

# B) There are two ways to write R codes to generate our
# analyses, both of which use the Source panel. Writing R
# scripts in an editor by following the steps:
# click File – New File – R Script


# C) Customizing Snippets for tracking
# Starting with an annotated description of who write the
# code and what the code does for track when you have to 
# look at or change it in the future
# https://blog.devgenius.io/how-to-automatically-create-headers-in-r-scripts-be69152ac23f

# D) Assignment Using <- Not =
# The assignment operator <- is preferred over = for clarity
# and consistency in R code

x <- 2

# E) using descriptive names for naming variables to improve
# readability and avoid using reserved names such as c, T, or F 
# as variable names to prevent conflicts with built-in functions

vec <- c(1, 2, 3)

# F) Defining a relative path for import files into R and
# export them out R environment. For example:

input_file <- "data/data.csv" 
output_file <- "data/results.csv"

# G) annotating and marking code using # or #- to set off 
#  code sections or separate the function definitions. 

input_data <- read.csv(input_file) # read input 
sample_number <- nrow(input_data) # get number of samples 
results <- some_other_function(input_file, 
                               sample_number) 

# H) If creating one or a few custom functions in script, 
#  put them toward the top of code. If many functions, put
#  them all in their own .R file and source those files

source("src/myMean.R")
myvector <- c(1, 2, 3)
myMean(myvector) # from myMean.R

# I) Proper indentation and spacing make code more readable 
#  and maintainable

mtcars <- mtcars[, c("mpg", "drat", "wt")]
mtcars_max <- apply(mtcars, 
                    MARGIN = 2, # applying the max() function to each column
                    FUN = max, 
                    na.rm = TRUE # na.rm is being passed to the max() function
)
mtcars_max

# J) Pipes are widely used for streamline code by chaining 
#  operations in a readable manner

library(dplyr)
data %>%
  filter(x > 1) %>%
  summarise(mean_y = mean(y))