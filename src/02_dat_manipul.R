# --------------------------------------------
# Script Name: Basic R
# Purpose: This section introduces data types and structures
#          used in R, and basic data manipulation. The main
#          websites refereed are
#          https://www.javatpoint.com/r-database
#          https://bbolker.github.io/R-ecology-lesson/03-dplyr.html
#          
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2024-03-15
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

###########################################################
# getwd() # current working directory
# setwd() # set working directory

# 01-values and variables 
# https://sdesabbata.github.io/r-for-geographic-data-science/introduction-to-r.html

# A) Numeric example
a_number <- 1.41

is.numeric(a_number)
is.integer(a_number)
is.double(a_number)

# numeric operators

5+2
5-2
5*2
5/2
5%/%2 # the integer division
5%%2 #  the remainder
5^2

abs(-2) # Absolute value
ceiling(3.475) # Upper round
floor(3.475) # Lower round
trunc(5.99) # the decimal portion
round(5.99, digits=1) #  the nearest integer

0 / 0
is.nan(0 / 0)

# Logical operators

5==2
first_value <- 5
second_value <- 2
first_value == 5
first_value == 2
second_value == 5
second_value == 2
first_value == second_value


# Character example
"String value" 
is.character("String value" )
is.numeric("String value")

# B) variable or object

a_variable <- 1.41 # Assignment a value
a_variable            

is.numeric(a_variable)

a_variable <- "Hello world!"
a_variable

is.character(a_variable)

##############################################
# 02-data types and structures 

# vector
east_midlands_cities <- c("Wuhan", "Zhengzhou", "Hefei")
length(east_midlands_cities)
east_midlands_cities[3] # Retrieve the third city
east_midlands_cities[c(1, 3)] # Retrieve first and third city

a<-4:-10 # create a vector 
a 

seq_vec<-seq(1,4,by=0.5)  
seq_vec  

# factor

x <- c("male", "female", "male", "male",  "female")
sex <- factor(x)
sex
is.factor(sex)

# list

employee <- list(name="Joe",salary=55000,union=T)
employee[[1]]
employee$name

# matrix

P <- matrix(c(5:16), nrow = 4, byrow = TRUE) # create a matrix 
print(P)  
Q <- matrix(c(3:14), nrow = 4, byrow = FALSE)  
print(Q)  

row_names = c("row1", "row2", "row3", "row4")  
col_names = c("col1", "col2", "col3")  
R <- matrix(c(3:14), nrow = 4, byrow = TRUE, 
            dimnames = list(row_names, col_names))  
print(R)  


print(R[3,2])  

R[R==12]<-0 # note "==" different from "=" in R
print(R)  

R <- matrix(c(5:16), nrow = 4,ncol=3)  # mathematical operations
S <- matrix(c(1:12), nrow = 4,ncol=3)  
sum<-R+S  # Addition 
print(sum) 

# data frame 

emp.data<- data.frame(  
  employee_id = c (1:5),   
  employee_name = c("Shubham","Arpita","Nishka","Gunjan","Sumit"),  
  sal = c(623.3,915.2,611.0,729.0,843.25),   
  
  starting_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",  
                            "2015-03-27")),  
  stringsAsFactors = FALSE  
)  

print(emp.data)  
class(emp.data)

final <- data.frame(emp.data$employee_id, emp.data$sal)  
print(final)  

final1 <- emp.data[1,]  
print(final1) 

final2 <- emp.data[4:5, ]  
print(final2)  

emp.data.1<-emp.data[-1,]  
print(emp.data.1)  

emp.data.2 <- emp.data[,2]  
print(emp.data.2) 

emp.data.3 <- emp.data$employee_name
print(emp.data.3) 

emp.data.4 <- emp.data[3,2]  
print(emp.data.4) 

# t(Matrix/data frame)  

a <- matrix(c(4:12),nrow=3,byrow=TRUE)  
a  
b <- t(a)  
b  

# array

vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)
result <- array(c(vector1, vector2), dim = c(3,3,2))
print(result)

###########################################
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

####################################################
# 04-operations for data file 

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

# Reshaping with gather and spread
download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
              dest="data/dat_type/surveys_wide.csv")
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

##------------------------------------------------------
# F) Data visualization with ggplot2
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
