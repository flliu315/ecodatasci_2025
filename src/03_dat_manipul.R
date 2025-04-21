# --------------------------------------------
# Script Name: Basic R
# Purpose: This section introduces data types and structures
#          used in R, and basic data manipulation.
#          
# Author:  Fanglin Liu
# Email:   flliu315@163.com
# Date:    2025-03-21
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

#####################################################
############ 01-data read, reshape and save #########
#####################################################
library("tidyverse")  # load the tidyverse packages, incl. dplyr
data(package = "ade4") 
library(ade4)
data("doubs")
str(doubs) # check the data

# select(), filter(), mutate(), and pipe
env <- doubs$env |> 
  select(dfs, alt, oxy) |>       # select dfs, alt, pen
  filter(alt > 300) |>           # filter alt > 300 
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) |>
  rename(distance = dfs, oxygen = oxy) |>  # rename
  arrange(desc(alt))     

# use pipes for manipulating data
env <- doubs$env |> 
  select(dfs, alt, oxy) |>  # select dfs, alt, pen
  filter(alt > 300)  # filter alt > 300 

str(env)
env

# use mutate() to create a new column
doubs$env |>
  filter(!is.na(oxy)) |>
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) |>
  head()

# Split-apply-combine data analysis
doubs$env |>
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) |>
  group_by(oxygen_category) |>
  summarise(mean_alt = mean(alt), 
            mean_pH = mean(pH), 
            .groups = "drop") 

# summary the above steps as follows

doubs$env |> 
  select(dfs, alt, oxy, pH) |> # select dfs, alt, oxy, pH
  filter(alt > 200) |>  # keep the rows in which alt > 200
  mutate(oxygen_category = ifelse(oxy > 90, "High", "Low")) |>  
  rename(distance = dfs, oxygen = oxy) |>   # rename
  arrange(desc(alt)) |> # order by alt decrease
  group_by(oxygen_category) |>  # groups by oxy
  summarise(mean_alt = mean(alt), 
            mean_pH = mean(pH), 
            .groups = "drop")  

# # Reshaping doubs between long and wide format
# A) using with tidyr::gather and spread

long_env <- doubs$env |> # from wide format to long 
  gather(key = "variable", value = "value", -dfs)

head(long_env)

# reverse the gather() operation
wide_env <- long_env |> # Convert back to wide format
  spread(key = "variable", value = "value")

head(wide_env)

# B) using pivot_longer() and pivot_wider()

long_env_new <- doubs$env |> 
  pivot_longer(cols = -dfs, 
               names_to = "variable", 
               values_to = "value")

print(long_env_new, n =30)

wide_env_new <- long_env_new |> 
  pivot_wider(names_from = "variable", values_from = "value")

###############################################
###### 02-Data visualization with ggplot2 #####
###############################################

env <- doubs$env 
# Plotting scatter plot
ggplot(data = env)
ggplot(data = env, 
       aes(x = alt, y = oxy)) # define aes
ggplot(data = env, 
       aes(x = alt, y = oxy)) +
  geom_point() # dot plots

# Assign plot to a variable and then add plot
basic_plot <- ggplot(data = env, 
                       aes(x = alt, y = oxy, 
                           color = dfs))
basic_plot + 
  geom_point(alpha = 0.8, # add transparency
  color = "blue") #  add colors


ggplot(data = env, 
       aes(x = alt, y = oxy)) +
  geom_line() # draw a line for each species 

ggplot(data = env, 
       aes(x = alt, y = oxy, color = dfs)) +
  geom_line() # add colors


# Usually plots with white background
ggplot(data = env, aes(x = alt, y = oxy, color = dfs)) +
  geom_line() +
  theme_bw() +
  theme(panel.grid = element_blank())

# Customization with aes and title
my_plot <- 
  ggplot(doubs$env, aes(x = alt, y = oxy, 
                        color = dfs, size = dfs)) +
  geom_point(alpha = 0.8) +  
  scale_color_gradient(low = "blue", high = "red") +  # 颜色渐变
  labs(title = "alt vs oxygen",
       x = "Altitude",
       y = "Oxygen Level",
       color = "Distance from Source",
       size = "dfs") +
  theme_minimal() +  
  theme(plot.title = element_text(hjust = 0.5, # middle
                                  face = "bold"))

ggsave("results/name_of_file.png", 
       my_plot, width = 15, height = 10)
