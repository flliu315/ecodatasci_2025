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
surveys <- read_csv("data/data_object/portal_data_joined.csv")
str(surveys) # check the data

# select(), filter(), mutate(), and pipe
select(surveys, plot_id, species_id, weight) # from dplyr
filter(surveys, year == 1995)

# use pipes for manipulating data
surveys_sml <- surveys |> # |>
  filter(weight < 5) |>
  select(species_id, sex, weight)
surveys_sml

# use mutate() to create a new column
surveys |>
  filter(!is.na(weight)) |>
  mutate(weight_kg = weight / 1000) |>
  head()

# Split-apply-combine data analysis
surveys |>
  group_by(sex) |>
  summarize(mean_weight = mean(weight, na.rm = TRUE))

surveys |>
  filter(!is.na(weight)) |>
  group_by(sex, species_id) |>
  summarize(mean_weight = mean(weight))

surveys |>
  group_by(sex) |>
  tally()

# # Reshaping with gather and spread
# download.file("tinyurl.com/dcmac2017dec/data/surveys_wide.csv",
#               dest="data/dat_type/surveys_wide.csv")
surveys_wide <- read.csv("data/data_object/surveys_wide.csv")
head(surveys_wide)

surveys_gather <- surveys_wide |>
  gather(key = species_abbrev, value = count, -c(month, day, year, plot_id))
str(surveys_gather)
head(surveys_gather)

surveys_long <- surveys_wide |>
  gather(key = species_abbrev, value = count, -(month:plot_id))
str(surveys_long)
# reverse the gather() operation
spread(surveys_long,key=species_abbrev,value=count)

surveys_gw <- surveys |>
  filter(!is.na(weight)) |>
  group_by(genus, plot_id) |>
  summarize(mean_weight = mean(weight))

str(surveys_gw)

# Exporting data
surveys_complete <- surveys |>
  filter(species_id != "",         # remove missing species_id
         !is.na(weight),           # remove missing weight
         !is.na(hindfoot_length),  # remove missing hindfoot_length
         sex != "")                # remove missing sex

species_counts <- surveys_complete |> # Extract the most common species_id
  group_by(species_id) |>
  tally() |>
  filter(n >= 50)

surveys_complete <- surveys_complete |> # Only keep the most common species
  filter(species_id %in% species_counts$species_id)

write_csv(surveys_complete, "results/surveys_complete.csv")

###############################################
###### 02-Data visualization with ggplot2 #####
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
