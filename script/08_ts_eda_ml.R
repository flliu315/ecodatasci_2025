# --------------------------------------------
# Script Name: timeseries and forecasting
# Purpose: The script will illustrate how to create
#          a ts object and to build a ml model for 
#          forecasting. 
# 
# Author:     Fanglin Liu
# Email:      flliu315@163.com
# Date:       2025-02-17
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

# 01-creating AND visualizing ts
# refer to https://www.r-bloggers.com/2021/12/time-series-forecasting-lab-part-1-introduction-to-feature-engineering/
# A) using ts() or zoo() to create ts
# https://rpubs.com/ravipmum/Time_Series_Fundamentals
mydata = runif(n = 50, min = 10, max = 45)# Create a Vector of 50 evenly observations
mytimeseries = ts(data = mydata, 
                  start = 1956,  
                  frequency = 4)# Data starts in 1956 - quarterly
plot(mytimeseries)# Plot the Time Series

# https://blog.csdn.net/CodeRoarX/article/details/132485615
library(zoo)
library(ggplot2)
date_index <- as.Date(c("2023-01-01","2023-01-05","2023-01-06",
                        "2023-01-08","2023-01-09")) # create an time index
values <- c(10,12,8,15) # create an value vector
value_ts <- zoo(values, date_index) # generate a timeseries
head(value_ts)

ts_plot <- ggplot(data.frame(time = index(value_ts),
                             values = coredata(value_ts)),
                             aes(x = time, y = values)) +
  geom_line(color = "blue") +
  ggtitle("value_ts")

# B) using timetk() to create ts from df
library(tsibbledata)  # for ts data
data(package="tsibbledata")
aus_retail

# The tsibble should be converted to a classic
# tibble in order to manipulate with timetk
library(dplyr)
library(timetk)

aus_retail_tbl <-
  aus_retail |>
  tk_tbl() 

library(tsibble)  # for month to Date conversion
library(tidyverse)
monthly_retail_tbl <- aus_retail_tbl |>
  filter(State == "Australian Capital Territory") |>
  mutate(Month = as.Date(Month)) |>
  mutate(Industry = as_factor(Industry)) |>
  select(Month, Industry, Turnover)
monthly_retail_tbl
Industries <- unique(monthly_retail_tbl$Industry)
Industries

# visualizing 20 time series
monthly_retail_tbl |>
  group_by(Industry) |>
  plot_time_series(Month, Turnover,
                   .facet_ncol  = 4,
                   .smooth      = FALSE,
                   .interactive = FALSE)

monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  plot_time_series(Month, Turnover,
                   .facet_ncol  = 4,
                   .smooth      = TRUE,
                   .interactive = FALSE)

# check some diagnostics with timetk package

monthly_retail_tbl |> # the regularity of ts by checking scale 
  group_by(Industry) |>
  tk_summary_diagnostics(.date_var = Month)

monthly_retail_tbl |> # perform the seasonal diagnostics 
  filter(Industry == Industries[1]) |>
  tk_seasonal_diagnostics(.date_var = Month,
                          .value = Turnover)

monthly_retail_tbl %>% 
  filter(Industry == Industries[1]) %>%
  plot_seasonal_diagnostics(.date_var = Month,
                            .value = Turnover,
                            .title = Industries[1],
                            .interactive = FALSE)

