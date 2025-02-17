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
# https://www.r-bloggers.com/2021/12/time-series-forecasting-lab-part-1-introduction-to-feature-engineering/
# https://www.r-bloggers.com/2020/06/time-series-in-5-minutes-part-1-visualization-with-the-time-plot/#google_vignette
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

# 02-pre-processing and exploratory analysis
# A) Find missing data and imputation 
# https://www.kaggle.com/code/janiobachmann/time-series-i-an-introductory-start?scriptVersionId=53165252
# https://business-science.github.io/timetk/articles/TK07_Time_Series_Data_Wrangling.html

library(DataExplorer)
library(ggthemes)

monthly_retail_tbl |> # detect missing data
  group_by(Industry) |>
  pad_by_time(Month, .by = "auto")

monthly_retail_tbl |> 
  group_by(Industry == Industries[1]) |>
  plot_missing(
    ggtheme = theme_calc(), 
    title = "Percent Missing Values by Column"
  )

monthly_retail_tbl |> # imputate missing data
  filter(Industry == Industries[1]) |>
  pad_by_time(Month, .by = "month") |>
  mutate_at(vars(Turnover), 
            .funs = ts_impute_vec, period = 1) 

monthly_retail_tbl |> # re-plot the ts
  filter(Industry == Industries[1]) |>
  plot_time_series(Month, Turnover, 
                   .facet_scale = "free",
                   .interactive = FALSE,
                   .title = "stock prices") 

# B) Detect the outlier in timeseries
# https://business-science.github.io/timetk/articles/TK08_Automatic_Anomaly_Detection.html
anomalizes <- 
  monthly_retail_tbl |> # detect outliers
  filter(Industry == Industries[1]) |>
  anomalize(.date_var = Month,
            .value = Turnover,
            .iqr_alpha = 0.05,
            .max_anomalies = 0.20,
            .message = FALSE) |> 
  glimpse()

anomalizes |> # Anomaly Detection Plot
  # filter(Industry == Industries[1]) |>
  plot_anomalies(Month,
                 .facet_ncol = 2)

anomalizes |> # Anomalies Cleaned Plot
  plot_anomalies_cleaned(Month,
                         .facet_ncol = 2)


# C) check regular and seasonal diagnostics 

monthly_retail_tbl |> # the regularity of ts by checking scale 
  group_by(Industry) |>
  tk_summary_diagnostics(.date_var = Month)

monthly_retail_tbl |> # perform the seasonal diagnostics 
  filter(Industry == Industries[1]) |>
  tk_seasonal_diagnostics(.date_var = Month,
                          .value = Turnover)

monthly_retail_tbl |> 
  filter(Industry == Industries[1]) |>
  plot_seasonal_diagnostics(.date_var = Month,
                            .value = Turnover,
                            .title = Industries[1],
                            .interactive = FALSE)

# D) perform ACF and PACF diagnostics 
# https://www.kaggle.com/code/janiobachmann/time-series-ii-feature-engineering-concepts

color_pal <- c("#5C5C5C", "#EA3B46")
monthly_retail_tbl |>
  tk_acf_diagnostics(
    .date_var = Month,
    .value = Turnover
  ) |> 
  pivot_longer(cols = c("ACF", "PACF")) |> 
  ggplot(aes(x=lag, y=value, color=name)) + 
  geom_line(size=1) + 
  geom_point(size=0.5) +
  facet_wrap(~name) + 
  theme_bw() + 
  scale_color_manual(values = color_pal) +
  theme(legend.position = "bottom", strip.background =element_rect(fill="#BDBDBD")) + 
  labs(
    title = "Autocorrelation and Partial Auto Correlation",
    color = "Metric"
  )
