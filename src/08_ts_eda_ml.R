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
mydata = runif(n = 50, min = 10, max = 45) # Create a Vector of 50 evenly observations
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

# 02-Exploratory analysis of timeseries
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


# C) check regular/seasonal/stl diagnostics 
# check the regularity of all time series
# https://www.r-bloggers.com/2021/12/time-series-forecasting-lab-part-1-introduction-to-feature-engineering/
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

# STL Diagnostics
# https://www.business-science.io/code-tools/2020/08/26/five-minute-time-series-seasonality.html
monthly_retail_tbl |> 
  filter(Industry == Industries[1]) |>
  plot_stl_diagnostics(
    Month, Turnover,
    .frequency = "auto", .trend = "auto",
    .feature_set = c("observed", "season", "trend", "remainder"),
    .interactive = FALSE)

# D) perform ACF and PACF diagnostics 
# https://www.business-science.io/code-tools/2020/06/17/five-minute-time-series-part-2.html

monthly_retail_tbl |> 
  filter(Industry == Industries[1]) |>
plot_acf_diagnostics(Month, Turnover, 
                     # .lags = 440,
                     .interactive = FALSE)

# 03-forecasting model with Feature engineering
# log transformation and standardization of data
# https://www.r-bloggers.com/2021/12/time-series-forecasting-lab-part-1-introduction-to-feature-engineering/
# https://www.kaggle.com/code/janiobachmann/time-series-ii-feature-engineering-concepts

monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  # mutate(Turnover =  log1p(x = Turnover)) |>
  # mutate(Turnover =  standardize_vec(Turnover)) |>
  plot_time_series(Month,Turnover,
                   .facet_scale = "free",
                   .interactive = FALSE)

# log transformation and standardization of data
monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  # log transformation and standardization of measure
  mutate(Turnover =  log1p(x = Turnover)) |>
  mutate(Turnover =  standardize_vec(Turnover)) |>
  # add Calendar-based features
  tk_augment_timeseries_signature(.date_var = Month) |> 
  glimpse()

# Add Calendar-based (or signature) features
library(fastDummies)  # for dummyfying categorical variables
monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  # log transformation and standardization of measure
  mutate(Turnover =  log1p(x = Turnover)) |>
  mutate(Turnover =  standardize_vec(Turnover)) |>
  # add Calendar-based features
  tk_augment_timeseries_signature(.date_var = Month) |>
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
  dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  # Perfom linear regression WITH FEATURE ENGINEERING
  plot_time_series_regression(.date_var = Month, 
                              .formula = Turnover ~ as.numeric(Month) + 
                                index.num + year + half + quarter + month +
                                month.lbl_January + month.lbl_February + month.lbl_March + month.lbl_April +
                                month.lbl_May + month.lbl_June + month.lbl_July + month.lbl_August + 
                                month.lbl_September + month.lbl_October + month.lbl_November + month.lbl_December, 
                              .title = "Linear Model With Feature Engineering",
                              .interactive = FALSE,
                              .show_summary = TRUE)

monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  # log transformation and standardization of measure
  mutate(Turnover =  log1p(x = Turnover)) |>
  mutate(Turnover =  standardize_vec(Turnover)) |>
  # # add Calendar-based features
  # tk_augment_timeseries_signature(.date_var = Month) |>
  # select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
  # dummy_cols(select_columns = c("month.lbl")) |>
  # select(-month.lbl) |>
  # mutate(index.num = normalize_vec(x = index.num)) |>
  # mutate(year = normalize_vec(x = year)) |>
  # Perfom linear regression
  plot_time_series_regression(.date_var = Month, 
                              .formula = Turnover ~ as.numeric(Month),
                              .title = "Linear Model Without Feature Engineering",
                              .interactive = FALSE,
                              .show_summary = TRUE)

# Add Fourier features
monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  # log transformation and standardization of measure
  mutate(Turnover =  log1p(x = Turnover)) |>
  mutate(Turnover =  standardize_vec(Turnover)) |>
  # add Calendar-based features
  tk_augment_timeseries_signature(.date_var = Month) |>
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
  dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  # add Fourier features
  tk_augment_fourier(.date_var = Month, .periods = 12, .K = 1) |>
  # Perfom linear regression
  plot_time_series_regression(.date_var = Month, 
                              .formula = Turnover ~ as.numeric(Month) + 
                                index.num + year + half + quarter + month +
                                month.lbl_January + month.lbl_February + month.lbl_March + month.lbl_April +
                                month.lbl_May + month.lbl_June + month.lbl_July + month.lbl_August + 
                                month.lbl_September + month.lbl_October + month.lbl_November + month.lbl_December +
                                Month_sin12_K1 + Month_cos12_K1,
                              .title = "Linear Model With Fourier terms",
                              .interactive = FALSE,
                              .show_summary = TRUE)

# Add lagged features

monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  mutate(Turnover =  log1p(x = Turnover)) |>
  mutate(Turnover =  standardize_vec(Turnover)) |> 
  tk_augment_timeseries_signature(.date_var = Month) |> 
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) %>%
  dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  tk_augment_fourier(.date_var = Month, .periods = 12, .K = 1) |>
  tk_augment_lags(.value = Turnover, .lags = c(12, 13)) |>
  plot_time_series_regression(.date_var = Month, 
                              .formula = Turnover ~ as.numeric(Month) +
                                index.num + year + half + quarter + month +
                                month.lbl_January + month.lbl_February + month.lbl_March + month.lbl_April +
                                month.lbl_May + month.lbl_June + month.lbl_July + month.lbl_August + 
                                month.lbl_September + month.lbl_October + month.lbl_November + month.lbl_December +
                                Month_sin12_K1 + Month_cos12_K1 +
                                Turnover_lag12 + Turnover_lag13, 
                              .title = "Linear Model With lagged features",
                              .interactive =FALSE,
                              .show_summary = TRUE)

# Rolling Lags features
monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  mutate(Turnover =  log1p(x = Turnover)) |>
  mutate(Turnover =  standardize_vec(Turnover)) |>
  tk_augment_timeseries_signature(.date_var = Month) |> 
  select(-diff, -matches("(.xts$)|(.iso$)|(hour)|(minute)|(second)|(day)|(week)|(am.pm)")) |>
  dummy_cols(select_columns = c("month.lbl")) |>
  select(-month.lbl) |>
  mutate(index.num = normalize_vec(x = index.num)) |>
  mutate(year = normalize_vec(x = year)) |>
  tk_augment_fourier(.date_var = Month, .periods = 12, .K = 1) |>
  tk_augment_lags(.value = Turnover, .lags = c(12, 13)) |>
  tk_augment_slidify(.value   = c(Turnover_lag12, Turnover_lag13),
                     .f       = ~ mean(.x, na.rm = TRUE), 
                     .period  = c(3, 6, 9, 12),
                     .partial = TRUE,
                     .align   = "center") |>
  plot_time_series_regression(.date_var = Month, 
                              .formula = Turnover ~ as.numeric(Month) + 
                                index.num + year + half + quarter + month +
                                month.lbl_January + month.lbl_February + month.lbl_March + month.lbl_April +
                                month.lbl_May + month.lbl_June + month.lbl_July + month.lbl_August + 
                                month.lbl_September + month.lbl_October + month.lbl_November + month.lbl_December +
                                Month_sin12_K1 + Month_cos12_K1 + 
                                Turnover_lag12 + Turnover_lag12_roll_3  + Turnover_lag12_roll_6  + Turnover_lag12_roll_9 + Turnover_lag12_roll_12 +
                                Turnover_lag13 + Turnover_lag13_roll_3  + Turnover_lag13_roll_6  + Turnover_lag13_roll_9 + Turnover_lag13_roll_12,
                              .title = "Linear Model With rolling features",
                              .interactive =FALSE,
                              .show_summary = TRUE)

# 04-uniforming feature engineering with recipes
# https://www.business-science.io/time-series/2020/03/18/time-series-machine-learning.html
# load the data
aus_retail_tbl <- aus_retail |> # convert df to tibble for timetk
  tk_tbl()

monthly_retail_tbl <- aus_retail_tbl |>
  filter(State == "Australian Capital Territory") |> # focus on a state
  mutate(Month = as.Date(Month)) |>
  mutate(Industry = as_factor(Industry)) |>
  select(Month, Industry, Turnover) |>
  glimpse()

monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  plot_time_series(Month,Turnover,
                   .facet_scale = "free",
                   .interactive = FALSE)

# split data into training/testing regions
library(tidyquant)
monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  ggplot(aes(x = Month, y = Turnover)) +
  geom_rect(xmin = as.numeric(ymd("2010-07-01")),
            xmax = as.numeric(ymd("2019-01-01")),
            ymin = 0, ymax = 50,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("1990-04-01"), y = 30,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2014-07-01"), y = 20,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  labs(title = "stock price", x = "") +
  theme_tq()

train_tbl <- monthly_retail_tbl |> 
  select(-Industry) |>
  filter(Month < ymd("2010-07-01"))
test_tbl  <- monthly_retail_tbl |> 
  select(-Industry) |>
  filter(Month >= ymd("2010-07-01"))

# Add time series signature to training data
library(recipes)
recipe_spec_timeseries <- recipe(Turnover ~ ., data = train_tbl) |>
  step_timeseries_signature(Month) 

bake(prep(recipe_spec_timeseries), new_data = train_tbl)

# Building Engineered Features on Top of the Recipe

recipe_spec_final <- recipe_spec_timeseries |>
  step_rm(Month) |>
  step_rm(contains("iso"), 
          contains("second"), contains("minute"), contains("hour"),
          contains("am.pm"), contains("xts")) |>
  step_normalize(contains("index.num"), Month_year) |>
  #step_interact(~ date_month.lbl * date_day) |>
  #step_interact(~ date_month.lbl * date_mweek) |>
  #step_interact(~ date_month.lbl * date_wday.lbl * date_yday) |>
  step_dummy(contains("lbl"), one_hot = TRUE) 

bake(prep(recipe_spec_final), new_data = train_tbl)

# 05-building forecasting model with workflows
# create a model specification
library(glmnet)
library(parsnip)
model_spec_glmnet <- linear_reg(mode = "regression", penalty = 10, mixture = 0.7) |>
  set_engine("glmnet")

library(workflows)
# mary up the preprocessing recipe and the model
workflow_glmnet <- workflow() |>
  add_recipe(recipe_spec_final) |>
  add_model(model_spec_glmnet)

workflow_glmnet

# training the model
workflow_trained <- workflow_glmnet |>
  fit(data = train_tbl)

# using test set for validation
prediction_tbl <- workflow_trained |> 
  predict(test_tbl) |>
  bind_cols(test_tbl) 

prediction_tbl

# Visualize the results

monthly_retail_tbl |>
  filter(Industry == Industries[1]) |>
  ggplot(aes(x = Month, y = Turnover)) +
  geom_rect(xmin = as.numeric(ymd("2010-07-01")),
            xmax = as.numeric(ymd("2019-01-01")),
            ymin = 0, ymax = 50,
            fill = palette_light()[[4]], alpha = 0.01) +
  annotate("text", x = ymd("1990-04-01"), y = 30,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2014-07-01"), y = 20,
           color = palette_light()[[1]], label = "Test Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  
  # Add predictions
  geom_point(aes(x = Month, y = .pred), data = prediction_tbl, 
             alpha = 0.5, color = palette_light()[[2]]) +
  theme_tq() +
  labs(title = "GLM: Sample Forecast")

# Calculating forecast error
library(yardstick)
prediction_tbl |> metrics(Turnover, .pred)

# visualize the residuals of test set
prediction_tbl |>
  ggplot(aes(x = Month, y = Turnover - .pred)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  geom_smooth(span = 0.05, color = "red") +
  geom_smooth(span = 1.00, se = FALSE) +
  theme_tq() +
  labs(title = "GLM Model Residuals", x = "") +
  scale_y_continuous(limits = c(-50, 50))

# 06-Forecasting Future Data

# Extract bikes index
idx <- monthly_retail_tbl |> tk_index()

# Get time series summary from index
summary <- idx |> tk_get_timeseries_summary()
summary[1:6] # general summary information
summary[7:12] # the periodicity information

idx_future <- idx |> tk_make_future_timeseries(length_out = 300)

future_tbl <- tibble(Month = idx_future) 

future_tbl

# predict the next 10-months
future_predictions_tbl <- workflow_glmnet |> 
  fit(data = monthly_retail_tbl) |>
  predict(future_tbl) |>
  bind_cols(future_tbl)

# Visualize the forecast
monthly_retail_tbl |>
  ggplot(aes(x = Month, y = Turnover)) +
  geom_rect(xmin = as.numeric(ymd("2010-07-01")),
            xmax = as.numeric(ymd("2019-01-01")),
            ymin = 0, ymax = 50,
            fill = palette_light()[[4]], alpha = 0.01) +
  geom_rect(xmin = as.numeric(ymd("2010-01-01")),
            xmax = as.numeric(ymd("2013-07-01")),
            ymin = 0, ymax = 50,
            fill = palette_light()[[3]], alpha = 0.01) +
  annotate("text", x = ymd("2011-10-01"), y = 30,
           color = palette_light()[[1]], label = "Train Region") +
  annotate("text", x = ymd("2012-10-01"), y = 50,
           color = palette_light()[[1]], label = "Test Region") +
  annotate("text", x = ymd("2013-4-01"), y = 50,
           color = palette_light()[[1]], label = "Forecast Region") +
  geom_point(alpha = 0.5, color = palette_light()[[1]]) +
  # fprecasting with future data
  geom_point(aes(x = Month, y = .pred), data = future_predictions_tbl,
             alpha = 0.5, color = palette_light()[[2]]) +
  geom_smooth(aes(x = Month, y = .pred), data = future_predictions_tbl,
              method = 'loess') + 
  labs(title = "10-Month Forecast", x = "") +
  theme_tq()

