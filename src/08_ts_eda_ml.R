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


library(tidyverse)
library(randomForest)
data=read.table('data/ts_data/fishBiomassData.txt',h=TRUE)

mydata <- data |>
  subset(STATION=="VOLPla" & SP == "CHE") |>
  select(YEAR, BIOMASS)

data_unique <- mydata |> # ensure Unique Yearly Records
  group_by(YEAR) |>
  summarise(BIOM = mean(BIOMASS, na.rm = TRUE),
            .groups = "drop")

data_missing <- data_unique |> # Fill missing years with NA
  # group_by(YEAR) |>
  # summarise(BIOM = mean(BIOM, na.rm = TRUE)) |>
  # ungroup() |>
  complete(YEAR = full_seq(YEAR, 1))

data_imputing <- data_missing |>
  mutate(BIOM = zoo::na.approx(BIOM, x = YEAR, na.rm = FALSE)) |>
  mutate(YEAR = ymd(paste0(YEAR, "-01-01"))) # Converting to Date 

ts <- ts(data_imputing$BIOM, start=1994, frequency=1)
plot(ts)
ts_org <- window(ts, end = 2018)
ts_trf <- ts_org |> log() |> diff(1)
lag_order <- 2 # how many past observations
horizon <- 2                                              
ts_mbd <- embed(ts_trf, lag_order + 1)
Y_train <- ts_mbd[, 1] 
X_train <- ts_mbd[, -1] 
y_test <- window(ts, start = 2019, end = 2020) 
x_test <- ts_mbd[nrow(ts_mbd), c(1:lag_order)]
pred_rf <- numeric(horizon)
for (i in 1:horizon){
  set.seed(1) 
  fit_rf <- randomForest(X_train, Y_train) 
  pred_rf[i] <- predict(fit_rf, x_test) 
  Y_train <- Y_train[-1] # training data update
  X_train <- X_train[-nrow(X_train), ] 
}
pred_rf
exp_term <- exp(cumsum(pred_rf)) # Undoes differencing and log-transform
last_obs <- as.vector(tail(ts_org, 1)) 
backtrans_fc <- last_obs * exp_term 
y_pred <- ts(backtrans_fc, start = 2019, frequency = 1)
forecast::accuracy(as.numeric(y_pred), as.numeric(y_test))
library(fpp2)
ts_fc <- cbind(ts,pred = c(rep(NA, length(ts_org)), y_pred)) 
plot_fc <- ts_fc |> autoplot() + theme_minimal() 
plot_fc

