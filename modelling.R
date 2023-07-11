#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################
# Load necessary library
library(mice)
library(glmnet)    # for lasso regression
library(forecast)
library(dplyr)

# seed for reproducibility
set.seed(84)

#######################################################################
###----------------------- Prepare Workspace -----------------------###
#######################################################################
#Set Working Directory
wd <- file.path(getwd())
setwd(wd)
Sys.setlocale("LC_TIME", "C")

save_plots <- "C:/Repositories/lasso_oil_price_forcast/plots/monthly"
df <- read.csv("C:/Repositories/lasso_oil_price_forcast/Data/csv/all_data_transformed.csv")


#######################################################################
###------------------------ Standadization -------------------------###
#######################################################################
# # Standardize the predictors
# data_scaled <- df
# data_scaled[-c(1, ncol(df))] <- scale(df[-c(1, ncol(df))])

# # Print the first few rows of the standardized data
# head(data_scaled)
# names(data_scaled)
# write.csv(data_scaled, file = "C:/Repositories/lasso_oil_price_forcast/Data/csv/all_data_standardized.csv",
#                 row.names = FALSE)

#######################################################################
###-------------------- modelling with lasso & CV ------------------###
#######################################################################

# a 12-month window for the smaller window and a 60-month window for the larger window.
# forecasts horizon for 1-month and 6-months ahead.
# for security ordered by date
#data_scaled <- data_scaled[order(data_scaled$date),]

data_scaled <- read.csv("C:/Repositories/lasso_oil_price_forcast/Data/csv/all_data_standardized.csv")

# Function to train Lasso model and make predictions
train_predict <- function(train_data, test_data, rule="min") {
    x_train <- as.matrix(train_data[, !names(train_data) %in% "original_one_lag_oil_return_price"])
    y_train <- train_data$original_one_lag_oil_return_price
    x_test <- as.matrix(test_data[, !names(test_data) %in% "original_one_lag_oil_return_price"])

    # Train Lasso model
    # TODO check number of default folds
    cvfit <- cv.glmnet(x_train, y_train, alpha = 1)

    if (rule == "min"){
    # Make predictions with the best lmabda
    preds <- predict(cvfit, s = cvfit$lambda.min, newx = x_test)
    }
    else{
    # Make prediction with one standard error rule
    preds <- predict(cvfit, s = cvfit$lambda.1se, newx = x_test)
    }
    return(preds)
}


# List to store predictions
predictions <- list()

# # Loop over rolling windows and horizons
for (window in c(12, 60)) { # rolling windows of 1 year and 5 years
    for (horizon in c(1, 6)) { # forecasting horizons of 1 month and 6 months
        pred_list <- list()

        for (i in seq(window + horizon, nrow(df))) {
        # Get train and test data for current window and horizon
        # TODO check the date for train and test
        # TODO check if i should drop the date or should i handle it in a way
        train_data <- data_scaled[(i-window-horizon+1):(i-horizon),][, !names(data_scaled) %in% "date"]
        test_date_with_date <- data_scaled[i:(i+horizon-1),]
        test_data <- data_scaled[i:(i+horizon-1),][, !names(data_scaled) %in% "date"]

        # Train model and make predictions
        preds <- train_predict(train_data, test_data)

        pred_list[[i]] <- data.frame(Date = test_date_with_date$date, Prediction = preds)
        }

        # Combine all predictions for this window and horizon
        predictions[[paste("Window", window, "Horizon", horizon)]] <- bind_rows(pred_list)
    }
}
