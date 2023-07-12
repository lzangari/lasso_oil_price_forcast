#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################
# load necessary library
library(mice)
library(glmnet)    # for lasso regression
library(forecast)
library(dplyr)
library(Metrics)

source("evaluation.R")
source("utils_plot.R")

# seed for reproducibility
set.seed(123)

#######################################################################
###----------------------- Prepare Workspace -----------------------###
#######################################################################
# set Working Directory
wd <- file.path(getwd())
setwd(wd)
Sys.setlocale("LC_TIME", "C")

save_plots <- "plots/monthly"
df <- read.csv("Data/csv/all_data_transformed.csv")

rolling <- TRUE
recursive <- TRUE
#######################################################################
###------------------------ Standadization -------------------------###
#######################################################################
# standardize the predictors
# data_scaled <- df
# data_scaled[-c(1, ncol(df))] <- scale(df[-c(1, ncol(df))])

# # print the first few rows of the standardized data
# head(data_scaled)
# names(data_scaled)
# write.csv(data_scaled, file = "Data/csv/all_data_standardized.csv",
#                 row.names = FALSE)

#######################################################################
###-------------------- defining benchmark models ------------------###
#######################################################################

# function to generate no-change (naive) forecast
naive_forecast <- function(y_train, horizon) {
    # the no-change forecast as the last value of the training set, repeated for the horizon
    rep(y_train[length(y_train)], horizon)
}

# function to generate historical mean forecast
historical_forecast <- function(y_train, horizon) {
    # the historical mean forecast is the mean of the training set, repeated for the horizon
    rep(mean(y_train), horizon)
}


#######################################################################
###-------------------- modelling with lasso & CV ------------------###
#######################################################################

# a 12-month window for the smaller window and a 60-month window for the larger window.
# forecasts horizon for 1-month and 6-months ahead.


data_scaled <- read.csv("Data/csv/all_data_standardized.csv")

# function to train Lasso model and make predictions
train_predict <- function(x_train, y_train, x_test, rule ="min", loss_function = "mse") {

    # train Lasso model using cross-validation to choose the optimal lambda value,
    # default folds: 10
    cvfit <- cv.glmnet(x_train, y_train, alpha = 1, type.measure = loss_function) # deviance

    if (rule == "min"){
    # Make predictions with the best lmabda
    preds <- predict(cvfit, s = cvfit$lambda.min, newx = x_test)
    }
    else{
    # Make prediction with one standard error rule
    preds <- predict(cvfit, s = cvfit$lambda.1se, newx = x_test)
    }

    # get the coefficients
    coefs <- setNames(numeric(ncol(x_train)), colnames(x_train))
    # extract non-zero coefficients
    non_zero_coefs <- as.vector(coef(cvfit, s = ifelse(rule == rule,
                    cvfit$lambda.min, cvfit$lambda.1se)))
    # update the relevant positions in the initialized vector with the non-zero coefficients
    coefs[names(coefs)] <- non_zero_coefs

    #coefs <- as.vector(coef(cvfit, s = "lambda.min"))

    return(list(preds = preds, coefs = coefs))
}

plot_model_variables <- function(coef_list, window, horizon, model_plot_path){
    model_coef <- plot_coef_heatmap(coefs = coefs_list, window = window,
                                        horizon = horizon, path = model_plot_path)

    model_size <- plot_model_size(coefs = coefs_list, window = window,
                                        horizon = horizon, path = model_plot_path)
    plot_actual_vs_predicted(predictions_df = predictions_df, window = window,
                                        horizon = horizon, path = model_plot_path)
    plot_line_actual_vs_predicted(predictions_df = predictions_df, window = window,
                                        horizon = horizon, path = model_plot_path)

    return(list(model_coef = model_coef, model_size = model_size))
}

save_overall_results <- function(predictions_df, evaluations_df, rule_model,
                        window_method, model_plot_path){
    # save the predictions
    write.csv(predictions_df, file = paste0(model_plot_path, "/", window_method,"_predictions_",
                                                    rule_model, ".csv"), row.names = FALSE)

    # save the evaluations
    write.csv(evaluations_df, file = paste0(model_plot_path, "/", window_method,"_evaluations_",
                                                    rule_model, ".csv"), row.names = FALSE)
}

###-------------------- modelling using rolling window  -------------------###

if (rolling == TRUE) {
    # loop over rolling windows and horizons (combination of rolling window size and forecasting horizon)
    for (loss_function in c("mse", "deviance")) {
            root_path <- paste0("plots/rolling/", loss_function, "/model_coefficients_")
        # rolling windows of 2 to 10 years
        for (rule_model in c("min", "1se")) { #c("min", "1se")
            # list to store predictions
            predictions_df <- data.frame()
            evaluations_df <- data.frame()
            cpse_df <- data.frame()
            model_plot_path <- paste0(root_path, rule_model)
            # rule_model <- "min"
            # model_plot_path <- paste0("plots/model_coefficients_", rule_model)
            for (num_year in 20:21) {  #3:20
                window <- num_year * 12

                # forecasting horizons of 1, 3, 6, 9, and 12 months
                for (horizon in c(2)) { #c(1, 2, 3, 6, 9, 12)
                    print(paste("Window:", window, ",Horizon:", horizon, ",Rule:", rule_model))
                    #######################################################################
                    # initialize lists and vectors
                    pred_list <- list()

                    all_preds <- NULL
                    all_actuals <- NULL
                    all_naives <- NULL
                    all_historicals <- NULL
                    coefs_list <- list()
                    cpse_list <- list()
                    #######################################################################
                    # prepare the data for the current window and horizon
                    for (i in seq(window + horizon, nrow(data_scaled), by = horizon)) {
                    # get train and test data for current window and horizon
                    train_data <- data_scaled[(i - window - horizon + 1):(i - 1), ][,
                                            !names(data_scaled) %in% c("date", "original_one_lag_oil_return_price")]
                    y_train <- data_scaled[(i - window - horizon + 1):(i - 1),
                                            "original_one_lag_oil_return_price"]
                    test_data_with_date <- data_scaled[i:(i + horizon - 1), ]
                    test_data <- data_scaled[i:(i + horizon - 1), ][,
                                            !names(data_scaled) %in% c("date", "original_one_lag_oil_return_price")]

                    # stop the loop if we reach the end of the dataset
                    if (i + horizon > nrow(data_scaled)) {
                        break
                    }
                    #######################################################################
                    # trains a Lasso model for each rolling window and forecasting horizon and makes predictions
                    results <- train_predict(x_train = as.matrix(train_data),
                                        y_train = y_train, x_test = as.matrix(test_data), rule = rule_model)
                    preds <- results$preds
                    # get the benchmark predictions
                    naive_preds <- naive_forecast(y_train = y_train, horizon = horizon)
                    historical_preds <- historical_forecast(y_train = y_train, horizon = horizon)
                    #######################################################################
                    # get the coefficients from the trained model
                    current_coefs <- results$coefs
                    # store the coefficients and repeat them for the length of the predictions
                    for (h in seq_len(horizon)) {
                        # store the coefficients for each month
                        coefs_list[[i + h - 1]] <- current_coefs
                    }
                    #######################################################################
                    # Create a data frame for this prediction
                    pred_df <- data.frame(Window = rep(window, length(preds)),
                                            Horizon = rep(horizon, length(preds)),
                                            Date = test_data_with_date$date,
                                            Prediction = as.vector(preds),
                                            Truth = test_data_with_date$original_one_lag_oil_return_price,
                                            NaiveForecast = naive_preds,
                                            HistoricalForecast = historical_preds)
                    pred_list[[i]] <- pred_df

                    all_preds <- c(all_preds, preds)
                    all_actuals <- c(all_actuals, test_data_with_date$original_one_lag_oil_return_price)
                    all_naives <- c(all_naives, naive_preds)
                    all_historicals <- c(all_historicals, historical_preds)
                    }

                    # combine all predictions for this window and horizon
                    predictions_df <- rbind(predictions_df, do.call(rbind, pred_list))
                    #######################################################################
                    # combine all evaluation metrics for this window and horizon
                    eval_df <- evaluate_predictions(actual = all_actuals, predicted = all_preds,
                                                        y_train = y_train, model_name = "Lasso")
                    # evaluation metrics for the benchmark models
                    eval_naive <- evaluate_predictions(actual = all_actuals, predicted = all_naives,
                                                    y_train = y_train, model_name = "Naive")

                    eval_historical <- evaluate_predictions(actual = all_actuals, predicted = all_historicals,
                                                    y_train = y_train, model_name = "Historical")
                    # merge the evaluations into one data frame
                    eval_df <- cbind(eval_df, eval_naive, eval_historical)

                    eval_df$Window <- window
                    eval_df$Horizon <- horizon
                    evaluations_df <- rbind(evaluations_df, eval_df)
                    #######################################################################
                    # calculate cpse for each window and horizon
                    cpse_preds <- cpse(actual = all_actuals, predicted = all_preds)
                    cpse_naive <- cpse(actual = all_actuals, predicted = all_naives)
                    cpse_historical <- cpse(actual = all_actuals, predicted = all_historicals)
                    # store the cpse in a dataframe
                    cpse_df_i <- data.frame(Window = rep(window, length(cpse_preds)),
                                Horizon = rep(horizon, length(cpse_preds)),
                                Date = predictions_df[predictions_df$Window == window & predictions_df$Horizon == horizon, ]$Date,
                                CPSE_Lasso = cpse_preds,
                                CPSE_Naive = cpse_naive,
                                CPSE_Historical = cpse_historical)
                    cpse_list[[i]] <- cpse_df_i
                    # combine all cpse for this window and horizon
                    cpse_df <- rbind(cpse_df, do.call(rbind, cpse_list))
                    #######################################################################
                    # plot the predictions and parameters
                    plot_model_variables(coef_list = coefs_list, window = window, horizon = horizon,
                                                                model_plot_path = model_plot_path)
                    plot_cpse(cpse_df = cpse_df, window = window, horizon = horizon,
                                                                path = model_plot_path)

                }
            }

            # save the predictions
            save_overall_results(predictions_df = predictions_df , evaluations_df = evaluations_df,
                    rule_model = rule_model, window_method = "rolling", model_plot_path = model_plot_path)
            }
    }
}
###-------------------- modelling using recursive window  -------------------###

if (recursive == TRUE) {
    for (loss_function in c("mse", "deviance")) {

        root_path <- paste0("plots/recursive/", loss_function, "/model_coefficients_")

        # loop over recursive windows and rule models (minimum and 1 standard error)
        for (rule_model in c("min", "1se")) {
            # list to store predictions
            predictions_df <- data.frame()
            evaluations_df <- data.frame()
            cpse_df <- data.frame()
            model_plot_path <- paste0(root_path, rule_model)
            # initialize lists and vectors
            pred_list <- list()
            coefs_list <- list()
            cpse_list <- list()
            all_preds <- NULL
            all_actuals <- NULL
            all_naives <- NULL
            all_historicals <- NULL
            print(paste("Recursive, Rule:", rule_model))
            #######################################################################
            # prepare the data for the current window and horizon (keep at least five year 12*5)
            for (i in seq(60, nrow(data_scaled), by = 1)) {
                # get train and test data for current window and horizon
                train_data <- data_scaled[1:(i - 1),
                                !names(data_scaled) %in% c("date", "original_one_lag_oil_return_price")]
                y_train <- data_scaled[1:(i - 1), "original_one_lag_oil_return_price"]
                test_data_with_date <- data_scaled[i, ]
                test_data <- data_scaled[i, ][!names(data_scaled) %in% c("date", "original_one_lag_oil_return_price")]

                #######################################################################
                # trains a Lasso model for each rolling window and forecasting horizon and makes predictions
                results <- train_predict(x_train = as.matrix(train_data),
                                    y_train = y_train, x_test = as.matrix(test_data), rule = rule_model)
                preds <- results$preds

                # get the benchmark predictions
                naive_preds <- naive_forecast(y_train = y_train, horizon = 1)
                historical_preds <- historical_forecast(y_train = y_train, horizon = 1)
                #######################################################################

                # get the coefficients from the trained model
                coefs_list[[i]] <- results$coefs
                #######################################################################
                # Create a data frame for this prediction
                pred_df <- data.frame(Window = "recursive",
                                        Horizon = 1,
                                        Date = test_data_with_date$date,
                                        Prediction = as.vector(preds),
                                        Truth = test_data_with_date$original_one_lag_oil_return_price,
                                        NaiveForecast = naive_preds,
                                        HistoricalForecast = historical_preds)
                pred_list[[i]] <- pred_df

                all_preds <- c(all_preds, preds)
                all_actuals <- c(all_actuals, test_data_with_date$original_one_lag_oil_return_price)
                all_naives <- c(all_naives, naive_preds)
                all_historicals <- c(all_historicals, historical_preds)
            }

            # combine all predictions for this window and horizon
            predictions_df <- rbind(predictions_df, do.call(rbind, pred_list))
            #######################################################################
            #complete_window <- c(60:nrow(data_scaled))
            # combine all evaluation metrics for this window and horizon
            eval_df <- evaluate_predictions(actual = all_actuals, predicted = all_preds,
                                            y_train = y_train, model_name = "Lasso")
            # evaluation metrics for the benchmark models
            eval_naive <- evaluate_predictions(actual = all_actuals, predicted = all_naives,
                                            y_train = y_train, model_name = "Naive")
            eval_historical <- evaluate_predictions(actual = all_actuals, predicted = all_historicals,
                                            y_train = y_train, model_name = "Historical")
            eval_df <- cbind(eval_df, eval_naive, eval_historical)

            eval_df$Window <- "recursive"
            eval_df$Horizon <- 1
            evaluations_df <- rbind(evaluations_df, eval_df)
            #######################################################################
            # calculate cpse for each window and horizon
            cpse_preds <- cpse(actual = all_actuals, predicted = all_preds)
            cpse_naive <- cpse(actual = all_actuals, predicted = all_naives)
            cpse_historical <- cpse(actual = all_actuals, predicted = all_historicals)
            # store the cpse in a dataframe
            cpse_df_i <- data.frame(Window = "recursive",
                        Horizon = rep(1, length(cpse_preds)),
                        Date = predictions_df[predictions_df$Window == "recursive" & predictions_df$Horizon == 1, ]$Date,
                        CPSE_Lasso = cpse_preds,
                        CPSE_Naive = cpse_naive,
                        CPSE_Historical = cpse_historical)
            cpse_list[[i]] <- cpse_df_i
            # combine all cpse for this window and horizon
            cpse_df <- rbind(cpse_df, do.call(rbind, cpse_list))
            #######################################################################
            # plot the predictions and parameters
            plot_model_variables(coef_list = coefs_list, window = "recursive", horizon = 1,
                                                    model_plot_path = model_plot_path)
            plot_cpse(cpse_df = cpse_df, window = "recursive", horizon = 1,
                                                            path = model_plot_path)
            #######################################################################

            # save the predictions
            save_overall_results(predictions_df = predictions_df, evaluations_df = evaluations_df,
                    rule_model = rule_model, window_method = "recursive", model_plot_path = model_plot_path)
        }
    }
}