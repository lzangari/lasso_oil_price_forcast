library(Metrics)
library(forecast)


r2 <- function(actual, predicted) {
    1 - sum((predicted - actual)^2) / sum((actual - mean(actual))^2)
}

r2_oos <- function(actual, predicted, y_train) {
    # calculate MSE of the model
    mse_model = mean((predicted - actual)^2)

    # calculate MSE of the benchmark model
    # The benchmark model predicts the mean of y_train for all points
    benchmark_predictions = rep(mean(y_train), length(actual))
    mse_benchmark = mean((benchmark_predictions - actual)^2)

    # calculate R2_OOS
    r2_oos = 1 - mse_model / mse_benchmark
    return(r2_oos)
}


cpse <- function(actual, predicted) {
    return(cumsum((actual - predicted)^2))
}


mase <- function(actual, forecast, y_train) {
    one_step_forecast <- mean(abs(diff(y_train)))
    return(mean(abs(actual - forecast) / one_step_forecast))
}


mspe_adj <- function(actual, predicted, n_predictions) {
    mse_score = Metrics::mse(actual, predicted)
    # Adjust the MSPE by dividing by the number of predictions
    return(mse_score / n_predictions)
}


# Compute evaluation metrics for a set of predictions
evaluate_predictions <- function(actual, predicted, y_train, model_name) {

    # number of predictions
    n_predictions = length(predicted)

    # compute metrics
    # r2 for out-of-sample predictions
    r2_out_of_sample = r2_oos(actual, predicted, y_train)
    # mean squared prediction error (MSPE)
    mspe_score = mspe_adj(actual, predicted, n_predictions)
    # mean absolute error (MAE)
    mae_score = Metrics::mae(actual, predicted)
    # mean absolute scaled error (MASE)
    mase_score = mase(actual, predicted, y_train)
    #forecast::accuracy(forecast::ts(predicted), forecast::ts(actual))[, "MASE"]
    # mean absolute percentage error (MAPE)
    mape_score = Metrics::mape(actual, predicted)


    col_names = c(paste("R2OutOfSample", model_name, sep = "_"),
                  paste("MSPE", model_name, sep = "_"),
                  paste("MAE", model_name, sep = "_"),
                  paste("MASE", model_name, sep = "_"),
                  paste("MAPE", model_name, sep = "_"))
    return(setNames(data.frame(r2_out_of_sample, mspe_score, mae_score, mase_score, mape_score), col_names))

}
