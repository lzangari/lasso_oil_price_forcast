library(Metrics)
library(forecast)


r2 <- function(actual, predicted) {
    1 - sum((predicted - actual)^2) / sum((actual - mean(actual))^2)
}

mase <- function(actual, forecast, train) {
    one_step_forecast <- mean(abs(diff(train)))
    return(mean(abs(actual - forecast) / one_step_forecast))
}

# Compute evaluation metrics for a set of predictions
evaluate_predictions <- function(actual, predicted, y_train) {

    # compute metrics
    # r2 for out-of-sample predictions
    r2_out_of_sample = r2(actual, predicted) #Metrics::r2(actual, predicted)
    # mean squared prediction error (MSPE)
    mspe_score = Metrics::mse(actual, predicted)
    # mean absolute error (MAE)
    mae_score = Metrics::mae(actual, predicted)
    # mean absolute scaled error (MASE)
    mase_score = mase(actual, predicted, y_train)
    #forecast::accuracy(forecast::ts(predicted), forecast::ts(actual))[, "MASE"]
    # mean absolute percentage error (MAPE)
    mape_score = Metrics::mape(actual, predicted)

    # cumulative squared prediction error (CSPE)
    #cspe_score = cumsum((actual - predicted)^2)

    # HAC standard errors - usually, HAC standard errors are computed for the
    # coefficients of a model, not for the predictions
    # TODO
    hac_se = NA

    return(data.frame(R2OutOfSample = r2_out_of_sample,
                      MSPE = mspe_score,
                      MAE = mae_score,
                      MASE = mase_score,
                      MAPE = mape_score,
                      #CSPE = cspe_score,
                      HAC_SE = hac_se))
}
