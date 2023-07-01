# Load necessary libraries
library(ggplot2)
library(forecast)

source("utils_plot.R")

# Define a function to decompose the time series and plot the ACF of the residuals
decompose_and_plot_acf <- function(df, column, lag_max, path, frequency = 4) {
    plot_path <- file.path(path, column)
    if (!dir.exists(plot_path)) {
        dir.create(plot_path)
    }
    # Convert the column to a time series object
    ts_data <- ts(df[[column]], frequency = frequency)

    # plot the time series of ts_data
    plot.ts(ts_data, main = paste("Time Series Plot for", column))
    # Save the plot as svg file
    ggsave(paste0(column, "ts_timeseries_plot.svg"), plot = plot.ts(ts_data), dpi = 300,
                                                   width = 9, height = 5, path = plot_path)


    # Decompose the time series
    decomposed <- decompose(ts_data)

    # Plot the decomposed time series and save the plot directly
    ggsave(paste0(column, "_decomposed_plot.svg"), plot = plot(decomposed), dpi = 300,
                                                    width = 9, height = 5, path = plot_path)

    # calculating the 1 and 10 percent significance levels
    threshold_1 <- 2.58/sqrt(length(df[[column]]))  # 1% significance level
    threshold_5 <- 2/sqrt(length(df[[column]]))     # 5% significance level
    threshold_10 <- 1.645/sqrt(length(df[[column]]))  # 10% significance level

    legend_df <- data.frame(
    y = c(NA, NA, NA),
    color = c("1% significance level", "5% significance level", "10% significance level")
    )
    # Plot the ACF of the residuals and add the significance levels and show it as a legend
    acf_plot_residual <- ggAcf(decomposed$random, lag.max = lag_max) +
                    geom_hline(yintercept = c(-threshold_1, threshold_1), linetype = "dashed", color = "red") +
                    geom_hline(yintercept = c(-threshold_5, threshold_5), linetype = "dashed", color = "blue") +
                    geom_hline(yintercept = c(-threshold_10, threshold_10), linetype = "dashed", color = "green")
                    # annotate("text", x = 45, y = -0.125, label = "1% significance", color = "red") +
                    # annotate("text", x = 45, y = -0.15, label = "5% significance", color = "blue") +
                    # annotate("text", x = 45, y = -0.175, label = "10% significance", color = "green")
    # Save the ACF plot
    ggsave(paste0(column, "_acf_plot_residual.svg"), plot = acf_plot_residual, dpi = 300,
                                                    width = 7, height = 5, path = plot_path)


    # plot the forecasterrors of the residuals to check the residuals are normally distributed
    plotForecastErrors(na.omit(decomposed$random))
    # Save the plot as svg file
    ggsave(paste0(column, "_forecast_errors.svg"), plot = plotForecastErrors(decomposed$random), dpi = 300,
                                                    width = 7, height = 5, path = plot_path)


    # Plot the ACF of the trend
    acf_plot_trend <- ggAcf(decomposed$trend, lag.max = lag_max)
    # Save the ACF plot
    ggsave(paste0(column, "_acf_plot_trend.svg"), plot = acf_plot_trend, dpi = 300,
                                                    width = 7, height = 5, path = plot_path)

    # Plot the ACF of the seasonal component
    acf_plot_seasonal <- ggAcf(decomposed$seasonal, lag.max = lag_max)
    # Save the ACF plot
    ggsave(paste0(column, "_acf_plot_seasonal.svg"), plot = acf_plot_seasonal, dpi = 300,
                                                    width = 7, height = 5, path = plot_path)
}


# Function to perform ADF test
perform_adf_analysis <- function(df, column, indicator = "none") {
    # Apply ADF test
    adf_result <- adf.test(na.omit(df[[column]]))

    # Create a dataframe with the results
    # if indicator is none, then the column name is used as the indicator
    if (indicator == "none") {
        indicator <- column
    }
    print(indicator)
    results_df <- data.frame(
        Indicator = indicator,
        Test_Statistic = adf_result$statistic,
        P_Value = adf_result$p.value,
        Significance_Level_1 = ifelse(adf_result$p.value <= 0.01, "Yes", "No"),
        Significance_Level_5 = ifelse(adf_result$p.value <= 0.05, "Yes", "No"),
        Significance_Level_10 = ifelse(adf_result$p.value <= 0.1, "Yes", "No")
    )
    return(results_df)
}

# Function to perform Breusch-Pagan test
analysis_heteroscedacity <- function(indicator, predictor, df) {
    model <- lm(indicator ~ predictor, data=df)
    # Breusch-Pagan test
    bp_test <- bptest(model)
    # create a dataframe with the results
    results_df <- data.frame(
        Indicator = indicator,
        Test_Statistic = bp_test$statistic,
        P_Value = bp_test$p.value,
        Significance_Level_1 = ifelse(bp_test$p.value <= 0.01, "Yes", "No"),
        Significance_Level_5 = ifelse(bp_test$p.value <= 0.05, "Yes", "No"),
        Significance_Level_10 = ifelse(bp_test$p.value <= 0.1, "Yes", "No")
    )
    return(results_df)
}