# Load necessary libraries
library(ggplot2)
library(forecast)

# Define a function to decompose the time series and plot the ACF of the residuals
decompose_and_plot_acf <- function(df, column, lag_max, path, frequency = 4) {
    # Convert the column to a time series object
    ts_data <- ts(df[[column]], frequency = frequency)

    # Decompose the time series
    decomposed <- decompose(ts_data)

    # Plot the decomposed time series and save the plot directly
    ggsave(paste0(column, "_decomposed_plot.svg"), plot = plot(decomposed), dpi = 300,
                                                    width = 9, height = 5, path = path)

    # Plot the ACF of the residuals
    acf_plot_residual <- ggAcf(decomposed$random, lag.max = lag_max)
    # Save the ACF plot
    ggsave(paste0(column, "_acf_plot_residual.svg"), plot = acf_plot_residual, dpi = 300,
                                                    width = 7, height = 5, path = path)

    # Plot the ACF of the trend
    acf_plot_trend <- ggAcf(decomposed$trend, lag.max = lag_max)
    # Save the ACF plot
    ggsave(paste0(column, "_acf_plot_trend.svg"), plot = acf_plot_trend, dpi = 300,
                                                    width = 7, height = 5, path = path)

    # Plot the ACF of the seasonal component
    acf_plot_seasonal <- ggAcf(decomposed$seasonal, lag.max = lag_max)
    # Save the ACF plot
    ggsave(paste0(column, "_acf_plot_seasonal.svg"), plot = acf_plot_seasonal, dpi = 300,
                                                    width = 7, height = 5, path = path)
}


# Function to perform ADF test
perform_adf_analysis <- function(df, column) {
    # Apply ADF test
    adf_result <- adf.test(na.omit(df[[column]]))

    # Create a dataframe with the results
    results_df <- data.frame(
        Column = column,
        Test_Statistic = adf_result$statistic,
        P_Value = adf_result$p.value,
        Significance_Level_1 = ifelse(adf_result$p.value <= 0.01, "Yes", "No"),
        Significance_Level_5 = ifelse(adf_result$p.value <= 0.05, "Yes", "No"),
        Significance_Level_10 = ifelse(adf_result$p.value <= 0.1, "Yes", "No")
    )
    return(results_df)
}