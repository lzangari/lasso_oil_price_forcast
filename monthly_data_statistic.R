#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################

library(moments)  # for skewness and kurtosis
library(tseries)  # for jarque.bera.test
library(urca)     # for ur.kpss

save_plots <- "C:/Repositories/lasso_oil_price_forcast/plots/monthly"
df <- read.csv("C:/Repositories/lasso_oil_price_forcast/Data/csv/all_data_transformed.csv")

source("utils_plot.R")

#######################################################################
###------------------- Statistical descriptive ---------------------###
#######################################################################

# Initialize an empty data frame to store the results
summary_stats <- data.frame()

# Loop through all column names in the dataframe
for (column_name in names(df)[-1]) {
    # Calculate the summary statistics for the column
    column_data <- df[[column_name]]
    column_data <- column_data[!is.na(column_data)]  # Remove NA values
    column_mean <- mean(column_data)
    column_sd <- sd(column_data)
    column_min <- min(column_data)
    column_max <- max(column_data)
    column_quantiles <- quantile(column_data, probs = c(0.25, 0.5, 0.75))
    column_skewness <- moments::skewness(column_data)
    column_kurtosis <- moments::kurtosis(column_data)

    # # Perform the Jarque-Bera test
    # jb_test <- tseries::jarque.bera.test(column_data)
    # jb_p_value <- jb_test$p.value

    # Perform the KPSS test
    kpss_test <- tseries::kpss.test(column_data, null = "Level")
    kpss_p_value <- kpss_test$p.value

    # Combine the results into a data frame
    column_summary <- data.frame(
        column_name = column_name,
        mean = column_mean,
        sd = column_sd,
        min = column_min,
        max = column_max,
        quantile_25 = column_quantiles[1],
        quantile_50 = column_quantiles[2],
        quantile_75 = column_quantiles[3],
        skewness = column_skewness,
        kurtosis = column_kurtosis,
        #jb_p_value = jb_p_value,
        kpss_p_value = kpss_p_value
    )


    # Append the results to the summary statistics data frame
    summary_stats <- rbind(summary_stats, column_summary)
}

create_svg_from_transformed_table(summary_stats, "C:/Repositories/lasso_oil_price_forcast/plots/monthly/summary_transformation.svg", table_width = 20, table_heigh = 12)

# Save the summary statistics data frame to a CSV file
write.csv(summary_stats, "C:/Repositories/lasso_oil_price_forcast/Data/csv/summary_stats.csv", row.names = FALSE)

#######################################################################
###------------------- Transformed data study ----------------------###
#######################################################################

# histogram of the data
# Loop through all column names in the dataframe
for (column_name in names(df)[-1]) {
    # Skip the 'one_lag_oil_price' column
    if (column_name != "original_one_lag_oil_return_price") {
        # Create a histogram for the column
        p <- ggplot(df, aes_string(column_name)) +
            geom_histogram(aes(y = ..density..), bins = 30, fill = "#163925", alpha = 0.5) +
            geom_density(color = "#163925") +
            labs(x = column_name, y = "Density")


        # # # Add a scatter plot of 'one_lag_oil_price'
        # p <- p + geom_point(aes_string(x = column_name, y = "original_one_lag_oil_return_price"), color = "red") +
        #     labs(title = paste("Histogram of", column_name, "with one_lag_oil_price overlay"))

        # Save the plot
        ggsave(paste0(column_name, "_histogram.png"), plot = p, path = paste0((save_plots), "/histogram_of_data"))
        #ggsave(paste0(column_name, "_histogram.svg"), plot = p, path = paste0((save_plots), "/histogram_of_data"))
    }
}

# # line plot over yearly change versus price
# # Convert the 'date' column to Date class
# df$date <- as.Date(df$date)

# # Create a new column for the year
# df$year <- format(df$date, "%Y")

# # Loop through all column names in the dataframe
# for (column_name in names(df)[-1]) {
#     # Skip the 'date' and 'year' columns
#     if (column_name != "date" && column_name != "year" && column_name != "original_one_lag_oil_return_price") {
#         # Create a line plot for the column
#         p <- ggplot(df, aes_string(x = "year", y = column_name)) +
#             geom_line(color = "#114e34") +
#             labs(x = "Year", y = column_name) +
#             theme(axis.text.x = element_text(angle = 90))

#         # Add a scatter plot of 'original_one_lag_oil_return_price'
#         p <- p + geom_point(aes_string(x = "year", y = "original_one_lag_oil_return_price"), color = "#b98c13") +
#             labs(title = paste("Yearly trend of", column_name, "with return price overlay"))

#         # Save the plot
#         ggsave(paste0(column_name, "_yearly_trend.svg"), plot = p)
#     }
# }
