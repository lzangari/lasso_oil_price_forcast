#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################

library(moments)  # for skewness and kurtosis
library(tseries)  # for jarque.bera.test
library(urca)     # for ur.kpss
library(xtable)

save_plots <- "results/monthly"
df <- read.csv("Data/csv/all_data_transformed.csv")

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
        quantile_25 = column_quantiles[[1]],
        quantile_50 = column_quantiles[[2]],
        quantile_75 = column_quantiles[[3]],
        skewness = column_skewness,
        kurtosis = column_kurtosis,
        #jb_p_value = jb_p_value,
        kpss_p_value = kpss_p_value
    )


    # Append the results to the summary statistics data frame
    summary_stats <- rbind(summary_stats, column_summary)
}

# convert the dataframe to latex table and save it to .tex file
summary_stats_tex <- xtable(summary_stats)#, digits = 4)
print(summary_stats_tex, type = "latex",
        file = paste0(save_plots, "/summary_transformation_table.tex"),
        include.rownames = FALSE)


create_svg_from_transformed_table(summary_stats,
                    "results/monthly/summary_transformation.svg", table_width = 20, table_heigh = 12)

# Save the summary statistics data frame to a CSV file
write.csv(summary_stats, "Data/csv/summary_stats.csv", row.names = FALSE)

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
            labs(x = column_name, y = "Density") +
            theme_minimal()

        # Save the plot
        plot_path <- paste0(save_plots, "/histogram_of_data")
        create_dir(plot_path)
        # Save the plot
        ggsave(paste0(column_name, "_histogram.png"), plot = p, path = plot_path)
    }
}