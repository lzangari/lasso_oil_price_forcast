library(ggplot2)
library(scales)
library(gridExtra)
library(gridSVG)
library(grid)

# set Working Directory
wd <- file.path(getwd())
setwd(wd)


plot_names <- list("oil_price" = "Oil Price",
                    "one_lag_oil_stock" = "U.S. Ending Stocks",
                    "three_lag_log_oil_export" = "U.S. Exports",
                    "twelve_lag_log_oil_production" = "U.S. Field Production",
                    "two_lag_product_supply" = "U.S. Product Supplied",
                    "one_lag_product_net_import" = "U.S. Net Imports",
                    "one_lag_opec_production" = "OPEC Oil Production",
                    "one_lag_kilian_index" = "Kilian Index",
                    "cpi_oecd" = "OECD CPI",
                    "two_lag_cpi_usa" = "U.S. CPI",
                    "twelve_lag_log_unrate_usa" = "U.S. Unemployment Rate",
                    "one_lag_unrate_eu" = "EU Unemployment Rate",
                    "one_lag_epui_eu" = "EU EPUI",
                    "one_lag_epui_usa" = "U.S. EPUI",
                    "one_lag_gdp_eu" = "EU GDP",
                    "twelve_lag_log_gdp_usa" = "U.S. GDP",
                    "one_lag_federal_fund" = "Federal Funds Rate",
                    "twelve_lag_log_tb3ms" = "3-Month Treasury Bill",
                    "two_lag_sp500" = "S&P 500",
                    "two_lag_msci" = "MSCI World",
                    "twelve_lag_log_stoxx600" = "EURO STOXX 600",
                    "one_lag_gsci" = "GSCI",
                    "one_lag_gold_price" = "Gold",
                    "one_lag_copper_future" = "Copper Future",
                    "original_geopolitical_risk" = "GRI",
                    "two_lag_wui" = "WUI",
                    "original_one_lag_oil_return_price" = "Return price [1 lag]")



#######################################################################
# create directory if it does not exis
create_dir <- function(path) {
    # create a new directory if it does not exist
    if (!dir.exists(path)) {
        dir.create(path, recursive = TRUE)
    }
}

# function to create the weekly plot
create_weekly_plot <- function(df, path, plot_name) {
    # convert the date format
    df$Week <- as.Date(df$Week, format = "%m/%d/%Y")
    # sort the data frame by date in ascending order
    df <- df[order(df$Week), ]
    # define the column names
    x_column <- names(df)[1]
    y_column <- names(df)[2]
    # Visualize the data as a time series plot
    p1 <- ggplot(df, aes_string(x = x_column, y = y_column)) +
        geom_line() +
        scale_x_date(date_breaks = "104 weeks", date_labels = "%Y-%m-%d") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(x = "Date", y = plot_name, title = paste("Time Series Plot for", plot_name)) +
        scale_y_continuous(labels = scientific)

    # Save the plot as svg file
    ggsave(paste0(y_column, "_timeseries_plot.svg"), p1, path = path)
}

# function to create the monthly plot
create_monthly_plot <- function(df, path, y_column, plot_name) {
    # convert the date format
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    # sort the data frame by date in ascending order
    df <- df[order(df$date), ]
    # define the column names
    x_column <- names(df)[1]

    # create shaded areas for recession periods
    shaded_areas <- data.frame(
        start = as.Date(c("1990-07-01", "2001-03-01", "2007-12-01", "2020-02-01")),
        end = as.Date(c("1991-03-01", "2001-11-01", "2009-06-01", "2020-04-01")),
        recession = "Recession years"
    )
    legend_data <- data.frame(fill = c("Recession years"))
    # Visualize the data as a time series plot
    p1 <- ggplot(df, aes_string(x = x_column, y = y_column)) +
        geom_line(color = "#000064") +
        geom_rect(data = shaded_areas, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf),
                                    fill = "#ff8000", alpha = 0.3, inherit.aes =FALSE) +
        #scale_color_manual(labels = c("Recession years"), values = c("#ff8000")) +
        scale_x_date(date_breaks = "24 months", date_labels = "%Y-%m") +
        theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
        labs(x = "Date", y = plot_name, title = paste("Time Series Plot for", plot_name))
        #scale_y_continuous(labels = scales::scientific)

    # create a new directory if it does not exist
    plot_path <- file.path(path, y_column)
    create_dir(plot_path)

    # Save the plot as svg file
    ggsave(paste0(y_column, "_timeseries_plot.svg"), p1, path = plot_path)

}

# function to get the plot for the residuals
plot_forecast_errors <- function(forecasterrors, column, path){
    # make a histogram of the forecast errors:
    mybinsize <- IQR(forecasterrors)/4
    mysd   <- sd(forecasterrors)
    mymin  <- min(forecasterrors) - mysd*5
    mymax  <- max(forecasterrors) + mysd*3
    # generate normally distributed data with mean 0 and standard deviation mysd
    mynorm <- rnorm(10000, mean=0, sd=mysd)
    mymin2 <- min(mynorm)
    mymax2 <- max(mynorm)
    if (mymin2 < mymin) { mymin <- mymin2 }
    if (mymax2 > mymax) { mymax <- mymax2 }
    # make a red histogram of the forecast errors, with the normally distributed data overlaid:
    mybins <- seq(mymin, mymax, mybinsize)
    # initiate the save plot
    svg(filename = paste0(path, "/", paste0(column, "_residual_hist.svg")))
    # create histrogram
    hist(forecasterrors, col="#075234", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve on top of the histogram of forecast errors:
    hist(forecasterrors, col="#075234", freq=FALSE, breaks=mybins)
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    points(myhist$mids, myhist$density, type="l", col="#ffae00", lwd=2)

}

# function to save the plots as svg files
create_svg_from_table <- function(results, name, table_width = 9, table_heigh = 7){
    # assemble the table
    result_table <- do.call(rbind, results)

    table_grob <- tableGrob(result_table, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
    grDevices::svg(name, width = table_width, height = table_heigh)
    grid.newpage()
    grid.draw(table_grob)
    grid.export(name)
    dev.off()
}

# function to save the plots as svg files with a transformed table
create_svg_from_transformed_table <- function(results, name, table_width = 9, table_heigh = 7){
    # assemble the table
    result_table <- do.call(rbind, results)

    # Transpose the table
    result_table <- as.data.frame(t(result_table))

    table_grob <- tableGrob(result_table, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
    grDevices::svg(name, width = table_width, height = table_heigh)
    grid.newpage()
    grid.draw(table_grob)
    grid.export(name)
    dev.off()
}

# function to plot the model coefficients
barplot_model_coefs <- function(cvfit, predictor_names, window, horizon) {

    # Get the coefficients
    coefs <- as.vector(coef(cvfit, s = "lambda.min"))

    # Create a data frame
    coefs_df <- data.frame(Variable = c("(Intercept)", predictor_names), Coefficient = coefs)

    # Remove the intercept from the coefficients
    coefs_df <- coefs_df[-1, ]

    # Create the plot
    p <- ggplot(coefs_df, aes(x = reorder(Variable, Coefficient), y = Coefficient)) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(x = "Variable", y = "Coefficient",
             title = paste("Lasso Coefficients (Window:", window, ", Horizon:", horizon, ")"))
    # save the plot
    plot_path <- paste0(path, "/bar_plot_coefficients")
    create_dir(plot_path)
    ggsave(filename = paste0(plot_path, "/monthly_coeff_window_", window, "_horizon_",
                                            horizon, ".png"), plot = p)

}


plot_coef_heatmap <- function(coefs, window, horizon, path) {

    # create a sequence of dates based on the length of the coefficients list
    dates <- seq.Date(from = as.Date("1990-08-01"), by = "month", length.out = length(coefs))

    # initialize an empty data frame
    df <- data.frame(Date = as.Date(character()), Variable = character(), Coefficients = double())

    # loop through the coefficients list
    for(i in seq_along(coefs)){
        # Check if names(coefs[[i]]) is not NULL before proceeding
        if(!is.null(names(coefs[[i]]))) {
            new_names <- sapply(names(coefs[[i]]), function(x) {
                if(x %in% names(plot_names)) {
                    return(plot_names[[x]])
                } else {
                    return(x)
                }
            })
            names(coefs[[i]]) <- new_names
            coefs_df <- data.frame(Date = rep(dates[i], length(coefs[[i]])),
                                Variable = names(coefs[[i]]),
                                Coefficients = coefs[[i]],
                                stringsAsFactors = FALSE)
            df <- rbind(df, coefs_df)
        }
    }
    # remove row for intercept
    df <- df[df$Variable != "(Intercept)", ]


    # create the plot
    p <- ggplot(df, aes(x = Date, y = Variable)) +
        geom_tile(aes(fill = Coefficients)) +
        scale_fill_gradient2(low = "#0c095b", mid = "#f0f2f2", high = "#760f08", midpoint = 0) +
        theme_classic() +
        labs(x = "Date", y = "Variable", fill = "Coefficient",
            title = paste("Lasso Coefficients Heatmap (Window:", window, ", Horizon:", horizon, ")"))
    # save the plot
    plot_path <- paste0(path, "/monthly_perfomance")
    create_dir(plot_path)
    ggsave(filename = paste0(plot_path, "/heatmap_coefficients_window_", window, "_horizon_",
                                            horizon, ".png"), plot = p)
    data_path <- paste0(path, "/coefficients_csv")
    create_dir(data_path)
    write.csv(df, file = paste0(data_path, "/coefficients_window_", window, "_horizon_",
                                            horizon, ".csv"), row.names = FALSE)
    return(df)

}


plot_model_size <- function(coefs, window, horizon, path) {

    # create a sequence of dates based on the length of the coefficients list
    dates <- seq.Date(from =  as.Date("1990-08-01"), by = "month", length.out = length(coefs))

    # initialize an empty data frame
    df <- data.frame(Date = as.Date(character()), ModelSize = integer())

    # loop through the coefficients list
    for(i in seq_along(coefs)){
        # Count the number of non-zero coefficients
        model_size <- sum(coefs[[i]] != 0)

        model_size_df <- data.frame(Date = dates[i],
                                    ModelSize = model_size,
                                    stringsAsFactors = FALSE)
        df <- rbind(df, model_size_df)
    }


    # create the plot
    p <- ggplot(df, aes(x = Date, y = ModelSize)) +
            geom_line(color = "#163925") +
            theme_minimal() +
            labs(x = "Date", y = "Model Size",
                title = paste("Lasso Model Size (Window:", window, ", Horizon:", horizon, ")"))

    # save the plot
    plot_path <- paste0(path, "/model_size")
    create_dir(plot_path)
    ggsave(filename = paste0(plot_path, "/model_size_window_", window, "_horizon_",
                                            horizon, ".png"), plot = p)
    data_path <- paste0(path, "/model_size_csv")
    create_dir(data_path)
    write.csv(df, file = paste0(data_path, "/model_size_window_", window, "_horizon_",
                                            horizon, ".csv"), row.names = FALSE)
    return(df)
}

# function to plot actual vs. predicted values
plot_actual_vs_predicted <- function(predictions_df, window, horizon, path) {

    # filter the data for the specified window and horizon
    data <- subset(predictions_df, Window == window & Horizon == horizon)
    #predictions_df[predictions_df$Window == window & predictions_df$Horizon == horizon, ]
    # create the plot
    p <- ggplot(data, aes(x = Truth, y = Prediction)) +
    geom_point(alpha = 0.5) +
    geom_abline(slope = 1, intercept = 0, color = "#760f08") +
    coord_equal() +
    theme_minimal() +
    labs(x = "Actual", y = "Predicted",
         title = paste("Actual vs Predicted (Window:", window, ", Horizon:", horizon, ")"))

    # save the plot
    plot_path <- paste0(path, "/actual_vs_predicted")
    create_dir(plot_path)
    ggsave(filename = paste0(plot_path, "/truth_vs_predicted_window_", window, "_horizon_",
                                            horizon, ".png"), plot = p, width = 10)

}


plot_line_actual_vs_predicted <- function(predictions_df, window, horizon, path){
    # filter the data for the specified window and horizon
    data <- subset(predictions_df, Window == window & Horizon == horizon)
    # create the plot
    data$Date <- as.Date(data$Date)
    p <- ggplot(data, aes(x = Date)) +
        geom_line(aes(y = Truth, colour = "Actual", linetype = "Actual"), size = 0.5, alpha = 0.65) +
        geom_line(aes(y = Prediction, colour = "Predicted", linetype = "Predicted"), size = 0.5) +
        geom_line(aes(y = NaiveForecast, colour = "Naive", linetype = "Naive"), size = 0.5) +
        geom_line(aes(y = HistoricalForecast, colour = "Historical", linetype = "Historical"), size = 0.5) +
        theme_minimal() +
        scale_color_manual(values = c("Actual" = "black", "Predicted" = "red",
                                                    "Naive" = "blue", "Historical" = "#24a724")) +
        scale_linetype_manual(values = c("Actual" = "solid", "Predicted" = "solid",
                                                    "Naive" = "dashed", "Historical" = "solid")) +
        labs(x = "Date", y = "Monthly crude oil return (%)",
            title = paste("Actual and Predicted over time (Window:", window, ", Horizon:", horizon, ")"),
            color = "Legend", linetype = "Legend") +
        theme(legend.position = "bottom", legend.key.size=unit(3,"lines"))

    # save the plot
    plot_path <- paste0(path, "/prediction_performance")
    create_dir(plot_path)
    ggsave(filename = paste0(plot_path, "/monthly_performance_window_", window, "_horizon_",
                                            horizon, ".png"), plot = p, width = 12)
}


plot_cpse <- function(cpse_df, window, horizon, path) {
    # convert the date column from character to date
    cpse_df$Date <- as.Date(cpse_df$Date)
    # filter the data for the specified window and horizon
    cpse_filtered <- cpse_df[cpse_df$Window == window & cpse_df$Horizon == horizon, ]

    p <- ggplot(cpse_filtered, aes(x = Date)) +
        geom_line(aes(y = CPSE_Lasso, color = "Lasso")) +
        geom_line(aes(y = CPSE_Naive, color = "Naive")) +
        geom_line(aes(y = CPSE_Historical, color = "Historical")) +
        labs(x = "Date", y = "CPSE",
            title = paste("Cumulative Prediction Squared Error for window:", window, "horizon:", horizon),
            color = "Model") +
        theme_minimal()
    # save the plot
    plot_path <- paste0(path, "/cpse")
    create_dir(plot_path)
    ggsave(filename = paste0(plot_path, "/cpse_window_", window, "_horizon_",
                                            horizon, ".png"), plot = p, width = 12)
    # save the data
    data_path <- paste0(path, "/cpse_csv")
    create_dir(data_path)
    write.csv(cpse_filtered, file = paste0(data_path, "/cpse_data_window_", window, "_horizon_",
                                            horizon, ".csv"), row.names = FALSE)
    return(cpse_filtered)
}


