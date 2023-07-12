#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################
library(tseries)
library(ggplot2)
library(dplyr)
library(forecast)
library(gridExtra)
library(gridSVG)
library(grid)
library(xtable)

source("data_assembly.R")
source("utils_plot.R")
source("correlation_analysis.R")
#######################################################################
###----------------------- Prepare Workspace -----------------------###
#######################################################################
#Set Working Directory
wd <- file.path(getwd())
setwd(wd)
Sys.setlocale("LC_TIME", "C")

save_plots <- "C:/Repositories/lasso_oil_price_forcast/plots/monthly"
df <- read.csv("C:/Repositories/lasso_oil_price_forcast/Data/csv/all_data_cleaned.csv")



analysis_stationary <- function(df, save_plots, y_column, plot_name, indicator,
                                                            lag_max=24, frequency=12){
    # visualize them
    create_monthly_plot(df = df, path = save_plots, y_column = y_column,
                                            plot_name = plot_name)
    # decompose the time series and plot the ACF of the residuals, trend and seasonal component
    decompose_and_plot_acf(df = df, column = y_column,
                            lag_max = 24, path = save_plots, frequency = 12)

    # perform ADF test
    adf_result <- perform_adf_analysis(df = df, column = y_column,
                                                indicator = indicator)
    # perform Breusch-Pegan test
    hetero_result <- analysis_heteroscedacity(indicator = y_column,
            predictor = "one_lag_oil_return_price", df = df)

    return(list(adf_result, hetero_result))

}

# add crude oil return price
df[["one_lag_oil_return_price"]] <- (df[["oil_price"]]/lag(df[["oil_price"]]) -1) *100

# #######################################################################
# ###------------- Check Autocorrelation with original ---------------###
# #######################################################################
plot_names <- list("oil_price" = "WTI Oil price [USD per barrel]",
                    "oil_stock" = "Ending Stocks of Crude Oil [Thousand Barrels]",
                    "oil_export" = "U.S. Exports of Crude Oil [Thousand Barrels]",
                    "oil_production" = "U.S. Field Production of Crude Oil [Thousand Barrels]",
                    "product_supply" = "U.S. Product Supplied of Petroleum Products [Thousand Barrels]",
                    "product_net_import" = "U.S. Net Imports of Crude Oil and Petroleum Products [Thousand Barrels]",
                    "opec_production" = "OPEC Oil Production [Thousand Barrels]",
                    "killian_index" = "Killian Index",
                    "cpi_oecd" = "OECD Consumer Price Index",
                    "cpi_usa" = "U.S. Consumer Price Index",
                    "unrate_usa" = "U.S. Unemployment Rate [% of total labor force]",
                    "unrate_eu" = "EU Unemployment Rate [% of total labor force]",
                    "epui_eu" = "EU Economic Policy Uncertainty Index",
                    "epui_usa" = "U.S. Economic Policy Uncertainty Index",
                    "gdp_eu" = "EU GDP",
                    "gdp_usa" = "U.S. GDP",
                    "federal_fund" = "Federal Funds Rate [%]",
                    "tb3ms" = "3-Month Treasury Bill: Secondary Market Rate [%]",
                    "sp500" = "S&P 500 Index",
                    "msci" = "MSCI World Index",
                    "stoxx600" = "EURO STOXX 600 Index",
                    "gsci" = "GSCI Index",
                    "gold_price" = "Gold Price [USD per ounce]",
                    "copper_future" = "Copper Future [USD per pound]",
                    "geopolitical_risk" = "Geopolitical Risk Index",
                    "wui" = "World Uncertainty Index",
                    "one_lag_oil_return_price" = "WTI oil return price [%]")

short_names <- list("oil_price" = "Oil Price",
                    "oil_stock" = "U.S. Ending Stocks",
                    "oil_export" = "U.S. Exports",
                    "oil_production" = "U.S. Field Production",
                    "product_supply" = "U.S. Product Supplied",
                    "product_net_import" = "U.S. Net Imports",
                    "opec_production" = "OPEC Oil Production",
                    "killian_index" = "Killian Index",
                    "cpi_oecd" = "OECD CPI",
                    "cpi_usa" = "U.S. CPI",
                    "unrate_usa" = "U.S. Unemployment Rate",
                    "unrate_eu" = "EU Unemployment Rate",
                    "epui_eu" = "EU EPUI",
                    "epui_usa" = "U.S. EPUI",
                    "gdp_eu" = "EU GDP",
                    "gdp_usa" = "U.S. GDP",
                    "federal_fund" = "Federal Funds Rate",
                    "tb3ms" = "3-Month Treasury Bill",
                    "sp500" = "S&P 500",
                    "msci" = "MSCI World",
                    "stoxx600" = "EURO STOXX 600",
                    "gsci" = "GSCI",
                    "gold_price" = "Gold",
                    "copper_future" = "Copper Future",
                    "geopolitical_risk" = "GRI",
                    "wui" = "WUI",
                    "one_lag_oil_return_price" = "Retrun price [1 lag]")
# do analysis for each data
adf_results <- list()
adf_results_df <- NULL
hetero_results <- list()
hetero_results_df <- NULL

for (name_index in 2:length(names(df))){
    result <- analysis_stationary(df = df, save_plots = save_plots,
                y_column = names(df)[name_index], plot_name = plot_names[[names(df)[name_index]]],
                    indicator = short_names[[names(df)[name_index]]])

    adf_results[[name_index]] <- result[[1]]
    adf_results_df <- rbind(adf_results_df, result[[1]])
    hetero_results[[name_index]] <- result[[2]]
    hetero_results_df <- rbind(hetero_results_df, result[[2]])

}
name <- paste0(save_plots, "/monthly_column_adf_table.svg")

# convert the dataframe to latex table and save it to .tex file
adf_results_tex <- xtable(adf_results_df)#, digits = 4)
print(adf_results_tex, type = "latex",
        file = paste0(save_plots, "/monthly_column_adf_table.tex"),
        include.rownames = FALSE)

# assemble the table for adf_table
create_svg_from_table(results = adf_results,
                name = paste0(save_plots, "/monthly_column_adf_table.svg"))

# assemble the table for heteroscedactiy table
create_svg_from_table(results = hetero_results,
                name = paste0(save_plots, "/monthly_column_bptest_table.svg"))

#######################################################################
###----------- data distribution before transformation -------------###
#######################################################################
# histogram of the data
# Loop through all column names in the dataframe
for (column_name in names(df)[-1]) {
    # Skip the 'one_lag_oil_price' column
    if (column_name != "one_lag_oil_return_price") {
        # Create a histogram for the column
        p <- ggplot(df, aes_string(column_name)) +
            geom_histogram(aes(y = ..density..), bins = 30, fill = "#163925", alpha = 0.5) +
            geom_density(color = "#163925") +
            labs(x = column_name, y = "Density") +
            theme_minimal()

        # Save the plot
        plot_path <- paste0(save_plots, "/histogram_of_data_before_transformation")
        create_dir(plot_path)
        ggsave(paste0(column_name, "_histogram.png"), plot = p, path = plot_path)

    }
}

#######################################################################
###--------------------- Apply transformation ----------------------###
#######################################################################
original_data <- c("one_lag_oil_return_price",
                     "geopolitical_risk")

one_lag_data <- c("oil_stock", "product_net_import", "epui_eu", "epui_usa",
                   "opec_production", "killian_index", "gdp_eu",
                   "gsci", "gold_price", "copper_future", "federal_fund","unrate_eu")
#ong_lag_log_data <- c("gdp_usa")

two_lags_data <- c("product_supply", "cpi_oecd", "cpi_usa", "sp500", "msci",
                     "wui")

three_lags_log_data <- c("oil_export")

twelve_lag_log_data <- c("unrate_usa","tb3ms", "stoxx600",
                        "oil_production", "gdp_usa") #"federal_fund","unrate_eu",

remove_column <- c("oil_price")

transformation_adf_list <- list()
transformation_bp_list <- list()
df_names <- names(df)[-1]
# Loop through all column names in the dataframe
for (column_name in df_names) {
    # Check which list the column name belongs to and apply the corresponding function
    if (column_name %in% remove_column){
        df <- df %>% select(-column_name)} else {
        if (column_name %in% original_data) {
            new_column <- paste0("original_", column_name)
            df[[new_column]] <- df[[column_name]]

        } else if (column_name %in% one_lag_data) {
            new_column <- paste0("one_lag_", column_name)
            df[[new_column]] <- c(NA, diff(df[[column_name]], lag=1))

        } else if (column_name %in% two_lags_data) {
            new_column <- paste0("two_lag_", column_name)
            df[[new_column]] <- c(NA, NA, diff(df[[column_name]], lag=2))

        } else if (column_name %in% twelve_lag_log_data) {
            new_column <- paste0("twelve_lag_log_", column_name)
            df[[new_column]] <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA,
                        diff(log(df[[column_name]]), lag=12))
        } else if (column_name %in% three_lags_log_data) {
            new_column <- paste0("three_lag_log_", column_name)
            df[[new_column]] <- c(NA, NA, NA, diff(log(df[[column_name]]), lag=3))
        }
        # apply statistical analysis
        result_stock <- analysis_stationary(df = df, save_plots = save_plots,
                y_column = new_column, plot_name = plot_names[[new_column]],
                    indicator = short_names[[column_name]])
        # drop and delete the old column
        df <- df %>% select(-column_name)
        # save the results
        transformation_adf_list[[new_column]] <- result_stock[[1]]
        transformation_bp_list[[new_column]] <- result_stock[[2]]
        }
    }

# assemble the table for adf_table
create_svg_from_table(results = transformation_adf_list,
                name = paste0(save_plots, "/monthly_transformed_column_adf_table.svg"))

# assemble the table for heteroscedactiy table
create_svg_from_table(results = transformation_bp_list,
                name = paste0(save_plots, "/monthly_transformed_column_bptest_table.svg"),
                table_width = 11)


#######################################################################
###------------------------- filter data ---------------------------###
#######################################################################
# filter the data before saving to avoid NA
df$date <- as.Date(df$date, format = "%Y-%m-%d")
# df <- df[df$date >= as.Date("1990-08-01") & df$date <= as.Date("2022-08-01"), ]
df <- df %>% filter(date >= "1990-08-01", date <= "2022-08-01")


# save the data as csv
write.csv(df, file = "Data/csv/all_data_transformed.csv", row.names = FALSE)

# Save as .rda the cleaned data
save(df, file = "Data/Rda/all_data_transformed.rda")


# df <- df %>%
#   mutate(return_2 = (price/lag(price, 2) - 1) * 100,
#          return_4 = (price/lag(price, 4) - 1) * 100,
#          return_8 = (price/lag(price, 8) - 1) * 100)
