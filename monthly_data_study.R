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

    return(adf_result)

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
# # do analysis for each data
adf_results <- list()
hetero_results <- list()
for (name_index in 2:length(names(df))){
    adf_result <- analysis_stationary(df = df, save_plots = save_plots,
                y_column = names(df)[name_index], plot_name = plot_names[[names(df)[name_index]]],
                    indicator = short_names[[names(df)[name_index]]])

    adf_results[[name_index]] <- adf_result

    hetero_result <- analysis_heteroscedacity(indicator = names(df)[name_index],
            predictor_index = 28, df = df, name_index = name_index)
    hetero_results[[name_index]] <- hetero_result

}

# assemble the table for adf_table
create_svg_from_table(results = adf_table, name = "monthly_column_adf_table.svg")

# assemble the table
hetero_table <- do.call(rbind, hetero_results)
table_grob <- tableGrob(hetero_table, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
grDevices::svg("monthly_column_bptest_table.svg", width = 9, height = 7)
grid.newpage()
grid.draw(table_grob)
grid.export("monthly_column_bptest_table.svg")
dev.off()
# #######################################################################
# ###------------- Check Autocorrelation with log ---------------###
# #######################################################################
# adf_results_log <- list()
# for (i in 2:ncol(df)) {
#     # Get the column name
#     column_name <- names(df)[i]
#     # Create the new column name
#     new_column_log_name <- paste0('log_', column_name)
#     # Compute the log of the column and add it to the data frame
#      df[[new_column_log_name]] <- log(df[[column_name]])

#     # # visualize them
#     # create_monthly_plot(df = df, path = save_plots, y_column = new_column_log_name,
#     #                                         plot_name = plot_names[[column_name]])
#     # # decompose the time series and plot the ACF of the residuals, trend and seasonal component
#     # decompose_and_plot_acf(df = df, column = new_column_log_name,
#     #                         lag_max = 24, path = save_plots, frequency = 12)
#     # # perform ADF test
#     # adf_result <- perform_adf_analysis(df = df, column = new_column_log_name,
#     #                                             indicator = short_names[[column_name]])
#     ########################## problem in ts_data
#     adf_result_log <- analysis_stationary(df = df, save_plots = save_plots,
#                 y_column = new_column_log_name,
#                 plot_name = paste0("log"," ", plot_names[[column_name]]),
#                     indicator = short_names[[column_name]])
#     ########################## problem

#     adf_results_log[[i]] <- adf_result_log
# }

# # assemble the table
# adf_table_log <- do.call(rbind, adf_results_log)
# table_grob <- tableGrob(adf_table_log, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
# grDevices::svg("monthly_column_adf_table_log.svg", width = 9, height = 7)
# grid.newpage()
# grid.draw(table_grob)
# grid.export("monthly_column_adf_table_log.svg")
# dev.off()



#######################################################################
###------------- Check Autocorrelation with lag ---------------###
#######################################################################
adf_results_lag_one <- list()
for (i in 2:ncol(df)) {
    # Get the column name
    column_name <- names(df)[i]
    # Create the new column name
    new_column_lag_one_name <- paste0('lag_one_', column_name)
    # Compute the log of the column and add it to the data frame
     df[[new_column_lag_one_name]] <- log(df[[column_name]])

    ########################## problem in ts_data
    adf_result_lag_one <- analysis_stationary(df = df, save_plots = save_plots,
                y_column = new_column_lag_one_name,
                plot_name = paste0("lag one"," ", plot_names[[column_name]]),
                    indicator = short_names[[column_name]])
    ########################## problem

    adf_results_lag_one[[i]] <- adf_result_lag_one
}

# assemble the table
adf_table_lag_one <- do.call(rbind, adf_results_lag_one)
table_grob <- tableGrob(adf_table_lag_one, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
grDevices::svg("monthly_column_adf_table_lag_one.svg", width = 9, height = 7)
grid.newpage()
grid.draw(table_grob)
grid.export("monthly_column_adf_table_lag_one.svg")
dev.off()



#######################################################################
###------------------------ Transformation  --.---------------------###
#######################################################################


# # monthly column of the above 3 columns
# monthly_us_ending_stock_of_crude_oil <- df$"mean_Net_Imports_of_Crude_Oil_and_Petroleum_Products [Thousand Barrels per Day]"
# monthly_us_net_imports_of_crude_oil_and_petroleum_products <- df$"mean_US_Product_Supplied_of_Petroleum_Products [Thousand Barrels per Day]"
# monthly_us_product_supplied_of_petroleum_products <- df$"mean_US_Product_Supplied_of_Petroleum_Products [Thousand Barrels per Day]"

# # check the stationaty and random walk by ADF test
# # us ending stock of crude oil
# adf.test(weekly_us_ending_stock_of_crude_oil$"Stocks_Crude_Oil [Thousand Barrels]")
# log_diff_data <- diff(log(monthly_us_ending_stock_of_crude_oil))
# adf.test(na.omit(log_diff_data))
# log_diff_12_data <- diff(log(monthly_us_ending_stock_of_crude_oil), lag = 12)
# adf.test(na.omit(log_diff_12_data))
# monthly_us_ending_stock_of_crude_oil <- na.omit(monthly_us_ending_stock_of_crude_oil)
# adf.test(monthly_us_ending_stock_of_crude_oil)
# # us net imports of crude oil and petroleum products
# adf.test(weekly_us_net_imports_of_crude_oil_and_petroleum_products$"Net_Imports_of_Crude_Oil_and_Petroleum_Products [Thousand Barrels per Day]")
# # apply log and diff on weekly data
# log_diff_data_weekly <- diff(log(weekly_us_net_imports_of_crude_oil_and_petroleum_products$"Net_Imports_of_Crude_Oil_and_Petroleum_Products [Thousand Barrels per Day]"))
# adf.test(na.omit(log_diff_data_weekly))
# log_diff_data <- diff(log(monthly_us_net_imports_of_crude_oil_and_petroleum_products))
# adf.test(na.omit(log_diff_data))
# log_diff_12_data <- diff(log(monthly_us_net_imports_of_crude_oil_and_petroleum_products), lag = 12)
# adf.test(na.omit(log_diff_12_data))
# monthly_us_net_imports_of_crude_oil_and_petroleum_products <- na.omit(monthly_us_net_imports_of_crude_oil_and_petroleum_products)
# adf.test(monthly_us_net_imports_of_crude_oil_and_petroleum_products)



# # Assuming df is your data frame
# for (i in 2:ncol(df)) {
#   # Get the column name
#   column_name <- names(df)[i]
#   # Create the new column name
#   new_column_name <- paste0('log_', column_name)
#   # Compute the log of the column and add it to the data frame
#   df[[new_column_name]] <- log(df[[column_name]])
# }


# # Assuming df is your data frame
# constant = 1  # Define your constant here

# for (i in 2:ncol(df)) {
#   # Get the column name
#   column_name <- names(df)[i]
#   # Create the new column name
#   new_column_name <- paste0('log_', column_name)
#   # Compute the log of the column (plus the constant) and add it to the data frame
#   df[[new_column_name]] <- log(df[[column_name]] + constant)
# }


# library(dplyr)

# # Assume 'df' is your data frame and 'price' is the column with the oil prices
# df <- df %>%
#   mutate(return = (price/lag(price) - 1) * 100)


# library(dplyr)

# # Assume 'df' is your data frame and 'price' is the column with the oil prices
# df <- df %>%
#   mutate(return_2 = (price/lag(price, 2) - 1) * 100,
#          return_4 = (price/lag(price, 4) - 1) * 100,
#          return_8 = (price/lag(price, 8) - 1) * 100)
