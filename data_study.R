#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################
library(tseries)
library(ggplot2)
library(dplyr)
library(urca)
library(scales)
library(forecast)

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

save_plots <- "C:/Repositories/lasso_oil_price_forcast/plots/weekly"

# Read the data from csv file
# df <- read.csv("Data/csv/all_data_cleaned.csv", header = TRUE, sep = ",")
#######################################################################
###----------------------- Check random walk -----------------------###
#######################################################################
# check if the weekly data is a random walk or not.
# check this for the 3 columns that were in weekly format in the original data.
weekly_data <- function(name) {
    # name: the name of the data
    # return: the data frame of the weekly data
    dataset <- find_dataset(name, "Weekly")
    data <- read_data(dataset)
    data <- filter_data(data, dataset, "1990-01-01", "2023-05-01")
    return(data)
}

# Weekly_U.S._Ending_Stocks_of_Crude_Oil
us_ending_stock<- weekly_data(name = "Weekly_U.S._Ending_Stocks_of_Crude_Oil")
create_weekly_plot(df = us_ending_stock, path = save_plots, plot_name = "U.S. Ending Stocks of Crude Oil")
decompose_and_plot_acf(df = us_ending_stock, column = names(us_ending_stock)[2], lag_max = 4, path = save_plots)

# Weekly_U.S._Net_Imports_of_Crude_Oil_and_Petroleum_Products
net_imports_products <- weekly_data(name = "Weekly_U.S._Net_Imports_of_Crude_Oil_and_Petroleum_Products")
create_weekly_plot(df = net_imports_products, path = save_plots, plot_name = "U.S. Net Imports")
decompose_and_plot_acf(df = net_imports_products, column = names(net_imports_products)[2], lag_max = 4, path = save_plots)
# Weekly_U.S._Product_Supplied_of_Petroleum_Products
product_supply <- weekly_data(name = "Weekly_U.S._Product_Supplied_of_Petroleum_Products")
create_weekly_plot(df = product_supply, path = save_plots, plot_name = "U.S. Product Supplied")
decompose_and_plot_acf(df = product_supply, column = names(product_supply)[2],lag_max = 4, path = save_plots)


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


