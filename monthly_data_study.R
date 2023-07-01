#######################################################################
###----------------- Import necessary Libraries  -------------------###
#######################################################################
library(tseries)
library(ggplot2)
library(dplyr)
library(urca)
library(scales)
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

#######################################################################
###--------------------- Check Autocorrelation ---------------------###
#######################################################################
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
                    "sp500" = "S&P 500 Index",
                    "msci" = "MSCI World Index",
                    "stoxx600" = "EURO STOXX 600 Index",
                    "gsci" = "GSCI Index",
                    "gold_price" = "Gold Price [USD per ounce]",
                    "copper_future" = "Copper Future [USD per pound]",
                    "geopolitical_risk" = "Geopolitical Risk Index",
                    "wui" = "World Uncertainty Index")

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
                    "sp500" = "S&P 500",
                    "msci" = "MSCI World",
                    "stoxx600" = "EURO STOXX 600",
                    "gsci" = "GSCI",
                    "gold_price" = "Gold",
                    "copper_future" = "Copper Future",
                    "geopolitical_risk" = "GRI",
                    "wui" = "WUI")
# do analysis for each data
adf_results <- list()
for (name_index in 2:length(names(df))){
    # visualize them
    create_monthly_plot(df = df, path = save_plots, y_column = names(df)[name_index], plot_name = plot_names[[names(df)[name_index]]])
    # decompose the time series and plot the ACF of the residuals, trend and seasonal component
    decompose_and_plot_acf(df = df, column = names(df)[name_index],
                            lag_max = 24, path = save_plots, frequency = 12)
    # perform ADF test
    adf_result <- perform_adf_analysis(df = df, column = names(df)[name_index], indicator = short_names[[names(df)[name_index]]])
    adf_results[[name_index]] <- adf_result
}


# assemble the table
adf_table <- do.call(rbind, adf_results)
table_grob <- tableGrob(adf_table, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
grDevices::svg("monthly_column_adf_table.svg", width = 10, height = 7)
grid.newpage()
grid.draw(table_grob)
grid.export("monthly_column_adf_table.svg")
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
