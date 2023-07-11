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

save_plots <- "C:/Repositories/lasso_oil_price_forcast/plots/weekly"

#######################################################################
###----------------------- Check random walk -----------------------###
#######################################################################
# check if the weekly data is a random walk or not.
# check this for the 3 columns that were in weekly format in the original data.

# load the weekly data
weekly_data <- function(name) {
    # name: the name of the data
    # return: the data frame of the weekly data
    dataset <- find_dataset(name, "Weekly")
    data <- read_data(dataset)
    data <- filter_data(data, dataset, "1989-01-01", "2023-05-01")
    return(data)
}

# create a list of the data names and plot names
data_names <- c("Weekly_U.S._Ending_Stocks_of_Crude_Oil")
               # "Weekly_U.S._Product_Supplied_of_Petroleum_Products")
plot_names <- list("Weekly_U.S._Ending_Stocks_of_Crude_Oil" = "U.S. Ending Stocks of Crude Oil")
                   #"Weekly_U.S._Product_Supplied_of_Petroleum_Products" = "U.S. Product Supplied")

# do analysis for each data
adf_results <- list()
for (name in data_names){
    # load the data
    data <- weekly_data(name)
    # visualize them
    create_weekly_plot(df = data, path = save_plots, plot_name = plot_names[[name]])
    # decompose the time series and plot the ACF of the residuals, trend and seasonal component
    decompose_and_plot_acf(df = data, column = names(data)[2], lag_max = 52, path = save_plots)
    # perform ADF test
    adf_result <- perform_adf_analysis(df = data, column = names(data)[2])
    adf_results[[name]] <- adf_result
}


# assemble the table
adf_table <- do.call(rbind, adf_results)
table_grob <- tableGrob(adf_table, theme = ttheme_default(base_size = 8, base_line_size = 0.5))
grDevices::svg("weekly_column_adf_table.svg", width = 14, height = 6)
grid.newpage()
grid.draw(table_grob)
grid.export("weekly_column_adf_table.svg")
dev.off()




