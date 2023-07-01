#######################################################################
#######################################################################
#######################################################################
###----------------------  DATA ASSEMBLY ---------------------------###
#######################################################################
#######################################################################
#######################################################################


#######################################################################
###--------------------------- Libraries ---------------------------###
#######################################################################
library(tidyverse)
library(readxl)
library(readr)
library(lubridate)
library(dplyr)
library(anytime)
library(Rcpp)

#######################################################################
###----------------------- Prepare Workspace -----------------------###
#######################################################################
#Set Working Directory
wd <- file.path(getwd())
setwd(wd)
Sys.setlocale("LC_TIME", "C")

# Read the data_info json file
data_info <- jsonlite::read_json("Data/data_info.json")
frequency <- "Monthly"
start_date <- "1990-01-01"
end_date <- "2023-05-01"

data_assembly <- F
#######################################################################
###---------------------- Helper Functions -------------------------###
#######################################################################
# Find the dataset based on the name and frequency                    #
#######################################################################
find_dataset <- function(name, frequency) {
    for (data in data_info) {
        for (ds in data$Datasets) {
            if (ds$file_name == name && ds$frequency == frequency) {
                return(ds)
                }
            }
        }
    return(NULL)
}
#######################################################################
# Read the data from a file based on the file type                    #
#######################################################################
read_data <- function(dataset) {
    if (dataset$file_type == "xlsx") {
        return(read_excel(dataset$path))
    } else if (dataset$file_type == "csv") {
        return(read_csv(dataset$path))
    } else {
        stop("Unsupported file type")
    }
}
#######################################################################
# Filter the data based on the start and end dates                    #
#######################################################################
filter_data <- function(data, dataset, start_date, end_date) {

    # # Convert the date column to Date type
    data[[dataset$date_column]] <- as.Date(data[[dataset$date_column]], format = dataset$date_format)
    # Filter the data based on the start and end dates
    data <- data %>%
        filter(data[[dataset$date_column]] >= as.Date(start_date) &
            data[[dataset$date_column]] <= as.Date(end_date))

    return(data)
}

#######################################################################
# Aggregate the data based on the frequency                           #
#######################################################################
aggregate_data <- function(data, dataset) {
    # Only proceed if the data is weekly or daily
    if ((dataset$frequency == "Weekly") || (dataset$frequency == "Daily")) {
        # Convert the date column to Date type
        data <- data %>%
            mutate(date = floor_date(data[[dataset$date_column]], "month")) %>%
            group_by(date) %>%
            summarise(mean_data = mean(!!sym(dataset$quantity_column), na.rm = TRUE),
            variance_data = var(!!sym(dataset$quantity_column), na.rm = TRUE)) %>%
            rename(!!paste0("mean_", dataset$quantity_column) := mean_data,
                !!paste0("variance_", dataset$quantity_column) := variance_data)
    }
  return(data)
}

#######################################################################
# Retrieve data from a file based on the name, start date, end date   #
# and frequency                                                       #
#######################################################################
retrieve_data <- function(name, start_date, end_date, frequency) {
    dataset <- find_dataset(name, frequency)
    if (is.null(dataset)) {
        dataset <- find_dataset(name, "Weekly")
        }
    if (is.null(dataset)) return(NULL)

    data <- read_data(dataset)
    data <- filter_data(data, dataset, start_date, end_date)
    data <- aggregate_data(data, dataset)

    # Check if dataset$date_column exists in the data
    if (dataset$date_column %in% names(data)) {
        #data[[dataset$date_column]] <- format(data$date, "%d-%m-%Y")
        # Rename the date column to date
        names(data)[names(data) == dataset$date_column] <- "date"
        data$date <- as.Date(data$date, format = "%Y-%m-%d")
        #format(data$date, "%d-%m-%Y")
    }
    return(data)
}

#######################################################################
# Retrieve and merge data from multiple files into one data frame     #
# based on the date column and save it as an rda and csv file         #
#######################################################################
retrieve_and_merge_data <- function(file_names, start_date, end_date, frequency, data_type) {
  merged_data <- NULL

  for (name in file_names) {
    data <- retrieve_data(name, start_date, end_date, frequency)
    if (!is.null(data)) {
      if (is.null(merged_data)) {
        merged_data <- data
      } else {
        merged_data <- merge(merged_data, data, by = "date", all = TRUE)
      }
    }
  }

  save(merged_data, file = paste("Data/Rda/", data_type, ".rda", sep = ""))
  write.csv(merged_data, file = paste("Data/csv/", data_type, ".csv", sep = ""), row.names = FALSE)

  return(merged_data)
}

if (data_assembly) {

    #######################################################################
    ###---------------------- Oil Prices -------------------------------###
    #######################################################################
    # Initialize an empty data frame for oil prices
    WTI_oil_data <- data.frame()
    data <- retrieve_data("WTI_CrudeOil_Monthly", start_date, end_date, frequency)
        if (!is.null(data)) {
            WTI_oil_data <- rbind(WTI_oil_data, data)
        }
    # Rename the columns by their positions
    names(WTI_oil_data)[1] <- "date"
    names(WTI_oil_data)[2] <- "WTI Crude Oil Price [USD/barrel]"
    # Save the data as an rda file and csv file
    save(WTI_oil_data, file = "Data/Rda/WTI_oil_data.rda")
    write.csv(WTI_oil_data, file = "Data/csv/WTI_oil_data.csv", row.names = FALSE)


    #######################################################################
    ###---------------------- Supply & Demand --------------------------###
    # #######################################################################
    supply_demand_file_names <- c("Weekly_U.S._Ending_Stocks_of_Crude_Oil",
                                "U.S._Exports_of_Crude_Oil",
                                "U.S._Field_Production_of_Crude_Oil",
                                "Weekly_U.S._Product_Supplied_of_Petroleum_Products",
                                "Monthly_U.S._Net_Imports_of_Crude_Oil_and_Petroleum_Products",
                                "OPEC_Oil_Production","Kilian_index_monthly")
    supply_demand_data <- retrieve_and_merge_data(supply_demand_file_names, start_date, end_date, frequency, "supply_demand_data")

    #######################################################################
    ###---------------------- Macroeconomic Factors --------------------###
    #######################################################################
    macroeconomic_file_names <- c("CPI_OECD_Monthly",
                                "CPI_USA_Monthly",
                                "UNRATE_USA",
                                "UNRATE_EU",
                                "Money_supply_M_USA",
                                "Economic_policy_uncerainty_index_USA",
                                "Economic_policy_uncerainty_index_EU",
                                "GDP_EU_Monthly",
                                "GDP_US_Monthly",
                                "FEDFUNDS",
                                "TB3MS")
    macroeconomic_data <- retrieve_and_merge_data(macroeconomic_file_names, start_date, end_date, frequency, "macroeconomic_data")

    #######################################################################
    ###---------------------- Financial Indicators ----------------------###
    #######################################################################
    financial_file_names <- c("S&P_500_Monthly",
                            "MSCIWORLD",
                            "STOXX600")
    financial_data <- retrieve_and_merge_data(financial_file_names, start_date, end_date, frequency, "financial_data")

    #######################################################################
    ###---------------------- Commodity Prices -------------------------###
    #######################################################################
    commodities_file_names <- c("S&P_GSCI_Commodity_Index_Monthly_cleaned",
                                "Gold_Futures_Historical_Monthly_cleaned",
                                "Copper_Futures_Historical_Monthly_cleaned")
    commodities_data <- retrieve_and_merge_data(commodities_file_names, start_date, end_date, frequency, "commodities_data")


    #######################################################################
    ###---------------------- Political Factors ------------------------###
    #######################################################################
    political_file_names <- c("Geopolitical_Risk_indicator_monthly", "WUI_monthly")
    political_data <- retrieve_and_merge_data(political_file_names, start_date, end_date, frequency, "political_data")

    # Assemble all of the data into one data frame and save it as an rda and csv file
    # Merge all data frames together
    all_data <- Reduce(function(x, y) merge(x, y, by = "date", all = TRUE),
                    list(WTI_oil_data, supply_demand_data, macroeconomic_data, financial_data, commodities_data, political_data))

    # Save as .rda
    save(all_data, file = "Data/Rda/all_data.rda")

    # Save as .csv
    write.csv(all_data, file = "Data/csv/all_data.csv", row.names = FALSE)


    #######################################################################
    # remove the columns that are not needed                              #
    # rename the columns of the data frame                                #
    #######################################################################
    # remove the columns that are not needed
    drops <- c("variance_Stocks_Crude_Oil_Thousand_Barrels",
                "variance_US_Product_Supplied_of_Petroleum_Products_Thousand_Barrels_per_Day",
                "variance_Net_Imports_of_Crude_Oil_and_Petroleum_Products_Thousand_Barrels_per_Day")

    all_data <- all_data[, !(names(all_data) %in% drops)]

    # rename the columns of the data frame
    new_names <- c("date","oil_price", "oil_stock", "oil_export", "oil_production",
                "product_supply", "product_net_import", "opec_production",
                "killian_index", "cpi_oecd", "cpi_usa", "unrate_usa", "unrate_eu",
                "epui_eu", "epui_usa", "gdp_eu", "gdp_usa", "federal_fund", "tb3ms",
                "sp500", "msci", "stoxx50", "gsci", "gold_price", "copper_future", "geopolitical_risk", "wui")

    names(all_data) <- new_names

    # Save as .rda the cleaned data
    save(all_data, file = "Data/Rda/all_data_cleaned.rda")

    # Save as .csv the cleaned data
    write.csv(all_data, file = "Data/csv/all_data_cleaned.csv", row.names = FALSE)

    #######################################################################
}