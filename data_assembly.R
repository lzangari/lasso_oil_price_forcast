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
names(WTI_oil_data)[2] <- "price [dollar / barrel]"
# Save the data as an rda file and csv file
save(WTI_oil_data, file = "Data/Rda/WTI_oil_data.rda")
write.csv(WTI_oil_data, file = "Data/csv/WTI_oil_data.csv", row.names = FALSE)


#######################################################################
###---------------------- Supply & Demand --------------------------###
#######################################################################
# Get the names of the supply and demand data
file_names <- c("Weekly_U.S._Ending_Stocks_of_Crude_Oil", "U.S._Exports_of_Crude_Oil",
                "U.S._Field_Production_of_Crude_Oil",
                "Weekly_U.S._Product_Supplied_of_Petroleum_Products",
                "Weekly_U.S._Net_Imports_of_Crude_Oil_and_Petroleum_Products",
                "OPEC_Oil_Production")
# Initialize an empty data frame for supply and demand data
supply_demand_data <- NULL

for (name in file_names) {
  data <- retrieve_data(name, start_date, end_date, frequency)
  if (!is.null(data)) {
    if (is.null(supply_demand_data)) {
      supply_demand_data <- data
    } else {
      supply_demand_data <- merge(supply_demand_data, data, by = "date", all = TRUE)
    }
  }
}

save(supply_demand_data, file = "Data/Rda/supply_demand_data.rda")
write.csv(supply_demand_data, file = "Data/csv/supply_demand_data.csv", row.names = FALSE)

#######################################################################
###------------------ Oil consumption indicators -------------------###
#######################################################################

#######################################################################
###------------------- Macroeconomic indicators --------------------###
#######################################################################
file_names <- c("CPI_OECD_Monthly", "CPI_USA_Monthly", "UNRATE_USA", "UNRATE_EU",
                "Money_supply_M_USA",
                "Economic_policy_uncerainty_index_USA",
                "Economic_policy_uncerainty_index_EU")
macroeconomic_data <- NULL

for (name in file_names) {
  data <- retrieve_data(name, start_date, end_date, frequency)
  if (!is.null(data)) {
    if (is.null(macroeconomic_data)) {
      macroeconomic_data <- data
    } else {
      macroeconomic_data <- merge(macroeconomic_data, data, by = "date", all = TRUE)
    }
  }
}

save(macroeconomic_data, file = "Data/Rda/macroeconomic_data.rda")
write.csv(macroeconomic_data, file = "Data/csv/macroeconomic_data.csv", row.names = FALSE)

#######################################################################
###------------------- Financial market factors --------------------###
#######################################################################


#######################################################################
###--------------------- Political indicators ----------------------###
#######################################################################


#######################################################################
###------ Commodities/clean energy/electric cars indicators --------###
#######################################################################
