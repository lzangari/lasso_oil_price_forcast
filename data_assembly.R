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


#######################################################################
###----------------------- Prepare Workspace -----------------------###
#######################################################################
#Set Working Directory
wd <- file.path(getwd())
setwd(wd)

# Read the data_info json file
data_info <- jsonlite::read_json("Data/data_info.json")
frequency <- "Monthly"
start_date <- "1990-01-01"
end_date <- "2023-05-01"

retrieve_data <- function(name, start_date, end_date, frequency) {
    # Find the dataset from the JSON data
    dataset <- NULL
    for (data in data_info) {
        for (ds in data$Datasets) {
            # filter the dataset based on the name and frequency
            if (ds$file_name == name && ds$frequency == frequency) {
                dataset <- ds
                break
            }
        }
        if (!is.null(dataset))
            break
    }

    # If the dataset is not found, return NULL
    if (is.null(dataset)) return(NULL)

    # Read the file based on its type
    if (dataset$file_type == "xlsx") {
        data <- read_excel(dataset$path)
    } else if (dataset$file_type == "csv") {
        data <- read_csv(dataset$path)
    } else {
        stop("Unsupported file type")
    }

    # Convert the date column to Date type
    data[[dataset$date_column]] <- as.Date(data[[dataset$date_column]],
                                    format = dataset$date_format)

    # Filter the data based on the start and end dates
    data <- data %>%
        filter(data[[dataset$date_column]] >= as.Date(start_date) &
                        data[[dataset$date_column]] <= as.Date(end_date))
    # Convert the date column to the desired format
    data[[dataset$date_column]] <- format(data[[dataset$date_column]], "%d-%m-%Y")

    return(data)
}

#######################################################################
###---------------------- Oil Prices -------------------------------###
#######################################################################
# Get the oil prices from data_info where name is Crude Oil Prices: West Texas Intermediate (WTI)
names <- c("WTI_CrudeOil_Monthly")
# Initialize an empty data frame
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
# Get the oil prices from data_info where name is Crude Oil Prices: West Texas Intermediate (WTI)
names <- c("WTI_CrudeOil_Monthly")

# # Initialize an empty data frame
# WTI_oil_data <- data.frame()

# # Loop over the names and retrieve the data
# for (name in names) {
#   data <- retrieve_data(name, start_date, end_date, frequency)
#   if (!is.null(data)) {
#     WTI_oil_data <- rbind(WTI_oil_data, data)
#   }
# }

# # Rename the columns by their positions
# names(WTI_oil_data)[1] <- "date"
# names(WTI_oil_data)[2] <- "dollar"

