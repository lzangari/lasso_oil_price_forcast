library(ggplot2)
library(scales)

# Function to create a plot
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