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
    if (!dir.exists(plot_path)) {
        dir.create(plot_path)
    }

    # Save the plot as svg file
    ggsave(paste0(y_column, "_timeseries_plot.svg"), p1, path = plot_path)
}


plotForecastErrors <- function(forecasterrors){
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
    hist(forecasterrors, col="#0d8736", freq=FALSE, breaks=mybins)
    # freq=FALSE ensures the area under the histogram = 1
    # generate normally distributed data with mean 0 and standard deviation mysd
    myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
    # plot the normal curve as a blue line on top of the histogram of forecast errors:
    points(myhist$mids, myhist$density, type="l", col="#ffae00", lwd=2)
}