library(data.table)

# Call the plot3() function to render the graph. Provide TRUE to it's first
#   argument to output to screen.
plot3 <- function(output_to_screen=FALSE) {
    hpc <- NULL
    DateTime <- NULL

    # Read the data from the household_power_consumption.txt file placed in the
    #   active working directory.
    read_data <- function() {
        # Predefine some operating parameters for this function here
        file <- 'household_power_consumption.txt'
        allowed_dates <- as.Date(c('2007-02-01','2007-02-02'), '%Y-%m-%d')
        
        # Read the file into a data.table
        hpc <<- fread(file, sep=';', colClasses="character", na.strings="?")
        
        # For this lesson we're only trying to get a graph of the dates
        #   2007-02-01 and 2007-02-02. For this reason we're going to 
        #   strip everything else out.
        
        # It's faster to do this prior to building the DateTime string since 
        #   pasting is a very heavy job with many objects
        hpc <<- hpc[as.Date(Date, '%d/%m/%Y') %between% allowed_dates,]
        
        # Next, combine the dateTime
        DateTime <<- strptime(paste(hpc$Date, hpc$Time), '%d/%m/%Y %H:%M:%S')
    }
    
    # Holds the functionality necessary for rendering the plot
    do_plot <- function() {
        #First setup the blank canvas
        plot(
            DateTime, hpc$Sub_metering_1,
            xlab = "", ylab="Energy sub metering", type="n"
        )
        
        # Generate a line for each of the 3 sub metering options
        lines(DateTime, hpc$Sub_metering_1, col="grey")
        lines(DateTime, hpc$Sub_metering_2, col="red")
        lines(DateTime, hpc$Sub_metering_3, col="blue")
        
        # Render a legend. lty tells it to render a line type in the legend
        legend("topright",
               col=c('grey', 'red', 'blue'), lty=1,
               legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')
        )
    }
    
    read_data()
    if(!output_to_screen) {
        png("plot3.png", width=480, height=480)
        do_plot()
        dev.off()
        return()
    }
    do_plot()
}