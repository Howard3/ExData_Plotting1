library(data.table)

plot4 <- function(output_to_screen=FALSE) {
    hpc <- NULL
    DateTime <- NULL
    
    # Read the data from the household_power_consumption.txt file in the
    #   current working directory
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
        
        # Next, combine the date and time to form a datetime posix object
        DateTime <<- strptime(paste(hpc$Date, hpc$Time), '%d/%m/%Y %H:%M:%S')
    }
    
    
    # This function holds all of the plotting functionality
    do_plot <- function() {
        # Define how many plots our one view should have.
        par(mfrow = c(2,2)) # Two Rows, Two Columns
        
        # Coerce Global Active power to to the numeric type
        hpc[,Global_active_power:=as.numeric(Global_active_power)]
        
        # First plot [ top left ]
        # Plot global_active_power on y axis over the date time on x axis.
        plot(
            DateTime, hpc$Global_active_power,
            type="l", xlab="", ylab="Global Active Power",
            main=""
        )
        
        # Second Plot [ top right ]
        # Plot the voltage over the date time
        plot(
            DateTime, hpc$Voltage,
            type="l", xlab="datetime", ylab="Voltage",
            main=""
        )
        
        # Third Plot [ bottom left ]
        # Plot energy sub metering over date time
        plot(
            DateTime, hpc$Sub_metering_1,
            xlab = "", ylab="Energy sub metering", type="n"
        )
        lines(DateTime, hpc$Sub_metering_1, col="grey")
        lines(DateTime, hpc$Sub_metering_2, col="red")
        lines(DateTime, hpc$Sub_metering_3, col="blue")
        
        legend("topright",
               col=c('grey', 'red', 'blue'), bty="n", lty=1,
               legend=c('Sub_metering_1', 'Sub_metering_2', 'Sub_metering_3')
        )
        
        # Fourth Plot [ bottom right ]
        # Plot the global reactive power
        plot(
            DateTime, hpc$Global_reactive_power, type="l",
            xlab="datetime", ylab="Global_reactive_power"
        )
    }
    
    # First, read the data
    read_data()
    
    # Next determine if we're writing to a file or outputting to screen.
    if(output_to_screen) {
        # Render to the screen graphics device.
        do_plot()
        return();
    }
    
    # Open a graphics device for PNG, plot, and close the device.
    png("plot4.png", width=480, height=480)
    do_plot()
    dev.off()
}
