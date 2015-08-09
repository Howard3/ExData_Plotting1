library(data.table)

# Render the global active power over date time
plot2 <- function(output_to_screen=FALSE) {
    # Predefine some operating parameters for this function here
    file <- 'household_power_consumption.txt'
    allowed_dates <- as.Date(c('2007-02-01','2007-02-02'), '%Y-%m-%d')
    
    # Read the file into a data.table
    hpc <- fread(file, sep=';', colClasses="character", na.strings="?")
    
    # For this lesson we're only trying to get a graph of the dates
    #   2007-02-01 and 2007-02-02. For this reason we're going to 
    #   strip everything else out.
    
    # It's faster to do this prior to building the DateTime string since 
    #   pasting is a very heavy job with many objects
    hpc <- hpc[as.Date(Date, '%d/%m/%Y') %between% allowed_dates,]
    
    # Next, combine the dateTime
    DateTime <- strptime(paste(hpc$Date, hpc$Time), '%d/%m/%Y %H:%M:%S')
    
    do_plot <- function() {
        plot(DateTime, hpc$Global_active_power, type="l", xlab = "", ylab="Global Active Power (kilowatts)")
    }
    
    # Determine if we're outputting to the screen.
    if (output_to_screen) {
        return(do_plot())
    }
    
    # output to file.
    png("plot2.png", width=480, height=480)
    do_plot()
    dev.off()
}