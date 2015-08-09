library(data.table)

plot1 <- function() {
    # Predefine some operating parameters for this function here
    file <- 'household_power_consumption.txt'
    allowed_dates <- as.Date(c('2007-02-01','2007-02-02'), '%Y-%m-%d')
    
    # Read the file into a data.table
    hpc <- fread(file, sep=';', colClasses="character", na.strings="?")
    
    # For this lesson we're only trying to get a graph of the dates
    #   2007-02-01 and 2007-02-02. For this reason we're going to 
    #   strip everything else out.
    hpc <- hpc[as.Date(Date, '%d/%m/%Y') %between% allowed_dates,]
    
    # Coerce Global Active power to to the numeric type
    hpc[,Global_active_power:=as.numeric(Global_active_power)]
    
    png("plot1.png", width=480, height=480)
    hist(hpc$Global_active_power, col="red", xlab="Global Active Power (kilowatts)", ylab="Frequency", main="Global Active Power")
    dev.off()
}