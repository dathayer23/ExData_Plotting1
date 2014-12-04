## File: plot1.R
## Author : David Thayer
## Date: 12/3/2014


## this function reads in the power consumption cs and 
## selects the values for the two studied dates  of Feb 1st and 2nd 2007.
## the function combines the Date and Time columns into a DateTime col 
## and gives prettier more informative names to the columns
readPowerData <- function() {
  ## read the file - some computers may take  time to do this 
  ## but on my personal workstation this only takes a few seconds 
  ## so I made no attempt to write a custom function to 
  ## select the lines from the data file that I wanted
  temp <- read.csv("household_power_consumption.txt", sep = ";"
                   , colClasses = 
                     c("character", "character", "numeric"
                        , "numeric","numeric", "numeric", "numeric"
                        , "numeric","numeric"), strip.white = TRUE
                   , na.strings = c("?", ""))
  
  ## select items for first study day
  data <- temp[temp$Date == "1/2/2007",]
  
  ## select items for second study day
  temp2 <- temp[temp$Date == "2/2/2007",]
  
  ## combine selections
  range <- (length(data$Date) + 1):(length(data$Date) + length(temp2$Date))
  data[range,] <- temp2
  
  ## combine date and Time fields
  datetime <- as.POSIXct(strptime(paste(as.character(data[,1]), 
                                        as.character(data[,2]), sep=" "), 
                                  format="%d/%m/%Y %H:%M:%S"))
  ## add to data set
  data <- cbind(data, datetime)
  ## remove Date and Time columns as unneeded
  data$Date <- NULL
  data$Time <- NULL
  
  ## create more informative names
  colnames(data) <- c("ActivePower", "ReactivePower", 
                      "Voltage", "Intensity", "KitchenUsage", 
                      "LaundryUsage", "HvacUsage", "DateTime")
  
  ## return data
  data
}

#read data
powerdata <- readPowerData()
dev <- png("plot1.png")
## Create histogram of ActivePower column with appropriate lables and color
## create plot - as there are issues with portability as regards
## setting transparent background, I have used a white background
hist(data$ActivePower, col="red", main="Global Active Power", xlab="Global Active Power (kilowatts)")
dev.off()


