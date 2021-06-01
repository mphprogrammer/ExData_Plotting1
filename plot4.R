##------------------------------------------------------------------------------
## plot4.R
## Reproduces the plot entitled "plot4.png".
##------------------------------------------------------------------------------
library(readr)
library(dplyr)

##------------------------------------------------------------------------------
## Download dataset if the csv containing it is missing.
##------------------------------------------------------------------------------
ds_path <- "https://d396qusza40orc.cloudfront.net/exdata/data/"
ds_filename <- "household_power_consumption"

remote_path <- paste0(ds_path, ds_filename, ".zip")

zip_filename <- file.path("data", paste0(ds_filename, ".zip"))
txt_filename <- file.path("data", paste0(ds_filename, ".txt"))
csv_filename <- file.path("data", paste0(ds_filename, ".csv"))

##------------------------------------------------------------------------------
## Function. Loader
## Downloads, unzips, and filters the data file. The Date and Time fields are
## combined into a single POSIXct field, and only 2/1/2007 and 2/2/2007 are
## written to the csv. Returns the data set as a tibble.
##------------------------------------------------------------------------------
loader <- function() {
  if (!dir.exists("data"))
    dir.create("data")
  
  if (!file.exists(zip_filename))
    download.file(remote_path, zip_filename)
  
  if (!file.exists(txt_filename))
    unzip(zip_filename, exdir = "data")
  
  if (!file.exists(csv_filename)) {
    data <- read.csv(txt_filename, sep = ";", na.strings = "?") %>%
      filter(Date == "1/2/2007" | Date == "2/2/2007") %>%
      mutate(Date.Time = strptime(paste(Date, Time), "%d/%m/%Y %H:%M:%S")) %>%
      select(Date.Time, Global_active_power, Global_reactive_power,
             Voltage, Global_intensity, Sub_metering_1, 
             Sub_metering_2, Sub_metering_3)
    
    write_csv(data, csv_filename)
  }
  
  read_csv(csv_filename)
}

data <- loader()

##------------------------------------------------------------------------------
## Draw the line graphs.
##------------------------------------------------------------------------------
png(filename = "plot4.png")
par(mfcol = c(2, 2))

##------------------------------------------------------------------------------
## First plot, reproduction (note: y axis does not include "(kilowatts)").
## Line Graph, Global Active Power by Date.Time
##------------------------------------------------------------------------------
with(data, {
  plot(Date.Time, Global_active_power, 
       type = "n", xlab = "", ylab = "Global Active Power")

  lines(Date.Time, Global_active_power, type = "l")
})

##------------------------------------------------------------------------------
## Second plot, reproduction
## Line Graph, Submetering 1, 2, and 3 by Date.Time
##------------------------------------------------------------------------------
with(data, {
  plot(Date.Time, Sub_metering_1, type = "n", xlab = "",  
       ylab = "Energy sub metering")

  lines(Date.Time, Sub_metering_1, type = "l", col = "black")
  lines(Date.Time, Sub_metering_2, type = "l", col = "red")
  lines(Date.Time, Sub_metering_3, type = "l", col = "blue")
})

legend("topright", lty = 1, col = c("black", "red", "blue"), 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"))


##------------------------------------------------------------------------------
## Third plot, reproduction
## Line Graph, Voltage by Date.Time
##------------------------------------------------------------------------------
with(data, {
  plot(Date.Time, Voltage, type = "n", xlab = "datetime",
       ylim = c(ceiling(min(Voltage)), floor(max(Voltage))))
     
  lines(Date.Time, Voltage, type = "l")
})

##------------------------------------------------------------------------------
## Fourth plot, reproduction
## Line Graph, Global Reactive Power by Date.Time
##------------------------------------------------------------------------------
with(data, {
  plot(Date.Time, Global_reactive_power, type = "n", xlab = "datetime")
  lines(Date.Time, Global_reactive_power, type = "l")
})

dev.off()