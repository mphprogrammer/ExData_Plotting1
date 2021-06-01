##------------------------------------------------------------------------------
## plot2.R
## Line Graph, Global Active Power by Date.Time
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
## Draw the line graph.
##------------------------------------------------------------------------------
png(filename = "plot2.png")
with(data, {
     plot(Date.Time, Global_active_power, 
       type = "n", xlab = "", ylab = "Global Active Power (kilowatts)")

     lines(Date.Time, Global_active_power, type = "l")
})

dev.off()
  


