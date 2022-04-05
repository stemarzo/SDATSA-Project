library(readr)
library(lubridate)

dataset <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))
dataset$Hour <- paste(dataset$Hour, ":00:00", sep="")

dataset$dttm <- ymd_hms(paste(dataset$Date, dataset$Hour, sep = " "))


firstHour <- 24*(as.Date("2004-03-10 18:00:00")-as.Date("2004-1-1 00:00:00"))
tt <- ts(dataset$CO,start=c(2006,firstHour),frequency=24*365)

plot(dataset$CO, type = 'l')

plot(tt)
