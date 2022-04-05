library(readr)
library(xts)
library(imputeTS) #per vedere info sui missing values

dataset <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))

data <- dataset$CO
dates <- seq(as.POSIXct("2004-03-10 18:00:00"), as.POSIXct("2005-02-28 23:00:00"), by="hour")
y <- xts(x=data, order.by=dates)

plot(y)

## INFORMAZIONI MISSING VALUES

ggplot_na_distribution(y)
ggplot_na_intervals(tsNH4)
statsNA(y)
y_imp <- na_kalman(y)


#PER ARIMA
# stazionarietà in varianza
# stazionarietà in media e quindi anche togliere la stagionalità
# studio gli acf e pacf

