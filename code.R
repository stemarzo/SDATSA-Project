library(readr)
library(xts)
library(imputeTS) #per vedere info sui missing values
library(forecast)

dataset <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))

data <- dataset$CO
dates <- seq(as.POSIXct("2004-03-10 18:00:00"), as.POSIXct("2005-02-28 23:00:00"), by="hour")
y <- xts(x=data, order.by=dates)
attr(y, 'frequency') <- 24
plot(y)

## INFORMAZIONI MISSING VALUES

ggplot_na_distribution(y)
ggplot_na_intervals(tsNH4)
statsNA(y)
y_imp <- na_kalman(y)

## SPLIT TRAIN E TEST SET

train_date <- nrow(y_imp) *0.8
y_train <- y_imp[1:train_date,]
y_test <- y_imp[-c(1:train_date),]
plot(y_train)

## RENDO STAZIONARIA IN VARIANZA
y_train_lambda <- BoxCox(y_train, "auto")
lambda_boxcox <- attributes(y_train_lambda)$lambda
plot(y_train_lambda)

##RENDO STAZIONARIO IN MEDIA

seasonplot(ts(y_train_lambda, frequency = 24))

y_train_stag <- diff(y_train_lambda, 24)
y_train_stag <- diff(y_train_stag)
plot(y_train_stag[24:124])

seasonplot(ts(y_train_stag, frequency = 24))
par(mfrow=c(1,2))
Acf(y_train_stag, 72)
Pacf(y_train_stag, 72)


mod1 <- Arima(y_imp, c(0, 1, 2), c(1, 1, 0),
              lambda = lambda_boxcox)
summary(mod1)

Acf(mod1$residuals, 72)
Pacf(mod1$residuals, 72)


fcst1 <- forecast(mod1, 1700)
plot(fcst1)

mean(abs((y_test[1:20]-fcst1$fitted[8506])/y_test[1:20])) * 100

auto.arima(y_imp, seasonal=TRUE, lambda=lambda_boxcox)

