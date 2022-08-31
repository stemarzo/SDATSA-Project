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
par(mfrow=c(1,1))
train_date <- nrow(y_imp) *0.8
y_train <- y_imp[1:train_date,]
y_test <- ts(y_imp[-c(1:train_date),], frequency = 24)
plot(y_train)

write.csv(y_train, file = "y_train.csv", row.names = FALSE)
write.csv(y_test, file = "y_test.csv", row.names = FALSE)

seasonplot(ts(y_train, frequency = 24))
seasonplot(ts(y_train, frequency = 168))

## RENDO STAZIONARIA IN VARIANZA
y_train_lambda <- BoxCox(y_train, "auto")
lambda_boxcox <- attributes(y_train_lambda)$lambda
plot(y_train_lambda)

##RENDO STAZIONARIO IN MEDIA

seasonplot(ts(y_train_lambda, frequency = 24))
seasonplot(ts(y_train_lambda, frequency = 168))

y_train_stag <- diff(y_train_lambda, 24)
#y_train_stag <- diff(y_train_stag)
plot(y_train_stag)

seasonplot(ts(y_train_stag, frequency = 24))
par(mfrow=c(1,2))
Acf(y_train_stag, 72)
Pacf(y_train_stag, 72)


#---- MODELLI ARIMA

mod1 <- Arima(y_train, c(2, 1, 1), c(2, 1, 1),
              lambda = lambda_boxcox, method="CSS")

mod1 <- Arima(y_train, c(1, 0, 1), c(3, 1, 1),
              lambda = lambda_boxcox, method="CSS")
summary(mod1)

Acf(mod1$residuals, 72)
Pacf(mod1$residuals, 72)

par(mfrow=c(1,1))
fcst1 <- forecast(mod1, h=length(y_test))
plot(fcst1)
mean(abs((y_test - as.numeric(fcst1$mean))/y_test)) * 100



## Arima sinusoidi

omega <- outer(1:length(y), 1:84) * 2 * pi / 168
cc <- cos(omega)
ss <- sin(omega[,-84])



mod2 <- Arima(y_train, c(1, 0, 1),
              list(order = c(3, 1, 0), period = 24),
              include.drift = TRUE,
              xreg = cbind(cc[,1:3], ss[, 1:3])[1:length(y_train), ])
summary(mod2)

par(mfrow=c(1,2))
Acf(mod2$residuals, 72)
Pacf(mod2$residuals, 72)

par(mfrow=c(1,1))
fcst2 <- forecast(mod2, h=length(y_test), xreg = cbind(cc, ss)[(length(y_train)+1):length(y), ] )
plot(fcst2)
plot(fcst2, 0)
mean(abs((y_test - as.numeric(fcst2$mean))/y_test)) * 100





