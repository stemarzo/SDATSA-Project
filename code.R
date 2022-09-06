# PREPARAZIONE --------
## LIBRERIE -------------------
library(readr)
library(xts)
library(imputeTS) #per vedere info sui missing values
library(forecast)
library(KFAS)

## CARICAMENTO ----------------

dataset <- read_csv("Project_data_2021_2022 (TRAINSET).csv", col_types = cols(Date = col_character()))

data <- dataset$CO
dates <- seq(as.POSIXct("2004-03-10 18:00:00"), as.POSIXct("2005-02-28 23:00:00"), by="hour")

y <- xts(x=data, order.by=dates)
attr(y, 'frequency') <- 24
plot(y)

# INFORMAZIONI MISSING VALUES ----

ggplot_na_distribution(y)
ggplot_na_intervals(tsNH4)
statsNA(y)
y_imp <- na_kalman(y)

# SPLIT TRAIN E TEST SET --------

par(mfrow=c(1,1))
train_date <- nrow(y_imp) *0.8
y_train <- y_imp[1:train_date,]
y_test <- ts(y_imp[-c(1:train_date),], frequency = 24)
plot(log(y_train))

seasonplot(ts(y_train, frequency = 24))
seasonplot(ts(y_train, frequency = 168))

write.csv(y_train, file = "y_train.csv", row.names = FALSE)
write.csv(y_test, file = "y_test.csv", row.names = FALSE)

# STAZIONARIETA
## STAZIONARIETA IN VARIANZA ------
y_train_lambda <- BoxCox(y_train, "auto")
lambda_boxcox <- attributes(y_train_lambda)$lambda
plot(y_train_lambda)

## STAZIONARIETA IN MEDIA ---------

seasonplot(ts(y_train_lambda, frequency = 24))
seasonplot(ts(y_train_lambda, frequency = 168))

y_train_stag <- diff(y_train_lambda, 24)

#y_train_stag <- diff(y_train_stag)

plot(y_train_stag)

seasonplot(ts(y_train_stag, frequency = 24))

par(mfrow=c(1,2))
Acf(y_train_stag, 72)
Pacf(y_train_stag, 72)


# MODELLI ARIMA ----
## ARIMA DUMMY ---------

mod1 <- Arima(y_train, c(0, 0, 0), c(0, 1, 0),
              lambda = lambda_boxcox, method="CSS")
summary(mod1)

par(mfrow=c(1,2))
Acf(mod1$residuals, 72)
Pacf(mod1$residuals, 72)


mod1 <- Arima(y_train, c(1, 0, 0), c(3, 1, 0),
              lambda = lambda_boxcox, method="CSS")
summary(mod1)

par(mfrow=c(1,2))
Acf(mod1$residuals, 72)
Pacf(mod1$residuals, 72)


mod1 <- Arima(y_train, c(1, 0, 1), c(3, 1, 1),
              lambda = lambda_boxcox, method="CSS")
summary(mod1)

par(mfrow=c(1,2))
Acf(mod1$residuals, 72)
Pacf(mod1$residuals, 72)

par(mfrow=c(1,1))
fcst1 <- forecast(mod1, h=length(y_test))
plot(fcst1)
mean(abs((y_test - as.numeric(fcst1$mean))/y_test)) * 100



## ARIMA SINUSOIDI -----------

omega <- outer(1:length(y), 1:3) * 2 * pi / 168
cc <- cos(omega)
ss <- sin(omega)



mod2 <- Arima(y_train, c(1, 0, 1),
              list(order = c(3, 1, 1), period = 24),
              include.drift = TRUE,
              xreg = cbind(cc, ss)[1:length(y_train), ])
summary(mod2)

par(mfrow=c(1,2))
Acf(mod2$residuals, 72)
Pacf(mod2$residuals, 72)

par(mfrow=c(1,1))
fcst2 <- forecast(mod2, h=length(y_test), xreg = cbind(cc, ss)[(length(y_train)+1):length(y), ] )
plot(fcst2)
plot(fcst2, 0)

mean(abs((y_test - as.numeric(fcst2$mean))/y_test)) * 100

## PREVISIONI ARIMA -----------

modPrevArima <- Arima(y_imp, c(1, 0, 1), c(3, 1, 1),
              lambda = lambda_boxcox, method="CSS")
summary(modPrevArima)

par(mfrow=c(1,2))
Acf(modPrevArima$residuals, 72)
Pacf(modPrevArima$residuals, 72)

par(mfrow=c(1,1))
fcstPrevArima <- forecast(modPrevArima, h=743)
plot(fcstPrevArima)

dates <- seq(as.POSIXct("2005-03-01 00:00:00"), as.POSIXct("2005-03-31 23:00:00"), by="hour")

length(dates)

y <- xts(x=fcstPrevArima$mean, order.by=dates)
attr(y, 'frequency') <- 24
plot(y)

# UCM MODELS ------

y_train = ts(y_train, frequency = 24)

## PRIMO MODELLO -----------

a1_in = c(y_train[1],0)
model = SSModel(y_train ~ SSMtrend(2,Q = list(NA,NA), a1 = a1_in) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)), 
                H= NA)
updt = function(pars, model){
  model$Q[1,1,1]= exp(pars[1])#varianza level
  model$Q[2,2,1]= exp(pars[2])#slope
  model$Q[3,3,1]= exp(pars[3])#stag
  model$H[1,1,1]= exp(pars[4])#errore di y
  model
}


vy = var(diff(y_train)) 
init = rep(log(vy/100), 4)
init_irw = c(log(vy/10000), log(vy/100),log(vy/100))

fit = fitSSM(model, init, updatefn = updt, method="BFGS")

fit$optim.out$convergence #se è zero la stima ha raggiunto convergenza

smo = KFS(fit$model, filtering = "state", smoothing = "state")

smo$alphahat

dim(smo$alphahat)
colnames(smo$alphahat)
level = smo$alphahat[,"level"]
plot(level)

seas = smo$alphahat[, 3]
plot(seas[1:168], type = "l")

pred = ts(predict(fit$model, n.ahead = 1706), frequency = 24)
plot(pred, type ="l")
y_test = ts(y_test, frequency = 24)
lines(y_test, col=2)

mean(abs((y_test - as.numeric(pred))/y_test)) * 100
smo$alphahat[6820,'slope']


## SECONDO MODELLO -----------

a1_in = c(y_train[1],0)
model = SSModel(y_train ~ SSMtrend(2,Q = list(0,NA), a1 = a1_in) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)), 
                H= NA)


updt_irw = function(pars, model){
  model$Q[2,2,1]= exp(pars[1])#slope
  model$Q[3,3,1]= exp(pars[2])#stag
  model$H[1,1,1]= exp(pars[3])#errore di y
  model
}


vy = var(diff(y_train)) 
init_irw = c(log(vy/10000), log(vy/100),log(vy/100))

fit = fitSSM(model, init_irw, updatefn = updt_irw, method="BFGS")

fit$optim.out$convergence 

smo = KFS(fit$model, filtering = "state", smoothing = "state")

smo$alphahat

dim(smo$alphahat)
colnames(smo$alphahat)
level = smo$alphahat[,"level"]
plot(level)

seas = smo$alphahat[, 3]
plot(seas[1:168], type = "l")

pred = ts(predict(fit$model, n.ahead = 1706), frequency = 24)
plot(pred, type ="l")
y_test = ts(y_test, frequency = 24)
lines(y_test, col=2)

mean(abs((y_test - as.numeric(pred))/y_test)) * 100
smo$alphahat[6820,'slope']



## TERZO MODELLO --------------------

time = 1:length(y_train)
time2 = time*time

model = SSModel(y_train ~ SSMregression( ~ time) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)), 
                H= NA)

updt_tr2 = function(pars, model){
  model$Q[1,1,1]= exp(pars[1])#stag
  model$H[1,1,1]= exp(pars[2])#errore di y
  model
}

init_trend = init_irw[-1]

fit = fitSSM(model, init_trend, updatefn = updt_tr2, method="BFGS")

fit$optim.out$convergence #se è zero la stima ha raggiunto convergenza

smo = KFS(fit$model, filtering = "state", smoothing = "state")

smo$alphahat

dim(smo$alphahat)
colnames(smo$alphahat)
level = smo$alphahat[,"(Intercept)"] + smo$alphahat[,"time"]*time #+
  #smo$alphahat[,"time2"]*time2
plot(level, ylim=c(800,2000))

seas = smo$alphahat[, 3]
plot(seas[1:168], type = "l")

plot(level[1:336]+seas[1:336], type = 'l')

time.test = (8526-1705):8526
time.test[1]
y.new = y_test
y.new[1:nrow(y.new),1:ncol(y.new)] = NA
y.new = ts(y.new, frequency = 24)
# mZ deve essere un array 1x26x1706
# i primi 10 valori sulla diagonale devono essere il prezzo della benzina e l'ind_fid_cons (alternati)
mZ = array(data = fit$model$Z[,,1:1706], dim = c(1,25,1706))
head(fit$model$Z)
mZ[1,2,1:1706] = time.test
#mZ[1,3,1:1706] = time.test^2
mT = fit$model$T
mR = fit$model$R
mQ = fit$model$Q
va1 = fit$model$a1 
mP1 = fit$model$P1
mP1i = fit$model$P1inf
mH = fit$model$H
statename = row.names(fit$model$P1)

newdata = SSModel(y.new ~ -1 + SSMcustom(Z = mZ, T = mT, R = mR, Q = mQ,
                                         a1 = va1, P1 = mP1, P1inf = mP1i,
                                         state_names = statename), H = mH)

pred = ts(predict(fit$model, newdata = newdata),frequency = 24)
plot(pred, type ="l")
y_test = ts(y_test, frequency = 24)
lines(y_test, col=2)

mean(abs((y_test - as.numeric(pred))/y_test)) * 100


## QUARTO MODELLO ---------

model = SSModel(y_traiN ~ SSMcycle(period = 8760, Q = NA) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)), 
                H= NA)
model$Q

updt_cycle = function(pars, model){
  model$Q[1,1,1] = model$Q[2,2,1]= exp(pars[1])#ciclo
  model$Q[3,3,1]= exp(pars[2])#stag
  model$H[1,1,1]= exp(pars[3])#errore di y
  model
}

init_cycle = c(log(vy/10),log(vy/10),log(vy/10))

fit = fitSSM(model, init_cycle, updatefn = updt_cycle, method="BFGS")
fit$optim.out$convergence #se è zero la stima ha raggiunto convergenza
smo = KFS(fit$model, filtering = "state", smoothing = "state")

dim(smo$alphahat)
colnames(smo$alphahat)
level = smo$alphahat[,"(Intercept)"] + smo$alphahat[,"cycle"]
plot(level, ylim=c(800,2000))

seas = smo$alphahat[, 2]
plot(seas[1:168], type = "l")

plot(level[1:336]+seas[1:336], type = 'l')
lines(as.numeric(y_train[1:336]),col = 2)
pred = ts(predict(fit$model, n.ahead = 1706), frequency = 24 )
plot(pred, type ="l")
y_test = ts(y_test, frequency = 24)
lines(y_test, col=2)

mean(abs((y_test - as.numeric(pred))/y_test)) * 100



## QUINTO MODELLO -----------------------

time = 1:length(y_train)

model = SSModel(ts(y_train, frequency = 24) ~ SSMregression( ~ time) +
                  SSMcycle(period = 8760, Q = NA) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)),
                H= NA)
model$Q

updt_cycle = function(pars, model){
  model$Q[1,1,1] = model$Q[2,2,1]= exp(pars[1])#ciclo
  model$Q[3,3,1]= exp(pars[2])#stag
  model$H[1,1,1]= exp(pars[3])#errore di y
  model
}

init_cycle = c(log(vy/10),log(vy/10),log(vy/10))

fit = fitSSM(model, init_cycle, updatefn = updt_cycle, method="BFGS")
fit$optim.out$convergence 

smo = KFS(fit$model, filtering = "state", smoothing = "state")
smo$alphahat

dim(smo$alphahat)
colnames(smo$alphahat)

level = smo$alphahat[,"(Intercept)"] + smo$alphahat[,"time"]*time +smo$alphahat[,"cycle"]
plot(level, ylim=c(800,2000))

seas = smo$alphahat[, 3]
plot(seas[1:168], type = "l")
plot(level[1:336]+seas[1:336], type = 'l')

# PREDIZIONI

time.test = (8526-1705):8526
time.test[1]
y.new = y_test
y.new[1:nrow(y.new),1:ncol(y.new)] = NA
y.new = ts(y.new, frequency = 24)
mZ = array(data = fit$model$Z[,,1:1706], dim = c(1,27,1706))
head(fit$model$Z)
mZ[1,2,1:1706] = time.test
mT = fit$model$T
mR = fit$model$R
mQ = fit$model$Q
va1 = fit$model$a1 
mP1 = fit$model$P1
mP1i = fit$model$P1inf
mH = fit$model$H
statename = row.names(fit$model$P1)


newdata = SSModel(y.new ~ -1 + SSMcustom(Z = mZ, T = mT, R = mR, Q = mQ,
                                         a1 = va1, P1 = mP1, P1inf = mP1i,
                                         state_names = statename), H = mH)

pred = ts(predict(fit$model, newdata = newdata),frequency = 24)


plot(pred, type ="l", ylim= c(800,2000))
y_test = ts(y_test, frequency = 24)
lines(y_test, col=2)

mean(abs((y_test - as.numeric(pred))/y_test)) * 100
model$Z


## SESTO MODELLO -----------------------

a1_in = c(y_train[1],diff(y_train)[1])
model = SSModel(ts(y_train, frequency = 24) ~
                  SSMtrend(2,Q = list(0,NA)) +
                  SSMcycle(period = 8760, Q = NA, P1inf = matrix(0,2,2)) +
                  SSMcycle(period = 8760/16, Q = NA, state_names = c("ciclo2", "ciclo2*"),P1inf = matrix(0,2,2)) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)), 
                H= NA)
model$Q

model$H

updt_cycle = function(pars, model){
  model$Q[2,2,1]= exp(pars[1])#slope
  model$Q[4,4,1] = model$Q[5,5,1]= exp(pars[3])#ciclo
  model$Q[6,6,1] = model$Q[7,7,1]= exp(pars[4])
  model$Q[3,3,1]= exp(pars[2])#stag
  model$H[1,1,1]= exp(pars[5])#errore di y
  model
}

init_cycle = c(log(vy/10000),log(vy/10000),log(vy/10000),log(vy/10000),log(vy/10000))

fit = fitSSM(model, init_cycle, updatefn = updt_cycle, method="BFGS")
fit$optim.out$convergence #se è zero la stima ha raggiunto convergenza
smo = KFS(fit$model, filtering = "state", smoothing = "state")

dim(smo$alphahat)
colnames(smo$alphahat)
level = smo$alphahat[,"level"] + smo$alphahat[,"cycle"]+ smo$alphahat[,"ciclo2"]
plot(level, ylim=c(800,2000))

seas = smo$alphahat[, 3]
plot(seas[1:168], type = "l")

plot(level[1:336]+seas[1:336], type = 'l')
lines(as.numeric(y_train[1:336]),col = 2)
pred = ts(predict(fit$model, n.ahead = 1706), frequency = 24 )
plot(pred, type ="l", ylim=c(800,1800))
y_test = ts(y_test, frequency = 24)
lines(y_test, col=2)

mean(abs((y_test - as.numeric(pred))/y_test)) * 100

## PREVISIONI UCM -------

vy = var(y_imp)
time = 1:length(y_imp)

model = SSModel(ts(y_imp, frequency = 24) ~ SSMregression( ~ time) +
                  SSMcycle(period = 8760, Q = NA) +
                  SSMseasonal(24, sea.type = "dummy", Q = NA, P1inf = diag(23)),
                H= NA)
model$Q

updt_cycle = function(pars, model){
  model$Q[1,1,1] = model$Q[2,2,1]= exp(pars[1])#ciclo
  model$Q[3,3,1]= exp(pars[2])#stag
  model$H[1,1,1]= exp(pars[3])#errore di y
  model
}

init_cycle = c(log(vy/10),log(vy/10),log(vy/10))

fit = fitSSM(model, init_cycle, updatefn = updt_cycle, method="BFGS")
fit$optim.out$convergence 

smo = KFS(fit$model, filtering = "state", smoothing = "state")
smo$alphahat

dim(smo$alphahat)
colnames(smo$alphahat)

level = smo$alphahat[,"(Intercept)"] + smo$alphahat[,"time"]*time +smo$alphahat[,"cycle"]
plot(level, ylim=c(800,2000))

seas = smo$alphahat[, 3]
plot(seas[1:168], type = "l")
plot(level[1:336]+seas[1:336], type = 'l')

# PREDIZIONI

time.pred = 1:743

y.new = y_test
y.new[1:nrow(y.new),1:ncol(y.new)] = NA
y.new = ts(y.new[1:743], frequency = 24)
mZ = array(data = fit$model$Z[,,1:743], dim = c(1,27,743))
head(fit$model$Z)
mZ[1,2,1:743] = time.pred
mT = fit$model$T
mR = fit$model$R
mQ = fit$model$Q
va1 = fit$model$a1 
mP1 = fit$model$P1
mP1i = fit$model$P1inf
mH = fit$model$H
statename = row.names(fit$model$P1)


newdata = SSModel(y.new ~ -1 + SSMcustom(Z = mZ, T = mT, R = mR, Q = mQ,
                                         a1 = va1, P1 = mP1, P1inf = mP1i,
                                         state_names = statename), H = mH)

pred = ts(predict(fit$model, newdata = newdata),frequency = 24)


plot(pred, type ="l", ylim= c(800,2000))

dates <- seq(as.POSIXct("2005-03-01 00:00:00"), as.POSIXct("2005-03-31 23:00:00"), by="hour")



y <- xts(x=pred, order.by=dates)
attr(y, 'frequency') <- 24
plot(y)


#COMPORRE PREVISIONI


