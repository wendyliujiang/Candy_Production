
library(forecast)
library(zoo)

 
#Reading file and creating data frame
candy_df <- read.csv("candy_production.csv");
head(candy_df)


#Renaming column name
colnames(candy_df)[1] ='date'
colnames(candy_df)[2] ='production_index'
head(candy_df)


# convert to time series data frame
prod.ts <- ts(candy_df$production_index, start = c(1973,3), end=c(2023,3),
               freq = 12);

head(prod.ts)

# data set decomposition
production.stl <- stl(prod.ts, s.window = "periodic")
autoplot(production.stl, main="Candy Production")


plot(prod.ts, 
     xlab = "Time", ylab = "Production Index", 
     ylim = c(30, 160), xaxt = 'n',
     main = "Candy Productions",
     col = "blue", bty = "l", lwd = 2)
axis(1, at = seq(1973, 2023, 3), labels = format(seq(1973, 2023, 3)))
 

autocor <- Acf(prod.ts, lag.max = 12, 
               main = "Autocorrelation for candy Production Data")


Lag <- round(autocor$lag, 0)
ACF <- round(autocor$acf, 3)
data.frame(Lag, ACF)

length(prod.ts)

# Summary Statistics
install.packages("pastecs")  
library(pastecs)
options(scipen = 999)
summary <-round(stat.desc(prod.ts),3)
summary

# pre-process data
which(is.na(prod.ts))
sum(is.na(prod.ts))

# Partition Time series
# Spliting the dataset into validation and training dataset
# Spliting the dataset into 80-20 ratio of training and validation set
# Since the dataset has 600 entries assiging 120 as size of validation set
###
nValid <- 120

nTrain <- length(prod.ts) - nValid
train.ts <- window(prod.ts, start = c(1973, 3), end = c(1973, nTrain))
train.ts
valid.ts <- window(prod.ts, start = c(1973, nTrain + 1), end = c(1973, nTrain + nValid))
valid.ts

## testing predictability
# method 1. Use Arima() function to fit AR(1) model, order = c(1,0,0)
prod.ar1<- Arima(prod.ts, order = c(1,0,0))
summary(prod.ar1)

ar1 <- 0.879
s.e. <- 0.0195
null_mean <- 1
alpha <- 0.05
z.stat <- (ar1-null_mean)/s.e.
z.stat
p.value <- pnorm(z.stat)
p.value
if (p.value<alpha) {
  "Reject null hypothesis"
} else {
  "Accept null hypothesis"
}

# method 2. Create differenced revenue.ts data using lag-1.
diff.prod <- diff(prod.ts, lag = 1)

Acf(diff.prod, lag.max = 12, 
    main = "Autocorrelation for First Differencing of Production")


#Model 1:regression model with linear trend and seasonality
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred


# model 2: Quadratic Trend and Seasonality 
# Use tslm() function to create quadratic trend and seasonal model.
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred





#Model 3:Regression model with linear trend and seasonality + Trailing MA

train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

#regression residual in the training period
train.lin.season.res <- train.lin.season$residuals
train.lin.season.res
#applying a trailing MA with window width of 3 for regression residual in training dataset
ma.trail.res <- rollmean(train.lin.season.res, k = 3, align = "right")
ma.trail.res


ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

fst.2level <- train.lin.season.pred$mean + ma.trail.res.pred$mean
fst.2level

#Model 4:Quadratic Trend and Seasonality + Trailing MA

train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

#regression residual in the training period
train.quad.season.res <- train.quad.season$residuals
train.quad.season.res
#applying a trailing MA with window width of 3 for regression residual in training dataset
ma.trail.res <- rollmean(train.quad.season.res, k = 3, align = "right")
ma.trail.res


ma.trail.res.pred <- forecast(ma.trail.res, h = nValid, level = 0)
ma.trail.res.pred

scd.2level <- train.quad.season.pred$mean + ma.trail.res.pred$mean
scd.2level




#Model 5： linear trend and seasonality + AR(1) for residuals
train.lin.season <- tslm(train.ts ~ trend + season)

# See summary of linear trend and seasonality model and associated parameters.
summary(train.lin.season)

# Apply forecast() function to make predictions for ts with 
# linear trend and seasonality data in validation set.  
train.lin.season.pred <- forecast(train.lin.season, h = nValid, level = 0)
train.lin.season.pred

Acf(train.lin.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Production' Training Residuals")

res.ar1 <- Arima(train.lin.season.pred$residuals, order = c(1,0,0))
summary(res.ar1)

Acf(res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Production' Training Residuals of Residuals")

res.ar1.pred <- forecast(res.ar1, h = nValid, level = 0)
res.ar1.pred

valid.lin.ar1.two.level.pred <- train.lin.season.pred$mean + res.ar1.pred$mean
valid.lin.ar1.two.level.pred

valid1.df <- data.frame(valid.ts, train.lin.season.pred$mean, 
                       res.ar1.pred$mean, valid.lin.ar1.two.level.pred)
names(valid1.df) <- c("Production", "Reg.Forecast", 
                     "AR(1)Forecast", "Combined.Forecast")
valid1.df




#Model 6 quadratic trend and seasonality + AR(1) for residuals
train.quad.season <- tslm(train.ts ~ trend + I(trend^2) + season)

# See summary of quadratic trend and seasonality model and associated parameters.
summary(train.quad.season)

# Apply forecast() function to make predictions for ts with 
# trend and seasonality data in validation set.  
train.quad.season.pred <- forecast(train.quad.season, h = nValid, level = 0)
train.quad.season.pred

Acf(train.quad.season.pred$residuals, lag.max = 12, 
    main = "Autocorrelation for Production' Training Residuals")

quad.res.ar1 <- Arima(train.quad.season.pred$residuals, order = c(1,0,0))
summary(quad.res.ar1)
quad.res.ar1.pred <- forecast(quad.res.ar1, h = nValid, level = 0)
quad.res.ar1.pred

valid.quad.ar1.two.level.pred <- train.quad.season.pred$mean + quad.res.ar1.pred$mean
valid.quad.ar1.two.level.pred

valid2.df <- data.frame(valid.ts, train.quad.season.pred$mean, 
                        quad.res.ar1.pred$mean, valid.quad.ar1.two.level.pred)
names(valid2.df) <- c("Production", "Reg.Forecast", 
                      "AR(1)Forecast", "Combined.Forecast")
valid2.df

Acf(quad.res.ar1$residuals, lag.max = 12, 
    main = "Autocorrelation for Production' Training Residuals of Residuals")



#Model 7 Auto ARIMA model
#auto.arima() for training
train.auto.arima <- auto.arima(train.ts)
summary(train.auto.arima)

# forecast revenue in the validation period
train.auto.arima.pred <- forecast(train.auto.arima, 
                                  h = nValid, level = 0)
train.auto.arima.pred


#Model 8 HW model
hw.ZZZ <- ets(train.ts, model = "ZZZ")
hw.ZZZ
hw.ZZZ.pred <- forecast(hw.ZZZ, h = nValid, level = 0)
hw.ZZZ.pred

round(accuracy(train.lin.season.pred$mean, valid.ts),3)
round(accuracy(train.quad.season.pred$mean, valid.ts),3)
round(accuracy(fst.2level, valid.ts), 3)
round(accuracy(scd.2level, valid.ts), 3)
round(accuracy(valid.lin.ar1.two.level.pred, valid.ts), 3)
round(accuracy(valid.quad.ar1.two.level.pred, valid.ts), 3)
round(accuracy(train.auto.arima.pred$mean, valid.ts), 3)
round(accuracy(hw.ZZZ.pred$mean, valid.ts), 3)

# TOP three performed models: 2 level model with linear trend and seasonality
#and ar1 model for residuals, HW MODEL, Auto ARIMA model
# using the entire data set

trend.season <- tslm(prod.ts ~ trend  + season)

# See summary of linear trend and seasonality equation 
# and associated parameters.
summary(trend.season)


trend.season.pred <- forecast(trend.season, h = 12, level = 0)
trend.season.pred

# Use Arima() function to fit AR(1) model for regression residuals.
# The ARIMA model order of order = c(1,0,0) gives an AR(1) model.

residual.ar1 <- Arima(trend.season$residuals, order = c(1,0,0))
residual.ar1.pred <- forecast(residual.ar1, h = 12, level = 0)

# Use summary() to identify parameters of AR(1) model.
summary(residual.ar1)

# Use Acf() function to identify autocorrealtion for the residual of residuals 
# and plot autocorrelation for different lags (up to maximum of 12).
Acf(residual.ar1$residuals, lag.max = 12, 
    main = 
      "Autocorrelation for 2 level model with linear trend and seasonality
and ar1 model for residuals for Entire Data Set")

# Identify two-level forecast for the 8 future periods 
# as sum of linear trend and seasonality model 
# and AR(1) model for residuals.
trend.season.ar1.pred <- trend.season.pred$mean + residual.ar1.pred$mean
trend.season.ar1.pred

# Create a data table with linear trend and seasonal forecast 
# for 12 future periods, AR(1) model for residuals for 12 
# future periods, and combined two-level forecast for
# 12 future periods. 
table.df <- data.frame(trend.season.pred$mean, 
                       residual.ar1.pred$mean, 
                       trend.season.ar1.pred)
names(table.df) <- c("Reg.Forecast", "AR(1)Forecast",
                     "Combined.Forecast")
table.df


# HW model
HW.ZZZ <- ets(prod.ts, model = "ZZZ")
HW.ZZZ 

# Use forecast() function to make predictions using this HW model for
# 12 month into the future.
HW.ZZZ.pred <- forecast(HW.ZZZ, h = 12 , level = 0)
HW.ZZZ.pred
Acf(HW.ZZZ$residuals, lag.max = 12, 
    main = "Autocorrelations of HW.ZZZ Residuals")

#Auto ARIMA model
auto.arima <- auto.arima(prod.ts)
summary(auto.arima)
auto.arima.pred <- forecast(auto.arima, h = 12, level = 0)
auto.arima.pred

Acf(auto.arima$residuals, lag.max = 12, 
    main = "Autocorrelations of Auto ARIMA Model Residuals")


# compare accuracy of these three models with seasonal naive model as baseline

round(accuracy(trend.season$fitted + residual.ar1$fitted, prod.ts), 3)
round(accuracy(HW.ZZZ.pred$fitted, prod.ts), 3)
round(accuracy(auto.arima.pred$fitted, prod.ts), 3)
round(accuracy((snaive(prod.ts))$fitted, prod.ts), 3)

#### best model is auto ARIMA model
## plot 
plot(prod.ts, 
     xlab = "Time", ylab = "Production (tons)", 
     ylim = c(50, 180), xaxt = "n",
     bty = "l", xlim = c(1973, 2025), lwd = 2,
     main = "Auto ARIMA Model for Entire Data Set") 
axis(1, at = seq(1973, 2025, 1), labels = format(seq(1973, 2025, 1)))
lines(auto.arima$fitted, col = "blue", lwd = 2)
lines(auto.arima.pred$mean, col = "red", lty = 5, lwd = 2)
legend(1973,180, legend = c("Candy Production", 
                            "Auto ARIMA Forecast", 
                            "Auto ARIMA Forecast for 12 Future Periods"), 
       col = c("black", "blue" , "red"), 
       lty = c(1, 1, 5), lwd =c(2, 2, 2), bty = "n")

lines(c(2023.4, 2023.4), c(50, 180))#future data
text(1973.3, 182, "Data Set")
text(2026, 182, "Future")
arrows(1973.3, 180, 2023, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)
arrows(2023, 180, 2026, 180, code = 3, length = 0.1,
       lwd = 1, angle = 30)
