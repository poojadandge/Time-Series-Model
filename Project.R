# Load packages
library(pacman)
p_load(TSA)
p_load(FSAdata)
p_load(fUnitRoots)
p_load(lmtest)
p_load(data.table)
p_load(lubridate)
p_load(tidyverse)
p_load(tseries)
p_load(forecast)
p_load(qqplotr)
p_load(FitAR)
p_load(cowplot)
theme_set(theme_gray())
library(ggplot2)

# Function to calculate Mean Absolute Scaled Error

MASE = function(observed , fitted ){
  Y.t = observed
  n = length(fitted)
  e.t = Y.t - fitted
  sum = 0 
  for (i in 2:n){
    sum = sum + abs(Y.t[i] - Y.t[i-1] )
  }
  q.t = e.t / (sum/(n-1))
  MASE = data.frame( MASE = mean(abs(q.t)))
  return(list(MASE = MASE))
}

# Function to plot residual analysis graphs

residual_analysis <- function(model, std = TRUE,start = 2, class = c("ARIMA","GARCH","ARMA-GARCH")[1]){
  p_load(TSA)
  p_load(FitAR)
  p_load(tidyverse)
  p_load(cowplot)
  p_load(qqplotr)
  theme_set(theme_gray())
  if (class == "ARIMA"){
    if (std == TRUE){
      res.model = rstandard(model)
    }else{
      res.model = residuals(model)
    }
  }else if (class == "GARCH"){
    res.model = model$residuals[start:model$n.used]
  }else if (class == "ARMA-GARCH"){
    res.model = model@fit$residuals
  }else {
    stop("The argument 'class' must be either 'ARIMA' or 'GARCH' ")
  }
  timeseries <- ggplot(mapping=aes(y=res.model,x=time(res.model))) + geom_point(color = "blue") + geom_line(color = "blue") + geom_hline(yintercept = 0) + ylab("Standardised Residuals") + xlab("") + ggtitle("Time Series Plot of Standardised Residuals") + theme(plot.title = element_text(hjust = 0.5))
  histogram <- ggplot(mapping=aes(x=res.model)) + geom_histogram(color = "black",fill = "blue") + xlab("") + ylab("Frequency") + ggtitle("Histogram of Standardised Residuals") + theme(plot.title = element_text(hjust = 0.5))
  qqplot <- ggplot(data=NULL,aes(sample=res.model)) + geom_qq_line(color = "red") + geom_qq() + ylab("Sample Quantiles") + xlab("") + ggtitle("Q-Q Plotof Standardised Residuals") + theme(plot.title = element_text(hjust = 0.5)) + xlim(-4,4)
  shapiro.test(res.model)
  acf <- autoplot(acf(res.model, plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("ACF") + xlab("") + ggtitle("ACF plot of Standardised Residuals") + theme(plot.title = element_text(hjust = 0.5))
  pacf <- autoplot(acf(res.model,type = "partial", plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("PACF") + xlab("") + ggtitle("PACF plot of Standardised Residuals")  + theme(plot.title = element_text(hjust = 0.5))
  lbq <- LjungBoxTest(res.model,lag.max = 150) %>% data.frame()
  lbqplot <- ggplot(mapping=aes(y=lbq$pvalue,x=lbq$m)) + geom_point(color = "blue") + geom_hline(yintercept = 0.05,linetype="dashed") + ylab("p-value") + xlab("") + ggtitle("Ljung-Box Test") + theme(plot.title = element_text(hjust = 0.5))
  plot_grid(timeseries,histogram,acf,pacf,qqplot,lbqplot,ncol = 2,nrow = 3,align = "h")
}

# Reading and cleaning the data
bitcoin <- fread("Bitcoin_Historical_Price.csv") # Actual data
bitcoin_forecast <- fread("Bitcoin_Prices_Forecasts.csv") # Actual forecast values
bitcoin$Close <- str_remove_all(bitcoin$Close,",") # Removing extra ',' in the closing price
bitcoin$Close <- as.numeric(bitcoin$Close) # Numeric conversion
head(bitcoin) # Top 6 rows of the data
summary(bitcoin) # Summary statistics
bitcoin$Date <- dmy(bitcoin$Date) # Converting date fromnumeric to date object
bitcoin_ts <- ts(bitcoin$Close,start = min(bitcoin$Date),end = max(bitcoin$Date)) # Converting data to timeseries object
summary(bitcoin_ts) # Summary statistics of timeseries data
time <- time(bitcoin_ts) %>% as.numeric()
summary(as.Date.numeric(time,origin = "1970-01-01")) # Summary statistics of Date

# Timeseries plot of Closing Price

ggplot(mapping=aes(y=bitcoin_ts,x=bitcoin$Date)) + geom_point(color = "blue") + geom_line(color = "blue") + ylab("Bitcoin Closing Price (USD)") + xlab("Date") + ggtitle("Time Series Plot of Bitcoin Closing Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = "1 year",date_labels = "%Y")
# Visible trend
# Changing variance in the latter part
# Seasonality not very obvious
# Intervention point could exist
# AR behaviour mostly

# Autocorrelation function plots

acf <- autoplot(acf(bitcoin_ts, plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ggtitle("Sample ACF plot of Bitcoin series") + theme(plot.title = element_text(hjust = 0.5))
pacf <- autoplot(acf(bitcoin_ts,type = "partial", plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("PACF") + ggtitle("Sample PACF plot of Bitcoin series")  + theme(plot.title = element_text(hjust = 0.5))
plot_grid(acf, pacf, ncol=2, align="h")

# Slowly decaying pattern in ACF confirms presence of trend
# Multiple significant lags after few insignificant lags suggest changing variance

# Normality check

shapiro.test(bitcoin_ts)
histogram <- ggplot(mapping=aes(x=bitcoin_ts)) + geom_histogram(color = "black",fill = "blue") + xlab("Bitcoin Closing Price") + ylab("Frequency") + ggtitle("Histogram of Bitcoin Closing Price") + theme(plot.title = element_text(hjust = 0.5))
qqplot <- ggplot(data=NULL,aes(sample=bitcoin_ts)) + geom_qq_line(color = "red") + geom_qq() + ylab("Sample Quantiles") + xlab("Theoretical Quantiles") + ggtitle("Normal Q-Q Plot") + theme(plot.title = element_text(hjust = 0.5)) + xlim(-4,4)
plot_grid(histogram, qqplot, ncol=2, align="h")
# Data is non-normal - extremely right skewed

# BoxCox transformation

b <- BoxCox.ar(bitcoin_ts,method = "yw")
ggplot(mapping = aes(y=b$loglike,x=b$lambda)) + geom_line(color="blue") + geom_vline(xintercept = b$ci,color="red",linetype="dashed") + geom_vline(xintercept = b$mle,linetype="dashed") + geom_hline(yintercept = quantile(b$loglike,0.95),linetype="dashed") + geom_text(aes(x=-1.5,y=max(b$loglike)*1.02),label = "95% Confidence Interval",size = 2.5)
# Suggested lambda value = 0 implying log transformation

bitcoin_log <- log(bitcoin_ts)
ggplot(mapping=aes(y=bitcoin_log,x=bitcoin$Date)) + geom_point(color = "blue") + geom_line(color = "blue") + ylab("Bitcoin Closing Price (USD)") + xlab("Date") + ggtitle("Time Series Plot of log of Bitcoin Closing Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = "1 year",date_labels = "%Y")
# Trend is still present
# Effect of changing variance looks reduced
# Seasonality not very obvious
# Intervention point does not exist
# AR behaviour mostly 

# Augmented Dickey-Fuller Test

ar(diff(bitcoin_log))
adfTest(bitcoin_log,31)
# ADF Test confirms the presence of trend

# Differencing

bitcoin_log_diff <- diff(bitcoin_log)
ggplot(mapping=aes(y=bitcoin_log_diff,x=bitcoin$Date[-1])) + geom_point(color = "blue") + geom_line(color = "blue") + ylab("Bitcoin Closing Price (USD)") + xlab("Date") + ggtitle("Time Series Plot of difference of log of Bitcoin Closing Price") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = "1 year",date_labels = "%Y")
# Trend has been removed
# Visible volatility clustering 
# Seasonality not very obvious
# Intervention point does not exist
# Both AR and MA behaviour are exhibited 

# Augmented Dickey-Fuller Test

ar(diff(bitcoin_log_diff))
adfTest(bitcoin_log_diff,32)
# ADF Test confirms that the series has been detrended

# Parameter Estimation

acf <- autoplot(acf(bitcoin_log_diff, plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ggtitle("Sample ACF plot of transformed Bitcoin series") + theme(plot.title = element_text(hjust = 0.5))
pacf <- autoplot(acf(bitcoin_log_diff,type = "partial", plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("PACF") + ggtitle("Sample PACF plot of transformed Bitcoin series")  + theme(plot.title = element_text(hjust = 0.5))
plot_grid(acf, pacf, ncol=2, align="h")
# ACF and PACF have similar pattern, Values of p and q can be 2 or 4
# Thus the set of possible models from ACF and PACF = {ARIMA(2,1,2),ARIMA(4,1,4)}

eacf(bitcoin_log_diff,ar.max = 8,ma.max = 8)
# EACF has vertex at (0,0)
# Thus the set of possible models from EACF = {ARIMA(0,1,1),ARIMA(1,1,1),ARIMA(0,1,2),ARIMA(1,1,2)}

res = armasubsets(y=bitcoin_log_diff,nar=8,nma=8,y.name='test',ar.method='ols')
plot(res)
# The set of possible models from BIC Table = {ARIMA(5,1,5),ARIMA(6,1,5),ARIMA(6,1,6)}

# Thus the final set of possible models is {ARIMA(0,1,1),ARIMA(0,1,2),ARIMA(1,1,1),ARIMA(1,1,2),ARIMA(2,1,2),ARIMA(4,1,4),ARIMA(5,1,5),ARIMA(5,1,6),ARIMA(6,1,5),ARIMA(6,1,6)}

# Model Specification

model.011.css = arima(bitcoin_log,order=c(0,1,1),method='CSS')
coeftest(model.011.css)
# Insignificant

model.012.css = arima(bitcoin_log,order=c(0,1,2),method='CSS')
coeftest(model.012.css)
# Insignificant

model.111.css = arima(bitcoin_log,order=c(1,1,1),method='CSS')
coeftest(model.111.css)
# Insignificant

model.112.css = arima(bitcoin_log,order=c(1,1,2),method='CSS')
coeftest(model.112.css)
# Insignificant

model.212.css = arima(bitcoin_log,order=c(2,1,2),method='CSS')
coeftest(model.212.css)
# All coefficients significant

model.414.css = arima(bitcoin_log,order=c(4,1,4),method='CSS')
coeftest(model.414.css)
# Mostly coefficients significant

model.515.css = arima(bitcoin_log,order=c(5,1,5),method='CSS')
coeftest(model.515.css)
# Very few coefficients significant - Rejected

model.615.css = arima(bitcoin_log,order=c(6,1,5),method='CSS')
coeftest(model.615.css)
# Mostly coefficients significant

model.616.css = arima(bitcoin_log,order=c(6,1,6),method='CSS')
coeftest(model.616.css)
# Very few coefficients significant - Rejected

# Residual Analysis for significant models

residual_analysis(model.212.css)
residual_analysis(model.414.css)
residual_analysis(model.615.css)
# ARIMA(6,1,5) selected due to best residuals


# Overfitting

model.715.css = arima(bitcoin_log,order=c(7,1,5),method='CSS')
coeftest(model.715.css)
# Coefficients mostly insignificant - Rejected

# Arch-Garch modelling over residuals to handle changing variance

res <- residuals(model.615.css)
acf <- autoplot(acf(res, plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ggtitle("Sample ACF plot of Residuals") + theme(plot.title = element_text(hjust = 0.5))
pacf <- autoplot(acf(res,type = "partial", plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("PACF") + ggtitle("Sample PACF plot of transformed Bitcoin series")  + theme(plot.title = element_text(hjust = 0.5))
plot_grid(acf, pacf, ncol=2, align="h")
# Multiple significant lags in ACF and PACF

eacf(res,ar.max = 8,ma.max = 8)
# EACF supports ARMA(0,0) suggesing series is white noise in terms of ARMA components

# Absolute of Residuals

abs <- abs(res)
acf <- autoplot(acf(abs, plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ggtitle("Sample ACF plot of Absolute of Residuals") + theme(plot.title = element_text(hjust = 0.5))
pacf <- autoplot(acf(abs,type = "partial", plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("PACF") + ggtitle("Sample PACF plot of transformed Bitcoin series")  + theme(plot.title = element_text(hjust = 0.5))
plot_grid(acf, pacf, ncol=2, align="h")
# ACF and PACF suggest that series is not independently and identically distributed
eacf(abs,ar.max = 8,ma.max = 8)

sq <- (res)^2
acf <- autoplot(acf(sq, plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ggtitle("Sample ACF plot of Absolute of Residuals") + theme(plot.title = element_text(hjust = 0.5))
pacf <- autoplot(acf(sq,type = "partial", plot = FALSE,lag.max=50)) + geom_hline(aes(yintercept = 0)) + ylab("PACF") + ggtitle("Sample PACF plot of transformed Bitcoin series")  + theme(plot.title = element_text(hjust = 0.5))
plot_grid(acf, pacf, ncol=2, align="h")
eacf(sq,ar.max = 8,ma.max = 8)
# ACF, PACF and EACF suggest that series is not independently and identically distributed

# Possible p-q orders ACF, PACF and EACF = {01, 11, 10, 12, 22, 34, 44}
# Thus, possible Arch-Garch orders (max(p,q),p) = {10, 11, 21, 22, 43, 44}

# Model Specification

garch.10 <- garch(res,c(1,0))
coeftest(garch.10)
# Insignificant

garch.11 <- garch(res,c(1,1))
coeftest(garch.11)
# All significant

garch.21 <- garch(res,c(2,1))
coeftest(garch.21)
# All significant

garch.22 <- garch(res,c(2,2))
coeftest(garch.22)
# Mostly significant

garch.43 <- garch(res,c(4,3))
coeftest(garch.43)
# Very few significant - rejected

garch.44 <- garch(res,c(4,4))
coeftest(garch.44)
# Insignificant

# Residual Analysis
residual_analysis(garch.11, std = TRUE,start = 3, class = "GARCH")
residual_analysis(garch.21, std = TRUE,start = 3, class = "GARCH")
residual_analysis(garch.22, std = TRUE,start = 3, class = "GARCH")
# Similar residuals
# Series looks almost white noise

AIC(garch.11,garch.21,garch.22)
# garch(2,1) has the best AIC and all significant components

# Goodness of Fit
mod = Arima(bitcoin_ts,order=c(6,1,5),method='CSS',lambda = 0)
MASE_fit <- MASE(bitcoin_ts,mod$fitted)$MASE
paste("Mean Absolute Scaled Error for the fit = ",round(MASE_fit,3))

# Forecasting

pred <- forecast(mod,h=10)

# Forecast plot

p <- ggplot() + geom_point(data=bitcoin[bitcoin$Date>dmy("01-01-2019")],mapping=aes(y=Close,x=Date,color = "Series")) + geom_line(data=bitcoin[bitcoin$Date>dmy("01-01-2019")],mapping=aes(y=Close,x=Date,color = "Series")) + ylab("Bitcoin Closing Price (USD)") + xlab("Date") + ggtitle("Next 10 days Bitcoin closing price predictions") + theme(plot.title = element_text(hjust = 0.5)) + scale_x_date(breaks = "1 month",date_labels = "%B %Y")
forecast_date <- seq(max(bitcoin$Date)+1,max(bitcoin$Date)+10,1)
p + geom_point(mapping=aes(y=pred$mean,x=forecast_date,color = "Forecasts")) + geom_line(mapping=aes(y=pred$mean,x=forecast_date,color = "Forecasts")) + geom_ribbon(mapping = aes(x=forecast_date,ymin=pred$lower[,1],ymax=pred$upper[,1],alpha = "80%"),fill="green")  + geom_ribbon(mapping = aes(x=forecast_date,ymin=pred$lower[,2],ymax=pred$upper[,2],alpha = "95%"),fill="green") + scale_colour_manual(name="Mean Level",values=c("Series" = "blue","Forecasts" = "red"), guide = guide_legend(fill = NULL,colour = NULL)) + scale_alpha_manual(name="Confidence Bounds",values=c("80%" = 0.35,"95%" = 0.15), guide = guide_legend(fill = NULL,colour = NULL))

# Prediction error

MASE_forecast <- MASE(bitcoin_forecast$`Closing price`,pred$mean)$MASE
paste("Mean Absolute Scaled Error for the forecast = ",round(MASE_forecast,3))
