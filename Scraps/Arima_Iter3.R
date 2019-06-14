library (dplyr)
library (lubridate)
library(tidyr)
library(zoo)
library(GGally)
library(forecast)
library(ggplot2)
library(DataCombine)
library(seasonal)
library(urca)

square <- function (x) {x^2}

# Function that gets the naive component of the MASE calculation. Specific for finding cross-validation accuracy
# The ts object that is passed to it is that of the target value
get_naive_cv_comp <- function (ts_obj, h=2, initial) {
  
  tsCV (ts_obj, naive, h=h, initial = initial) %>%
    as.data.frame() %>%
    abs() %>% 
    drop_na() %>%
    mutate(Sum_Errors = rowSums(.)) %>% 
    select(Sum_Errors) %>%
    colMeans()
}

# Function that calculates MASE. Specific for finding cross-validation
# The ts object that is passed is that of the errors of the forecast
# Very important to make sure that the naive benchmark that is passed has the same parameters of the evaluated model
get_mase_cv <- function (ts_obj, h=2, naive=naive_error) {
  
  ts_obj %>% 
    as.data.frame() %>% 
    #mutate(Index = row.names(.)) %>% 
    drop_na() %>%
    abs() %>% 
    mutate(Sum_Errors = rowSums(.)) %>%
    mutate(Q =  Sum_Errors / naive_error) %>%
    select(Q) %>%
    rename(MASE = Q) %>% 
    colMeans()
  
}



#creating a timeseries object with data
item_ts <- item_df %>%
  filter (Period >= '2015-01-01') %>% 
  filter(Period < "2019-04-01") %>% 
  #filter( Period <='2019-01-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_train <- item_df %>% 
  filter (Period >= '2015-01-01') %>% 
  filter( Period <= '2019-02-01') %>% 
  select(-Period) %>% 
  ts(start = c(2015,1), frequency = 12)

#creating a train data set that is a timeseries object
item_ts_test <- item_df %>% 
  filter (Period > '2019-02-01') %>% 
  filter( Period < '2019-05-01') %>% 
  select(-Period) %>% 
  ts(start = c(2019,3), frequency = 12)


h=1
#naïve forecast of the seasonally adjusted data and a seasonal naïve forecast of the seasonal component, 
#- after an STL decomposition of the data
fit.loess <- item_ts_train[, 1] %>%
  stl(t.window=5, s.window=5, robust=TRUE)

#Checking residuals
fit.loess$time.series[,"remainder"] %>% ggAcf()
fit.loess$time.series[,"remainder"] %>% autoplot()

loess %>% seasadj() %>% naive() %>%
  autoplot()

fc.loess <- fit.loess %>%
  forecast(method="naive", h=h) 

fc.loess %>% 
  accuracy(x=item_ts_test[,1])
  
fc.loess %>%
  autoplot() + autolayer(item_ts_test[,1])


#Simple Exponential Smoothing
fc.ses <- ses(item_ts_train[,1], h=h)

checkresiduals(fit.ses)

fc.ses %>% 
  accuracy (item_ts_test[,1])

fc.ses %>% 
  autoplot() + autolayer(item_ts_test[,1])



#Holt
fc.holt <- holt(item_ts_train[,1], h=h)

checkresiduals(fc.holt)

fc.holt %>% 
  accuracy(item_ts_test[,1])

fc.holt %>% 
  autoplot() + autolayer(item_ts_test[,1])



#Holt-Winters
#multiplicative works better
fc.holwinters1 <- hw(item_ts_train[,1],seasonal="multiplicative", h=h)

checkresiduals(fc.holwinters1)

fc.holwinters1  %>% 
  accuracy(item_ts_test[,1])

fc.holwinters1 %>% 
  autoplot() + autolayer(item_ts_test[,1])



#Using ETS function (Exponential smoothing state space model)
fit.ets <- ets(item_ts_train[,1])

checkresiduals(fit.ets)

summary(fit.ets)

forecast(fit.ets, h=h) %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual")
accuracy(forecast(fit.ets, h=4), x=item_ts_test[,1])
#one of best accuracy in test yet!



# Non-seasonal and Seasonal Arima

#Is transformation necessary? (it looks like not)
BoxCox.lambda(item_ts_train[,1]) 

item_ts_train[,1] %>% BoxCox(0.9)%>% autoplot()

#checking how BoxCox stabilizes change in variance along years
#the lambda that sets a straight line is 0.35
# however, this is a multiplicative tseries, so I would expect that transformation won´t help unless it´s mild
item_df %>% 
  select(Period,Sum_Quantity) %>%
  mutate(Sum_Quantity = BoxCox(Sum_Quantity,0.35)) %>% 
  group_by(year(Period)) %>% 
  summarise(Var = var(Sum_Quantity)) %>% 
  ungroup() %>% 
  select(Var) %>%
  ts(start=c(2015,1), frequency = 1) %>% 
  autoplot()


#Unit Root test to find out if differenced data is still not stationary (null hypothesis), 
#and thus needs further differencing
#p-value is just above 0.05, so data IS STILL NOT stationary and may require further differencing

item_ts[,1] %>% 
  diff() %>%
  ur.kpss() %>% 
  summary()

item_ts[,1] %>% 
  diff() %>%
  diff() %>%
  ur.kpss()%>%
  summary()

#However function ndiffs diagnoses only one differencing for non seasonal, and 0 for seasonal
ndiffs(item_ts[,1])
nsdiffs(item_ts[,1])


#Checking Autocorrelation and Partial Autocorrelation to determine where the ARIMA model is (p,d,q)
# With only one differentiation, neither ACF is exponentially decaying, 
# - but PACF does have a spike at lag 1 (although it is ), so it is not conclusive
item_ts[,1] %>% 
  diff() %>%
  #diff() %>%
  ggAcf(lag.max=40)

item_ts[,1] %>% 
  diff() %>%
  #diff() %>%
  ggPacf(lag.max=40)

# It is more or less the same case with differentiation of order 2, 
# but there are important spikes after lag 1 in PACF
item_ts[,1] %>% 
  diff() %>%
  diff() %>%
  ggAcf(lag.max=40)

item_ts[,1] %>% 
  diff() %>%
  diff() %>%
  ggPacf(lag.max=40)

#From previous exploration, we could conclude that 1,1,1 or 1,2,1 would be appropiate
fit.manual_arima <- Arima(item_ts_train[,1], order=c(1,2,1))

summary(fit.manual_arima)

checkresiduals(fit.manual_arima)

fc.manual_arima <- fit.manual_arima %>% forecast(h=2)

fc.manual_arima %>% autoplot() +autolayer(item_ts_test[,1])

fc.manual_arima %>% accuracy(item_ts_test[,1], series="Actual")


# Non-seasonal Arima found by auto.arima has worse AICc (and all other metrics), but its accuracy is MUCH better
fit.auto_arima <- auto.arima(item_ts_train[,1] ,stepwise = FALSE, approximation=FALSE, seasonal= FALSE)

summary(fit.auto_arima)

checkresiduals(fit.auto_arima)

fc.auto_arima <- forecast (fit.auto_arima, h=2) 

fc.auto_arima %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.auto_arima , x=item_ts_test[,1])


#Seasonal Arima

#Trying seasonal differencing
#The ts needs one seasonal differencing, and a second normal differencing to be stationary
item_ts[,1] %>% diff(lag=12) %>% ggtsdisplay(lag.max = 25)
item_ts[,1] %>% diff(lag=12) %>% diff() %>% ggtsdisplay(lag.max = 25)

#Looking at ACF and PACF of seasonally differenced ts 
#Lag 1 has a significant spikes in Lag 1 and 2,
#so that means add a non seasonal MA component of order 1 (PACF shows spikeonly in lag 1) or 2
item_ts[,1] %>% diff(lag=12) %>% diff() %>% ggtsdisplay(lag.max = 25)


#after much iterating, 
#fit.sarima <- item_ts_train[,1] %>% Arima(order=c(1,1,2), seasonal=c(0,1,0)) 
fit.sarima <- item_ts_train[,1] %>% Arima(order=c(0,1,2), seasonal=c(0,1,0)) 

summary(fit.sarima)

checkresiduals(fit.sarima)

fc.sarima <- forecast (fit.sarima, h=2) 

fc.sarima %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual")

accuracy(fc.sarima, x=item_ts_test[,1])


#It certainly looks like seasonal auto.arima performs worst of all
fit.auto_sarima <- auto.arima(item_ts_train[,1] ,stepwise = FALSE, approximation=FALSE, seasonal= TRUE)

summary(fit.auto_sarima)

checkresiduals(fit.auto_sarima)

fc.auto_sarima <- forecast (fit.auto_sarima, h=4) 

fc.auto_sarima %>% autoplot() + autolayer(item_ts_test[,1] , series="Actual") + theme(axis.title.x = element_blank(),
                                                                                      axis.title.y = element_blank())

accuracy(fc.auto_sarima , x=item_ts_test[,1])








#finding out which arima is best using time series cross validation
#because I am not sure when to start cross validation best, we´ll make a loop that will try a few possibilities
auto_arima <- c()
manual_arima <- c()
auto_sarima<- c()
manual_sarima <- c()
h <- 2
steps_back <- 12
initial <- round(nrow(item_ts)-steps_back)
naive_error <- get_naive_cv_comp(item_ts[,1], h=h, initial=initial)
  
#THIS IS AUTO ARIMA
my_arima <- function(x, h){ x %>%  Arima(order=c(0,1,1)) %>% forecast(h=h)}
auto_arima <- tsCV(item_ts[,1], my_arima, h=h, initial=initial) %>% #  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h, naive = naive_error)
#a1 <- append(a1,trial)

#THIS IS MANUAL ARIMA
my_arima <- function(x, h){ x %>%  Arima(order=c(1,2,1)) %>% forecast(h=h)}
manual_arima <- tsCV(item_ts[,1], my_arima, h=h, initial=initial) %>% #  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h, naive = naive_error)
#a2 <- append(a2,trial)

#THIS IS AUTO SEASONAL ARIMA
my_sarima <-function(x, h){ x %>%  Arima(order=c(1,1,0), seasonal=c(0,0,1)) %>% forecast(h=h)}
auto_sarima <- tsCV(item_ts[,1], my_sarima, h=h, initial=initial) %>% # square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h, naive = naive_error)
#sa1 <- append(sa1,trial)

#THIS IS MANUAL SEASONAL ARIMA
my_sarima <-function(x, h){ x %>%  Arima(order=c(0,1,2), seasonal=c(0,1,0)) %>% forecast(h=h)}
manual_sarima <- tsCV(item_ts[,1], my_sarima, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)
#sa2 <- append(sa2,trial)

holt(item_ts_train[,1], h=h)
hw()
ets

my_holt <- function (x,h) {x %>% holt() %>% forecast(h=h)}
holt_model <- tsCV(item_ts[,1], my_holt, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)

my_hw <- function (x,h) {x %>% hw(seasonal = "additive") %>% forecast(h=h)}
holt_winters <- tsCV(item_ts[,1], my_hw, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)

my_ets <- function (x,h) {x %>% ets() %>% forecast(h=h)}
ets_model <- tsCV(item_ts[,1], my_ets, h=h, initial=initial) %>%#  square() %>% mean(na.rm = TRUE)
  get_mase_cv(h=h)
  
#no matter where timeseres starts, first arima model (0,1,1) is the best
model_comparison <- cbind(auto_arima, manual_arima, auto_sarima,manual_sarima, holt_model, holt_winters, ets_model) 

