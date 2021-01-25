require(data.table)
require(lubridate)
require(ggplot2)
require(urca)
library(stats)
setwd('Downloads')
dt = fread('RealTimeConsumption-01012017-22012021.csv')
str(dt)

# The structure of the data is not really good, we should convert char type date hour to a better format like timestamp and char type consumption to a numeric type.
dt[,Date:=as.Date(Date,"%d.%m.%Y")]
dt[,timestamp := ymd_hm(paste(Date, Hour))]
dt[,Hour := NULL]
setnames(dt, 'Consumption (MWh)', 'Consumption')
dt[,Consumption:=as.double(gsub(",", "",Consumption))]

#Since making predictions on an hourly basis is difficult and it is hard to capture hourly fluctuations and those fluctuations might affect our model, we will use daily mean comsumption values.

daily_mean_consuption = dt[,.(Consumption = mean(Consumption)),by=.(Date)] 
daily_mean_consuption_train <- daily_mean_consuption[Date < as.Date('2021-01-09')]
daily_mean_consuption_valid <- daily_mean_consuption[Date >= as.Date('2021-01-09')]

ggplot(data = daily_mean_consuption, aes(x = Date, y = Consumption)) + geom_line()
# Although there is no significant trend, we can observe the yearly seasonality. We don't really need to do any BoxCox or log transformation because although mean is changing variation seems to be constant.

summary(ur.kpss(daily_mean_consuption$Consumption))
# Summary of KPSS unit root test shows there is no significant evidence to reject the null hypothesis stating series is stationary.
# So although the test states the data might be stationary, value of test statistics is very close to critical values, so we should not be satisfied with that since we can observe an obvious seasonality.

mean_consumption = ts(daily_mean_consuption_train$Consumption, freq = 7)
ts_decomposed = decompose(mean_consumption)
plot(ts_decomposed)
# Apparently outliers deteriorate the random series left, so we can build a simple model to find outliers then impute a reasonable value such as the value from last week so that we will left with more stationary residuals.

#Outlier Analysis
daily_mean_consuption_train[,wok:=as.factor(weekdays(Date))]
daily_mean_consuption_train[,month:=as.factor(month(Date))]
daily_mean_consuption_train[,trend:=1:.N]
fit_outlier = lm(Consumption ~ wok + month + trend,daily_mean_consuption_train)
summary(fit_outlier)
plot(daily_mean_consuption_train[,.(Consumption, residuals= residuals(fit_outlier))])
# Apparently it would be better if we fit a autoregressive model to find outliers
acf(daily_mean_consuption_train$Consumption)
pacf(daily_mean_consuption_train$Consumption)
pacf(residuals(fit_outlier))
# A model with lag 1, 2 and 7 might work fine
daily_mean_consuption_train[,lag_1:=shift(Consumption,1)]
daily_mean_consuption_train[,lag_2:=shift(Consumption,2)]
daily_mean_consuption_train[,lag_7:=shift(Consumption,7)]

fit_outlier_2 = lm(Consumption ~ wok + month + lag_1 + lag_2+ lag_7,daily_mean_consuption_train)
summary(fit_outlier_2)

# We have a pretty good model with adjusted r-squared value 0.9186, we can make use of this model to find outlier
daily_mean_consuption_train[,residual_armodel:=c(NA,NA,NA,NA,NA,NA,NA,fit_outlier_2$residuals)]

# Due to the fact we have lag_7, first 7 predictions will be NUll
daily_mean_consuption_train[,quant5:=quantile(residual_armodel,0.05, na.rm = TRUE)]
daily_mean_consuption_train[,quant95:=quantile(residual_armodel,0.95, na.rm = TRUE)]
daily_mean_consuption_train[residual_armodel>quant95]
# When we check the first date 2017-05-02 is the date after international workers' day, the second date 2017-05-20 is the day after the youth and spots day and the third date 2017-06-28 is the day after the starting day of ramadan. It is reasonable to have excessive consumption in those days.
daily_mean_consuption_train[,outlier_large:= as.numeric(residual_armodel>quant95)]
daily_mean_consuption_train[,outlier_small:= as.numeric(residual_armodel<quant5)]

# To interpolate those days, I will use the last weeks value, altough this might not be reasonable to some extent due to the fact that there might be some consecutive outlier weekdays. So we will go twice week back for those days
special_days = daily_mean_consuption_train[outlier_large == 1 | outlier_small == 1 , Date]

normal_outlier_days = daily_mean_consuption_train[Date %in% (special_days - 7) & outlier_large != 1 & outlier_small != 1, Date+7 ]
double_outlier_days = daily_mean_consuption_train[Date %in% (special_days - 7) & (outlier_large == 1 | outlier_small == 1), Date+7  ]

daily_mean_consuption_train[Date %in% normal_outlier_days, Consumption:=(daily_mean_consuption_train[Date %in% (normal_outlier_days-7), Consumption]) ]
daily_mean_consuption_train[Date %in% double_outlier_days, Consumption:=(daily_mean_consuption_train[Date %in% (double_outlier_days-14), Consumption]) ]

daily_mean_consuption[Date %in% normal_outlier_days, Consumption:=(daily_mean_consuption[Date %in% (normal_outlier_days-7), Consumption]) ]
daily_mean_consuption[Date %in% double_outlier_days, Consumption:=(daily_mean_consuption[Date %in% (double_outlier_days-14), Consumption]) ]


# We can try decomposition now

mean_consumption_ts = ts(daily_mean_consuption_train$Consumption, freq = 7)
ts_decomposed = decompose(mean_consumption_ts)
plot(ts_decomposed)


# Although there are still some outliers, new random series is definitely better than the previous one

ndiffs(mean_consumption_ts)
nsdiffs(mean_consumption_ts)

# We need one seasonal differencing

autoplot(diff(mean_consumption_ts, 7))
autoplot(diff(mean_consumption, 7))

# Although we managed to remove most of them, there are still some outliers remaining. But we can be content with the one we achieved so far.

summary(ur.kpss(diff(mean_consumption_ts, 7)))

# It does not really matter if we take the difference now, because we can take the difference while building the ARIMA model. 

random = ts_decomposed$random
model_1 = auto.arima(random)
auto.arima(random, trace = TRUE)

# We have an ARIMA model with seasonal differencing 7 days and one day lag moving average component as expected.We an AIC corrected value of 24083.54 which can be improved with a better model and thorough outlier analysis.
trend= as.numeric(rep(tail(ts_decomposed$trend[!is.na(ts_decomposed$trend)],1), 14)) 
season= as.numeric(tail(ts_decomposed$seasonal, 14))

results_1 = predict(model_1, n.ahead = 14)$pred + trend + season

daily_mean_consuption_valid[,arima_1 := results_1]

accu = function(actual, forecast){
  n = length(actual)
  error = actual - forecast
  mean = mean(actual)
  sd = sd(actual)
  FBias = sum(error)/sum(actual)
  MPE = sum(error/actual)/n
  MAPE = sum(abs(error/actual))/n
  RMSE = sqrt(sum(error^2)/n)
  MAD = sum(abs(error))/n
  WMAPE = MAD/mean
  return(data.frame(n,sd,FBias,MPE,MAPE,RMSE,MAD,WMAPE)) 
}

daily_mean_consuption_valid[,accu(Consumption, arima_1)]
# In performance measure, it is better to compare WMAPE in our electricity consumption data because consumption ranges between large and small values. It is not really reasonable to penalize the model when it is low actual values as it is the case in MAPE.


#Sliding window model

test_start = as.Date('2021-01-09')
daily_report = data.frame(n = as.numeric(),sd= as.numeric(),FBias= as.numeric(),MPE= as.numeric(),MAPE= as.numeric(),RMSE= as.numeric(),MAD= as.numeric(),WMAPE= as.numeric())
results
for(i in 0:13){
  
  current_test_date = test_start + i
  train_data = daily_mean_consuption[Date < current_test_date]
  rand = decompose(ts(train_data$Consumption, freq = 7))$random
  fitday = auto.arima(rand)
  #results[i+1] = as.numeric(predict(fitday, n.ahead = 1)$pred)
  daily_report = rbind(daily_report,accu(daily_mean_consuption_valid[i+1,Consumption],as.numeric(predict(fitday, n.ahead = 1)$pred + trend[1]+season[i+1])))
}
daily_report

# Because we have only one variable, sd values are NA. One important remark we can make is that our bias is always positive except the first prediction. So our model is underpredicting.


results_2 = as.numeric(results) + trend + season
daily_mean_consuption_valid[,arima_2_sw := results_2]
daily_mean_consuption_valid[,accu(Consumption, arima_2_sw)]

# Although the difference is not significant, we came up with a better model. We can also add a naive base model to discuss validation of our models better.
daily_mean_consuption_valid[,naive:= tail(daily_mean_consuption_train$Consumption,14)]
daily_mean_consuption_valid[,accu(Consumption, naive)]


#Conclusion








