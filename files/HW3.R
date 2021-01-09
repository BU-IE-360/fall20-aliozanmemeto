library('data.table', quietly = TRUE)
library('ggplot2', quietly = TRUE)
library('dplyr')
library('lubridate', quietly = TRUE)
library('readxl')
library('zoo')
library ('fpp2')
library('ggcorrplot')
library('corrplot')


df <- data.table(read_xlsx('/Users/aliozan.memetoglu/Downloads/EVDS (1).xlsx'))[,c('Date', 'TP DK USD A YTL','TP KTF10', 'TP FG J01', 'TP TG2 Y01')]

colnames(df)

simple_manipulator <- function(someDT){
  
  someDT <- someDT %>% filter(grepl('-', Date))
  someDT[,Date := as.Date(as.yearmon(Date))]
  someDT <- someDT[Date >=  ymd(20120101) & Date <=  ymd(20201201) ]
  someDT[,Date := as.yearmon(Date)]
  setnames(someDT, 'TP DK USD A YTL', 'USD')
  setnames(someDT, 'TP KTF10', 'IR')
  setnames(someDT, 'TP TG2 Y01', 'CCI')
  setnames(someDT, 'TP FG J01', 'CPI')
  return(someDT)
}

df <- simple_manipulator(df)
df[,USD := as.double(USD)]
df[,IR := as.double(IR)]
summary(df)

ggplot(df, aes(x=Date, y=log(CPI))) + geom_line()+
  geom_smooth(method = "lm", color = 'lightgreen')+
  labs(title = "Monthly CPI over 2012-2020", 
       x = "Month",
       y = "CPI") 

ggplot(df, aes(x=Date, y=CCI)) + geom_line()+
  geom_smooth(method = "lm", color = 'lightpink')+
  labs(title = "Monthly CCI over 2012-2020", 
       x = "Month",
       y = "CCI") 

ggplot(df, aes(x=Date, y=USD)) + geom_line()+
  geom_smooth(method = "lm", color = 'lightpink')+
  labs(title = "Monthly IR over 2012-2020", 
       x = "Month",
       y = "USD") 

ggplot(df, aes(x=Date, y=IR)) + geom_line()+
  geom_smooth(method = "lm", color = 'lightpink')+
  labs(title = "Monthly IR over 2012-2020", 
       x = "Month",
       y = "IR") 

ggplot(df, aes(x=Date, y=USD)) + geom_line()+
  geom_smooth(method = "lm", color = 'lightyellow')+
  labs(title = "Monthly USD over 2012-2020", 
       x = "Month",
       y = "USD") 

corrplot(cor(df[,-1]), method= "number")

df_log <- df
df_log[,CPI := log(CPI)]
df_log[,USD := log(USD)]

fit1=lm( CPI ~ USD + CCI + IR, df_log)
summary(fit1)

fit2=lm( CPI ~ USD + CCI, df_log)
summary(fit2)

fit3=lm( CPI ~ USD + IR, df_log)
summary(fit3)

fit4=lm( CPI ~ USD, df_log)
summary(fit4)


df_log[,trend:=1:.N]
fit5=lm( CPI ~ trend, df_log)
summary(fit5)
checkresiduals(fit5)

fitted_res = data.table(residuals = residuals(fit5),fitted = fitted(fit5))
ggplot(fitted_res, aes(x = fitted, y = residuals)) +
  geom_point()



#####

df <- cbind(df, month)
df[,month:=as.factor(month)]

fit6=lm( CPI ~ trend + USD + month, df)
summary(fit6)
checkresiduals(fit6)



month <- seq(1,12,by=1)
df_log <- cbind(df_log, month)
df_log[,month:=as.factor(month)]

######
#####
fit6=lm( CPI ~ trend + USD + month + season + res_lag1 , df_log)
summary(fit6)
checkresiduals(fit6)

season <- c(rep(1,2),rep(2,3), rep(3,3), rep(4,3), 1)
df_log <- cbind(df_log, season)
df_log[,season:=as.factor(season)]


######

df_log[,res_lag1 := lag(residuals(fit6),1)]
df_log[, outlier:=0]
df_log[79, outlier:=1]
df_log[79, 'CPI'] = mean(df_log[80, CPI],df_log[81, CPI],df_log[78, CPI], df_log[77, CPI] )

df_log[80, CPI]

df_log[,outlier := NULL]

fit8=lm( CPI ~ trend + USD + month + season + res_lag1, df_log)
summary(fit8)
checkresiduals(fit8)

fitted_res = data.table(residuals = residuals(fit8),fitted = fitted(fit8))
ggplot(fitted_res, aes(x = fitted, y = residuals)) +
  geom_point()


visual_df = data.table(actual = df_log[2:108,CPI], predicted = fitted(fit8), trend = 1:107)
ggplot()+
  geom_line(data = visual_df, aes(x=trend, y=actual, color = 'orange'))+
  geom_line(data = visual_df, aes(x=trend, y=predicted, color = 'blue'))

fit_final=lm( CPI ~ trend + USD + month + season + res_lag1, df_log[1:107])
dec_2012 <- df_log[108]
dec_2012[,'CPI'] = NA
predict(fit_final, dec_2012)

