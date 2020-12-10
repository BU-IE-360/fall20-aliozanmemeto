library('data.table', quietly = TRUE)
library('ggplot2', quietly = TRUE)
library('dplyr')
library('lubridate', quietly = TRUE)
library('readxl')
library('zoo')
library ('fpp2')
library('ggcorrplot')
number_of_employed <- data.table(read_xlsx('/Users/aliozan.memetoglu/Downloads/İstihdam edilenler.xlsx'))[,c('Tarih', 'TP TIG03')]
number_of_exported <- data.table(read_xlsx("/Users/aliozan.memetoglu/Downloads/Genel ticaret sistemine göre dış ticaret ekonomik faaliyetlere göre ihracat.xlsx"))[,c('Tarih','TP IHRISICREV4 TT')]
industrial_production_index <- data.table(read_xlsx('/Users/aliozan.memetoglu/Downloads/Sanayi Üretim Endeksi.xlsx' ))[,c('Tarih', 'TP SANAYREV4 Y1')]


colname_filterdate <- function(someDT,old_colname,new_colname){
  
  setnames(someDT, 'Tarih', 'date')
  someDT <- someDT %>% filter(grepl('-', date))
  
  setnames(someDT, old_colname, new_colname)
  someDT[,date := as.Date(as.yearmon(date))]
  someDT <- someDT[date >=  ymd(20130101) & date <  ymd(20200101) ]
  
  someDT$month <- format( someDT$date, "%m")
  someDT$year <- format( someDT$date,"%Y")
  someDT$day <- as.numeric( format(someDT$date, "%d"))
  # because there is no forecasting in this study, no further time tag extraction is applied
  
  someDT[,data:=as.numeric(data)]
  
  return(someDT)
}


number_of_employed <- colname_filterdate(number_of_employed, 'TP TIG03', 'data')
number_of_exported <- colname_filterdate(number_of_exported, 'TP IHRISICREV4 TT', 'data')
industrial_production_index <- colname_filterdate(industrial_production_index, 'TP SANAYREV4 Y1', 'data')


ggplot(industrial_production_index, aes(x=date, y=data)) +
  geom_line()+ geom_smooth(method = "lm", color = 'lightblue')+
  ylim(50, 150)+
  labs(title = "Monthly Industrial Production Index over 2013-2019", 
       x = "IPI",
       y = "Month of Year") 


ggplot(number_of_employed, aes(x=date, y=data)) + geom_line()+
  geom_smooth(method = "lm", color = 'lightgreen')+
  ylim(23000, 30000)+
  labs(title = "Monthly Number of Employed People over 2013-2019", 
       x = "Number of Employed People",
       y = "Month of Year") 

  
ggplot(number_of_exported, aes(x=date, y=data)) + 
  geom_line() +
  geom_smooth(method = "lm", color = 'lightpink') +
  ylim(9900000, 17000000)+
  labs(title = "Monthly Number of Exported Products over 2013-2019", 
       x = "Number of Exported Products",
       y = "Month of Year") 

#+facet_wrap( ~ year, scales = "free_x")

number_of_employed_seasonal <- ts(number_of_employed[,"data"],frequency=12, start=c(2013,1))
industrial_production_index_seasonal <- ts(industrial_production_index[,"data"],frequency=12, start=c(2013,1))
number_of_exported_seasonal <- ts(number_of_exported[,"data"],frequency=12, start=c(2013,1))


autoplot(Acf(number_of_exported_seasonal,plot=FALSE))+
  ggtitle("Autocorrelation Function of Fed Funds Rate")



ggseasonplot(number_of_employed_seasonal, year.labels=TRUE, year.labels.left=TRUE) +
  theme(plot.title = element_text(hjust = 0.5)) +  # to center the plot title
  ylab("Number of Employed People by Year") +
  ggtitle("Monthly Number of Employed People over 2013-2019")


ggplot(number_of_employed, aes(x=data)) +
  geom_histogram(aes(y=..density..), colour="lightgreen",fill='white', bins = 50)+ 
  geom_density(alpha=.4, fill="#FF6666") +
  labs(title = "Annually Distributions of Number of Employed People in Every Month", 
       x = "Number of Employed People",
       y = "Frequency")  +
  scale_y_continuous()+
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~year, ncol = 3)


ggplot(industrial_production_index, aes(x=data)) +
  geom_histogram(aes(y=..density..), colour="lightblue",fill='white', bins = 50)+ 
  geom_density(alpha=.4, fill="#FF6666") +
  labs(title = "Annually Distributions of IPI in Every Month", 
       x = "IPI",
       y = "Frequency")  +
  scale_y_continuous()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~year, ncol = 3)


ggplot(number_of_exported, aes(x=data)) +
  geom_histogram(aes(y=..density..), colour="lightpink",fill='white', bins = 50)+ 
  geom_density(alpha=.4, fill="#FF6666") +
  labs(title = "Annually Distributions of Exported Goods in Every Month", 
       x = "Number of Exported Goods",
       y = "Frequency")  +
  scale_y_continuous()+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_wrap(~year, ncol = 3)




total_dataset <- cbind(employed = number_of_employed$data, 
                            ipi = industrial_production_index$data,
                            exported = number_of_exported$data)

ggcorrplot(cor(total_dataset), hc.order = TRUE,
           lab = TRUE)



print(cor.test(total_dataset[,'employed'], total_dataset[,'ipi']))

total_dataset <- data.table(employed = number_of_employed$data, 
                            ipi = industrial_production_index$data,
                            exported = number_of_exported$data,
                            date = number_of_exported$date)

cor_emp_ipi <- total_dataset[,.(cor_emp_ipi = cor(employed, ipi)),by=year(date)]
cor_emp_exp <- total_dataset[,.(cor_emp_exp = cor(employed, exported)),by=year(date)]
cor_exp_ipi <- total_dataset[,.(cor_exp_ipi = cor(exported, ipi)),by=year(date)]

ggplot(cor_emp_ipi, aes(x=year, y=cor_emp_ipi)) + 
  geom_point(aes(size=cor_emp_ipi)) +
  labs(title = "Annual Correlations Between Number of Employed and IPI", 
       x = "Year",
       y = "Correlation")

ggplot(cor_emp_exp, aes(x=year, y=cor_emp_exp)) + 
  geom_point(aes(size=cor_emp_exp)) +
  labs(title = "Annual Correlations Between Number of Employed and Exported Goods", 
       x = "Year",
       y = "Correlation")

ggplot(cor_exp_ipi, aes(x=year, y=cor_exp_ipi)) + 
  geom_point(aes(size=cor_exp_ipi)) +
  labs(title = "Annual Correlations Between Exported Goods and IPI", 
       x = "Year",
       y = "Correlation")


