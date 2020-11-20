library('data.table', quietly = TRUE)
library('ggplot2', quietly = TRUE)
library('dplyr')
library('lubridate', quietly = TRUE)
#https://github.com/owid/covid-19-data/tree/master/public/data
setwd('/Users/aliozan.memetoglu/Downloads')
covid_data <- fread("owid-covid-data.csv", 
              select = c("location", "total_cases", "date"))[location == ('Turkey')]

head(covid_data)


covid_data[,location:=NULL]

fancy_scientific <- function(l) {
  # turn in to character string in scientific notation
  l <- format(l, scientific = TRUE)
  # quote the part before the exponent to keep all the digits
  l <- gsub("^(.*)e", "'\\1'e", l)
  # turn the 'e+' into plotmath format
  l <- gsub("e", "%*%10^", l)
  # return this as an expression
  parse(text=l)
}



ggplot(covid_data, aes(x=date, y=total_cases)) + geom_line() +
  labs(title = "Total covid cases in Turkey", 
       x = "Date",
       y = "# of cases") +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(labels=fancy_scientific) + theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Histogram of the total # of cases in every month.
#first montly plot data 


monthly_plot = covid_data[month(date)!=3 & month(date)!= 11]
ggplot(monthly_plot, aes(x=total_cases)) +
  geom_histogram(aes(y=..density..), colour="lightgreen",fill='white', binwidth = 10000)+ 
  geom_density(alpha=.4, fill="#FF6666") +
  labs(title = "Histograms of the Total Number of Covid Cases in Every Month", 
       x = "Covid Cases",
       y = "Frequency") + scale_x_continuous(labels=fancy_scientific)+ scale_y_continuous(labels=fancy_scientific)   +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(~month(date))

ggplot(monthly_plot, aes(x= as.factor(month(date)), y = total_cases, fill=as.factor(month(date)))) +
  geom_boxplot(aes(group = month(date))) +
  labs(title = "Daily Data Boxplots of the Total Number of Covid Cases in Every Month", 
       x = "Months",
       y = "Total Cases") + 
  theme(legend.position="none") + scale_y_continuous(labels=fancy_scientific) +
  scale_fill_brewer(palette="Dark2")

  #```{r fig.height=10, fig.width=12}

korona_belirtisi <- fread("multiTimeline.csv", select = c("Week", "korona belirtisi: (Turkey)"))
setnames(korona_belirtisi, "korona belirtisi: (Turkey)", "interest")
setnames(korona_belirtisi, "Week", "week")

print(min(as.Date(covid_data$date)))
print(max(as.Date(covid_data$date)))


korona_belirtisi <- korona_belirtisi[week >= ymd('20200401') & week < ymd('20201101')]

ggplot(korona_belirtisi, aes(x= as.factor(month(week)), y = interest, fill=as.factor(month(week)))) +
  geom_boxplot(aes(group = month(week))) +
  labs(title = "Weekly Data Boxplots of the Interest over Search Term - korona belirtisi in Every Month", 
       x = "Months",
       y = "the Interest over Search Term - korona belirtisi") + 
  theme(legend.position="none")  +
  scale_fill_brewer(palette="Dark2")


apple_stock_closing <- fread("AAPL.csv", 
                             select = c("Date", "Close"))[Date >= ymd('20200401') & Date < ymd('20201101')]


ggplot(apple_stock_closing, aes(x=Date, y=Close)) + geom_line() +
  labs(title = "Total covid cases in Turkey", 
       x = "Date",
       y = "# of cases") +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(labels=fancy_scientific) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(apple_stock_closing, aes(x=Close)) +
  geom_histogram(aes(y=..density..), colour="lightgreen",fill='white', bins = 20)+ 
  geom_density(alpha=.4, fill="#FF6666") +
  labs(title = "Histograms of the Apple Stock Closing Price in Every Month", 
       x = "Apple Stock Closing Price",
       y = "Frequency") + scale_x_continuous(labels=fancy_scientific)+ scale_y_continuous(labels=fancy_scientific)   +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(~month(Date))


ggplot(apple_stock_closing, aes(x= as.factor(month(Date)), y = Close, fill=as.factor(month(Date)))) +
  geom_boxplot(aes(group = month(Date))) +
  labs(title = "Daily Data Boxplots of the Closing Price in Every Month", 
       x = "Months",
       y = "Closing Price") + 
  theme(legend.position="none") + scale_y_continuous(labels=fancy_scientific) +
  scale_fill_brewer(palette="Dark2")


iphone <- fread("multiTimeline (1).csv", 
                    select = c("Week", "iphone: (Turkey)"))[Week >= ymd('20200401') & Week < ymd('20201101')]
setnames(iphone, "iphone: (Turkey)", "interest")

ggplot(iphone, aes(x= as.factor(month(Week)), y = interest, fill=as.factor(month(Week)))) +
  geom_boxplot(aes(group = month(Week))) +
  labs(title = "Weekly Data Boxplots of the Interest over Search Term - iphone in Every Month", 
       x = "Months",
       y = "the Interest over Search Term - iphone") + 
  theme(legend.position="none")  +
  scale_fill_brewer(palette="Dark2")

#### delta

delta_closing_price <- fread("DAL.csv", 
                             select = c("Date", "Close"))[Date >= ymd('20200401') & Date < ymd('20201101')]
ggplot(delta_closing_price, aes(x=Date, y=Close)) + geom_line() +
  labs(title = "DAL Stock Closing Prices", 
       x = "Date",
       y = "Closing Prices") +
  scale_x_date(date_breaks = "1 month") +
  scale_y_continuous(labels=fancy_scientific) + theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(delta_closing_price, aes(x=Close)) +
  geom_histogram(aes(y=..density..), colour="lightgreen",fill='white', bins = 20)+ 
  geom_density(alpha=.4, fill="#FF6666") +
  labs(title = "Histograms of the DAL Stock Closing Prices in Every Month", 
       x = "DAL Stock Closing Price",
       y = "Frequency") + scale_x_continuous(labels=fancy_scientific)+ scale_y_continuous(labels=fancy_scientific)   +
  
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  facet_grid(~month(Date))


ggplot(delta_closing_price, aes(x= as.factor(month(Date)), y = Close, fill=as.factor(month(Date)))) +
  geom_boxplot(aes(group = month(Date))) +
  labs(title = "Daily Data Boxplots of the Closing Price in Every Month", 
       x = "Months",
       y = "Closing Price") + 
  theme(legend.position="none") + scale_y_continuous(labels=fancy_scientific) +
  scale_fill_brewer(palette="Dark2")




uçak_biletleri <- fread("multiTimeline (2).csv", 
                        select = c("Week", "uçak biletleri: (Turkey)"))[Week >= ymd('20200401') & Week < ymd('20201101')]
setnames(uçak_biletleri, "uçak biletleri: (Turkey)", "interest")

ggplot(uçak_biletleri, aes(x= as.factor(month(Week)), y = interest, fill=as.factor(month(Week)))) +
  geom_boxplot(aes(group = month(Week))) +
  labs(title = "Weekly Data Boxplots of the Interest over Search Term - uçak biletleri in Every Month", 
       x = "Months",
       y = "the Interest over Search Term - uçak biletleri") + 
  theme(legend.position="none")  +
  scale_fill_brewer(palette="Dark2")










