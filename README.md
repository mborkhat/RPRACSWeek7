# RPRACSWeek7
install.packages("quantmod")
install.packages("xlsxjars")

library(quantmod)
library(xlsx)

start = as.Date('2018-01-01')
end = as.Date("2018-08-31")

getSymbols("AAPL",src="yahoo", from=start, to=end)
plot(AAPL[, 'AAPL.Close'],main="Apple")

candleChart(AAPL, up.col="green", dn.col="red", theme="white")


getSymbols(c('MSFT','AAPL','GOOG','TSLA'), from=start)

stocks = as.xts(data.frame(AAPL=AAPL[,'AAPL.Close'], MSFT=MSFT[,'MSFT.Close'],
                           GOOG=GOOG[,'GOOG.Close'], TSLA=TSLA[,'TSLA.Close']))
plot(as.zoo(stocks), screen=1, lty=1:4, xlab='Data', ylab='Price', col=1:4)
plot(as.zoo(stocks), lty=1:4, xlab='Data', ylab='Price', col=1:4)
legend('right',c('AAPL','MSFT','GOOG','TSLA'), lty=1:4, cex=0.5)

par(new=T)
par(mfrow=c(1,1))

a = allReturns(GOOG)
GOOG$diff = diff(GOOG$GOOG.Close)
q1 = index(stocks[which.max(stocks$MSFT.Close)])

cor(stocks)

index(MSFT)

MSFT$Date = as.character(index(MSFT))
write.csv(MSFT, '123.csv')

library(ggplot2)
titanic = read.csv("C:/Users/hungu/Documents/MTech DS Docs/R Programming/titanic.csv", stringsAsFactors = F)
View(titanic)

titanic$Pclass = as.factor(titanic$Pclass)
titanic$Survived = as.factor(titanic$Survived)
titanic$Sex = as.factor(titanic$Sex)
titanic$Embarked = as.factor(titanic$Embarked)

ggplot(titanic, aes(x=Survived)) + geom_bar()

prop.table(table(titanic$Survived))

summary(titanic$Survived)
table(titanic$Survived) # specific for factors

ggplot(titanic, aes(x=Survived)) +
  theme_bw() + geom_bar() + labs(y = "Passenger Count",
                                 title="Titanic Survival Rate")
ggplot(titanic, aes(x=Sex, fill=Survived)) +
  theme_bw() + geom_bar() + labs(y = "Passenger Count",
                                 title="Titanic Survival Rate by Sex")

ggplot(titanic, aes(x=Sex, fill=Pclass)) +
  theme_bw() + geom_bar() + labs(y = "Passenger Count",
                                 title="Titanic Pclass Rate by Sex")

ggplot(titanic, aes(x=Pclass, fill=Survived)) +
  theme_bw() + geom_bar() + labs(y = "Passenger Count",
                                 title="Titanic Pclass Rate by Sex")

ggplot(titanic, aes(x=Sex, fill=Survived)) +
  theme_bw() + facet_wrap(~ Pclass) + 
  geom_bar() + labs(y = "Passenger Count",
                                 title="Titanic Survived Rate by Sex")

ggplot(titanic, aes(x=Age)) +
  theme_bw() +
  geom_histogram(binwidth = 5) + labs(y = "Passenger Count",
                                      x = "Age (Binwidth=5)",
                    title="Titanic Age Distribution")

ggplot(titanic, aes(x=Age, fill=Survived)) +
  theme_bw() +
  geom_histogram(binwidth = 5) + labs(y = "Passenger Count",
                                      x = "Age (Binwidth=5)",
                                      title="Titanic Age Distribution")

ggplot(titanic, aes(x=Survived, y=Age)) +
  theme_bw() +
  geom_boxplot() + labs(y = "Age Dist",
                                      x = "Survived",
                                      title="Titanic Age Distribution")

ggplot(titanic, aes(x=Age, fill=Survived)) +
  theme_bw() + facet_wrap(Sex ~ Pclass) +
  geom_density(alpha = 0.5) + labs(y = "Age Dist",
                        x = "Survived",
                        title="Titanic Survived rate by Age, Pclass, and Sex")

ggplot(titanic, aes(x=Age, fill=Survived)) +
  theme_bw() + facet_wrap(Sex ~ Pclass) +
  geom_histogram(binwidth = 5) + labs(x = "Age",
                                title="Titanic Survived rate by Age, Pclass, and Sex")

#-------------------------------------------------------------------------------------#

install.packages("quantmod")
install.packages("xlsxjars")

library(quantmod)
library(xlsx)

start = as.Date('2018-01-01')
end = as.Date("2018-08-31")

getSymbols("AAPL",src="yahoo", from=start, to=end)
plot(AAPL[, 'AAPL.Close'],main="Apple")

candleChart(AAPL, up.col="green", dn.col="red", theme="white")


getSymbols(c('MSFT','AAPL','GOOG','TSLA'), from=start)

stocks = as.xts(data.frame(AAPL=AAPL[,'AAPL.Close'], MSFT=MSFT[,'MSFT.Close'],
                           GOOG=GOOG[,'GOOG.Close'], TSLA=TSLA[,'TSLA.Close']))
plot(as.zoo(stocks), screen=1, lty=1:4, xlab='Data', ylab='Price', col=1:4)
plot(as.zoo(stocks), lty=1:4, xlab='Data', ylab='Price', col=1:4)
legend('right',c('AAPL','MSFT','GOOG','TSLA'), lty=1:4, cex=0.5)

par(new=T)
par(mfrow=c(1,1))

a = allReturns(GOOG)
GOOG$diff = diff(GOOG$GOOG.Close)
q1 = index(stocks[which.max(stocks$MSFT.Close)])

cor(stocks)

index(MSFT)

MSFT$Date = as.character(index(MSFT))
write.csv(MSFT, '123.csv')


library(ggplot2)
library(dplyr)

setwd('C:/Users/hungu/Documents/MTech DS Docs/R Programming')

df.car_torque = read.csv('car_torque.csv')
df.car_top_speed = read.csv('car_top_speed.csv')
df.car_power_to_weight = read.csv('car_power_to_weight.csv')
df.car_horsepower = read.csv('car_horsepower.csv')
df.car_engine_size = read.csv('car_engine_size.csv')
df.car_0_60_time = read.csv('car_0_60_time.csv')

df.car_torque %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_top_speed %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_power_to_weight %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_horsepower %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_engine_size %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
df.car_0_60_time %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)


df.car_torque = distinct(df.car_torque, car_full_nm, .keep_all = T)
df.car_top_speed = distinct(df.car_top_speed, car_full_nm, .keep_all = T)
df.car_power_to_weight = distinct(df.car_power_to_weight, car_full_nm, .keep_all = T)
df.car_horsepower = distinct(df.car_horsepower, car_full_nm, .keep_all = T)
df.car_engine_size = distinct(df.car_engine_size, car_full_nm, .keep_all = T)
df.car_0_60_time = distinct(df.car_0_60_time, car_full_nm, .keep_all = T)

df.car_spec_data = left_join(df.car_horsepower, df.car_torque, by="car_full_nm")
df.car_spec_data = left_join(df.car_spec_data, df.car_top_speed, by="car_full_nm")
df.car_spec_data = left_join(df.car_spec_data, df.car_power_to_weight, by="car_full_nm")
df.car_spec_data = left_join(df.car_spec_data, df.car_engine_size, by="car_full_nm")
df.car_spec_data = left_join(df.car_spec_data, df.car_0_60_time, by="car_full_nm")

df.car_spec_data %>% group_by(car_full_nm) %>% summarise(count=n()) %>% filter(count!=1)
summary(df.car_spec_data)

df.car_spec_data = mutate(df.car_spec_data, year=sub(".*\\[([(0-9)]{4})\\]","\\1", car_full_nm))

df.car_spec_data = mutate(df.car_spec_data,
                          decade = paste(substring(df.car_spec_data$year,1,3),"0s", sep=""))

df.car_spec_data = mutate(df.car_spec_data, make_nm = gsub(" .*$","",df.car_spec_data$car_full_nm))

df.car_spec_data = mutate(df.car_spec_data, car_weight_tons = horsepower_bhp / horsepower_per_ton_bhp)

df.car_spec_data = mutate(df.car_spec_data, torque_per_ton = torque_lb_ft / car_weight_tons)

df.car_spec_data %>% group_by(decade) %>% summarise(count = n())

df.car_spec_data %>%
  group_by(make_nm) %>%
  summarise(make_count = length(make_nm)) %>%
  arrange(desc(make_count))

ggplot(data = df.car_spec_data,
       aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.4, size=4, color='blue') +
  ggtitle("Horsepower vs Top Speed") +
  labs(x="Horsepower, bhp", y="Top speed, \n mph")

ggplot(data = df.car_spec_data,
       aes(x=top_speed_mph)) +
  geom_histogram(fill='blue') +
  ggtitle("Histogram of top speed") +
  labs(x="Top Speed, mph", y="Count \n of Records")

df.car_spec_data %>% filter(top_speed_mph > 149 & top_speed_mph<159) %>%
  ggplot(aes(x=as.factor(top_speed_mph))) +
  geom_bar(fill='red')+
  labs(x = "Top Speed mph")

ggplot(data = df.car_spec_data,
       aes(x=top_speed_mph)) +
  geom_histogram(fill='blue') +
  ggtitle("Histogram of top speed") +
  labs(x="Top Speed, mph", y="Count \n of Records") + 
  facet_wrap(~decade)

df.car_spec_data %>% 
  filter(top_speed_mph == 155 & year>=1990) %>%
  group_by(make_nm) %>%
  summarize(count_speed_controlled =n()) %>%
  arrange(desc(count_speed_controlled))

ggplot(data = df.car_spec_data,
       aes(x=horsepower_bhp, y=top_speed_mph)) +
  geom_point(alpha=.4, size=4, color='blue') +
  ggtitle("Horsepower vs Top Speed") +
  labs(x="Horsepower, bhp", y="Top speed, \n mph") +
  facet_wrap(~decade)

df.car_spec_data %>% 
  group_by(year) %>%
  summarise(max_speed = max(top_speed_mph, na.rm=T)) %>%
  ggplot(aes(x=year, y=max_speed, group=1)) + 
  geom_point(size=5, alpha=.8, color='red') +
  stat_smooth(method = "auto", size=1.5) +
  scale_x_discrete(breaks = c("1950","1960","1970","1980","1990","2000","2010")) +
  ggtitle("Speed of year's \n Fastest Car by year") + 
  labs(x="Year",y="Top Speed \n (Fastest Car)")

df.car_spec_data %>%
  select(car_full_nm, top_speed_mph) %>%
  filter(min_rank(desc(top_speed_mph))<=10) %>%
  arrange(desc(top_speed_mph))%>%
  ggplot(aes(x=reorder(car_full_nm, top_speed_mph), y=top_speed_mph)) +
  geom_bar(stat="identity", fill="red")+
  coord_flip()+
  ggtitle("Top 10 Fastest Car (through 2012)") + 
  labs(x="",y="")+
  theme(axis.text.y = element_text(size=rel(1.5)))+
  theme(plot.title = element_text(hjust=1))

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=car_0_60_time_seconds)) +
  geom_point()

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=car_0_60_time_seconds)) +
  geom_point(position = "jitter")

ggplot(data=df.car_spec_data, aes(x=horsepower_bhp, y=car_0_60_time_seconds)) +
  geom_point(position = "jitter", size=4, alpha=.7,color='red')+
  stat_smooth(method="auto", size=1.5)
  ggtitle("0 to 60 times by Horsepower") + 
  labs(x="Horsepower, bhp",y="0-60 time\nseconds")
