library(tidyverse)
library(ggplot2)
library(lubridate)
# Read in the coal dataset
ssa <- read_csv("http://594442.youcanlearnit.net/ssadisability.csv")
glimpse(ssa)
ssa_long <- gather(ssa,month,total,-Fiscal_Year)
print(ssa_long,n=10)

#convert variables to lower case
names(ssa_long)<-tolower(names(ssa_long))
unique(ssa_long$month)
#split total and Internet
ssa_long<-separate(ssa_long,month,c('month','application_method'),sep='_')
print(ssa_long,n=5)

#rename the month column - all months down to three characters (ex.August to Aug)
unique(ssa_long$month)
ssa_long$month<-substr(ssa_long$month,1,3) #only keep the first to third characters

#rename the fiscal year
unique(ssa_long$fiscal_year)
ssa_long$fiscal_year <- str_replace(ssa_long$fiscal_year,'FY','20')

#create a new column called date using paste()
#use lubridate library to convert it into date data type
ssa_long$date <- dmy(paste('01',ssa_long$month,ssa_long$fiscal_year))
print(ssa_long,n=5)

#convert fiscal year to calendar year
advanced_date <- which(month(ssa_long$date)>=10)
year(ssa_long$date[advanced_date]) <- year(ssa_long$date[advanced_date]) - 1

#remove the fiscal_year and month variables
summary(ssa_long)
ssa_long$fiscal_year<-NULL
ssa_long$month <- NULL

#change the application_method data type from character to factor
ssa_long$application_method<-as.factor(ssa_long$application_method)
summary(ssa_long)

#widening the dataset
ssa <- spread(ssa_long, application_method, total)
print(ssa, n=10)

#visualizing the data
ssa$online_percentage <- ssa$Internet / ssa$Total * 100

ggplot(data=ssa, aes(date, online_percentage)) +
  geom_point()




















