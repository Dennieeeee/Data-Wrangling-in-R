install.packages('tidyverse')
install.packages('ggplot2')
library('ggplot2')
library(tidyverse)

log(42/7.3)
floor(5.44)
ceiling(5.44)
y<-c(1,2,3,4)
is.numeric(y) #true
x<-seq(1,10,2)
x
class(x)

r<-list(c(1,2,3),'t')
r[1] #1,2,3
r[2] #t
r[[1]][3] #3


str<-c('a','b','c')
class(str)

rep(9,4)
rep(1:4,7)

mean(c(1,2,3))
median(c(1,2,3))
max(c(1,2,3,3))
min(c(1,2,3))
sum(c(1,2,3))
std(c(1,2,55,3,55))
TRUE > FALSE # 1>0; true is 1, false is 0
TRUE == FALSE

rnorm(n=100, mean=0, sd=1)
hist(rnorm(n=100, mean=0, sd=1), density = TRUE)

rm(list=ls())
(function(x,y){z<-x^2+y^2; x+y+z})(0:7,1)

path<-'/Users/dennie/Desktop/KPMG virtual internship/KPMG_new_data.csv'
data<-read.csv(path)
names(data)
summary(data)
names(data)<-tolower(names(data))#change all variables to lower case
head(data)
tail(data)
data[1:3,1:3]#first 3 columns, first three rows
data[1:3,-1] #skip the first column
data[1:3,'state']
data[1:3,c('state','year')]
#subset data
data2<-data[data['year']>2000, c('state','country','year')]
data2
#frequency table
table(data[,'year'],data[,'state'])
#proportion table, margin = 1 is row, 2 is column
prop.table(table(data[,'year'],data[,'state']),margin=1) 
#aggregate
aggregate(data=data, list_price~year+state,FUN=max)
aggregate(data=data, list_price~year+state,
          FUN=function(x) {min(x)+max(x)})
glimpse(data)
#histogram
ggplot(data=data) +
  geom_histogram(mapping=aes(x=list_price)) +
  coord_cartesian(ylim = c(0,1000),xlim = c(0,2500))
#boxplot
ggplot(data=data) +
  geom_boxplot(mapping=aes(brand,list_price))

#filter to list price greater than 2000
summary(data$list_price)
highPrice<-filter(data, list_price > 2000)
unique(data$customer_id)

ggplot(data=highPrice) +
  geom_point(mapping = aes(brand,list_price))+
  theme(axis.text.x = element_text(angle = 45,vjust = 1,hjust = 1))
#identify null values
no_cost <- which(is.na(data$standard_cost))
inspections[no_cost,] 

