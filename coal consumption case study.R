coal <- read_csv("http://594442.youcanlearnit.net/coal.csv",skip=2)
glimpse(coal)
names(coal)
colnames(coal)[1]<-'regions'
summary(coal)

#convert from wide to long
coal_long<-gather(coal,year,coal_consumption,-regions)
glimpse(coal_long)

#convert the data type
coal_long$year<-as.integer(coal_long$year)
coal_long$coal_consumption<- as.numeric(coal_long$coal_consumption)
summary(coal_long)

#segmenting the dataset
###separate the continents and countries from the region column
unique(coal_long$regions)
noncountries<-c("Middle East",'North America', "Central & South America", "Antarctica", "Europe", "Eurasia",
                "Africa", "Asia & Oceania", "World")
match <- which(!is.na(match(coal_long$regions,noncountries)))
coal_country<-coal_long[-match,]          
coal_region<-coal_long[-match,]

#visualization
library(tidyverse)
library(ggplot2)
ggplot(data=coal_region, mapping=aes(x=year,y=coal_consumption)) +
  geom_point()

ggplot(data=coal_region, mapping=aes(x=year, y=coal_consumption)) +
  geom_line()  
