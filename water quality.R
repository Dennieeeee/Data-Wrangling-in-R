library(tidyverse)
library(ggplot2)
library(lubridate)
# Read in the dataset
water <- read_csv('http://594442.youcanlearnit.net/austinwater.csv')
summary(water)
#convert to lower case
names(water)<-tolower(names(water))
#rename variables
glimpse(water)

water <- tibble('siteName'=water$site_name,
                'siteType'=water$site_type,
                'sampleTime'=water$sample_date,
                'parameterType'=water$param_type,
                'parameter'=water$parameter,
                'result'=water$result,
                'unit'=water$unit)

unique(water$parameter)

unique(water[which(str_detect(water$parameter,'PH')),]$parameter)

filter_water<-subset(water,(parameterType=='Alkalinity/Hardness/pH') |
                       parameterType=='Conventionals')

filter_water<-subset(filter_water, ((parameter=='PH') | (parameter=='WATER TEMPERATURE')))

#convert the data type
summary(filter_water)
filter_water$siteType<-as.factor(filter_water$siteType)
filter_water$parameter<-as.factor(filter_water$parameter)
filter_water$parameterType<-as.factor(filter_water$parameterType)
filter_water$unit<-as.factor(filter_water$unit)
filter_water$sampleTime<-mdy_hms(filter_water$sampleTime) #ymd_hms

#other data entry errors
#check which row in the unit column has feet
which(filter_water$unit=='Feet')
#look at the row
subset(filter_water, unit='Feet')
#change Feet to Deg. Fahrenheit
convert<-which(filter_water$unit=='Feet')
convert$unit
filter_water$unit[convert]
filter_water$unit[convert]<-'Deg. Fahrenheit'

#MG/L
subset(filter_water,unit=='MG/L')
subset(filter_water,unit=='MG/L' & parameter=='PH')
convert<-which(filter_water$unit=='MG/L' & filter_water$parameter=='PH')
filter_water$unit[convert]<-'Standard units'
summary(filter_water)

subset(filter_water,unit=='MG/L')
subset(filter_water,unit=='MG/L' & filter_water$result>70)
convert <- which(filter_water$unit=='MG/L' & filter_water$result>70)
filter_water$unit[convert] <- 'Deg. Fahrenheit'

subset(filter_water,unit=='MG/L')
convert <- which(filter_water$unit=='MG/L')
filter_water$unit[convert] <- 'Deg. Celsius'

#look at result
ggplot(data=filter_water,mapping = (aes(x=sampleTime,y=result))) +
  geom_point()

subset(filter_water, result > 100000)
remove <- which(filter_water$result > 100000 |
                  is.na(filter_water$result)) #remove result higher than 100,000 and a null
filter_water <- filter_water[-remove,]

subset(filter_water, result>1000)
remove <- which(filter_water$result>1000)
filter_water<- filter_water[-remove,]

#visualize the result; two points in the Celsius should be recorded as Fahrenheit                        
ggplot(data=filter_water, mapping = (aes(x=unit, y=result))) +
  geom_boxplot()                  

convert <- which(filter_water$result>60 & filter_water$unit=='Deg. Celsius')
filter_water$unit[convert] <- 'Deg. Fahrenheit'                  

#convert Fahrenheit to Celsius
fahrenheit <- which(filter_water$unit=='Deg. Fahrenheit')
filter_water$result[fahrenheit] <- (filter_water$result[fahrenheit] - 32) * (5.0/9.0)
#converting the Fahrenheit to Celsius didn't change the units, so let's change it
filter_water$unit[fahrenheit] <- 'Deg. Celsius'

#remove levels that have empty values in the unit variable
filter_water$unit <- droplevels(filter_water$unit)
summary(filter_water)

#remove the unit and parameterType columns
filter_water <- filter_water[,-c(4,7)]

#widening the filter_water data
filter_water_wide <- spread(filter_water,parameter,result)
filter_water[c(49274, 49342, 49219, 49284,49409, 49413),] #look into the duplicates
#the duplicate is different result was recorded at the same time, the system doesn't which
#result to take when widening

#create a table except the result column
dup_check <-filter_water[,-5]
#contrains all the duplicated values
dupes<-which(duplicated(dup_check))
#exclude the dupes table from filter_water table
filter_water <- filter_water[-dupes,]

#widening the filter_water data
filter_water_wide <- spread(filter_water,parameter,result)

glimpse(filter_water_wide)

#rename the PH and water temperature columns
colnames(filter_water_wide)[4] <- 'pH'
colnames(filter_water_wide)[5] <- 'temperature'

glimpse(filter_water_wide) #this table is ready for analysis