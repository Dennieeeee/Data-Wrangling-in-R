library(tidyverse)
library(ggplot2)
library(scales)
install.packages(scale)

url='/Users/dennie/Desktop/NEU/ALY 6070/Module 5/House_Price_data.csv'
data <- read.csv(url)

glimpse(data)
summary(data)
names(data) <- tolower(names(data))

names(data)

head(data[1:5])

hist(data[,'lotarea'])


ggplot(data, aes(x = lotarea)) +
  geom_histogram()

plot(lotarea ~ yearbuilt,
     col = factor(neighborhood),
     data = data[data[,'neighborhood'] %in% c('OldTown', 'Sawyer'),])

legend('topleft',
       legend = c('OldTown', 'Sawyer'),
       col = c('black','red'),
       pch = 1)

#scatter plot
ggplot(data=data[data[,'neighborhood'] %in% c('OldTown', 'Sawyer'),],
       aes(x=yearbuilt,
           y=lotarea,
           color = neighborhood))+
  geom_point()

#house built after 1950
new_house <- filter(data, yearbuilt > 1950)
ggplot(new_house, aes(x = yearbuilt)) +
  geom_histogram()

ggplot(data = data,
       aes(x=lotfrontage, y=lotarea))+
  geom_point()

#remove outlier on lotfrontage
ggplot(data, aes(x = lotfrontage)) +
  geom_boxplot()
subset(data, lotfrontage > 150)
remove <- which(data$lotfrontage>150)
data<-data[-remove,]

#remove outlier on lotarea
ggplot(data, aes(x = lotarea)) +
  geom_boxplot()
subset(data, lotarea >60000)
remove <- which(data$lotarea>60000)
data<-data[-remove,]

#
ggplot(data=data,
       aes(x=yearbuilt, y=saleprice)) +
  geom_point()

ggplot(data=data[data[,'neighborhood'] %in% c('OldTown', 'Sawyer'),],
       aes(x=lotarea,
           y=saleprice,
           color = neighborhood))+
  geom_point()

p3 <- ggplot(data,
             aes(x=neighborhood,
                 y=saleprice)) +
  theme(legend.position = 'top',
        axis.text = element_text(size=6)) #output show nothing; doesn't generate a graph
p3
(p4 <- p3 + geom_point(aes(color = yearbuilt),
                       alpha = 0.5,
                       size = 1.5,
                       position = position_jitter(width = 0.25, height = 0)))

#modify the breaks for x-axis and color sales
p4 + scale_x_discrete(name='neighborhood')+
  scale_color_continuous(name='',
                         breaks=c(1880,1920,1960,2000),
                         labels=c('80','20','60','00'))
#change the colors of low and high values
p4 + scale_x_discrete(name='neighborhood')+
  scale_color_continuous(name='',
                         breaks=c(1880,1920,1960,2000),
                         labels=c('80','20','60','00'),
                         low='red',high='blue')

p5 <- ggplot(data, aes(x=yearbuilt, y=saleprice))
p5 + geom_line(aes(color = neighborhood))
(p5 <- p5+geom_line()+
    facet_wrap(~neighborhood, ncol=10,scales='free_y',
               as.table = TRUE))






