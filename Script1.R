library(dplyr)
library(spatstat)
library(sp)
library(rgeos)
library(maptools)
library(GISTools)
library(tmap)
library(sf)
library(geojson)
library(geojsonio)
library(tmaptools)
library(leafpop)
library(leaflet)
library(tmap)
library(tmaptools)
library(tidyverse)
library(tidypredict)

rm(Population)


Population <- HKdistrict %>%
  pull(t_pop)


hist(as.numeric(Population), 
     main="Histogram of Total Polulation", 
     xlab="Population", 
     ylab="Frequency")

AvgIncome <- HKdistrict %>%
  pull(t_mmearn)


hist(as.numeric(AvgIncome), 
     main="Histogram of Average Income", 
     xlab="Avg Income", 
     ylab="Frequency")


library(ggplot2)
# set up the basic histogram
gghist <- ggplot(HKdistrict, 
                 aes(x=t_pop)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Hong Kong Total Population", 
       x="Population", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(t_pop, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))


# histogram of Avg Income
# set up the basic histogram
gghist <- ggplot(HKdistrict, 
                 aes(x=t_mmearn)) + 
  geom_histogram(color="black", 
                 fill="white")+
  labs(title="Ggplot2 histogram of Hong Kong Average Income", 
       x="Income", 
       y="Frequency")
# add a vertical line to the hisogram showing mean tempearture
gghist + geom_vline(aes(xintercept=mean(t_mmearn, 
                                        na.rm=TRUE)),
                    color="blue", 
                    linetype="dashed", 
                    size=1)+
  theme(plot.title = element_text(hjust = 0.5))
