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
##Load all our data

library(tmap)
library(tmaptools)
library(tidyverse)
library(tidypredict)


# load Hong Kong district
HKdistrict <- st_read("HKGS_2015.shp") %>%
  st_transform(., 2326)

# load Hong Kong hospitals
HKhospital <- st_read("Hospital_1980.shp") %>%
  st_transform(., 2326)

# qtm(HKdistrict)

tmap_mode("view")
tm_shape(HKdistrict) +
  tm_polygons(col = 'DISTRICT_E', alpha = 0.5,
              title = 'Hong Kong Destrict') +
  tm_shape(HKhospital) +
  tm_dots(col = "blue")

# Population and Hospital

tmap_mode("view")
tm_shape(HKdistrict) +
  tm_polygons(col = 't_pop', 
              palette="PuBu",
              alpha = 0.5,
              title = 'Total Population') +
  tm_shape(HKhospital) +
  tm_dots(col = "blue")


# Income and Hospital

tmap_mode("view")
tm_shape(HKdistrict) +
  tm_polygons(col = 't_mmearn', 
              palette="YlOrRd",
              alpha = 0.5,
              title = 'Average Income ($)') +
  tm_shape(HKhospital) +
  tm_dots(col = "blue")


# Hospital Density

tmap_mode("view")
tm_shape(HKdistrict) +
  tm_polygons(col = 'tpop_densi', alpha = 0.5,
            title = 'Population Density (per sqrKM)') +
tm_shape(HKhospital) +
  tm_dots(col = "blue")

# Hospital/ Population Density ratio
tmap_mode("view")
breaks = c(0, 0.03, 0.1, 0.5, 1, 1.5, 2, 5, 10, 40, 200, 250)
tm_shape(HKdistrict) +
  tm_polygons(col = 'hospop_den', alpha = 0.5,
              palette = 'YlGnBu',
              breaks = breaks,
              title = 'Hospital/Population Density Ratio') +
  tm_shape(HKhospital) +
  tm_dots(col = "blue")

library(colorRamps)
# score
tmap_mode("view")
breaks = c(0, 1, 4, 6, 7, 8, 10)
tm_shape(HKdistrict) +
  tm_polygons(col = 'Score', alpha = 0.5,
              palette = 'RdYlGn',#'colorRamps::kovesi.rainbow',
              #breaks = breaks,
              title = 'Score') +
  tm_shape(HKhospital) +
  tm_dots(col = "blue")


#  Points Density

window <- as.owin(HKdistrict)
plot(window)
HKhospitalSub<- HKhospital %>%
  as(., 'Spatial')

HKhospitalSub.ppp <- ppp(x=HKhospitalSub@coords[,1],
                          y=HKhospitalSub@coords[,2],
                          window=window)
HKhospitalSub.ppp %>%
  density(., sigma=500) %>%
  plot()

HKhospitalSub.ppp %>%
  density(., sigma=1000) %>%
  plot(main = 'HK Hospital Density')

K <- HKhospitalSub.ppp %>%
  Kest(., correction="border") %>%
  plot(main = 'Ripley??s K of Hospital')




###
library(rsample)


ggplot(HKdistrict, aes(t_pop,
                     t_mmearn))+
  # we want our lines to be from the fitted column grouped by our bootstrap id
  geom_line(aes(t_mmearn), alpha = .2, col = "cyan3") +  
  # remember out apparent data is the original within the bootstrap
  geom_point(data=HKdistrict)+
  #add some labels to x and y
  labs(x="Total Population",
       y="Average Income")


ggplot(HKdistrict, aes(t_pop,
                       No_Hsp))+
  geom_point(data=HKdistrict)+
  #add some labels to x and y
  labs(x="Total Population",
       y="No of Hospital")

ggplot(HKdistrict, aes(tpop_densi,
                       No_Hsp))+
  geom_point(data=HKdistrict)+
  #add some labels to x and y
  labs(x="Population Density",
       y="No of Hospital")


ggplot(HKdistrict, aes(t_mmearn,
                       No_Hsp))+
  geom_point(data=HKdistrict)+
  #add some labels to x and y
  labs(x="Average Income",
       y="No of Hospital")