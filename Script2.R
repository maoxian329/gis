library(sf)
library(tmap)
library(tmaptools)
library(tidyverse)
library(here)
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
# load Hong Kong district
HKdistrict <- st_read("HKGS_2015.shp") %>%
  st_transform(., 2326)

# load Hong Kong hospitals
HKhospital <- st_read("Hospital_1980.shp") %>%
  st_transform(., 2326)


# Total polulation of Hong Kong

tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(6014, 13779, 15998, 18331, 20945, 26271) 

# plot each map
tm1 <- tm_shape(HKdistrict) + 
  tm_polygons("t_pop", 
              breaks=breaks,
              palette="PuBu")+
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(a)", position=c(0,0.85), size=1.5)


legend <- tm_shape(HKdistrict) +
  tm_polygons("t_pop",
              palette="PuBu",
              breaks=breaks,
              title = 'Number of People') +
  tm_scale_bar(position=c(0.01,0.3), text.size=0.6)+
  tm_compass(north=0, position=c(0.25,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.01,0.4),asp=0.1)+
  tm_credits("The Total Population of Hong Kong", position=c(0.01,0.25))


t=tmap_arrange(tm1, legend, ncol=2)

t


# Average Income by District

tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(5000, 10000, 14500, 17630, 23000, 36400)

tm2 <- tm_shape(HKdistrict) + 
  tm_polygons("t_mmearn",
              breaks=breaks, 
              palette="YlOrRd") + 
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(b)", position=c(0,0.85), size=1.5)


legend2 <- tm_shape(HKdistrict) +
  tm_polygons("t_mmearn",
              breaks=breaks,
              palette="YlOrRd",
              title = 'Income ($)') +
  tm_scale_bar(position=c(0.01,0.3), text.size=0.6)+
  tm_compass(north=0, position=c(0.25,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.01,0.4),asp=0.1)+
  tm_credits("Average Income by District", position=c(0.01,0.25))


t2=tmap_arrange(tm2, legend2, ncol=2)

t2


# Hospital Distribution

tmap_mode("plot")


tm3 <- tm_shape(HKdistrict)+ 
  tm_polygons(col="gray")+
  tm_layout(frame=FALSE)+
  tm_shape(HKhospital) +
  tm_symbols(col = "Cluster", scale = .4) +
  tm_legend(show=FALSE) +
  tm_credits("(c)", position=c(0,0.85), size=1.5)


legend3 <- tm_shape(HKhospital) +
  tm_dots(col='Cluster', size = 0.2, shape= 19, title = 'Hospital Cluster') +
  tm_scale_bar(position=c(0.01,0.3), text.size=0.6)+
  tm_compass(north=0, position=c(0.25,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.01,0.4),asp=0.1)+
  tm_credits("Hospitals Distribution", position=c(0.01,0.25))


t3=tmap_arrange(tm3, legend3, ncol=2)

t3



# Population Density = population / area * 1000000 

tmap_mode("plot")

# set the breaks
# for our mapped data
breaks = c(100, 20000, 100000, 300000) 

# plot each map
tm4 <- tm_shape(HKdistrict) + 
  tm_polygons("tpop_densi", 
              breaks=breaks,
              palette="YlOrRd")+
  
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(d)", position=c(0,0.85), size=1.5)


legend4 <- tm_shape(HKdistrict) +
  tm_polygons("tpop_densi",
              breaks=breaks,
              palette="YlOrRd",
              title = 'Population Density') +
  
  tm_scale_bar(position=c(0.01,0.3), text.size=0.6)+
  tm_compass(north=0, position=c(0.25,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.01,0.4),asp=0.1)+
  tm_credits("The Population Density", position=c(0.01,0.25))


t4=tmap_arrange(tm4, legend4, ncol=2)

t4


#Population Hospital Density = no of hospitals/ population density *10000

tmap_mode("plot")

# set the breaks
# for our mapped data
 
breaks = c(0, 0.03, 0.1, 0.5, 1, 1.5, 2, 5, 10, 40, 200, 250) 

# plot each map
tm5 <- tm_shape(HKdistrict) + 
  tm_polygons("hospop_den", 
              breaks=breaks,
              palette="YlGnBu")+
  
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)+
  tm_credits("(d)", position=c(0,0.85), size=1.5)


legend5 <- tm_shape(HKdistrict) +
  tm_polygons("hospop_den",
              breaks=breaks,
              palette="YlGnBu",
              title = 'Hospital/Population Density') +
  
  tm_scale_bar(position=c(0.01,0.3), text.size=0.6)+
  tm_compass(north=0, position=c(0.25,0.6))+
  tm_layout(legend.only = TRUE, legend.position=c(0.01,0.35),asp=0.1)+
  tm_credits("Hospital/Population Density", position=c(0.01,0.25))


t5=tmap_arrange(tm5, legend5, ncol=2)

t5


# HK Distric

tmap_mode("plot")

# set the breaks
# for our mapped data

#breaks = c(0, 0.03, 0.1, 0.5, 1, 1.5, 2, 5, 10, 40, 200, 250) 

# plot each map
tm6 <- tm_shape(HKdistrict) + 
  tm_polygons("DISTRICT_E", 
              palette="Spectral")+
  
  tm_legend(show=FALSE)+
  tm_layout(frame=FALSE)# +
  #tm_credits("(d)", position=c(0,0.85), size=1.5)


legend6 <- tm_shape(HKdistrict) +
  tm_polygons("DISTRICT_E",
              palette="Spectral",
              title = 'District') +
  
  tm_scale_bar(position=c(0.01,0.3), text.size=0.6)+
  tm_compass(north=0, position=c(0.25,0.75))+
  tm_layout(legend.only = TRUE, legend.position=c(0.01,0.35),asp=0.01) #+
  #tm_credits("Hong Kong District", position=c(0.1,0.8))


t6=tmap_arrange(tm6, legend6, ncol=2)

t6

