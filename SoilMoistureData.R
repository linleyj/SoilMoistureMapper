#install.packages("googlesheets")
#install.packages("ggmap")
#install.packages("akima")
#install.packages("automap")
#install.packages("data.table")
#install.packages("ggrepel")
#install.packages("maptools",dependencies = T)
#install.packages("sm")

library(plyr)
library(googlesheets)
library(ggplot2)
library(akima)
library(automap)
library(data.table)
library(ggmap)
library(ggrepel)
library(dplyr)
library(maptools)
library(sm)
(my_sheets <- gs_ls())
# to fix
df_url <- "https://docs.google.com/spreadsheets/d/1YuAtsx0_mRJmXus3FBpyzDeHs4RarJAMBK680JnOxMM"
df_key <- extract_key_from_url(df_url)
gs_DF <- df_key %>% 
  gs_key()
gs_DF %>% gs_browse(ws = 1)
SoilDF <- gs_DF %>%
  gs_read(ws = 1)

SoilDF <- SoilDF[!duplicated(SoilDF[,3:5]),]
SoilDF$row <- seq(1:dim(SoilDF)[1])
SoilDF[SoilDF$row=="71",5] <- 28
x <- strsplit(SoilDF$Timestamp, " ")
SoilDF$Time <- sapply(x, "[[", 2)
SoilDF$Date <- sapply(x, "[[", 1)


HoleDefs <- read.csv("~/Downloads/Data form (Responses) - Form responses 1.csv")
HoleDefs <- HoleDefs %>% 
  mutate(Hole=as.factor(Hole.)) %>% 
  group_by(Hole) %>% 
  summarise(minLn=min(Longitude),maxLn=max(Longitude),minLat=min(Latitude),maxLat=max(Latitude))

Onedfs <- SoilDF_OnePerimeter %>% 
  summarise(minLn=min(Longitude),maxLn=max(Longitude),minLat=min(Latitude),maxLat=max(Latitude))
HoleDefs[HoleDefs$Hole=="One",2:5] <- Onedfs

Holedefs.fun <- function(data, minLat,maxLat,minLn,maxLn,Hole){
  holedat <- SoilDF[data$Latitude>minLat&data$Latitude<maxLat&data$Longitude>minLn&data$Longitude<maxLn,]
  return(Hole=holedat)
}

HoleData <- mdply(HoleDefs,Holedefs.fun,data=SoilDF)

SoilDF_Perimeters <- HoleData %>% 
  mutate(row=seq(1:dim(HoleData)[1])) %>% 
  filter(Hole=="One"|Hole=="Six") %>% 
  filter(is.na(SoilMoisture)) %>% 
  filter(Date=="07/01/2017") %>% 
  filter(row!=123)



  
ggplot(dat=HoleData, aes(x=Longitude, y=Latitude))+geom_point(data=HoleData, aes(x=Longitude, y=Latitude))+aes(colour=SoilMoisture) + scale_colour_gradient(low = 'red', high = 'green')+theme_bw()+facet_wrap(~Hole,scales="free")
  

# once we have hole coordinates we can filter new data so that it is in the appropriate hole
kidnappers <- c(SoilDF$Longitude[20],SoilDF$Latitude[20])
kidnappers.map = get_map(,maptype = "satellite",location = kidnappers, zoom = 16, color = "color")
ggmap(kidnappers.map, extent="device",maprange = T)+
  geom_point(data = SoilDF, mapping = aes(Longitude, Latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'green', high = 'red')+geom_tile()




# Do spline interpolation with the akima package

map.fun <- function(h="Six"){
dat <- HoleData %>% 
  filter(Hole==h) %>% 
  filter(!is.na(SoilMoisture))

per <- SoilDF_Perimeters %>% 
  filter(Hole==h) %>% 
  arrange(Time) %>% 
  rbind(.,per[1,])



SoilDF_Perimeters
fld <-  with(dat, interp(x = Longitude, y = Latitude, z = SoilMoisture, duplicate="median",xo=seq(min(dat$Longitude), max(dat$Longitude), length = 100),yo=seq(min(dat$Latitude), max(dat$Latitude), length = 100),
  extrap=T, linear=FALSE))

melt_x = rep(fld$x, times=length(fld$y))
melt_y = rep(fld$y, each=length(fld$x))
melt_z = as.vector(fld$z)
level_data = data.frame(longitude=melt_x, latitude=melt_y, SoilMoisture=melt_z)
interp_data = na.omit(level_data)
grid_points = SpatialPoints(interp_data[,1:2])
perpoly <- Polygons(list(Polygon(per[,c("Longitude","Latitude")])),ID=h)
per_poly <- SpatialPolygons(list(perpoly))

in_points = !is.na(over(grid_points,per_poly))
inside_points = interp_data[in_points, ]

map.latln <- c(level_data$longitude[1],level_data$latitude[1])
map = get_map(location = map.latln, maptype="satellite",zoom = 19, color = "bw")

colhigh <- "forestgreen"
collow <- "gold"

ggmap(map, extent="device",maprange = F)+
  geom_point(data = inside_points ,mapping = aes(longitude, latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'red', high = 'green')+geom_tile()+geom_path(dat=per, aes(x=Longitude, y=Latitude), colour="black")+  geom_label_repel(data=dat,aes(x=Longitude, y=Latitude,label=SoilMoisture))
}
  
map.fun("One")
inside_points

?geom_text
  



