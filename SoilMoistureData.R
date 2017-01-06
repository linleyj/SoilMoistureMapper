#install.packages("googlesheets")
install.packages("ggmap")
install.packages("akima")
install.packages("automap")
install.packages("data.table")

library(plyr)
suppressMessages(library(dplyr))
library(googlesheets)
library(ggplot2)
library(akima)
library(automap)
library(data.table)
library(ggmap)

(my_sheets <- gs_ls())
# to fix
df_url <- "https://docs.google.com/spreadsheets/d/1YuAtsx0_mRJmXus3FBpyzDeHs4RarJAMBK680JnOxMM"
df_key <- extract_key_from_url(df_url)
gs_DF <- df_key %>% 
  gs_key()
gs_DF %>% gs_browse(ws = 1)
SoilDF <- gs_DF %>%
  gs_read(ws = 1)

SoilDF_holes <- read.csv("~/Downloads/Data form (Responses) - Form responses 1.csv")
Hole1.def <- SoilDF_holes %>% 
  filter(Hole.=="One")

x <- strsplit(SoilDF$Timestamp, " ")
SoilDF$Time <- sapply(x, "[[", 2)
SoilDF$Time <- as.Date(SoilDF_One$Time, format="h:m:s")
head(SoilDF)
o <- c(1,3,2,6,8,7,9,10,11,12,13,14,15,4,16,17,20,19,20,5)
dim(SoilDF_OnePerimeter)
SoilDF_OnePerimeter <- SoilDF %>% 
  mutate(row=seq(1:dim(SoilDF)[1])) %>% 
  filter(is.na(SoilMoisture)) %>% 
  filter(row!="135") %>% 
  filter(row!="136") %>% 
  mutate(row=replace(row,row=="137",132)) %>% 
  mutate(row=replace(row,row=="140",139.1)) %>% 
  mutate(row=replace(row,row=="139",140)) %>% 
  mutate(row=replace(row,row=="139.1",139)) %>% 
  arrange(row)
  

  x <- SoilDF_OnePerimeter[SoilDF_OnePerimeter$row==132,]
  x$row <- 160
  SoilDF_OnePerimeter <- rbind(SoilDF_OnePerimeter,x)


  
SoilDF_OnePerimeter$row
ggplot(dat=SoilDF_OnePerimeter, aes(x=Longitude, y=Latitude))+geom_path()+geom_point(data=Hole1.def, aes(x=Longitude, y=Latitude))+aes(colour=SoilMoisture) + scale_colour_gradient(low = 'red', high = 'green')+theme_bw()
  
Hole1.def


SoilDF_OnePerimeter
minLat <- min(SoilDF_OnePerimeter$Latitude)
maxLat <- max(SoilDF_OnePerimeter$Latitude)
SoilDF_One <- SoilDF %>% 
  filter(Latitude>min(SoilDF_OnePerimeter$Latitude)) %>% 
  filter(Latitude<max(SoilDF_OnePerimeter$Latitude)) %>% 
  filter(Longitude>min(SoilDF_OnePerimeter$Longitude)) %>% 
  filter(Longitude<max(SoilDF_OnePerimeter$Longitude)) 

dim(SoilDF_OnePerimeter)
  
head(SoilDF_OnePerimeter)
  

dim(SoilDF_One)
library(ggmap)
SoilDF_filter <- SoilDF %>% 
  filter(Hole.!="Shop")

# once we have hole coordinates we can filter new data so that it is in the appropriate hole
kidnappers <- c(SoilDF$Longitude[20],SoilDF$Latitude[20])
kidnappers.map = get_map(location = kidnappers, zoom = 16, color = "color")
ggmap(kidnappers.map, extent="device",maprange = F)+
  geom_point(data = SoilDF_filter, mapping = aes(Longitude, Latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'green', high = 'red')+geom_tile()




# Do spline interpolation with the akima package
Hole1 <- SoilDF%>% 
  filter(Hole.=="One")



fld <-  with(Hole1, interp(x = Longitude, y = Latitude, z = SoilMoisture, duplicate="median",xo=seq(min(Hole1$Longitude), max(Hole1$Longitude), length = 100),yo=seq(min(Hole1$Latitude), max(Hole1$Latitude), length = 10),
  extrap=T, linear=FALSE))

head(fld)
melt_x = rep(fld$x, times=length(fld$y))
melt_y = rep(fld$y, each=length(fld$x))
melt_z = as.vector(fld$z)
level_data = data.frame(longitude=melt_x, latitude=melt_y, SoilMoisture=melt_z)
interp_data = na.omit(level_data)
grid_points = SpatialPoints(interp_data[,2:1])

colhigh <- "forestgreen"
collow <- "gold"
ggplot(data=level_data, aes(x=longitude, y=latitude, z=SoilMoisture)) + 
  geom_raster(aes(fill=SoilMoisture)) +
  coord_equal() + 
  geom_path(dat=SoilDF_OnePerimeter, aes(x=Longitude, y=Latitude), colour="black")+
  geom_point(data=Hole1.def, aes(x=Longitude, y=Latitude),colour="black", size=3)+ 
  geom_point(data=Hole1.def, aes(x=Longitude, y=Latitude),size=2)+aes(colour=SoilMoisture)+
  scale_fill_gradient(low=collow,high=colhigh, name="Soil Moisture")+
  scale_colour_gradient(low=collow,high=colhigh, name = "Soil Moisture")+
 guides(colour="none")+
  theme(axis.text.x=element_blank(),axis.text.y=element_blank())

?interp

  
  


kidnappers.Hole1 <- c(Hole1$Longitude[1],Hole1$Latitude[1])
Hole1.map = get_map(location = kidnappers.Hole1, zoom = 20, color = "bw")
ggmap(Hole1.map, extent="device",maprange = F)+
  geom_point(data = Hole1, mapping = aes(Longitude, Latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'red', high = 'green')+geom_tile()+
  geom_contour(data = level_data, aes(x=latitude,y=longitude,z=APPT))


