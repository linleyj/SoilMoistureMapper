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

# for now
SoilDF <- read.csv("~/Downloads/Data form (Responses) - Form responses 1.csv",T)
str(SoilDF)
SoilDF$SoilMoisture <- as.numeric(SoilDF$SoilMoisture)
SoilDF <- na.omit(SoilDF)
SoilDF[SoilDF$SoilMoisture>100,5] <- 28
SoilDF$Hole. <- factor(SoilDF$Hole., levels = c("One","Six","Nine","Fourteen","PG"))

library(ggmap)
SoilDF_filter <- SoilDF %>% 
  filter(Hole.!="Shop")

kidnappers <- c(SoilDF$Longitude[20],SoilDF$Latitude[20])
kidnappers.map = get_map(location = kidnappers, zoom = 16, color = "color")
ggmap(kidnappers.map, extent="device",maprange = F)+
  geom_point(data = SoilDF_filter, mapping = aes(Longitude, Latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'green', high = 'red')+geom_tile()



ggplot(SoilDF_filter,aes(x=Longitude,y=Latitude,z=SoilMoisture))+
  geom_point(aes(colour=SoilMoisture))+
  facet_wrap(~Hole.,scales="free")+
  scale_color_continuous(low="red",high = "green")
  
  

filled.contour(x = SM_int$x,
               y = SM_int$y,
               z = SM_int$z,
               color.palette =
                 colorRampPalette(c("white", "blue")),
               xlab = "Longitude",
               ylab = "Latitude",
               main = "Soil Moisture")

# Do spline interpolation with the akima package
Hole1 <- SoilDF%>% 
  filter(Hole.=="One")

ggmap(Hole1.map, extent="device",maprange = F)+
  geom_point(data = Hole1, mapping = aes(Longitude, Latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'green', high = 'red')+geom_tile()+theme_bw()

foo = function(x) {
  hole = unique(x$Hole.)
  print(hole)
  Polygons(list(Polygon(x[,c("Latitude","Longitude")])),ID=hole)
}
library(plyr)
hole_pg

fld <-  with(Hole1, interp(x = Longitude, y = Latitude, z = SoilMoisture, duplicate="median",xo=seq(min(Hole1$Longitude), max(Hole1$Latitude), length = 100),
yo=seq(min(Hole1$Latitude), max(Hole1$Latitude), length = 10),
  extrap=TRUE, linear=FALSE))
head(fld)
melt_x = rep(fld$x, times=length(fld$y))
melt_y = rep(fld$y, each=length(fld$x))
melt_z = as.vector(fld$z)
level_data = data.frame(longitude=melt_x, latitude=melt_y, APPT=melt_z)
interp_data = na.omit(level_data)
grid_points = SpatialPoints(interp_data[,2:1])

filled.contour(x = fld$x,
               y = fld$y,
               z = fld$z,
               color.palette =
                 colorRampPalette(c("white", "blue")),
               xlab = "Longitude",
               ylab = "Latitude",
               main = "Soil Moisture")


ggplot(data=level_data, aes(x=longitude, y=latitude)) + 
  geom_tile(aes(fill=APPT)) +
  stat_contour(aes(z=APPT)) +
  coord_equal() + 
  scale_fill_gradient2(low="blue",mid="white",high="red", midpoint=mean(inside_points$APPT)) 
  


kidnappers.Hole1 <- c(Hole1$Longitude[1],Hole1$Latitude[1])
Hole1.map = get_map(location = kidnappers.Hole1, zoom = 20, color = "bw")
ggmap(Hole1.map, extent="device",maprange = F)+
  geom_point(data = Hole1, mapping = aes(Longitude, Latitude, colour=SoilMoisture), size=1, alpha=0.5, shape=16) + scale_colour_gradient(low = 'red', high = 'green')+geom_tile()+
  geom_contour(data = level_data, aes(x=latitude,y=longitude,z=APPT))


