#Getting shape file of Australian states

download.file("https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/STE_2021_AUST_SHP_GDA2020.zip" , destfile="aus_shape_file.zip")

unzip("aus_shape_file.zip", files = NULL, list = FALSE, overwrite = TRUE,
      junkpaths = FALSE, exdir = ".", unzip = "internal",
      setTimes = FALSE)

library(rgdal)
aus.states <- readOGR( 
  dsn= ".",
  layer="STE_2021_AUST_GDA2020"
)

#Example of using shapefile in ggplot

Temp.df = data.frame(Lon = Fused.Locations$Lon, Lat = Fused.Locations$Lat, Precip = FusedData[,263])
#Fused locations not found. Therefore Temp.df cannot be created.

library(ggplot2)

ggplot() + 
  geom_point(Temp.df, mapping = aes(x = Lon, y = Lat, colour = Precip), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "2000-4 Precipitation", colour = "Precipitation (mm)")+ 
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = aus.states, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))


#Example of using shapefile to plot just part of the data based on state

A1 = as.data.frame(Fused.Locations)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=GRS80 +no_defs"
a3 = over(A1, aus.states)

ggplot() + 
  geom_point(Temp.df[which(a3$STE_NAME21 == "Victoria"),], mapping = aes(x = Lon, y = Lat, colour = Precip), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "2000-4 Precipitation (Victoria)", colour = "Precipitation (mm)")+ 
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = aus.states, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))

# a3$STE_NAME21 gives each location a code based on what state it falls into







