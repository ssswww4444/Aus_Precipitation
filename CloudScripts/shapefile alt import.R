shape.coord = list()
labpt.1 = list()
area.1 = list()
hole.1 = list()
ringDir.1 = list()


for(i in 1:97){
  shape.coord[[i]] = read.csv(paste0("coord.",i), row.names = NULL)[,2:3]
  labpt.1[[i]] = read.csv(paste0("labpt.",i))
  area.1[[i]] = read.csv(paste0("area.",i))
  hole.1[[i]] = read.csv(paste0("hole.",i))
  ringDir.1[[i]] = read.csv(paste0("ringDir.",i))
}

p.1 = list()
for(i in 1:length(shape.coord)){
  p.1[[i]] = Polygon(shape.coord[[i]])
}

ps.1 = Polygons(p.1, 1)
sps1 = SpatialPolygons(list(ps.1))
data1 = data.frame(FIPS = "AS", ISO2 = "AU",ISO3 = "AUS", UN  = 36, NAME ="Australia", AREA = 768230, POP2005 = 20310208, REGION = 9, SUBREGION = 53 , LON =  136.189, LAT = -24.973)
ausmap = SpatialPolygonsDataFrame(sps1, data1)

library(sp)
a1 = read.csv("ShapeIndex")

S1 = list()
for(i in unique(a1[,1])){
  S1[[i]] = list()
}

for(i in 1:nrow(a1)){
  file.name = paste0("https://raw.githubusercontent.com/hinestein/World-Shape/master/coord.", a1[i,1], ".", a1[i,2])
  S1[[a1[i,1]]][[a1[i,2]]] = read.csv(file.name)
  print(i)
}

load("S.shape")
library(sp)
a1 = read.csv("ShapeIndex")
S2 = S1
S2 = S.shape
for(i in 1:nrow(a1)){
  S2[[a1[i,1]]][[a1[i,2]]] = Polygon(S2[[a1[i,1]]][[a1[i,2]]])
}

S3 = list()
for(i in 1:length(S2)){
  S3[[i]] = Polygons(S2[[i]], i)
}

sps2 = SpatialPolygons(S3)
data1 = read.csv("shapedata")
shape = SpatialPolygonsDataFrame(sps2, data1)

proj4string(shape) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"


a1 = read.csv("KoppenIndex")

S1 = list()
for(i in unique(a1[,1])){
  S1[[i]] = list()
}

for(i in 1:nrow(a1)){
  file.name = paste0("https://raw.githubusercontent.com/hinestein/Koppen-Shape/master/coord.", a1[i,1], ".", a1[i,2])
  S1[[a1[i,1]]][[a1[i,2]]] = read.csv(file.name)
  print(i)
}

load("S.Koppen")
a1 = read.csv("KoppenIndex")
S2 = S1
S2 = S.Koppen
for(i in 1:nrow(a1)){
  S2[[a1[i,1]]][[a1[i,2]]] = Polygon(S2[[a1[i,1]]][[a1[i,2]]])
  print(i)
}

S3 = list()
for(i in 1:length(S2)){
  S3[[i]] = Polygons(S2[[i]], i)
}

sps2 = SpatialPolygons(S3)
data1 = read.csv("KoppenShapeData")
Koppen = SpatialPolygonsDataFrame(sps2, data1)

proj4string(Koppen) = "+proj=longlat +ellps=bessel +no_defs"















