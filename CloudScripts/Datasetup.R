library("dplyr")
library("fields")
library("gstat")
library("ggplot2")
library("RcolorBrewer")
library("STRbook")
library("sp")
library("spacetime")
library(ncdf4)

load("S.shape")
library(sp)
a1 = read.csv("ShapeIndex")
S2 = S1
S2 = S.shape
for(i in 1:nrow(a1)){
  S2[[a1[i,1]]][[a1[i,2]]] = Polygon(S2[[a1[i,1]]][[a1[i,2]]])
  print(i)
}

S3 = list()
for(i in 1:length(S2)){
  S3[[i]] = Polygons(S2[[i]], i)
}

sps2 = SpatialPolygons(S3)
data1 = read.csv("shapedata")
shape = SpatialPolygonsDataFrame(sps2, data1)

proj4string(shape) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

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

ncin <- nc_open("NOAA.nc")
t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)

years1 = c(rep(1981:(1981 + floor(nt/12) - 1), each = 12),rep(1981 + floor(nt/12), nt %% 12))
rest = NULL
if(nt %% 12 != 0){
  rest = 1:(nt %% 12)
}
months1 = c(rep(1:12, floor(nt/12)), rest)
dates1 = cbind(years1, months1)
days1 = NULL
for(i in 1:nrow(dates1)){
  if(dates1[i,2] == 1 | dates1[i,2] == 3 | dates1[i,2] == 5 | dates1[i,2] == 7 | dates1[i,2] == 8 | dates1[i,2] == 10 | dates1[i,2] == 12){
    days1 = c(days1, 31)
  }else if(dates1[i,2] == 2){
    if(dates1[i,1] %% 4 == 0){
      days1 = c(days1, 29)
    }else{
      days1 = c(days1, 28)
    }
  }else if(dates1[i,2] == 4 | dates1[i,2] == 6 | dates1[i,2] == 9 | dates1[i,2] == 11){
    days1 = c(days1, 30)
  }
}

dates2 = cbind(dates1, days1)

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/WorldPrecip")

precip2 = list()
for(i in 1:nrow(dates2)){
  a1 = dates2[i,2]
  if(nchar(a1) == 1){
    a1 = paste0("0", a1)
  }
  file.name = paste0("Precip.reduced.", dates2[i,1], ".", a1)
  precip2[[i]] = read.csv(file.name , header = TRUE)
  print(i)
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

m01 = seq(1,nrow(dates2),12)
m02 = seq(2,nrow(dates2),12)
m03 = seq(3,nrow(dates2),12)
m04 = seq(4,nrow(dates2),12)
m05 = seq(5,nrow(dates2),12)
m06 = seq(6,nrow(dates2),12)
m07 = seq(7,nrow(dates2),12)
m08 = seq(8,nrow(dates2),12)
m09 = seq(9,nrow(dates2),12)
m10 = seq(10,nrow(dates2),12)
m11 = seq(11,nrow(dates2),12)
m12 = seq(12,nrow(dates2),12)

M = list()
M[[1]] = m01
M[[2]] = m02
M[[3]] = m03
M[[4]] = m04
M[[5]] = m05
M[[6]] = m06
M[[7]] = m07
M[[8]] = m08
M[[9]] = m09
M[[10]] = m10
M[[11]] = m11
M[[12]] = m12

M.all = matrix(0, nrow = nrow(precip2[[1]]), ncol = length(precip2))
for(i in 1:length(precip2)){
  M.all[,i] = precip2[[i]][,3]
  if(i %% 100 == 0){
    print(i)
  }
}

precip3 = list()
for(i in 1:length(precip2)){
  precip3[[i]] = precip2[[i]][-which(rowSums(is.na(M.all)) > 0),]
  print(i)
}

A1 = precip3[[1]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

Aus.all = matrix(0, nrow = length(Aus), ncol = length(NOAA.idw))
for(i in 1:length(NOAA.idw)){
  Aus.all[,i] = NOAA.idw[[i]][,3]
  if(i %% 100 == 0){
    print(i)
  }
}

in1 = function(x){
  ifelse(x >= mean(x),1,0)
}

aver = rep(0, nrow(precip3[[1]]))
for(i in 1:length(precip3)){
  precip3[[i]]$Ave = aver
}

M1.all = M.all[-which(rowSums(is.na(M.all)) > 0),]

M4 = list()
for(i in 1:length(M)){
  M4[[i]] = apply(M1.all[, M[[i]]], 1, in1)
  print(i)
}

for(i in 1:length(M)){
  n1 = M[[i]]
  for(j in 1:length(n1)){
    precip3[[n1[j]]]$Ave = M4[[i]][j,]
  }
}

A.all = matrix(0, nrow = nrow(precip3[[1]]), ncol = length(precip3))
for(i in 1:length(precip3)){
  A.all[,i] = precip3[[i]]$Ave
  if(i %% 100 == 0){
    print(i)
  }
}

Aus.A.all = matrix(0, nrow = length(Aus), ncol = length(precip3))
for(i in 1:length(precip3)){
  Aus.A.all[,i] = precip3[[i]]$Ave[Aus]
  if(i %% 100 == 0){
    print(i)
  }
}

set.seed(1998)
m.Aus = sort(sample(1:length(Aus), 1000))
m.World = sort(sample(1:nrow(M1.all), 10000))

precip4 = list()
for(i in 1:length(precip3)){
  precip4[[i]] = precip3[[i]][m.World,]
}

M2.all = M1.all[m.World,]

A1 = precip4[[1]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=bessel +no_defs"
a3 = over(A1, Koppen)


for(i in 1:length(precip4)){
  precip4[[i]]$Kclass = a3[,2]
}

K1 = list()
k = 1
for(i in (unique(a3[,2])[!is.na(unique(a3))])){
  K1[[k]] = which(precip4[[1]]$Kclass == i)
  k = k + 1
}

K2 = list()
k = 1
for(i in 1:length(K1)){
  if(length(K1[[i]]) > 5){
    K2[[k]] = K1[[i]]
    k = k + 1
  }
}


Austra1 = read.csv("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid202001")
A1 = Austra1[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus2 = which(a3$ISO3 == "AUS")
Austra2 = Austra1[Aus2,]


ausmap = shape[shape$ISO3 == "AUS",]



d1 = NULL
for(i in 1:length(precip2)){
  d1 = rbind(d1, precip2[[i]][1,4:5])
}

plot(Austra1$Lon[Aus1], Austra1$Lat[Aus1])

Austra2 = Austra1[Aus1,]

A1 = Austra2[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=bessel +no_defs"
a3 = over(A1, Koppen)

Austra2$Koppen = a3[,2]

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Austra2, 
             mapping = aes(x = Lon, y = Lat, colour = Koppen), size = 0.1)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Quantile", x = "Longitude", y = "Latitude",
       title = "MEDQ Locations") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom")



ggplot() + geom_polygon(data = Koppen, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))


