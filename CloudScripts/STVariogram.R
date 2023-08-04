library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos)

data = read.csv("https://gitlab.ethz.ch/tec/public/opensense/raw/582f75d2ebc4e8af37f865a0b32cf5e2a32b648f/data/ozon_tram1_14102011_14012012.csv", sep = ",", header = TRUE)
data$TIME = as.POSIXlt(as.numeric(substr(paste(data$generation_time), 1, 10)), origin = "1970-01-01")
data$LAT = as.numeric(substr(paste(data$latitude),1,2)) + (as.numeric(substr(paste(data$latitude),3,10))/60)
data$LON = as.numeric(substr(paste(data$longitude),1,1)) + (as.numeric(substr(paste(data$longitude),2,10))/60)

min(data$TIME)

sub = data[data$TIME>as.POSIXct('2011-12-12 00:00 CET')&data$TIME<=as.POSIXct('2011-12-14 23:00 CET'),]
nrow(sub)

coordinates(sub) = ~LON + LAT
projection(sub) = CRS("+init=epsg:4326")
ozone.UTM = spTransform(sub, CRS("+init=epsg:3395"))

ozoneSP = SpatialPoints(ozone.UTM@coords, CRS("+init=epsg:3395"))
dupl = zerodist(ozoneSP)

ozoneDF = data.frame(PPB = ozone.UTM$ozone_ppb[-dupl[,2]])

ozoneTM = as.POSIXct(ozone.UTM$TIME[-dupl[,2]], tz = "CET")

timeDF = STIDF(ozoneSP, ozoneTM, data = ozoneDF)
stplot(timeDF)

var = variogramST(PPB ~ 1, data = timeDF, tunit = "hours", assumeRegular = F, na.omit = T)

Lat1 = seq(-44, by = 0.025, length.out = 1361)
Lon1 = seq(112, by = 0.025, length.out = 1681)

cord1 = matrix(0, nrow = length(Lat1) * length(Lon1), ncol = 2)
for(i in 1:length(Lat1)){
  cord1[(1:length(Lon1)) + length(Lon1) * (i - 1), ] = cbind(rep(Lat1[i], length(Lon1)), Lon1)
  print(i)
}


plot(newmap, xlim = c(112.0,154), ylim = c(-44,-10))
scatter2D(cord1[,2], cord1[,1], cex = 0.2)


Koppen = read.table("https://raw.githubusercontent.com/hinestein/Koppen/master/Fixedkpngrp.txt")

Koppen.Mat = cord1
Koppen.Mat = cbind(Koppen.Mat, rep(0, nrow(cord1)))
for(i in 1:nrow(Koppen)){
  Koppen.Mat[1:ncol(Koppen) + ncol(Koppen) * (i - 1),3] = as.matrix(Koppen)[i,]
}

colnames(Koppen.Mat) = c("Lat", "Lon", "Class")
Koppen.Mat = as.data.frame(Koppen.Mat)

Koppen.Mat2 = Koppen.Mat
coordinates(Koppen.Mat) = ~Lon + Lat

proj4string(Koppen.Mat) = "+proj=longlat +ellps=GRS80 +no_defs"
a3 = over(Koppen.Mat, aus)

Koppen.Mat2 = Koppen.Mat2[!is.na(a3[,1]),]
Koppen.Mat2 = Koppen.Mat2[Koppen.Mat2[,3] != -9999, ]

scatter2D(Koppen.Mat2[,2], Koppen.Mat2[,1], colvar = Koppen.Mat2[,3], cex = 0.1)


Koppen.Mat3 = matrix(0, nrow = nrow(Austra[[1]]), ncol = 3)
K4 = as.matrix(Koppen.Mat2)
j = 1
for(i in 1:nrow(Koppen.Mat2)){
  if(round((((Koppen.Mat2[i,1] %% 1) * 10) %% 1) * 10, 3) == 5 & round((((Koppen.Mat2[i,2] %% 1) * 10) %% 1) * 10, 3) == 5){
    Koppen.Mat3[j,] = K4[i,]
    j = j + 1
  }
  if(j %% 1000 == 0){
    print(j)
  }
}



K2 = rep(0, nrow(Austra[[1]]))
for(i in 1:nrow(Austra[[1]])){
  K2[i] = Koppen.Mat3[which(round(Koppen.Mat3[,1],2) == round(Austra[[1]][i,1],2) &  round(Koppen.Mat3[,2],2) == round(Austra[[1]][i,2],2)), 3]
  if(i %% 100 == 0){
    print(i)
  }
}

KClass = K2

for(i in 1: length(Austra)){
  Austra[[i]][,8] = KClass
}

scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = Austra[[1]][,8], cex = 0.1)







