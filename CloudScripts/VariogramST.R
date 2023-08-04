
IODS1 = IOD.Mat[553 + seq(1,220,12),2]
ONIS1 = ONI.Mat[601 + seq(1,220, 12),2]

S1.1 = S1[[1]]

Betas = matrix(0, nrow = nrow(S1[[1]]), ncol = 3)
res1 = matrix(0, nrow = nrow(S1[[1]]), ncol = 19)
for(i in 1:nrow(S1[[1]])){
  mod1 = gam(S1.1[i,] ~ IODS1 + ONIS1)
  Betas[i,] = mod1$coefficients
  res1[i,] = mod1$residuals
  if(i %% 1000 == 0){
    print(i)
  }
}


plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "Febraury")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = res1[,2], cex = 0.3, pch = 19, add = TRUE)

set.seed(1998)
samp1 = sample(1:nrow(S1[[1]]), size = 1000)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "February")
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], cex = 0.3, col = "blue", pch = 19, add = TRUE)



library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos)

data = read.csv("https://gitlab.ethz.ch/tec/public/opensense/raw/582f75d2ebc4e8af37f865a0b32cf5e2a32b648f/data/ozon_tram1_14102011_14012012.csv", header = TRUE)
data$TIME = as.POSIXlt(as.numeric(substr(paste(data$generation_time), 1, 10)), origin = "1970-01-01")

data$LAT = as.numeric(substr(paste(data$latitude),1,2)) + (as.numeric(substr(paste(data$latitude), 3, 10))/60)

data$LON = as.numeric(substr(paste(data$longitude),1,1)) + (as.numeric(substr(paste(data$longitude), 2, 10))/60)

data = na.omit(data)

sub = data[data$TIME >= as.POSIXct("2011-12-12 00:00 CET") & data$TIME <= as.POSIXct("2011-12-14 23:00 CET"),]
nrow(sub)

coordinates(sub) = ~LON + LAT
projection(sub) = CRS("+init=epsg:4326")

ozone.UTM = spTransform(sub, CRS("+init=epsg:3395"))

ozoneSP = SpatialPoints(ozone.UTM@coords, CRS("+init=epsg:3395"))

dupl = zerodist(ozoneSP)
ozoneDF = data.frame(PPB= ozone.UTM$ozone_ppb[-dupl[,2]])
ozoneTM = as.POSIXct(ozone.UTM$TIME[-dupl[,2]], tz = "CET")
timeDF = STIDF(ozoneSP, ozoneTM, data = ozoneDF)

stplot(timeDF)
var = variogramST(PPB ~ 1, data = timeDF, tunit = "hours", assumeRegular = FALSE, na.omit = TRUE)


AUS1 = NULL
for(i in 1:length(Austra)){
  AUS1 = rbind(AUS1, Austra[[i]][samp1, c(1,2,3,5,6)])
  print(i)
}

unix.start = 1049155199


TIME = NULL
for(i in 1:nrow(AUS1)){
  if(nchar(AUS1$Month[i]) == 1){
    AUS1$Month[i] = paste0("0", AUS1$Month[i])
  }
  if(AUS1$Month[i] == 2 & (AUS1$Year[i] == 2004 | AUS1$Year[i] == 2008 | AUS1$Year[i] == 2012 | AUS1$Year[i] == 2016)){
    day = 29
  }else if(AUS1$Month[i] == 2){
    day = 28
  }else if(AUS1$Month[i] == "01" | AUS1$Month[i] == "03" | AUS1$Month[i] == "05" | AUS1$Month[i] == "07" | AUS1$Month[i] == "08" | AUS1$Month[i] == "10" | AUS1$Month[i] == "12"){
    day = 31
  }else{
    day = 30
  }
  TIME = c(TIME, paste0(AUS1$Year[i], "-", AUS1$Month[i], "-", day, " 23:00:00 CEST"))
  if(i %% 10000 == 0){
    print(i)
  }
}
class(TIME) = class(data$TIME)
TIME = AUS1$TIME
AUS1$TIME = TIME

coordinates(AUS1) = ~Lon + Lat

AUSsp = SpatialPoints(AUS1@coords)
dupl = zerodist(AUSsp)

AUSdf = data.frame(Rain = AUS1$RainRate)
AUStm = as.POSIXct(AUS1$TIME)

timeDF = STFDF(AUSsp, AUStm, data = AUSdf)



Austra2 = Austra
unix.start = 1049155199
for(i in 1:length(Austra2)){
  Time = unix.start + i * 3600
  Austra2[[i]]$Time = rep(Time, nrow(Austra2[[i]]))
}

AUS1 = NULL
for(i in 1:length(Austra2)){
  AUS1 = rbind(AUS1, cbind(Austra2[[i]]$Lon[samp1], Austra2[[i]]$Lat[samp1], Austra2[[i]]$RainRate[samp1], Austra2[[i]]$Year[samp1], Austra2[[i]]$Month[samp1], Austra2[[i]]$Time[samp1]))
}
colnames(AUS1) = c("Lon", "Lat", "Rain", "Year", "Month", "Unix")
AUS1 = as.data.frame(AUS1)
AUS1$TIME = as.POSIXlt(AUS1$Unix, origin = "1970-01-01")

coordinates(AUS1) = ~Lon + Lat

AUSsp = SpatialPoints(AUS1@coords)
AUSdf = data.frame(Rain = AUS1$Rain)
AUStm = as.POSIXct(AUS1$TIME)

locations1 = Austra[[1]][samp1, 1:2]
coordinates(locations1) = ~Lon + Lat

AUS1 = as.data.frame(AUS1)

t1 = AUS1[1 + seq(0, 230000,1000),4]
t2 = as.POSIXlt(t1, origin = "1970-01-01")

g.d = as.data.frame(AUS1$Rain)
colnames(g.d) = "Rain"

timeDF = STFDF(AUSsp, AUStm, data = AUSdf)
st.g <- STFDF(sp = locations1, time = t2, data = g.d)

var = variogramST(Rain ~ 1, st.g, tlags = 0:100, tunit = "hours", cutoff = 100)
plot(var)
plot(var, wireframe = TRUE)
plot(var, map = FALSE)


fit.1 = fit.StVariogram(var, separable)


sep = vgmST("separable", space = vgm(psill = -10, model = "Sph", range = 1, nugget = 1), time = vgm(psill = 10, model = "Per", range = 1, nugget = -1), sill = 10)
plot(var, map = FALSE)




A1 = matrix(rnorm(4), 2,2)
B1 = matrix(rnorm(6), 2, 3)
C1 = matrix(rnorm(3), 3, 1)

t(A1 %*% B1 %*% C1 %*%t(C1))

