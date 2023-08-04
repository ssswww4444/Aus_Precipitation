# "https://www.ncei.noaa.gov/data/ssmi-ssmis-hydrological-products/"

setwd("~/Projects/Benjamin_code/peiyuns")

file1 = "ftp://ftp2.psl.noaa.gov/Datasets/cpc_global_precip/precip.2020.nc"
download.file(file1, destfile = "NOAA/precip.2020.nc", method = "libcurl")

ncin = nc_open("NOAA/precip.2020.nc")
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
lat = ncvar_get(ncin, "lat")
nlat = dim(lat)

t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)

dname = "precip"

tmp.array <- ncvar_get(ncin, dname)

substr(Sys.time(),0,10)
curr.year = as.numeric(substr(Sys.time(),0,4))
curr.month = as.numeric(substr(Sys.time(),6,7))
curr.day = as.numeric(substr(Sys.time(), 9,10))

years1 = c(rep(2000:(curr.year - 1), each = 12), if(curr.month != 1){rep(curr.year, curr.month - 2)})

months1 = c(rep(1:12, length(2000:(curr.year - 1))), if(curr.month != 1){1:(curr.month - 2)})
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

datesNOAA = cbind(dates1, days1)



years = 2000:2022



WORLD1 = matrix(0, nrow = nlat * nlon, ncol = 2 + 365)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD1[k, 1:2] = c(lat[i], lon[j])
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}

WORLD2 = matrix(0, nrow = nlat * nlon, ncol = 2 + 366)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD2[k, 1:2] = c(lat[i], lon[j])
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}


Y1 = WORLD1[,1:14]

WORLD1 = WORLD1[,-c(1:2)]
WORLD2 = WORLD2[,-c(1:2)]

NOAA.mon = list()
file0 = "ftp://ftp2.psl.noaa.gov/Datasets/cpc_global_precip/precip."
pb <- txtProgressBar(min = 1, max = length(years), style = 3)
unlink("NOAA/precip.2020.nc")
for(i in 1:length(years)){
  file1 = paste0(file0, years[i], ".nc")
  # download.file(file1, destfile = paste0("NOAA.nc", years[i]), method = "libcurl")
  ncin = nc_open(paste0("NOAA/precip.", years[i], ".nc"))
  lon <- ncvar_get(ncin, "lon")
  nlon <- dim(lon)
  lat = ncvar_get(ncin, "lat")
  nlat = dim(lat)
  
  t <- ncvar_get(ncin, "time")
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(t)
  
  dname = "precip"
  
  tmp.array <- ncvar_get(ncin, dname)
  
  if(nt == 365){
    for(j in 1:nt){
      WORLD1[,j] = as.vector(t(tmp.array[,,j]))
    }
  }else{
    for(j in 1:nt){
      WORLD2[,j] = as.vector(t(tmp.array[,,j]))
    }
  }
  w1 = which(datesNOAA[,1] == years[i])
  w2 = c(0,cumsum(datesNOAA[w1,3]))
  if(nt == 365){
    for(j in 1:(length(w2) - 1)){
      Y1[,j + 2] = rowSums(WORLD1[,(w2[j] + 1):w2[j + 1]])
    }
  }else{
    for(j in 1:(length(w2) - 1)){
      Y1[,j + 2] = rowSums(WORLD2[,(w2[j] + 1):w2[j + 1]])
    }
  }
  colnames(Y1)= c("Lat", "Lon", "Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  
  NOAA.mon[[i]] = as.data.frame(Y1)
  unlink("NOAA.nc1")
  setTxtProgressBar(pb, i)
}

NOAA = list()
k = 1
for(i in 1:length(NOAA.mon)){
  for(j in 1:12){
    if(k < 512){
      NOAA[[k]] = NOAA.mon[[i]][,c(1,2,j + 2)]
    }
      k = k + 1
  }
  print(i)
}

for(i in 1:length(NOAA)){
  NOAA[[i]] = cbind(NOAA[[i]], rep(datesNOAA[i,2], nrow(NOAA[[1]])), rep(datesNOAA[i,1], nrow(NOAA[[1]])))
  colnames(NOAA[[i]]) = c("Lat", "Lon", "Precipitation", "Month", "Year")
  NOAA[[i]] = as.data.frame(NOAA[[i]])
}

a1 = rep(TRUE, nrow(NOAA[[1]]))
a2 = Inf
a3 = 1
a4 = NULL
for(i in 1:length(NOAA)){
  if(a2 > sum(!is.na(NOAA[[i]][,3]))){
    a2 = sum(!is.na(NOAA[[i]][,3]))
    a3 = i
  }
  a4 = c(a4, sum(!is.na(NOAA[[i]][,3])))
  a1 = a1 & (!is.na(NOAA[[i]][,3]))
}

a1 = rep(TRUE, nrow(NOAA[[1]]))
for(i in which(a4 == max(a4))){
  a1 = a1 & (!is.na(NOAA[[i]][,3]))
}


for(i in 1:length(NOAA1)){
  NOAA1[[i]] = NOAA1[[i]][complete.cases(NOAA1[[i]]),]
}




for(i in 1:length(NOAA)){
  if(a4[i] < nrow(NOAA[[i]])){
    pred.grid = data.frame(Lat = NOAA[[i]]$Lat[is.na(NOAA[[i]]$Precipitation)], Lon = NOAA[[i]]$Lon[is.na(NOAA[[i]]$Precipitation)])
    idw1 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = NOAA1[[i]], newdata = pred.grid, idp = 2)
    NOAA[[i]]$Precipitation[is.na(NOAA[[i]]$Precipitation)] = idw1$var1.pred
  }
  print(i)
}


a1 = rep(TRUE, nrow(NOAA[[1]]))
for(i in 1:length(NOAA)){
  a1 = a1 & (!is.na(NOAA[[i]][,3]))
}



for(i in 1:length(NOAA)){
  NOAA[[i]] = NOAA[[i]][a1,]
}


for(i in 1:length(NOAA)){
  NOAA[[i]][,2] = ifelse(NOAA[[i]][,2] > 180, NOAA[[i]][,2] - 360, NOAA[[i]][,2])
}

for(i in 1:length(NOAA)){
  NOAA[[i]][,1] = ifelse(NOAA[[i]][,1] > 0, NOAA[[i]][,1] - 90, NOAA[[i]][,1] + 90)
}

for(i in 1:length(NOAA)){
  t1 = NOAA[[i]][,1]
  t2 = NOAA[[i]][,2]
  NOAA[[i]]$Lat = (-t2) + 90
  NOAA[[i]]$Lon = -t1 * 2
}

for(i in 1:length(NOAA)){
  NOAA[[i]][,1] = ifelse(NOAA[[i]][,1] > 90, NOAA[[i]][,1] - 180, NOAA[[i]][,1])
}

g.cor = ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = NOAA[[1]], 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Gauge-NOAA Correlation")

g.cor1 = ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = NOAA[[180]], 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Gauge-NOAA Correlation")

g.cor

grid.arrange(g.cor, g.cor1)

NOAA[[1]]$Lat[1]

which(NOAA[[1]]$Lat == NOAA[[1]]$Lat[1] + 180 & NOAA[[1]]$Lon == NOAA[[1]]$Lon)




plot(NOAA[[1]]$Precipitation - NOAA[[181]]$Precipitation)




ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = NOAA.aus[[443]], 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Gauge-NOAA Correlation")

write.csv(NOAA.aus[[433]], file = "NOAAFC", row.names = FALSE)



used = NULL
for(i in 1:length(Gauge.Sat.2)){
  if(length(which(Gauge.Sat.2[[i]]$Year == 2015) > 0)){
    used = c(used, i)
  }
}


BOMFC = NULL
for(i in used){
  BOMFC = rbind(BOMFC, Gauge.Sat.2[[i]][which(Gauge.Sat.2[[i]]$Year == 2015 & Gauge.Sat.2[[i]]$Month == 1),])
}


write.csv(BOMFC, file = "BOMFC", row.names = FALSE)

N1 = list()
N1[[1]] = t(apply(NOAA.all.aus, 1, cumsum))

M1 = MEDQ(N1, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

dd1 = cbind(M1, seq(0,1,0.001))
v1 = NULL
for(i in unique(M1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Locations = data.frame(NOAA.aus[[1]][, 1], NOAA.aus[[1]][, 2]) 

NOAAEDQloc = data.frame(longitude = Locations[unique(M1),2], latitude = Locations[unique(M1),1], Quantile = v1)

g.EDQ1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(NOAAEDQloc, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.EDQ1


write.csv(NOAAEDQloc, file = "NOAAEDQLoc", row.names = FALSE)










