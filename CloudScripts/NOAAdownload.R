
substr(Sys.time(),0,10)
curr.year = as.numeric(substr(Sys.time(),0,4))
curr.month = as.numeric(substr(Sys.time(),6,7))
curr.day = as.numeric(substr(Sys.time(), 9,10))

years1 = c(rep(1979:(curr.year - 1), each = 12), if(curr.month != 1){rep(curr.year, curr.month - 1)})

months1 = c(rep(1:12, length(1979:(curr.year - 1))), if(curr.month != 1){1:(curr.month - 1)})
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



file1 = "ftp://ftp.cdc.noaa.gov/Datasets/cmap/std/precip.mon.mean.nc"
download.file(file1, destfile = "NOAA.nc", method = "libcurl")

ncin = nc_open("NOAA.nc")
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)

t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)

dname = "precip"

tmp.array <- ncvar_get(ncin, dname)


WORLD = matrix(0, nrow = nlat * nlon, ncol = 5)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD[k,] = c(lat[i], lon[j], 0, 0, 0)
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}


NOAA = list()

for(i in 1:nt){
  a1 = as.vector(tmp.array[ , ,i])
  WORLD[,3] = as.numeric(a1) * datesNOAA[i,3]
  WORLD[,4] = as.numeric(rep(datesNOAA[i,1], nrow(WORLD)))
  WORLD[,5] = as.numeric(rep(datesNOAA[i,2], nrow(WORLD)))
  NOAA[[i]] = WORLD
}

a1 = rep(TRUE, nrow(WORLD))
for(i in 1:length(NOAA1)){
  a1 = a1 & (!is.na(NOAA[[i]][,3]))
}

for(i in 1:nt){
  NOAA[[i]] = NOAA[[i]][a1,]
}

NOAA1 = list()
for(i in 1:nt){
  colnames(NOAA[[i]]) = c("Lat", "Lon", "Precipitation", "Year", "Month")
  NOAA1[[i]] = as.data.frame(NOAA[[i]])
}


A1 = as.data.frame(NOAA[[1]][,1:2])
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)


for(i in 1:length(NOAA)){
  file1 = "NOAA"
  file2 = NOAA[[i]][1,5]
  file3 = NOAA[[i]][1,4]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  file4 = paste(file1, file2, file3, sep = ".")
  write.csv(NOAA[[i]], file = file4, row.names = FALSE)
}

Aus1 = which(a3$ISO3 == "AUS")

Aus1 = which(A1$Lat < -8 & A1$Lat > -45 & A1$Lon > 110 & A1$Lon < 155)

NOAA.aus = list()
for(i in 1:length(NOAA)){
  NOAA.aus[[i]] = NOAA[[i]][Aus1,]
}

pred.grid.NOAA = data.frame(Lat = precip3[[1]][Aus, 1], Lon = precip3[[1]][Aus,2])

NOAA.loc = NULL
for(i in 1:nrow(NOAA.aus[[1]])){
  d1 = distm(precip3[[1]][Aus, 2:1], NOAA.aus[[1]][i,2:1])
  NOAA.loc = c(NOAA.loc, which(d1 == min(d1))[1])
  print(i)
}

for(i in 1:length(NOAA.aus)){
  NOAA.aus[[i + 24]]$Rain = (precip3[[i]][Aus,3])[NOAA.loc]
}



NOAA.idw = list()
for(i in 1:nt){
  idw1 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = NOAA.aus[[i]], newdata = precip3[[1]][Aus,], idp = 0.75)
  NOAA.idw[[i]] = data.frame(Lat = pred.grid.NOAA$Lat, Lon = pred.grid.NOAA$Lon,
                             Precipitation = idw1$var1.pred, Year = rep(NOAA.aus[[i]][1,4], nrow(pred.grid.NOAA)),
                             Month = rep(NOAA.aus[[i]][1,5], nrow(pred.grid.NOAA)))
  print(i)
}

pred.grid.single = data.frame(Lat = -29.068, Lon = 126.284)

idw1 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = NOAA.aus[[505]], newdata = pred.grid.single, idp = 10)




min.2 = Inf
for(i in 1:length(Gauge.2)){
  if(!is.na(as.numeric(Gauge.2[[i]][1,2]))){
    if(as.numeric(Gauge.2[[i]][1,2]) < min.2){
      min.2 = as.numeric(Gauge.2[[i]][1,2])
    }
  }
}

max.2 = 0
for(i in 1:length(Gauge.2)){
  if(!is.na(as.numeric(Gauge.2[[i]][1,2]))){
    if(as.numeric(Gauge.2[[i]][nrow(Gauge.2[[i]]),2]) > max.2){
      max.2 = as.numeric(Gauge.2[[i]][nrow(Gauge.2[[i]]),2])
    }
  }
}

pred.grid.2 = data.frame(Lat = as.numeric(Gauge.Loc.2[,2]), Lon = as.numeric(Gauge.Loc.2[,3]))
coordinates(pred.grid.2) = ~Lon + Lat

datesNOAA1 = datesNOAA[datesNOAA[,1] >= 1981,]

NOAA.gauge = list()
k = 1
for(i in (which(datesNOAA[,1] == min.2)[1]):tail(which(datesNOAA[,1] == max.2),1)){
  idw.2 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = NOAA.aus[[i]], newdata = pred.grid.2, idp = 3)
  NOAA.gauge[[k]] = data.frame(Lat = idw.2$Lat, Lon = idw.2$Lon, Estimate = idw.2$var1.pred)
  k = k + 1
}

Gauge.Sat.2 = Gauge.2
for(i in 1:length(Gauge.2)){
  a1 = NULL
  if(!is.na(as.numeric(Gauge.2[[i]][1,2]))){
    for(j in 1:nrow(Gauge.2[[i]])){
      indices = which(dates3[,1] == as.numeric(Gauge.2[[i]][j,2]) & dates3[,2] == as.numeric(Gauge.2[[i]][j,1]))
      a1 = c(a1, NOAA.gauge[[indices]][i,3])
    }
    Lon.2 = rep(as.numeric(Gauge.Loc.2[i,3]), nrow(Gauge.2[[i]]))
    Lat.2 = rep(as.numeric(Gauge.Loc.2[i,2]), nrow(Gauge.2[[i]]))
    Rain.2 = as.numeric(Gauge.2[[i]][,3])
    Month.2 = as.numeric(Gauge.2[[i]][,1])
    Year.2 = as.numeric(Gauge.2[[i]][,2])
    Est = a1
    Gauge.Sat.2[[i]] = data.frame(Lat = Lat.2, Lon = Lon.2, Month = Month.2, Year = Year.2, Precipitation = Rain.2, Estimate = a1)  
  }
  print(i)
}



Gauge.Sat.All = c(Gauge.Sat.2)
X.mat.sq = NULL
m3 = NULL
for(i in 1:length(Gauge.Sat.All)){
  if(!is.na(as.numeric(Gauge.Sat.All[[i]][1,2])) & nrow(Gauge.Sat.All[[i]]) > 24){
    y1 = sqrt(Gauge.Sat.All[[i]]$Precipitation)
    x1 = sqrt(Gauge.Sat.All[[i]]$Estimate)
    mod1 = rlm(y1 ~ x1, method = "M", maxit = 200)
    corr.1 = cor(y1,x1)
    X.mat.sq = rbind(X.mat.sq, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, as.numeric(Gauge.Sat.All[[i]][1,1:2]), nrow(Gauge.Sat.All[[i]]), corr.1))
    m3 = c(m3, i)
  }
}

colnames(X.mat.sq) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "cor")
X.mat.sq = as.data.frame(X.mat.sq)
par(mfrow = c(1,1))
hist(X.mat.sq$Intercept)
hist(X.mat.sq$Slope)

X.mat.sq = X.mat.sq[X.mat.sq$Intercept < 2 & X.mat.sq$Intercept > -2 &X.mat.sq$Slope > 0.6 & X.mat.sq$Slope < 1.3,]

g.int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.sq, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Intercept Estimate")

g.slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.sq, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Slope Estimate")


grid.arrange(g.int + labs(x = " ") + theme(legend.position = "left"), g.slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))


par(mfrow = c(1,2))
hist(X.mat.sq$Intercept)
hist(X.mat.sq$Slope)

g.cor = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.sq, 
             mapping = aes(x = Lon, y = Lat, colour = cor), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Gauge-NOAA Correlation")

g.cor


for(i in 1:length(NOAA.aus)){
  NOAA.aus[[i]] = NOAA.aus[[i]][!duplicated(NOAA.aus[[i]][,1:2]),]
}


NOAA.all.aus = matrix(0, nrow = nrow(NOAA.aus[[1]]), ncol = length(NOAA.aus))
for(i in 1:length(NOAA.aus)){
  NOAA.all.aus[,i] = NOAA.aus[[i]][,3]
}

set.seed(1998)
s.world = sample(1:nrow(NOAA[[1]]), nrow(NOAA.all.aus), replace = FALSE)

NOAA.all = matrix(0, nrow = length(s.world), ncol = length(NOAA))
for(i in 1:length(NOAA)){
  NOAA.all[,i] = NOAA[[i]][s.world,3]
}



SOINOAA = SOI[-c(1:(which(rownames(SOI) == "1979") - 1)),]
IODNOAA = IOD[-c(1:(which(rownames(IOD) == "1979") - 1)),]

SOI1 = as.vector(t(SOINOAA))
IOD1 = as.vector(t(IODNOAA))

SOI1 = SOI1[SOI1 < 90]
IOD1 = IOD1[IOD1 < 90]
SOI1 = SOI1[1:length(IOD1)]

m.pos.pos = which(SOI1 > 0 & IOD1 > 0)
m.pos.neg = which(SOI1 > 0 & IOD1 < 0)
m.neg.pos = which(SOI1 < 0 & IOD1 > 0)
m.neg.neg = which(SOI1 < 0 & IOD1 < 0)

NOAA.cor.pos.pos = list()
NOAA.cor.pos.neg = list()
NOAA.cor.neg.pos = list()
NOAA.cor.neg.neg = list()
for(i in 1:18){
  NOAA.cor.pos.pos[[i]] = matrix(0, nrow = nrow(NOAA.all.aus), ncol = nrow(NOAA.all))
  NOAA.cor.pos.neg[[i]] = matrix(0, nrow = nrow(NOAA.all.aus), ncol = nrow(NOAA.all))
  NOAA.cor.neg.pos[[i]] = matrix(0, nrow = nrow(NOAA.all.aus), ncol = nrow(NOAA.all))
  NOAA.cor.neg.neg[[i]] = matrix(0, nrow = nrow(NOAA.all.aus), ncol = nrow(NOAA.all))
  print(i)
}


pb <- txtProgressBar(min = 1, max = 18, style = 3)
t1 = 1
  for(j in 1:18){
    m.pos.pos1 = m.pos.pos[m.pos.pos - j > 0 & m.pos.pos < 512]
    m.pos.neg1 = m.pos.neg[m.pos.neg - j > 0 & m.pos.neg < 512]
    m.neg.pos1 = m.neg.pos[m.neg.pos - j > 0 & m.neg.pos < 512]
    m.neg.neg1 = m.neg.neg[m.neg.neg - j > 0 & m.neg.neg < 512]
    NOAA.cor.pos.pos[[j]] = cor(t(NOAA.all.aus[, m.pos.pos1]), t(NOAA.all[,m.pos.pos1]))
    NOAA.cor.pos.neg[[j]] = cor(t(NOAA.all.aus[, m.pos.neg1]), t(NOAA.all[,m.pos.neg1]))
    NOAA.cor.neg.pos[[j]] = cor(t(NOAA.all.aus[, m.neg.pos1]), t(NOAA.all[,m.neg.pos1]))
    NOAA.cor.neg.neg[[j]] = cor(t(NOAA.all.aus[, m.neg.neg1]), t(NOAA.all[,m.neg.neg1]))
    setTxtProgressBar(pb, t1)
    t1 = t1 + 1
}

X.test = matrix(rnorm(1000), ncol = 100)

X.test[,1] = 1
X.test[,2] = -10

save(NOAA.cor.pos.pos, file = "NOAA.cor.pos.possave")
save(NOAA.cor.pos.neg, file = "NOAA.cor.pos.negsave")
save(NOAA.cor.neg.pos, file = "NOAA.cor.neg.possave")
save(NOAA.cor.neg.neg, file = "NOAA.cor.neg.negsave")

load("NOAA.cor.pos.pos")

NOAA.cor.pos.pos1 = NOAA.cor.pos.pos
NOAA.cor.pos.neg1 = NOAA.cor.pos.neg
NOAA.cor.neg.pos1 = NOAA.cor.neg.pos
NOAA.cor.neg.neg1 = NOAA.cor.neg.neg

for(i in 1:length(NOAA.cor.pos.pos)){
  NOAA.cor.pos.pos1[[i]][NOAA.cor.pos.pos[[i]] < 0] = 0
  NOAA.cor.pos.neg1[[i]][NOAA.cor.pos.neg[[i]] < 0] = 0
  NOAA.cor.neg.pos1[[i]][NOAA.cor.neg.pos[[i]] < 0] = 0
  NOAA.cor.neg.neg1[[i]][NOAA.cor.neg.neg[[i]] < 0] = 0
}


for(i in 1:length(NOAA.cor.neg.neg1)){
  for(j in 1:nrow(NOAA.cor.neg.neg1[[1]])){
    NOAA.cor.pos.pos1[[i]][j,] = NOAA.cor.pos.pos1[[i]][j,]/sum(NOAA.cor.pos.pos1[[i]][j,])
    NOAA.cor.pos.neg1[[i]][j,] = NOAA.cor.pos.neg1[[i]][j,]/sum(NOAA.cor.pos.neg1[[i]][j,])
    NOAA.cor.neg.pos1[[i]][j,] = NOAA.cor.neg.pos1[[i]][j,]/sum(NOAA.cor.neg.pos1[[i]][j,])
    NOAA.cor.neg.neg1[[i]][j,] = NOAA.cor.neg.neg1[[i]][j,]/sum(NOAA.cor.neg.neg1[[i]][j,])
  }
}

m.all = ifelse(SOI1 > 0 & IOD1 > 0, 1, ifelse(SOI1 > 0 & IOD1 < 0, 2, ifelse(SOI1 < 0 & IOD1 > 0, 3, 4)))

NOAA.cor = list()
NOAA.cor[[1]] = NOAA.cor.pos.pos1
NOAA.cor[[2]] = NOAA.cor.pos.neg1
NOAA.cor[[3]] = NOAA.cor.neg.pos1
NOAA.cor[[4]] = NOAA.cor.neg.neg1


m01 = seq(1,nrow(datesNOAA),12)
m02 = seq(2,nrow(datesNOAA),12)
m03 = seq(3,nrow(datesNOAA),12)
m04 = seq(4,nrow(datesNOAA),12)
m05 = seq(5,nrow(datesNOAA),12)
m06 = seq(6,nrow(datesNOAA),12)
m07 = seq(7,nrow(datesNOAA),12)
m08 = seq(8,nrow(datesNOAA),12)
m09 = seq(9,nrow(datesNOAA),12)
m10 = seq(10,nrow(datesNOAA),12)
m11 = seq(11,nrow(datesNOAA),12)
m12 = seq(12,nrow(datesNOAA),12)

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




B1 = function(X.list){
  out = 0
  for(i in 1:length(X.list)){
    out = out + t(X.list[[i]]) %*% X.list[[i]]
  }
  return(out)
}

B2 = function(X.list, Y.list){
  out = 0
  for(i in 1:length(X.list)){
    out = out + t(X.list[[i]]) %*% Y.list[[i]] 
  }
  return(out)
}

B3 = function(W.list, Y.all){
  out = list()
  for(i in 1:length(W.list)){
    out[[i]] = W.list[[i]] %*% Y.all[[i]]
  }
  return(out)
}

B3.1 = function(X, Z.list){
  out = list()
  for(i in 1:length(Z.list)){
    out[[i]] = t(X) %*% Z.list[[i]]
  }
  return(out)
}

B3.2 = function(phi.vec, Z.list){
  out = 0
  for(i in 1:length(phi.vec)){
    out = phi.vec[i] * Z.list[[i]]
  }
  return(out)
}

B4 = function(Z.list){
  out = 0
  for(i in 1:length(Z.list)){
    out = out + Z.list[[i]]
  }
  return(out)
}





P1 = function(Y.all, W.list, ind){
  out = 0
  for(i in 1:length(W.list)){
    out = out + (t(Y.all[[i]][[ind]]) %*% t(W.list[[i]][[ind]]) %*% (W.list[[i]][[ind]]) %*% (Y.all[[i]][[ind]]))[1,1]
  }
  return(out)
}

P2 = function(Y.list, Y.all, W.list, ind){
  out = 0
  for(i in 1:length(Y.list)){
    out = out + t(Y.list[[i]]) %*% W.list[[i]][[ind]] %*% Y.all[[i]][[ind]]
  }
  return(out)
}

P3 = function(Y.list, Y.all, W.list, ind){
  out = 0
  for(i in 1:length(Y.list)){
    out = out + t(Y.all[[i]][[ind]]) %*% t(W.list[[i]][[ind]]) %*% Y.list[[i]]
  }
  return(out)
}

P4 = function(X.list, W.list, Y.all, ind){
  out = 0
  for(i in 1:length(X.list)){
    out = out + t(X.list[[i]]) %*% W.list[[i]][[ind]] %*% Y.all[[i]][[ind]]
  }
  return(out)
}

P5 = function(X.list, W.list, Y.all, ind){
  out = 0
  for(i in 1:length(X.list)){
    out = out + t(Y.all[[i]][[ind]]) %*% t(W.list[[i]][[ind]]) %*% X.list[[i]]
  }
  return(out)
}

P6 = function(W.list, Y.all){
  out = list()
  for(i in 1:length(W.list)){
    out[[i]] = list()
    for(j in 1:length(W.list[[i]])){
      out[[i]][[j]] = W.list[[i]][[j]] %*% Y.all[[i]][[j]]
    }
  }
  return(out)
}

P6.1 = function(phi.vec, Z.list, ind){
  out = list()
  for(i in 1:length(Z.list)){
    out[[i]] = 0
    for(j in (1:length(Z.list[[i]]))[-ind]){
      out[[i]] = out[[i]] + phi.vec[j] * Z.list[[i]][[j]]
    }
  }
  return(out)
}

P6.2 = function(Y.all, W.list, Z.list, ind){
  out = list()
  for(i in (1:length(Y.all))){
    out[[i]] = t(Y.all[[i]][[ind]]) %*% t(W.list[[i]][[ind]]) %*% Z.list[[i]]
  }
  return(out)
}

P7 = function(Z.list){
  out = 0
  for(i in (1:length(Z.list))){
    out = out + Z.list[[i]]
  }
  return(out)
}

P8 = function(W.list, Y.all, ind){
  out = list()
  k = 1
  for(i in (1:length(Y.all))[-ind]){
    out[[k]] = t(Y.all[[i]]) %*% t(W.list[[i]])
    k = k + 1
  }
  return(out)
}

P8.1 = function(Y.all, W.list, Z.list){
  out = list()
  for(i in (1:length(Z.list))){
    out[[i]] = (Z.list[[i]]) %*% W.list %*% Y.all
  }
  return((out))
}

P8.2 = function(phi.vec, Z.list){
  out = 0
  for(i in (1:length(phi.vec))){
    out = out + phi.vec[i] * t(Z.list[[i]])
  }
  return(out)
}

P9 = function(Z.list){
  out = 0
  for(i in 1:length(Z.list)){
    out = out + Z.list[[i]]
  }
  return(out)
}


k.start = 1
q = k.start + 1

phi.vec = rep(0, length(k.start:q))

X.list = list()
for(i in 1:40){
  X.list[[i]] = cbind(1, rnorm(nrow(NOAA.all.aus)))
}

B1(X.list)

Y.list = list()
for(i in 1:40){
  Y.list[[i]] = NOAA.all.aus[,i]
}

B2(X.list, Y.list)

W.list = list()
for(i in 1:42){
  W.list[[i]] = list()
  k = 1
  for(j in k.start:q){
    W.list[[i]][[k]] = NOAA.cor[[m.all[i]]][[j]]
    print(k)
    k = k + 1
  }
}

Y.all = list()
l = 1
for(i in 11:50){
  Y.all[[l]] = list()
  k = 1
  for(j in k.start:q){
    Y.all[[l]][[k]] = NOAA.all[,i - j]
    k = k + 1
  }
  l = l + 1
}

Z.list = B3(W.list[[2]], Y.all[[2]])

Z.list = B3.1(X.list[[1]], Z.list)

B3.2(phi.vec, Z.list)

P1(Y.all, W.list, 2)

P3(Y.list, Y.all, W.list, 2)

P5(X.list, W.list, Y.all, 1)

Z.list = P6(W.list[[1]], Y.all[[1]], 2)

Z.list = P6.1(Y.all[[1]], W.list[[1]], Z.list, 2)

P6.2(c(0.5, 6), Z.list, 2)


t1 = m.all[m01]

k.start = 2
q = k.start + 1

phi.vec = rep(0, length(k.start:q))

m01.1 = m01[m01 - q > 0]

X.list = list()
for(i in 1:length(m01.1)){
  X.list[[i]] = cbind(1, rnorm(nrow(NOAA.all.aus)))
}

Y.list = list()
k = 1
for(i in 1:length(m01.1)){
  Y.list[[k]] = NOAA.all.aus[,i]
  k = k + 1
}


W.list = list()
l = 1
for(i in m01.1){
  W.list[[l]] = list()
  k = 1
  for(j in k.start:q){
    W.list[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]]
    k = k + 1
  }
  l = l + 1
}


Y.all = list()
l = 1
for(i in m01.1){
  Y.all[[l]] = list()
  k = 1
  for(j in k.start:q){
    Y.all[[l]][[k]] = NOAA.all[,i - j]
    k = k + 1
  }
  l = l + 1
}


beta.vec = rep(0, 2)




B01 = solve(B1(X.list))
B02 = B2(X.list, Y.list)
B03 = list()
B03.1 = list()
for(i in 1:length(X.list)){
  B03[[i]] = B3(W.list[[i]], Y.all[[i]])
  B03.1[[i]] = B3.1(X.list[[i]], B03[[i]])
}

P01 = list()
P02 = list()
P03 = list()
P04 = list()
P05 = list()
for(i in 1:length(phi.vec)){
  P01[[i]] = P1(Y.all, W.list, i)
  P02[[i]] = P2(Y.list, Y.all, W.list, i)
  P03[[i]] = P3(Y.list, Y.all, W.list, i)
  P04[[i]] = P4(X.list, W.list, Y.all, i)
  P05[[i]] = P5(X.list, W.list, Y.all, i)
}

P06 = list()
P06.1 = list()
for(i in 1:length(phi.vec)){
  P06[[i]] = list()
  P06.1[[i]] = list()
  for(t1 in 1:length(Y.list)){
      P06[[i]][[t1]] = P6(W.list[[t1]], Y.all[[t1]])
      P06.1[[i]][[t1]] = P6.1(Y.all[[t1]][[i]], W.list[[t1]][[i]], P06[[i]][[t1]])
  }
}

P06.2 = list()
for(i in 1:length(phi.vec)){
  P06.2[[i]] = list()
  for(t1 in 1:length(Y.list)){
    P06.2[[i]][[t1]] = P6.2(phi.vec[-i], P06.1[[i]][[t1]])
  }
}


P07 = list()
for(i in 1:length(phi.vec)){
  P07[[i]] = P7(P06.2[[i]])
}

beta.vec = rep(0, ncol(X.list[[1]]))
phi.vec = rep(0, length(k.start:q))
phi1 = Inf
while(abs(phi.vec[1] - phi1) > 10^(-10)){
  phi1 = phi.vec[1]
  B03.2 = list()
  for(j in 1:length(X.list)){
    B03.2[[j]] = B3.2(phi.vec, B03.1[[j]])
  }
  B04 = B4(B03.2)
  beta.vec = B01 %*% (B02 - B04)
  for(j in 1:length(phi.vec)){
    P06.2[[j]] = list()
    for(t1 in 1:length(Y.list)){
      P06.2[[j]][[t1]] = P6.2(phi.vec, P06.1[[j]][[t1]], j)
    }
    P07[[j]] = P7(P06.2[[j]])
    phi.vec[j] = (P02[[j]] + P03[[j]] - t(beta.vec) %*% P04[[j]] - P05[[j]] %*% beta.vec - P07[[j]] - t(P07[[j]]))/(2 * P01[[j]])
  }
}


pb <- txtProgressBar(min = 1, max = length(M) * 6 * 19 * 10, style = 3)
count1 = 1
CV.err = list()
for(mon in 1:length(M)){
  CV.err[[mon]] = list()
  m1 = M[[mon]]
  for(k.start in 1:6){
    CV.err[[mon]][[k.start]] = list()
    Folds = createFolds(1:nrow(NOAA.all.aus), 10)
    lag.errs = rep(0, length(0:18))
    for(lag in 0:6){
      q = k.start + lag
      m1.1 = m1[m1 > q]
      errs = rep(0, length(Folds))
      for(f in 1:length(Folds)){
        phi.vec = rep(0, length(k.start:q))
        X.list = list()
        for(i in 1:length(m1.1)){
          X.list[[i]] = cbind(1, rowMeans(sqrt(NOAA.all.aus[-Folds[[f]], m1])))
        }
        
        beta.vec = rep(0, ncol(X.list[[1]]))
        
        Y.list = list()
        k = 1
        for(i in m1.1){
          Y.list[[k]] = sqrt(NOAA.all.aus[-Folds[[f]],i])
          k = k + 1
        }
        
        
        W.list = list()
        l = 1
        for(i in m1.1){
          W.list[[l]] = list()
          k = 1
          for(j in k.start:q){
            W.list[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]][-Folds[[f]],]
            k = k + 1
          }
          l = l + 1
        }
        
        
        Y.all = list()
        l = 1
        for(i in m1.1){
          Y.all[[l]] = list()
          k = 1
          for(j in k.start:q){
            Y.all[[l]][[k]] = sqrt(NOAA.all[,i - j])
            k = k + 1
          }
          l = l + 1
        }
        
        B01 = solve(B1(X.list))
        B02 = B2(X.list, Y.list)
        B03 = list()
        B03.1 = list()
        for(i in 1:length(X.list)){
          B03[[i]] = B3(W.list[[i]], Y.all[[i]])
          B03.1[[i]] = B3.1(X.list[[i]], B03[[i]])
        }
        
        P01 = list()
        P02 = list()
        P03 = list()
        P04 = list()
        P05 = list()
        for(i in 1:length(phi.vec)){
          P01[[i]] = P1(Y.all, W.list, i)
          P02[[i]] = P2(Y.list, Y.all, W.list, i)
          P03[[i]] = P3(Y.list, Y.all, W.list, i)
          P04[[i]] = P4(X.list, W.list, Y.all, i)
          P05[[i]] = P5(X.list, W.list, Y.all, i)
        }
        P06 = P6(Y.all, W.list)

        
        phi.vec = rep(0, length(k.start:q))
        beta.vec = rep(0, ncol(X.list[[1]]))
        
        phi1 = Inf
        h = 1
        while(abs(phi.vec[1] - phi1) > 10^(-8) & h < 10000){
          h = h + 1
          phi1 = phi.vec[1]
          B03.2 = list()
          for(j in 1:length(X.list)){
            B03.2[[j]] = B3.2(phi.vec, B03.1[[j]])
          }
          B04 = B4(B03.2)
          beta.vec = B01 %*% (B02 - B04)
          P06.1 = P6.1(phi.vec, P06)
          P07 = list()
          for(i in 1:length(phi.vec)){
            P07[[i]] = P7(P06.1, i)
          }
          for(i in 1:length(phi.vec)){
            phi.vec[i] = (P02[[i]] + P03[[i]] - t(beta.vec) %*% P04[[i]] - P05[[i]] %*% beta.vec - P07[[i]] - t(P07[[i]]))/(2.1 * P01[[i]])
          }
        }
        print(h)
        
        
        X.new = list()
        k = 1
        for(i in m1.1){
          X.new[[k]] = cbind(1, rowMeans(sqrt(NOAA.all.aus[Folds[[f]], m1])))
          k = k + 1
        }
        
        Y.new = list()
        k = 1
        for(i in m1.1){
          Y.new[[k]] = sqrt(NOAA.all.aus[Folds[[f]], i])
          k = k + 1
        }
        
        
        W.new = list()
        l = 1
        for(i in m1.1){
          W.new[[l]] = list()
          k = 1
          for(j in k.start:q){
            W.new[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]][Folds[[f]],]
            k = k + 1
          }
          l = l + 1
        }
        
        Y.fitted = NULL
        Y.actual = NULL
        k = 1
        for(i in m1.1){
          Y.fitted = c(Y.fitted, X.new[[k]] %*% beta.vec + L.phi.add(phi.vec, W.new[[k]], Y.all[[k]]))
          Y.actual = c(Y.actual, Y.new[[k]])
          k = k + 1
        }
        errs[f] = sum((Y.fitted - Y.actual)^2)/(length(Y.fitted) - 1 - lag)
        setTxtProgressBar(pb, count1)
        count1 = count1 + 1
        
      }
      lag.errs[lag + 1] = mean(errs)
    }
    CV.errs[[mon]][[k.start]] = lag.errs
  }
}


L.phi.add = function(phi.vec, W.list, Y.all){
  out = 0
  for(i in 1:length(phi.vec)){
    out = out + phi.vec[i] * W.list[[i]] %*% Y.all[[i]]
  }
  return(out)
}



P.all = function(Y.all.1, W.all.1, phi.vec){
  out = list()
  for(i in 1:length(Y.all.1)){
    out[[i]] = 0
    for(j in (1:length(phi.vec))[-i]){
      out[[i]] = out[[i]] + t(Y.all.1[[i]]) %*% t(W.all.1[[i]]) %*% (phi.vec[j] * W.all.1[[j]] %*% Y.all.1[[j]])
    }
  }
  return(out)
}

P06.2= list()
for(i in 1:length(Y.all)){
  P06.2[[i]] = P.all(Y.all[[i]], W.list[[i]], phi.vec)
}

P6 = function(Y.all, W.list){
  out = list()
  for(t1 in 1:length(Y.all)){
    out[[t1]] = list()
    for(i in 1:length(Y.all[[t1]])){
      out[[t1]][[i]] = list()
      for(j in (1:length(Y.all[[t1]]))){
        out[[t1]][[i]][[j]] = t(Y.all[[t1]][[i]]) %*% t(W.list[[t1]][[i]]) %*% W.list[[t1]][[j]] %*% Y.all[[t1]][[j]]
      }
    }
  }
  return(out)
}


P06 = P6(Y.all, W.list)


P6.1 = function(phi.vec, Z.list){
  out = list()
  for(t1 in 1:length(Z.list)){
    out[[t1]] = list()
    for(i in 1:length(Z.list[[i]])){
      out[[t1]][[i]] = 0
    }
    for(i in 1:length(Z.list[[i]])){
      for(j in (1:length(Z.list[[i]]))[-i]){
        out[[t1]][[i]] = out[[t1]][[i]] + phi.vec[j] %*% Z.list[[t1]][[i]][[j]]
      }
    }
  }
  return(out)
}

P06.1 = P6.1(phi.vec, P06)

P7 = function(Z.list, ind){
  out = 0
  for(i in 1:length(Z.list)){
    out = out + Z.list[[i]][[ind]]
  }
  return(out)
}


P07 = list()
for(i in 1:length(phi.vec)){
  P07[[i]] = P7(P06.1, i)
}


SOI.cor = rep(0, nrow(NOAA.all.aus))
for(i in 1:nrow(NOAA.all.aus)){
  m.1 = NOAA.all.aus[i,1:511]
  for(j in 1:length(M)){
    l1 = M[[j]]
    l1 = l1[l1 <= length(SOI1)]
    m.1[l1] = scale(m.1[l1])
  }
  SOI.cor[i] = cor(SOI1[1:511], m.1[1:511])
}

IOD.cor = rep(0, nrow(NOAA.all.aus))
for(i in 1:nrow(NOAA.all.aus)){
  m.1 = NOAA.all.aus[i,1:511]
  for(j in 1:length(M)){
    l1 = M[[j]]
    l1 = l1[l1 <= length(IOD1)]
    m.1[l1] = scale(m.1[l1])
  }
  IOD.cor[i] = cor(IOD1[1:511], m.1[1:511])
}

Osc.df = data.frame(SOI = SOI.cor, IOD = IOD.cor, Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat)


idw1 = idw(formula = IOD ~ 1, locations = ~Lon + Lat, data = Osc.df, newdata = pred.grid, idp = 3)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "DMI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))



mon = 1
m1 = M[[mon]]

k.start = 2
lag = 2
q = k.start + lag
m1.1 = m1[m1 > q]

X.ti = cbind(rep(1, nrow(NOAA.all.aus)), NOAA.aus[[1]][,1], NOAA.aus[[1]][,2], rowMeans(sqrt(NOAA.all.aus[, m1])))

G <- auto_basis(data = NOAA.aus[[1]][, c("Lon","Lat")] %>% # Take Tmax
                  SpatialPoints(), # To sp obj
                nres = 1, # One resolution
                type = "Gaussian")

S <- eval_basis(basis = G, # basis functions
                s = NOAA.aus[[1]][, c("Lon","Lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S))

X.ti <- cbind(X.ti, S)

cor.SOI = SOI.cor
cor.IOD = IOD.cor

X.list = list()
for(i in 1:length(m1.1)){
  X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[m1.1[i]], cor.IOD * IOD1[m1.1[i]], cor.SOI * SOI1[m1.1[i]] * cor.IOD * IOD1[m1.1[i]])
}


phi.vec = rep(0, length(k.start:q))
X.list = list()
for(i in 1:length(m1.1)){
  X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[m1.1[i]]+ rnorm(nrow(X.ti),0,0.01),
                      cor.IOD * IOD1[m1.1[i]] + rnorm(nrow(X.ti),0,0.01),
                      cor.SOI * SOI1[m1.1[i]] * cor.IOD * IOD1[m1.1[i]] + rnorm(nrow(X.ti),0,0.01))
}


beta.vec = rep(0, ncol(X.list[[1]]))

Y.list = list()
k = 1
for(i in m1.1){
  Y.list[[k]] = sqrt(NOAA.all.aus[,i])
  k = k + 1
}


W.list = list()
l = 1
for(i in m1.1){
  W.list[[l]] = list()
  k = 1
  for(j in k.start:q){
    W.list[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]][,]
    k = k + 1
  }
  l = l + 1
}


Y.all = list()
l = 1
for(i in m1.1){
  Y.all[[l]] = list()
  k = 1
  for(j in k.start:q){
    Y.all[[l]][[k]] = sqrt(NOAA.all[,i - j])
    k = k + 1
  }
  l = l + 1
}

B01 = solve(B1(X.list))
B02 = B2(X.list, Y.list)
B03 = list()
B03.1 = list()
for(i in 1:length(X.list)){
  B03[[i]] = B3(W.list[[i]], Y.all[[i]])
  B03.1[[i]] = B3.1(X.list[[i]], B03[[i]])
}

P01 = list()
P02 = list()
P03 = list()
P04 = list()
P05 = list()
for(i in 1:length(phi.vec)){
  P01[[i]] = P1(Y.all, W.list, i)
  P02[[i]] = P2(Y.list, Y.all, W.list, i)
  P03[[i]] = P3(Y.list, Y.all, W.list, i)
  P04[[i]] = P4(X.list, W.list, Y.all, i)
  P05[[i]] = P5(X.list, W.list, Y.all, i)
}
P06 = P6(Y.all, W.list)


phi.vec = rep(0, length(k.start:q))
beta.vec = rep(0, ncol(X.list[[1]]))

phi1 = Inf
h = 1
while(abs(phi.vec[1] - phi1) > 10^(-10) & h < 100000){
  h = h + 1
  phi1 = phi.vec[1]
  B03.2 = list()
  for(j in 1:length(X.list)){
    B03.2[[j]] = B3.2(phi.vec, B03.1[[j]])
  }
  B04 = B4(B03.2)
  beta.vec = B01 %*% (B02 - B04)
  P06.1 = P6.1(phi.vec, P06)
  P07 = list()
  for(i in 1:length(phi.vec)){
    P07[[i]] = P7(P06.1, i)
  }
  for(i in 1:length(phi.vec)){
    phi.vec[i] = (P02[[i]] + P03[[i]] - t(beta.vec) %*% P04[[i]] - P05[[i]] %*% beta.vec - P07[[i]] - t(P07[[i]]))/(2.01 * P01[[i]])
  }
}
print(h)


X.new = list()
k = 1
for(i in m1.1){
  X.new[[k]] = X.list[[k]]
  k = k + 1
}

Y.new = list()
k = 1
for(i in m1.1){
  Y.new[[k]] = sqrt(NOAA.all.aus[, i])
  k = k + 1
}


W.new = list()
l = 1
for(i in m1.1){
  W.new[[l]] = list()
  k = 1
  for(j in k.start:q){
    W.new[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]]
    k = k + 1
  }
  l = l + 1
}

Y.fitted = NULL
Y.actual = NULL
k = 1
for(i in m1.1){
  Y.fitted = cbind(Y.fitted, X.new[[k]] %*% beta.vec + L.phi.add(phi.vec, W.new[[k]], Y.all[[k]]))
  Y.actual = cbind(Y.actual, Y.new[[k]])
  k = k + 1
}


mean((Y.fitted - Y.actual)^2)


pred.grid = data.frame(Lon = Austra1$Lon[Aus2], Lat = Austra1$Lat[Aus2])
STAR.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Precipitation = Y.fitted[,40]^2, Actual = Y.actual[,40]^2)

idw1 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = STAR.df, newdata = pred.grid, idp = 2)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "DMI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))






coordinates(STAR.df) = ~Lon + Lat
coordinates(pred.grid) = ~Lon + Lat

krig1 = autoKrige(Precipitation ~ 1, STAR.df, pred.grid)

m = vgm(psill = 7798, model = "Ste", range = 29, nugget = 71, kappa = 1.7)


krig1 = krige(Precipitation ~ 1, STAR.df, pred.grid, model = m)
krig2 = krige(Actual ~ 1, STAR.df, pred.grid, model = m)

Pred.df19.2m = data.frame(Fitted = krig1$var1.pred, Lead = "Two Months", Year = "2019", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df19.3m = data.frame(Fitted = krig1$var1.pred, Lead = "Three Months", Year = "2019", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df19.4m = data.frame(Fitted = krig1$var1.pred, Lead = "Four Months", Year = "2019", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df20.2m = data.frame(Fitted = krig1$var1.pred, Lead = "Two Months", Year = "2020", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df20.3m = data.frame(Fitted = krig1$var1.pred, Lead = "Three Months", Year = "2020", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df20.4m = data.frame(Fitted = krig1$var1.pred, Lead = "Four Months", Year = "2020", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df21.2m = data.frame(Fitted = krig1$var1.pred, Lead = "Two Months", Year = "2021", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df21.3m = data.frame(Fitted = krig1$var1.pred, Lead = "Three Months", Year = "2021", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])
Pred.df21.4m = data.frame(Fitted = krig1$var1.pred, Lead = "Four Months", Year = "2021", Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2])

Pred.df = rbind(Pred.df19.2m, Pred.df19.3m, Pred.df19.4m,
                Pred.df20.2m, Pred.df20.3m, Pred.df20.4m,
                Pred.df21.2m, Pred.df21.3m, Pred.df21.4m)

plot(krig1)
plot(krig2)

krig1.df = data.frame(Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2],
                      Precipitation = krig1$var1.pred)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Pred.df, 
             mapping = aes(x = Lon, y = Lat, colour = Fitted), size = 0.5) + facet_grid(Year ~ Lead) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation\n(mm)", x = "Longitude", y = "Latitude",
       title = "January Forecasting") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))



krig2.df = data.frame(Lon = coordinates(krig2)[,1], Lat = coordinates(krig2)[,2],
                      Precipitation = krig2$var1.pred)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = krig2.df, 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "DMI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))



krig2021.df = data.frame(Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2],
                      Precipitation = krig1$var1.pred, Type = "Fitted", Year = "2021")

krig2021a.df = data.frame(Lon = coordinates(krig2)[,1], Lat = coordinates(krig2)[,2],
                      Precipitation = krig2$var1.pred, Type = "Actual", Year = "2021")

krig2020.df = data.frame(Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2],
                         Precipitation = krig1$var1.pred, Type = "Fitted", Year = "2020")

krig2020a.df = data.frame(Lon = coordinates(krig2)[,1], Lat = coordinates(krig2)[,2],
                          Precipitation = krig2$var1.pred, Type = "Actual", Year = "2020")

krig2019.df = data.frame(Lon = coordinates(krig1)[,1], Lat = coordinates(krig1)[,2],
                         Precipitation = krig1$var1.pred, Type = "Fitted", Year = "2019")

krig2019a.df = data.frame(Lon = coordinates(krig2)[,1], Lat = coordinates(krig2)[,2],
                          Precipitation = krig2$var1.pred, Type = "Actual", Year = "2019")

krig.df = rbind(krig2019.df, krig2019a.df, krig2020.df, krig2020a.df, krig2021.df, krig2021a.df)


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_grid(Year ~ Type) +
  geom_point(data = krig.df, 
             mapping = aes(x = Lon, y = Lat, colour = (abs(Precipitation))), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation\n(mm)", x = "Longitude", y = "Latitude",
       title = "July Precipitation") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))


hist(krig1$var1.pred - krig2019a.df$Precipitation)

krig.df = read.csv("JulyKrig")

CV = matrix(0, nrow = 12, ncol = 12)
for(mon in 8:12){
  m1 = M[[mon]]
  m1 = m1[m1 < ncol(NOAA.all.aus)]
  for(k.start in 1:11){
    errs = Inf
    for(lag in 0:6){
      q = k.start + lag
      m1.1 = m1[m1 > q]
      
      X.ti = cbind(rep(1, nrow(NOAA.all.aus)), NOAA.aus[[1]][,1], NOAA.aus[[1]][,2], rowMeans(sqrt(NOAA.all.aus[, m1])))
      
      G <- auto_basis(data = NOAA.aus[[1]][, c("Lon","Lat")] %>% # Take Tmax
                        SpatialPoints(), # To sp obj
                      nres = 1, # One resolution
                      type = "Gaussian")
      
      S <- eval_basis(basis = G, # basis functions
                      s = NOAA.aus[[1]][, c("Lon","Lat")] %>% # spat locations
                        as.matrix()) %>% # conv. to matrix
        as.matrix() # results as matrix
      colnames(S) <- paste0("B", 1:ncol(S))
      
      X.ti <- cbind(X.ti, S)
      
      cor.SOI = SOI.cor
      cor.IOD = IOD.cor
      
      X.list = list()
      for(i in 1:length(m1.1)){
        X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[m1.1[i]], cor.IOD * IOD1[m1.1[i]], cor.SOI * SOI1[m1.1[i]] * cor.IOD * IOD1[m1.1[i]])
      }
      
      
      phi.vec = rep(0, length(k.start:q))
      X.list = list()
      for(i in 1:length(m1.1)){
        X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[m1.1[i]]+ rnorm(nrow(X.ti),0,0.01),
                            cor.IOD * IOD1[m1.1[i]] + rnorm(nrow(X.ti),0,0.01),
                            cor.SOI * SOI1[m1.1[i]] * cor.IOD * IOD1[m1.1[i]] + rnorm(nrow(X.ti),0,0.01))
      }
      
      
      beta.vec = rep(0, ncol(X.list[[1]]))
      
      Y.list = list()
      k = 1
      for(i in m1.1){
        Y.list[[k]] = sqrt(NOAA.all.aus[,i])
        k = k + 1
      }
      
      
      W.list = list()
      l = 1
      for(i in m1.1){
        W.list[[l]] = list()
        k = 1
        for(j in k.start:q){
          W.list[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]][,]
          k = k + 1
        }
        l = l + 1
      }
      
      
      Y.all = list()
      l = 1
      for(i in m1.1){
        Y.all[[l]] = list()
        k = 1
        for(j in k.start:q){
          Y.all[[l]][[k]] = sqrt(NOAA.all[,i - j])
          k = k + 1
        }
        l = l + 1
      }
      
      B01 = solve(B1(X.list))
      B02 = B2(X.list, Y.list)
      B03 = list()
      B03.1 = list()
      for(i in 1:length(X.list)){
        B03[[i]] = B3(W.list[[i]], Y.all[[i]])
        B03.1[[i]] = B3.1(X.list[[i]], B03[[i]])
      }
      
      P01 = list()
      P02 = list()
      P03 = list()
      P04 = list()
      P05 = list()
      for(i in 1:length(phi.vec)){
        P01[[i]] = P1(Y.all, W.list, i)
        P02[[i]] = P2(Y.list, Y.all, W.list, i)
        P03[[i]] = P3(Y.list, Y.all, W.list, i)
        P04[[i]] = P4(X.list, W.list, Y.all, i)
        P05[[i]] = P5(X.list, W.list, Y.all, i)
      }
      P06 = P6(Y.all, W.list)
      
      
      phi.vec = rep(0, length(k.start:q))
      beta.vec = rep(0, ncol(X.list[[1]]))
      
      phi1 = Inf
      h = 1
      den = 2.01
      if(lag > 2){
        den = 4.01
      }
      if(lag > 3){
        den = 6.01
      }
      if(lag > 4){
        den = 8.01
      }
      if(lag > 5){
        den = 10.1
      }
      while(abs(phi.vec[1] - phi1) > 10^(-10) & h < 100000){
        h = h + 1
        phi1 = phi.vec[1]
        B03.2 = list()
        for(j in 1:length(X.list)){
          B03.2[[j]] = B3.2(phi.vec, B03.1[[j]])
        }
        B04 = B4(B03.2)
        beta.vec = B01 %*% (B02 - B04)
        P06.1 = P6.1(phi.vec, P06)
        P07 = list()
        for(i in 1:length(phi.vec)){
          P07[[i]] = P7(P06.1, i)
        }
        for(i in 1:length(phi.vec)){
          phi.vec[i] = (P02[[i]] + P03[[i]] - t(beta.vec) %*% P04[[i]] - P05[[i]] %*% beta.vec - P07[[i]] - t(P07[[i]]))/(den * P01[[i]])
        }
      }
      print(h)
      
      
      X.new = list()
      k = 1
      for(i in m1.1){
        X.new[[k]] = X.list[[k]]
        k = k + 1
      }
      
      Y.new = list()
      k = 1
      for(i in m1.1){
        Y.new[[k]] = sqrt(NOAA.all.aus[, i])
        k = k + 1
      }
      
      
      W.new = list()
      l = 1
      for(i in m1.1){
        W.new[[l]] = list()
        k = 1
        for(j in k.start:q){
          W.new[[l]][[k]] = NOAA.cor[[m.all[i]]][[j]]
          k = k + 1
        }
        l = l + 1
      }
      
      Y.fitted = NULL
      Y.actual = NULL
      k = 1
      for(i in m1.1){
        Y.fitted = cbind(Y.fitted, X.new[[k]] %*% beta.vec + L.phi.add(phi.vec, W.new[[k]], Y.all[[k]]))
        Y.actual = cbind(Y.actual, Y.new[[k]])
        k = k + 1
      }
      
      cur.err = mean((Y.fitted - Y.actual)^2)
      if(cur.err < errs){
        errs = cur.err
        CV[mon, k.start] = lag
      }
      
      
      print(c(mon, k.start, lag))
    }
  }
}


CV2 = data.frame(Components = as.vector(t(CV1)),
                 Month = factor(rep(c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"),each = ncol(CV1)),
                                      levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
                 Lead = rep(1:ncol(CV1), nrow(CV1)))

CV2$W = sample(8:15, prob = c(1.5/8, 1.5/8, 2/8, 1/8, 0.75/8, 0.5/8, 0.25/8, 0.25/8), size = nrow(CV2), replace = TRUE)


ggplot(CV2, aes(x = Lead, y = Components, colour = W)) + geom_line(size = 1) + facet_wrap(~Month, nrow = 3) +
  labs(x = "Lead (Months)", y = "AR Components",
       title = "Optimal Lag Components") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) + 
  scale_x_continuous(breaks= c(2,4,6,8,10,12))



dim(NOAA.all.aus)


Diff.df = data.frame(Diff = c(rowMeans(NOAA.all.aus[,m01[1:21]]) - rowMeans(NOAA.all.aus[,m01[22:42]]),
                              rowMeans(NOAA.all.aus[,m07[1:21]]) - rowMeans(NOAA.all.aus[,m07[22:42]])),
                     Lon = rep(NOAA.aus[[1]]$Lon, 2), Lat = rep(NOAA.aus[[1]]$Lat, 2),
                     Month = factor(rep(c("January", "July"), each = nrow(NOAA.all.aus)), levels = c("January", "July")))

DiffJan.df = data.frame(Diff = c(rowMeans(NOAA.all.aus[,m01[1:21]]) - rowMeans(NOAA.all.aus[,m01[22:42]])),
                     Lon = rep(NOAA.aus[[1]]$Lon), Lat = rep(NOAA.aus[[1]]$Lat))

DiffJul.df = data.frame(Diff = c(rowMeans(NOAA.all.aus[,m07[1:21]]) - rowMeans(NOAA.all.aus[,m07[22:42]])),
                     Lon = rep(NOAA.aus[[1]]$Lon), Lat = rep(NOAA.aus[[1]]$Lat))


idw1 = idw(formula = Diff ~ 1, locations = ~Lon + Lat, data = DiffJan.df, newdata = pred.grid, idp = 3)

idw2 = idw(formula = Diff ~ 1, locations = ~Lon + Lat, data = DiffJul.df, newdata = pred.grid, idp = 3)

Diff.df = data.frame(Diff = c(idw1$var1.pred, idw2$var1.pred),
                     Lon = pred.grid$Lon, Lat = pred.grid$Lat,
                     Month = factor(rep(c("January", "July"), each = nrow(pred.grid)), levels = c("January", "July")))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_grid(Year ~ Type) +
  geom_point(data = Diff.df, 
             mapping = aes(x = Lon, y = Lat, colour = Diff), size = 0.5) + facet_wrap(~Month, nrow = 1) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation\n(mm)", x = "Longitude", y = "Latitude",
       title = "Precipitation Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(0.9, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))



D.test = data.frame(Lon = NOAA[[i]][s.world,2], Lat = NOAA[[i]][s.world,1], Correlation = NOAA.cor.pos.pos[[1]][1,])
Dist.test = dist(D.test, method = "euclidean")

hc1 <- hclust(Dist.test, method = "complete")
max(cutree(hc1, h = 50))


