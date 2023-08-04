readBin("/Users/bhines/Downloads/gsmap_nrt.20000301.0.1d.daily.00Z-23Z.dat")
to.read = file("https://stats.idre.ucla.edu/stat/r/faq/bintest.dat", "rb")


file = "ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
temp <- tempfile()
download.file(file,temp)
a3 = unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
           junkpaths = FALSE, exdir = ".", unzip = "internal",
           setTimes = FALSE)
a4 = read.csv(a3)


install.packages("chron")
install.packages("ncdf4")
library(ncdf4)
library(chron)
library(RColorBrewer)
library(lattice)
"ftp://ftp.cdc.noaa.gov/Datasets/noaa.oisst.v2/sst.mnmean.nc"
download.file()


dname <- "sst"  # note: tmp means temperature (not temporary)

# open a NetCDF file
ncin <- nc_open(temp)
print(ncin)
lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat)
print(c(nlon, nlat))

t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)

tmp.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(tmp.array)

title <- ncatt_get(ncin, 0, "title")
institution <- ncatt_get(ncin, 0, "institution")
datasource <- ncatt_get(ncin, 0, "source")
references <- ncatt_get(ncin, 0, "references")
history <- ncatt_get(ncin, 0, "history")
Conventions <- ncatt_get(ncin, 0, "Conventions")

nc_close(ncin)

# split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth = as.integer(unlist(tdstr)[2])
tday = as.integer(unlist(tdstr)[3])
tyear = as.integer(unlist(tdstr)[1])
chron(t, origin = c(tmonth, tday, tyear))

tmp.array[tmp.array == fillvalue$value] <- NA

length(na.omit(as.vector(tmp.array[, , 1])))

m <- 100
tmp.slice <- tmp.array[, , m]

#image(lon, lat, tmp.slice, col = rev(brewer.pal(10, "RdBu")))

grid <- expand.grid(lon = lon, lat = lat)
levelplot(tmp.slice ~ lon * lat, data = grid)

max(tmp.array[2, , ], na.rm = TRUE)


SST = list()
for(i in 1:453){
  T.Mat = NULL
  for(j in 1:nlat){
    T.Mat = rbind(T.Mat, cbind(tmp.array[ , j, i], lon, rep(lat[j], nlon), rep(t[i], nlon)))
  }
  SST[[i]] = T.Mat
  print(i)
}

tmp.array[,181,]


years = c(1981,sort(rep(seq(1982,2018),12)), rep(2019,8))
months = c(12, rep(1:12,37), 1:8)
months = c("12", rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),37), "01", "02", "03", "04", "05", "06", "07", "08")
days = c(rep(c("31","28","31","30","31","30","31","31","30","31","30","31"),19),"31","28","31","30", "31", "30")
months = cbind(months, days)
months = months[-(1:2),]
dates = cbind(years, months)
for(i in 1:(dim(dates)[1])){
  if(dates[i,2] == "02"){
    if(dates[i,1] == "2004" | dates[i,1] == "2008" | dates[i,1] == "2012" | dates[i,1] == "2016"){
      dates[i,3] = "29"
    }
  }
}

for(i in 1:length(SST)){
  SST[[i]] = as.data.frame(cbind(SST[[i]], rep(dates[i,1], nrow(SST[[i]])), rep(dates[i,2], nrow(SST[[i]]))))
  colnames(SST[[i]]) = c("SST", "Lon", "Lat", "T", "Year", "Month")
}

for(i in 1:length(SST)){
  row.names(SST[[i]]) = NULL
}

W1 = readOGR(".","TM_WORLD_BORDERS-0.3")


SST1 = SST[[1]]
for(i in 1:length(SST1[,2])){
  if(SST1[i,2]>180){
    SST1[i,2] = SST1[i,2] - 360
  }
}

SST2 = SST1
coordinates(SST2) = ~ Lon + Lat

proj4string(SST2) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

class(SST1)

O1 = over(SST2, W1)
plot(newmap)
scatter2D(SST1[which(is.na(O1[,4])),2], SST1[which(is.na(O1[,4])),3], colvar = SST[[453]][which(is.na(O1[,4])),1], cex = 0.2, add = TRUE)

for(i in 1:length(SST)){
  SST[[i]][,2] = SST1[,2]
}

head(SST[[220]])

VB.vec = NULL
for(i in 1:length(Austra)){
  VB.vec = rbind(VB.vec, Austra[[i]][56577,1:6])
}

SST.location = list()
for(j in 1:nrow(SST[[1]])){
  SST.Temp = NULL
  for(i in 1:length(SST)){
    SST.Temp = rbind(SST.Temp, SST[[i]][j,])
  }
  SST.location[[j]] = SST.Temp
  if(j %% 1000 == 0){
    print(j)
  }
}

k.lag = 24
C.Mat = NULL
for(i in 1:length(SST.location)){
  C.Mat = rbind(C.Mat, c(cor(VB.vec[,3], SST.location[[i]][(220 - k.lag):(450 - k.lag),1]),
                         sqrt((VB.vec[1,1] - SST.location[[i]][1,3])^2 + (VB.vec[1,2] - SST.location[[i]][1,2])^2)))
  if(i %% 1000 == 0){
    print(i)
  }
}


plot(newmap)
scatter2D(SST1[which(is.na(O1[,4])),2], SST1[which(is.na(O1[,4])),3], colvar = C.Mat[which(is.na(O1[,4])),1], cex = 0.2, add = TRUE)




#Southern Osciallation Index
SOI = read.csv("https://www.ncdc.noaa.gov/teleconnections/enso/indicators/soi/data.csv", header = FALSE)
SOI = SOI[-c(1:2),]
head(SOI)

SOI1 = SOI[,1]
SOI2 = SOI[,2]

if(class(SOI1) == "factor"){
  a.4 = as.numeric(as.character(SOI1))
  a.4[is.na(a.4)] = 0
  SOI1 = a.4
}

if(class(SOI2) == "factor"){
  a.4 = as.numeric(as.character(SOI2))
  a.4[is.na(a.4)] = 0
  SOI2 = a.4
}

SOI = cbind(SOI1, SOI2)

SOI.cor = NULL
VB.vec = NULL
for(i in 1:length(Austra)){
  VB.vec = rbind(VB.vec, Austra[[i]][56577,1:6])
}


for(k in 0:100){
  
  k.lag = k
  SOI.cor = c(SOI.cor, cor(VB.vec[,3], SOI[(591 - k.lag):(821 - k.lag),2]))
}

k.lag = 2
SOI.cor = NULL
for(j in 1:nrow(Austra[[1]])){
  Tmp.Vec = NULL
  for(i in 1:length(Austra)){
    Tmp.Vec = c(Tmp.Vec, Austra[[i]][j,3])
  }
  SOI.cor = c(SOI.cor, cor(Tmp.Vec, SOI[(591- k.lag):(821 - k.lag),2] * 10))
  if(j %% 1000 == 0){
    print(j)
  }
}


plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor, cex = 0.5, pch = 19, add = TRUE)

plot(SOI[,2], type = "l")

ONI = read.table("ONI.data.txt", header = FALSE)
colnames(ONI) = c("Year", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

ONI.Mat = NULL
for(i in 1:nrow(ONI)){
  for(j in 2:ncol(ONI)){
    ONI.Mat = rbind(ONI.Mat, c(as.numeric(paste0(ONI[i,1], "0", j-1)), ONI[i,j]))
  }
}

ONI.Mat = ONI.Mat[-c(836:840),]

k.lag = 0
ONI.cor = NULL
for(j in 1:nrow(Austra[[1]])){
  Tmp.Vec = NULL
  for(i in 1:length(Austra)){
    Tmp.Vec = c(Tmp.Vec, Austra[[i]][j,3])
  }
  ONI.cor = c(ONI.cor, cor(Tmp.Vec, ONI.Mat[(603- k.lag):(833 - k.lag),2] * 10))
  if(j %% 1000 == 0){
    print(j)
  }
}

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor, cex = 0.5, pch = 19, add = TRUE)


myts = ts(ONI.Mat[,2], start = c(1950, 1), end = c(2019, 6), frequency = 12)
plot(myts)
ONI.ar = ar(myts, method = "ols")
ONI.ar

f2 = NULL
for(j in 0:100){
  f1 = ONI.ar$x.intercept
  for(i in 1:ONI.ar$order){
    f1 = f1 + ONI.ar$ar[i] * myts[835 - j - i]
  }
  f2 = c(f1,f2)
}

plot(f2, myts[(835 - 100):835], cex = 0.5, pch = 19)

ONI.ar = ar(myts[1:735], method = "ols")
ONI.ar

f3 = NULL
for(j in 0:100){
  f1 = 0
  for(i in 1:ONI.ar$order){
    f1 = f1 + ONI.ar$ar[i] * myts[835 - j - i]
  }
  f3 = c(f1,f3)
}

plot(f3, myts[(835 - 100):835], cex = 0.5, pch = 19)

f4 = myts[(735-27):735]
for(i in 0:20){
  f1 = ONI.ar$x.intercept
  for(j in 1:ONI.ar$order){
    f1 = ONI.ar$ar[j] * f4[length(f4) + 1 - j]
  }
  f4 = c(f4, f1)
}

f5 = NULL
for(i in 736:835){
  f1 = ONI.ar$x.intercept
  for(j in 1:ONI.ar$order){
    f1 = f1 + myts[i - j] * ONI.ar$ar[j]
  }
  f5 = c(f5, f1)
}

plot(f5, myts[736:835], cex = 0.2)

f6 = NULL
for(i in 736:835){
  f.1 = myts[(i - 2):(i - 29)]
  f1 = ONI.ar$x.intercept
  for(j in 1:ONI.ar$order){
    f1 = f1 + f.1[j] * ONI.ar$ar[j]
  }
  f.1 = c(f1, f.1)
  for(j in 1:ONI.ar$order){
    f1 = f1 + f.1[j] * ONI.ar$ar[j]
  }
  f6 = c(f6, f1)
}

plot(f6, myts[736:835], cex = 0.2)


ONI.vec = ONI.Mat[,2]
X.ar = NULL
k.lag = 3
p.lag = 50
for(i in 1:100){
  X.ar = rbind(X.ar, c(1, ONI.vec[(i + k.lag):(i + k.lag + (p.lag - 1))]))
}

phi.hat = solve(t(X.ar) %*% X.ar) %*% t(X.ar) %*% ONI.vec[1:100]

Y.hat = X.ar %*% phi.hat
plot(Y.hat, ONI.vec[1:100], cex = 0.2)


plot(ONI.vec1, cex = 0.2)

ONI.vec1 = rep(0, length(ONI.vec))
for(i in 1:length(ONI.vec)){
  if(ONI.vec[i] > 0.5){
    k = 1
  }else if(ONI.vec[i] < -0.5){
    k = -1
  }else{
    k = 0
  }
  ONI.vec1[i] = k
}

plot(ONI.vec1, cex = 0.2)
Nino = rep(1, length(1:(length(ONI.vec1))))
for(i in 5:(length(ONI.vec1) - 5)){
  if(sum(ONI.vec1[i:(i + 4)]) == 5 | sum(ONI.vec1[(i - 1):(i + 3)]) == 5 | sum(ONI.vec1[(i - 2):(i + 2)]) == 5 | sum(ONI.vec1[(i - 3):(i + 1)]) == 5 | sum(ONI.vec1[(i - 4):(i)] == 5 )){
    Nino[i] = 4
  }
  if(sum(ONI.vec1[i:(i + 4)]) == -5 | sum(ONI.vec1[(i - 1):(i + 3)]) == -5 | sum(ONI.vec1[(i - 2):(i + 2)]) == -5 |
     sum(ONI.vec1[(i - 3):(i + 1)]) == -5 | sum(ONI.vec1[(i - 4):(i)] == -5 )){
    Nino[i] = 3
  }
}

Nino
Nina

plot(ONI.vec, col = Nino, cex = 0.2)



k.lag = 0
ONI.cor = NULL
for(j in 1:nrow(Austra[[1]])){
  Tmp.Vec = NULL
  for(i in 1:length(Austra)){
    Tmp.Vec = c(Tmp.Vec, Austra[[i]][j,3])
  }
  ONI.cor = c(ONI.cor, cor(Tmp.Vec, ONI.vec1[(603- k.lag):(833 - k.lag)]))
  if(j %% 1000 == 0){
    print(j)
  }
}


plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor, cex = 0.5, pch = 19, add = TRUE)


M1 = matrix(0, nrow = nrow(Austra[[1]]), ncol = length(Austra))
for(i in 1:length(Austra)){
  M1[,i] = Austra[[i]][,3]
  print(i)
}

dim(M1)
ME = rowMeans(M1)


IOD = read.table("IODdata.txt", header = FALSE)

IOD.Mat = NULL
for(i in 1:nrow(IOD)){
  for(j in 2:ncol(IOD)){
    IOD.Mat = rbind(IOD.Mat, c(as.numeric(paste0(IOD[i,1], "0", j-1)), IOD[i,j]))
  }
}

IOD.Mat = IOD.Mat[-nrow(IOD.Mat),]
IOD.Mat = IOD.Mat[-(1:1008),]

IOD.ts = ts(IOD.Mat[,2], start = c(1870, 1), end = c(2018, 11), frequency = 12)

IOD.ar = ar(IOD.ts, method = "ols")
IOD.ar
plot(IOD.ts)

f2 = NULL
for(j in 0:100){
  f1 = IOD.ar$x.mean
  for(i in 1:IOD.ar$order){
    f1 = f1 + IOD.ar$ar[i] * IOD.ts[length(IOD.ts) - j - i]
  }
  f2 = c(f1,f2)
}


plot(f2, IOD.ts[(length(IOD.ts) - 100):length(IOD.ts)], cex = 0.5, pch = 19)

k.lag = 0
IOD.cor = NULL
for(j in 1:nrow(Austra[[1]])){
  Tmp.Vec = NULL
  for(i in 1:(length(Austra) - 6)){
    Tmp.Vec = c(Tmp.Vec, Austra[[i]][j,3])
  }
  IOD.cor = c(IOD.cor, cor(Tmp.Vec, IOD.Mat[(555 - k.lag):(779 - k.lag),2]))
  if(j %% 1000 == 0){
    print(j)
  }
}

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor, cex = 0.5, pch = 19, add = TRUE)

#Seasonal 1 anomaly 

M1 = S1[[1]]
ME1 = rowMeans(M1)
M.1 = matrix(0, nrow = nrow(M1), ncol = ncol(M1))
for(i in 1:nrow(M1)){
  M.1[i,] = M1[i,] > ME1[i]
}
nrow(M.1)

IOD.S1 = IOD.Mat[554 + seq(1,220,12),]
ONI.S1 = ONI.Mat[600 + seq(1,220, 12),]


IOD.cor1 = rep(0, nrow(M.7))
for(j in 1:nrow(M1)){
  IOD.cor1[j] = cor(M1[j,], IOD.S1[,2])
}

ONI.cor1 = rep(0, nrow(M1))
for(j in 1:nrow(M1)){
  ONI.cor1[j] = cor(M1[j,], ONI.S1[,2])
}
par(mfrow = c(1,1))

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor1, cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor1, cex = 0.5, pch = 19, add = TRUE)

#Seasonal 2 anomaly 

M2 = S1[[2]]
ME2 = rowMeans(M2)
M.2 = matrix(0, nrow = nrow(M2), ncol = ncol(M2))
for(i in 1:nrow(M2)){
  M.2[i,] = M2[i,] > ME2[i]
}
nrow(M.2)

IOD.S2 = IOD.Mat[554 + seq(1,217,12) + 1,]

IOD.cor2 = rep(0, nrow(M.7))
for(j in 1:nrow(M.2)){
  IOD.cor2[j] = cor(M.2[j,], IOD.S2[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor2, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 3 anomaly 

M3 = S1[[3]]
ME3 = rowMeans(M3)
M.3 = matrix(0, nrow = nrow(M3), ncol = ncol(M3))
for(i in 1:nrow(M3)){
  M.3[i,] = M3[i,] > ME3[i]
}
nrow(M.3)

IOD.S3 = IOD.Mat[554 + seq(1,217,12) + 2,]

IOD.cor3 = rep(0, nrow(M.7))
for(j in 1:nrow(M.3)){
  IOD.cor3[j] = cor(M.3[j,], IOD.S3[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor3, cex = 0.5, pch = 19, add = TRUE)

#Seasonal 4 anomaly 

M4 = S1[[4]]
ME4 = rowMeans(M4)
M.4 = matrix(0, nrow = nrow(M4), ncol = ncol(M4))
for(i in 1:nrow(M4)){
  M.4[i,] = M4[i,] > ME4[i]
}
nrow(M.4)

IOD.S4 = IOD.Mat[554 + seq(1,217,12) + 3,]

IOD.cor4 = rep(0, nrow(M.7))
for(j in 1:nrow(M.4)){
  IOD.cor4[j] = cor(M4[j,], IOD.S4[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor4, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 5 anomaly 

M5 = S1[[5]]
ME5 = rowMeans(M5)
M.5 = matrix(0, nrow = nrow(M5), ncol = ncol(M5))
for(i in 1:nrow(M5)){
  M.5[i,] = M5[i,] > ME5[i]
}
nrow(M.5)

IOD.S5 = IOD.Mat[554 + seq(1,217,12) + 4,]

IOD.cor5 = rep(0, nrow(M.7))
for(j in 1:nrow(M.5)){
  IOD.cor5[j] = cor(M5[j,], IOD.S5[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor5, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 6 anomaly 

M6 = S1[[6]]
ME6 = rowMeans(M6)
M.6 = matrix(0, nrow = nrow(M6), ncol = ncol(M6))
for(i in 1:nrow(M6)){
  M.6[i,] = M6[i,] > ME6[i]
}
nrow(M.6)

IOD.S6 = IOD.Mat[554 + seq(1,217,12) + 5,]

IOD.cor6 = rep(0, nrow(M.7))
for(j in 1:nrow(M.6)){
  IOD.cor6[j] = cor(M6[j,], IOD.S6[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor6, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 7 anomaly 

M7 = S1[[7]]
ME7 = rowMeans(M7)
M.7 = matrix(0, nrow = nrow(M7), ncol = ncol(M7))
for(i in 1:nrow(M7)){
  M.7[i,] = M7[i,] > ME7[i]
}
nrow(M.7)

IOD.S7 = IOD.Mat[554 + seq(1,217,12) + 6,]

IOD.cor7 = rep(0, nrow(M.7))
for(j in 1:nrow(M.7)){
  IOD.cor7[j] = cor(M7[j,], IOD.S7[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor7, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 8 anomaly 

M8 = S1[[8]]
ME8 = rowMeans(M8)
M.8 = matrix(0, nrow = nrow(M8), ncol = ncol(M8))
for(i in 1:nrow(M8)){
  M.8[i,] = M8[i,] > ME8[i]
}
nrow(M.8)

IOD.S8 = IOD.Mat[554 + seq(1,217,12) + 7,]

IOD.cor8 = rep(0, nrow(M.8))
for(j in 1:nrow(M.8)){
  IOD.cor8[j] = cor(M8[j,], IOD.S8[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor8, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 9 anomaly 

M9 = S1[[9]]
ME9 = rowMeans(M9)
M.9 = matrix(0, nrow = nrow(M9), ncol = ncol(M9))
for(i in 1:nrow(M9)){
  M.9[i,] = M9[i,] > ME9[i]
}
nrow(M.9)

IOD.S9 = IOD.Mat[554 + seq(1,217,12) + 8,]

IOD.cor9 = rep(0, nrow(M.9))
for(j in 1:nrow(M.9)){
  IOD.cor9[j] = cor(M9[j,], IOD.S9[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor9, cex = 0.5, pch = 19, add = TRUE)

#Seasonal 10 anomaly 

M10 = S1[[10]][,1:18]
ME10 = rowMeans(M10)
M.10 = matrix(0, nrow = nrow(M10), ncol = ncol(M10))
for(i in 1:nrow(M10)){
  M.10[i,] = M10[i,] > ME10[i]
}
nrow(M.10)

IOD.S10 = IOD.Mat[554 + seq(1,205,12) + 9,]

IOD.cor10 = rep(0, nrow(M.10))
for(j in 1:nrow(M.10)){
  IOD.cor10[j] = cor(M10[j,], IOD.S10[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor10, cex = 0.5, pch = 19, add = TRUE)


#Seasonal 11 anomaly 

M11 = S1[[11]][,1:18]
ME11 = rowMeans(M11)
M.11 = matrix(0, nrow = nrow(M11), ncol = ncol(M11))
for(i in 1:nrow(M11)){
  M.11[i,] = M11[i,] > ME11[i]
}
nrow(M.11)

IOD.S11 = IOD.Mat[554 + seq(1,205,12) + 10,]

IOD.cor11 = rep(0, nrow(M.11))
for(j in 1:nrow(M.11)){
  IOD.cor11[j] = cor(M11[j,], IOD.S11)
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor11, cex = 0.5, pch = 19, add = TRUE)



#Seasonal 12 anomaly 

M12 = S1[[12]][,1:18]
ME12 = rowMeans(M12)
M.12 = matrix(0, nrow = nrow(M12), ncol = ncol(M12))
q.1 = apply(M12, 1, quantile, probs = 0.8)
for(i in 1:nrow(M12)){
  M.12[i,] = M12[i,] > q.1[i]
}
nrow(M.12)

IOD.S12 = IOD.Mat[554 + seq(1,205,12) + 11,]

IOD.cor12 = rep(0, nrow(M.12))
for(j in 1:nrow(M.12)){
  IOD.cor12[j] = cor(M12[j,], IOD.S12[,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor12, cex = 0.5, pch = 19, add = TRUE)




M.Tot = NULL
for(i in 1:18){
  M.Tot = cbind(M.Tot, M.1[,i], M.2[,i], M.3[,i], M.4[,i], M.5[,i], M.6[,i], M.7[,i], M.8[,i], M.9[,i], M.10[,i], M.11[,i], M.12[,i])
}

IOD.tot = IOD.Mat[555:770,]


IOD.S1 = IOD.Mat[554 + seq(1,217,12),2]
IOD.S2 = IOD.Mat[554 + seq(1,217,12) + 1,2]
IOD.S3 = IOD.Mat[554 + seq(1,217,12) + 2,2]
IOD.S4 = IOD.Mat[554 + seq(1,217,12) + 3,2]
IOD.S5 = IOD.Mat[554 + seq(1,217,12) + 4,2]
IOD.S6 = IOD.Mat[554 + seq(1,217,12) + 5,2]
IOD.S7 = IOD.Mat[554 + seq(1,217,12) + 6,2]
IOD.S8 = IOD.Mat[554 + seq(1,217,12) + 7,2]
IOD.S9 = IOD.Mat[554 + seq(1,217,12) + 8,2]
IOD.S10 = IOD.Mat[554 + seq(1,205,12) + 9,2]
IOD.S11 = IOD.Mat[554 + seq(1,205,12) + 10,2]
IOD.S12 = IOD.Mat[554 + seq(1,205,12) + 11,2]

IOD.tot = c(IOD.S1, IOD.S2, IOD.S3, IOD.S4, IOD.S5, IOD.S6, IOD.S7, IOD.S8, IOD.S9, IOD.S10 , IOD.S11, IOD.S12)
IOD.cor.tot = c(IOD.cor1, IOD.cor2, IOD.cor3, IOD.cor4, IOD.cor5, IOD.cor6, IOD.cor7, IOD.cor8, IOD.cor9, IOD.cor10, IOD.cor11, IOD.cor12)

range(IOD.cor.tot)

IOD.cor = rep(0, nrow(M.Tot))
for(j in 1:nrow(M.Tot)){
  IOD.cor[j] = cor(M.Tot[j,], IOD.tot)
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "March")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor1, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "April")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor2, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "May")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor3, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "June")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor4, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "July")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor5, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "August")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor6, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "September")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor7, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "October")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor8, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "November")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor9, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "December")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor10, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "January")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor11, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "Febraury")
scatter2D(c(Austra[[1]][,2], 0, 0), c(Austra[[1]][,1], 0, 0), colvar = c(IOD.cor12, min(IOD.cor.tot), max(IOD.cor.tot)), cex = 0.5, pch = 19, add = TRUE)



which(IOD.cor7 == max(IOD.cor7))

S.1 = S1[[7]][IOD.cor7 > 0.5 | IOD.cor7 < -0.5,]


fit1 = arima(S1[[8]][18750,], xreg = IOD.S8[,2], order = c(15,0,0))

fit1$residuals

ar1 = arima(S1[[7]][1,], method = "ML", order = c(9,0,0))


ar.coef = NULL
ar.fit = NULL
for(i in 1:nrow(S.1)){
  tryCatch({
  fit1 = arima(S.1[i,], xreg = IOD.S7[,2], order = c(10,0,0))
  ar.coef = rbind(ar.coef, fit1$coef)
  ar.fit = rbind(ar.fit, fit1$residuals)
  }, error = function(e){})
  if(i %% 1000 == 0){
    print(i)
  }
}



plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "Febraury")
scatter2D(c(Austra[[1]][IOD.cor7 > 0.5 | IOD.cor7 < -0.5,2]), c(Austra[[1]][IOD.cor7 > 0.5 | IOD.cor7 < -0.5,1]), colvar = ar.coef[,1], cex = 0.5, pch = 19, add = TRUE)

library(gstat)













