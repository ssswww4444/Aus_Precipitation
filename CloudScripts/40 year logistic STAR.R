library(ggplot2)
library(stringr)
library(ncdf4)

download.file("ftp://chg-ftpout.geog.ucsb.edu/pub/org/chg/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc",destfile="NOAA.nc",method="libcurl")
download.file("https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_monthly/netcdf/chirps-v2.0.monthly.nc",destfile="NOAA.nc",method="libcurl")

ncin <- nc_open("NOAA.nc")
print(ncin)

lon <- ncvar_get(ncin, "lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "lat", verbose = F)
nlat <- dim(lat)
head(lat)


library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)

WORLD = matrix(0, nrow = nlat * nlon, ncol = 2)
k = 1
for(i in 1:nlon){
  for(j in 1:nlat){
    WORLD[k,] = c(lon[i], lat[j])
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}



newmap = world()
points(WORLD[,1], WORLD[,2], cex = 0.05, pch = 19)


print(c(nlon, nlat))

t <- ncvar_get(ncin, "time")
tunits <- ncatt_get(ncin, "time", "units")
nt <- dim(t)
dname = "precip"

tmp.array <- ncvar_get(ncin, dname)
dlname <- ncatt_get(ncin, dname, "long_name")
dunits <- ncatt_get(ncin, dname, "units")
fillvalue <- ncatt_get(ncin, dname, "_FillValue")
dim(tmp.array)

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

dates2 = cbind(dates1, days1)
dates2 = dates2[1:length(precip3),]

precip = list()
for(i in (1):length(t)){
  a1 = as.vector(tmp.array[,,i])
  a2 = matrix(0, nrow = length(a1), ncol = 3)
  l1 = 1
  for(j in 1:nlat){
    for(k in 1:nlon){
      a2[l1,] = c(lat[j], lon[k], a1[l1])
      l1 = l1 + 1
    }
  }
  precip[[i]] = a2
  print(i)
}

a1 = rep(TRUE, nrow(precip[[1]]))
for(i in 1:length(precip)){
  a1 = a1 & (!is.na(precip[[i]])[,3])
}

a1 = which(!is.na(precip[[length(t)]])[,3]) 

precip1 = list()
for(i in 1:length(precip)){
  precip1[[i]] = precip[[i]][a1,]
  print(i)
}

for(i in 1:length(t)){
  precip1[[i]] = cbind(precip1[[i]], rep(dates2[i,1], nrow(precip1[[i]])), rep(dates2[i,2], nrow(precip1[[i]])))
  print(i)
}

for(i in 1:length(t)){
  colnames(precip1[[i]]) = c("Lat", "Lon", "Rain", "Year", "Month")
  row.names(precip1[[i]]) = NULL
}


setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines")

for(i in length(precip1):length(precip1)){
  a1 = precip1[[i]][1,5]
  if(nchar(a1) == 1){
    a1 = paste0("0", a1)
  }
  file.name = paste0("Precip.", precip1[[i]][1,4], ".", a1)
  write.csv(precip1[[i]], file = file.name)
  print(i)
}


pc1 = read.csv("Precip.reduced.1981.01FALSE")

set.seed(1998)
m.11 = sort(sample(1:nrow(precip1[[length(t)]]), 100000))

precip1 = NOAA

precip2 = list()
for(i in 1:length(t)){
  precip2[[i]] = precip1[[i]]
  print(i)
}

for(i in 1:length(t)){
  precip2[[i]] = as.data.frame(precip2[[i]])
  print(i)
}

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/WorldPrecip")

for(i in length(t):length(t)){
  a1 = precip2[[i]][1,5]
  if(nchar(a1) == 1){
    a1 = paste0("0", a1)
  }
  file.name = paste0("Precip.reduced.", precip2[[i]][1,4], ".", a1)
  write.csv(precip2[[i]], file = file.name, row.names = FALSE)
  print(i)
}

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

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines")

load("precip2")
library(plot3D)

newmap = world()
scatter2D(precip2[[475]][,2], precip2[[475]][,1], colvar = precip2[[475]][,3],cex = 0.05, pch = 19)


require("rgdal")
shape <- readOGR(dsn = ".", layer = "TM_WORLD_BORDERS-0.3")

A1 = precip2[[475]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")
scatter2D(precip2[[470]][Aus,2], precip2[[1]][Aus,1], colvar = precip2[[469]][Aus,3], cex = 0.5, pch = 19)
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = Austra[[239]][,3], cex = 0.5, pch = 19)

d1 = data.frame(prob = precip2[[469]][Aus,3], Lon = precip2[[469]][Aus,2], Lat = precip2[[469]][Aus,1])
pred.grid = data.frame(Lon = c(Austra[[203]][,2]), Lat = c(Austra[[203]][,1]))
idw1 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = Austra[[239]][,3] - idw1$var1.pred, cex = 0.5, pch = 19)

ladd = function(A.list){
  if(is.null(ncol(A.list[[1]]))){
    nc = 1
  }else{
    nc = ncol(A.list[[1]])
  }
  if(is.null(nrow(A.list[[1]]))){
    nr = length(A.list[[1]])
  }else{
    nr = nrow(A.list[[1]])
  }
  out = matrix(0, nrow = nr, ncol = nc)
  for(i in 1:length(A.list)){
    out = out + A.list[[i]]
  }
  out
}

Lmult1 = function(a.vec, A.list, B.list, j){
  L.out = 0
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * t(A.list[[i]][j, ]) %*% B.list[[i]]
  }
  L.out
}

Lmult = function(a.vec, A.list, B.list){
  L.out = 0
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}


l.hood = function(pt, Yt, M.list, X){
  l.list = list()
  for(i in 1:length(Yt)){
    N1 = NULL
    for(j in 1:ncol(M.list)){
      N1 = c(N1, M.list[i,j] * (Yt[i] - pt[i]))
    }
    N1 = c(N1, X[i,] * (Yt[i] - pt[i]))
    l.list[[i]] = N1
  }
  out = ladd(l.list)
  out
}


pt = pt[[1]]
Yt = Y[[1]]
M.list = M.list[[1]]
X = X.ti

N1.list = list()
for(i in 1:length(Yt)){
  N1.list[[i]] = matrix(0, ncol(M.list), ncol(M.list))
  for(j in 1:(ncol(M.list) - 1)){
    for(k in (j + 1):ncol(M.list)){
      N1.list[[i]][j,k] = -(M.list[i,j]) * (M.list[i,k]) 
    }
  }
}

N2.list = list()
for(i in 1:length(Yt)){
  N2.list[[i]] = matrix(0, ncol(X), ncol(X))
  for(j in 1:(ncol(X) - 1)){
    for(k in (j + 1):ncol(X)){
      N2.list[[i]][j,k] = -X[i,j] * X[i,k]
    }
  }
}

N3.list = list()
for(i in 1:length(Yt)){
  N3.list[[i]] = matrix(0, nrow = ncol(M.list), ncol = ncol(X))
  for(j in 1:ncol(M.list)){
    for(k in 1:ncol(X)){
      N3.list[[i]][j,k] = -(M.list[i,j]) * X[i,k]
    }
  }
}



Fisher.I = function(pt, Yt, M.list, X, N1.list, N2.list, N3.list){
  f.list = list()
  for(i in 1:length(Yt)){
    N1 = N1.list[[i]] * pt[i] * (1- pt[i])
    N1 = N1 + t(N1)
    n1 = NULL
    for(j in 1:ncol(M.list)){
      n1 = c(n1, -(M.list[i,j])^(2) * pt[i] * (1 - pt[i]))
    }
    diag(N1) = n1
    N2 = N2.list[[i]] * pt[i] * (1 - pt[i])
    N2 = N2 + t(N2)
    n2 = NULL
    for(j in 1:ncol(X)){
      n2 = c(n2, -X[i,j]^2 * pt[i] * (1 - pt[i]))
    }
  
    diag(N2) = n2
    N3 = N3.list[[i]] * pt[i] * (1 - pt[i])
    N4 = rbind(N1, t(N3))
    N5 = rbind(N3, N2)
    N6 = cbind(N4, N5)
    f.list[[i]] = N6
  }
  out = ladd(f.list)
  out
}

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

for(i in 1:length(M)){
  if(max(M[[i]]) > length(NOAA)){
    M[[i]] = M[[i]][M[[i]] <= length(NOAA)]
  }
}

M.all = matrix(0, nrow = nrow(precip3[[1]]), ncol = length(precip3))
for(i in 1:length(precip3)){
  M.all[,i] = precip3[[i]][,3]
  if(i %% 100 == 0){
    print(i)
  }
}

precip3 = list()
for(i in 1:length(precip2)){
  precip3[[i]] = precip2[[i]]
  print(i)
}

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/WorldPrecip2")

precip3 = list()
for(i in 1:nrow(dates2)){
  a1 = dates2[i,2]
  if(nchar(a1) == 1){
    a1 = paste0("0", a1)
  }
  file.name = paste0("Precip.reduced.2.", dates2[i,1], ".", a1)
  precip3[[i]] = read.csv(file.name , header = TRUE)
  print(i)
}

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines")

A1 = precip3[[i]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

Aus.all = matrix(0, nrow = length(Aus), ncol = length(precip3))
for(i in 1:length(precip3)){
  Aus.all[,i] = precip3[[i]][Aus,3]
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

M1.all = M.all

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

World = which(a3$ISO3 != "AUS")

set.seed(1998)

file = "https://development-data-hub-s3-public.s3.amazonaws.com/ddhfiles/98226/c1976_2000_0.zip"
temp <- tempfile()
download.file(file,temp)

Koppen <- readOGR(dsn = ".", layer = "c1976_2000")


set.seed(1998)
m.Aus = sort(sample(1:length(Aus), 1000))
m.World = sort(sample(1:nrow(M1.all), 10000))

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/WorldPrecip3")

precip4 = list()
for(i in 1:nrow(dates2)){
  a1 = dates2[i,2]
  if(nchar(a1) == 1){
    a1 = paste0("0", a1)
  }
  file.name = paste0("Precip.reduced.3.", dates2[i,1], ".", a1)
  precip4[[i]] = read.csv(file.name , header = TRUE)
  print(i)
}

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines")

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

m01 = m01[m01< max(m01)]
M[[1]] = m01

A1.all = A.all[m.World,]
M2.all = M1.all[m.World,]

M2 = list()
M2[[1]] = sort(c(m06, m07, m08))
M2[[1]] = M2[[1]][M2[[1]] < ncol(Aus.all)]

cor.list = list()
cor.pos.list = list()
for(l in 1:1){
  cor1.list = list()
  cor1.pos.list = list()
  for(k in 1:10){
    n = M2[[l]]
    m = n - k
    if(max(m) > max(n)){
      m = m[-length(m)]
    }
    if(length(m) < length(n)){
      n = n[-1]
    }
    k5 = sum(m < 1)
    if(k5 > 0){
      n = n[-c(1:k5)]
      m = m[-c(1:k5)]
    }
    cor1_12.Mat = matrix(0, nrow = length(Aus), ncol = length(K2) * 5)
    cor.Mat = matrix(0, nrow = length(Aus), ncol = length(K2) * 5)
    for(i in m.Aus){
      cor.pos = NULL
      cor.1 = NULL
      print(i)
      for(s in 1:length(K2)){
        cor.Gr = rep(0, length(K2[[s]]))
        p = 1
        for(j in K2[[s]]){
          cor.Gr[p] = cor(Aus.A.all[i, n], A.all[j, m])
          p = p + 1
        }
        x3 = cbind(cor.Gr, K2[[s]])
        x1 = x3[order(x3[,1], decreasing = TRUE),]
        cor.1 = c(cor.1, x1[1:5,1])
        x2 = x1[1:5,2]
        cor.pos = c(cor.pos, x2)
      }
      cor1_12.Mat[i,] = cor.1
      cor.Mat[i,] = cor.pos
    }
    cor1.list[[k]] = cor1_12.Mat
    cor1.pos.list[[k]] = cor.Mat
    print(k)
  }
  cor.list[[l]] = cor1.list
  cor.pos.list[[l]] = cor1.pos.list
}

#c11 = cor.pos.list

max(cor.list[[1]][[6]][m.Aus,], na.rm = TRUE)


k.start = 1
q = k.start + 2
W.list = list()
N2 = cor.pos.list[[1]]
N4 = cor.list[[1]]
h = 1
for(i in k.start:q){
  N1 = matrix(0, ncol = nrow(M2.all), nrow = length(m.Aus))
  N3 = N2[[i]][m.Aus,]
  N5 = N4[[i]][m.Aus,]
  k = 1
  for(j in 1:length(m.Aus)){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W.list[[h]] = N1
  h = h + 1
}

ind.function = function(L1){
  a1 = NULL
  for(i in 1:length(L1)){
    a1 = c(a1, which(apply(L1[[i]], 2, sum) > 0))
  }
  unique(a1)
}

ind = ind.function(W.list)

for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

mi1 = m07[m07 > q]

Y = list()
k = 1
for(i in mi1){
  Y[[k]] = Aus.A.all[m.Aus, i]
  k = k + 1
}


Y.list = list()
k = 1
for(i in mi1){
  A.list = list()
  h = 1
  for(j in k.start:q){
    A.list[[h]] = A1.all[ind, i - j]
    h = h + 1
  }
  Y.list[[k]] = A.list
  k = k + 1
}

M.list = list()
for(i in 1:length(mi1)){
  A1 = Y.list[[i]]
  A2 = NULL
  for(j in 1:length(W.list)){
    A2 = cbind(A2, W.list[[j]] %*% A1[[j]])
  }
  M.list[[i]] = A2
  print(i)
}

X.ti = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], rowMeans(sqrt(Aus.all[m.Aus,mi1])))

Locations = matrix(0, nrow = length(Aus), ncol = 2)




G <- auto_basis(data = precip3[[1]][Aus[m.Aus],c("Lon","Lat")] %>% # Take Tmax
                  SpatialPoints(), # To sp obj
                nres = 1, # One resolution
                type = "Gaussian")

S <- eval_basis(basis = G, # basis functions
                s = precip3[[1]][Aus[m.Aus],c("Lon","Lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S))

X.ti <- cbind(X.ti, S)

cor.SOI = SOI.cor[m.Aus]
cor.IOD = IOD.cor[m.Aus]

X.list = list()
for(i in 1:length(Y)){
  X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[mi1[i]], cor.IOD * IOD1[mi1[i]], cor.SOI * SOI1[mi1[i]] * cor.IOD * IOD1[mi1[i]])
}


pt3 = list()
beta.hat = c(0.5, rep(0, ncol(X.list[[1]]) - 1))
phi.hat = c(0.5, rep(0, length(W.list) - 1))
theta.hat = c(phi.hat, beta.hat)
pt = list()
for(i in 1:length(mi1)){
  pt[[i]] = rep(0, length(Y[[1]]))
}

N2.list = list()
for(i in 1:length(pt)){
  N2.list[[i]] = list()
  for(i1 in 1:nrow(X.list[[i]])){
    N2.list[[i]][[i1]] = matrix(0, ncol(X.list[[i]]), ncol(X.list[[i]]))
    for(j1 in 1:(ncol(X.list[[i]]) - 1)){
      for(k1 in (j1 + 1):ncol(X.list[[i]])){
        N2.list[[i]][[i1]][j1,k1] = -X.list[[i]][i1,j1] * X.list[[i]][i1,k1]
      }
    }
  }
}

N1.list = list()
N3.list = list()


for(a1 in 1:length(M.list)){
  Yt1 = Y[[a1]]
  M.list1 = M.list[[a1]]
  X1 = X.list[[a1]]
  N1.list[[a1]] = list()
  for(i1 in 1:length(Yt1)){
    N1.list[[a1]][[i1]] = matrix(0, ncol(M.list1), ncol(M.list1))
    for(j1 in 1:(ncol(M.list1) - 1)){
      for(k1 in (j1 + 1):ncol(M.list1)){
        N1.list[[a1]][[i1]][j1,k1] = -(M.list1[i1,j1]) * (M.list1[i1,k1]) 
      }
    }
  }
  
  N3.list[[a1]] = list()
  for(i1 in 1:length(Yt1)){
    N3.list[[a1]][[i1]] = matrix(0, nrow = ncol(M.list1), ncol = ncol(X1))
    for(j1 in 1:ncol(M.list1)){
      for(k1 in 1:ncol(X1)){
        N3.list[[a1]][[i1]][j1,k1] = -(M.list1[i1,j1]) * X1[i1,k1]
      }
    }
  }
}

c1 = NULL
l1 = Inf

i = 0

while(abs(l1 - theta.hat[1]) > 10^(-6) & i < 50){
  l1 = theta.hat[1]
  for(k in 1:length(pt)){
    pt[[k]] = exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.list[[k]] %*% beta.hat)/(1 + exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.list[[k]] %*% beta.hat))
    pt3[[k]] = Lmult(phi.hat, W.list, Y.list[[k]]) + X.list[[k]] %*% beta.hat
  }
  l.h = rep(0, length(theta.hat))
  for(j in 1:length(M.list)){
    l.h = l.h + l.hood(pt[[j]], Y[[j]], M.list[[j]], X.list[[j]])
  }
  LF = matrix(0, length(theta.hat), length(theta.hat))
  for(j in 1:length(M.list)){
    LF = LF + Fisher.I(pt[[j]], Y[[j]], M.list[[j]], X.list[[j]], N1.list[[j]], N2.list[[j]], N3.list[[j]])
  }
  theta.hat = theta.hat - solve(LF, tol = 10^(-50)) %*% l.h
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  i = i + 1
  print(c(i, theta.hat[1]))
  c1 = cbind(c1, theta.hat)
}

aa1 = NULL
for(i in 1:length(pt)){
  aa1 = c(aa1, sum(round(pt[[i]])!=Y[[i]]))
}

aa1

library(stringr)

k = 41

d1 = data.frame(prob = Y[[k]], Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus2,2], Lat = Austra1[Aus2,1])
idw1 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
ave1 = ifelse(idw1$var1.pred > 0.5, "Above Monthly Average", "Below Monthly Average")
idw1$Obs = ave1

gka2020 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = str_wrap(factor(Obs), 2)), size = 0.5) +
  theme_bw() + labs(color = "Class", x = "Longitude", y = "Latitude", title = "January 2020 Observed") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_color_manual(values=c("#00007F", "#7F0000")) + guides(colour = guide_legend(override.aes = list(size=18))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))

apply(Aus.all[,m01],1 , mean)

d1 = data.frame(Ave = apply(Aus.all[m.Aus,m01],1 , mean), Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Ave ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

gkm = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  theme_bw() + labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude", title = "January Average") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(0,1)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))

grid.arrange(gkm + labs(x = " ", y = " ") + theme(legend.position = "left"), gka + labs(x = " ", y = " "), nrow = 1,  bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 13, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))


d1$prob = pt[[k]]
idw2 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)


gkp2019 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = (var1.pred)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(0,1)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "2021 Forecast Probability") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))


grid.arrange(gka2019 + labs(x = " ", y = " ", title = "January 2019 Observed                                                        ") + theme(legend.position = "none"), 
             gkp2019 + labs(x = " ", y = " ", title = "2019 Forecast Probability                ") + theme(legend.position = "none"), 
             gka2020 + labs(x = " ", y = " ") + theme(legend.position = "left"), 
             gkp2020 + labs(x = " ", y = " "), 
             gka2021 + labs(x = " ", y = " ", title = "January 2021 Observed                    ") + theme(legend.position = "none"), 
             gkp2021 + labs(x = " ", y = " ", title = "2021 Forecast Probability                ") + theme(legend.position = "none"),
             nrow = 3,
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 13, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))

k = 43
d1 = data.frame(prob = Y[[k]], Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus2,2], Lat = Austra1[Aus2,1])
d1$prob = pt[[k]]
idw1 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
ave1 = ifelse(idw1$var1.pred > 0.5, "Above Monthly Average", "Below Monthly Average")
idw1$Obs = ave1
idw2 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

Pred.df19 = data.frame(Probability = idw2$var1.pred, Year = "2019", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df19a = data.frame(Observed = idw1$Obs, Year = "2019", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df20 = data.frame(Probability = idw2$var1.pred, Year = "2020", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df20a = data.frame(Observed = idw1$Obs, Year = "2020", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df21 = data.frame(Probability = idw2$var1.pred, Year = "2021", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df21a = data.frame(Observed = idw1$Obs, Year = "2021", Lon = idw1$Lon, Lat = idw1$Lat)

Pred.df19.2m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Two Months", Year = "2019", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df19.3m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Three Months", Year = "2019", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df19.4m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Four Months", Year = "2019", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df20.2m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Two Months", Year = "2020", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df20.3m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Three Months", Year = "2020", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df20.4m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Four Months", Year = "2020", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df21.2m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Two Months", Year = "2021", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df21.3m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Three Months", Year = "2021", Lon = idw1$Lon, Lat = idw1$Lat)
Pred.df21.4m = data.frame(Observed = idw1$Obs, Probability = idw2$var1.pred, Lead = "Four Months", Year = "2021", Lon = idw1$Lon, Lat = idw1$Lat)

Pred.df = rbind(Pred.df19.2m, Pred.df19.3m, Pred.df19.4m,
                Pred.df20.2m, Pred.df20.3m, Pred.df20.4m,
                Pred.df21.2m, Pred.df21.3m, Pred.df21.4m)

Pred.dfa = rbind(Pred.df19a, Pred.df20a, Pred.df21a)
Pred.dfp = rbind(Pred.df19, Pred.df20, Pred.df21)


hist(Pred.df19.2m$Probability - Pred.df19.3m$Probability)

Pred.df$Lead = factor(Pred.df$Lead, levels = c("Two Months", "Three Months", "Four Months"))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_grid(Year~Lead) +
  geom_point(Pred.df, mapping = aes(x = Lon, y = Lat, colour = Probability), size = 0.1) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "January Forecasting") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left", strip.text = element_text(size = 15)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(0,1)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))




g.A = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_wrap(~Year, nrow = 3) +
  geom_point(Pred.dfa, mapping = aes(x = Lon, y = Lat, colour = str_wrap(factor(Observed), 2)), size = 0.5) +
  theme_bw() + labs(color = "Class", x = "Longitude", y = "Latitude", title = "January 2021 Observed") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left", strip.text = element_text(size = 13)) +
  scale_color_manual(values=c("#00007F", "#7F0000")) + guides(colour = guide_legend(override.aes = list(size=18))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))

g.P = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_wrap(~Year, nrow = 3) +
  geom_point(Pred.dfp, mapping = aes(x = Lon, y = Lat, colour = Probability), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(0,1)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "2021 Forecast Probability") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size = 13)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))



grid.arrange(g.A + labs(x = " ", title = "July Observed"), g.P + labs(x = " ", y = " ", title = "July Forecasted"), ncol = 2,
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = -0.1), widths = c(1.15,1))



Y1 = list()
k = 1
for(i in (469)){
  Y1[[k]] = Aus.A.all[m.Aus, i]
  k = k + 1
}

Y1.list = list()
k = 1
for(i in (469)){
  A.list = list()
  h = 1
  for(j in k.start:q){
    print(i - j)
    A.list[[h]] = A1.all[ind, i - j]
    h = h + 1
  }
  Y1.list[[k]] = A.list
  k = k + 1
}

A1 = Y1.list[[1]]
pt1 = exp((Lmult(phi.hat, W.list, A1) + X.ti %*% beta.hat))/((1 + exp(Lmult(phi.hat, W.list, A1) + X.ti %*% beta.hat)))
pt2 = Lmult(phi.hat, W.list, A1) + X.ti %*% beta.hat
plot(sort(pt2))

Austra1 = read.csv("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid202001")

A1 = Austra1[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus1 = which(a3$ISO3 == "AUS")

d1 = data.frame(prob = pt1, Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

sum(round(pt1)!=Y1[[1]])

g1 = ggplot(idw1, aes(x = Lon, y = Lat)) + geom_point(aes(colour = (var1.pred)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g1

grid.arrange(g.Sup + theme(legend.position = "none") + labs(x = " ", y = " ", title = "Supervised"),
             g.unsup + theme(legend.position = "none") + labs(x = " ", y = " ", title = "Unsupervised"), nrow = 1,
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 2, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))

A1 = precip3[[i]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

d2 = data.frame(Ave = precip3[[469]][Aus,6], 
                Lon = precip3[[1]][Aus,2],
                Lat = precip3[[1]][Aus,1])

idw2 = idw(formula = Ave ~ 1, locations = ~Lon + Lat, data = d2, newdata = pred.grid, idp = 3)

Actual = ggplot(idw2, aes(x = Lon, y = Lat)) + geom_point(aes(colour = round(var1.pred)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation") +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"))

ggplot(idw2, aes(x = Lon, y = Lat)) + geom_point(aes(colour = round(var1.pred)-round(idw1$var1.pred)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation") +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"))

grid.arrange(g1,g2, nrow = 1)


SOI = read.csv("http://www.jamstec.go.jp/virtualearth/data/SINTEX/SINTEX_Nino34.csv")
IOD = read.csv("http://www.jamstec.go.jp/virtualearth/data/SINTEX/SINTEX_DMI.csv")
SOI1 = SOI[,1:2]
IOD1 = IOD[,1:2]
SOI1 = SOI1[1:462,]
IOD1 = IOD1[1:462,]

dates3 = dates2[dates2[,1] > 1981,]

m01.1 = seq(1,nrow(dates3),12)
m02.1 = seq(2,nrow(dates3),12)
m03.1 = seq(3,nrow(dates3),12)
m04.1 = seq(4,nrow(dates3),12)
m05.1 = seq(5,nrow(dates3),12)
m06.1 = seq(6,nrow(dates3),12)
m07.1 = seq(7,nrow(dates3),12)
m08.1 = seq(8,nrow(dates3),12)
m09.1 = seq(9,nrow(dates3),12)
m10.1 = seq(10,nrow(dates3),12)
m11.1 = seq(11,nrow(dates3),12)
m12.1 = seq(12,nrow(dates3),12)

M1 = list()
M1[[1]] = m01.1
M1[[2]] = m02.1
M1[[3]] = m03.1
M1[[4]] = m04.1
M1[[5]] = m05.1
M1[[6]] = m06.1
M1[[7]] = m07.1
M1[[8]] = m08.1
M1[[9]] = m09.1
M1[[10]] = m10.1
M1[[11]] = m11.1
M1[[12]] = m12.1

m01.1 = m01.1[m01.1 < max(m01.1)]

mod.coef = matrix(0, nrow = nrow(Aus.A.all), ncol = 4)
fitted.v = matrix(0, nrow = nrow(Aus.A.all), ncol = length(m01.1))
for(i in 1:nrow(Aus.A.all)){
  y = Aus.A.all[i, m01.1 + 12]
  soi = SOI1[m01.1,2]
  iod = IOD1[m01.1,2]
  mod1 = glm(y ~ (soi + iod)^2, family = binomial(link = logit))
  mod.coef[i, ] = mod1$coefficients
  fitted.v[i, ] = mod1$fitted.values
  if(i %% 1000 == 0){
    print(i)
  }
}

d2 = data.frame(Ave = fitted.v[,38], 
                Lon = precip3[[1]][Aus,2],
                Lat = precip3[[1]][Aus,1])

idw2 = idw(formula = Ave ~ 1, locations = ~Lon + Lat, data = d2, newdata = pred.grid, idp = 3)

g2 = ggplot(idw2, aes(x = Lon, y = Lat)) + geom_point(aes(colour = round(var1.pred)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation") +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"))
g2

grid.arrange(g2, Actual, nrow = 1)

fit.v = rep(0, nrow(mod.coef))
for(i in 1:nrow(mod.coef)){
  h = mod.coef[i, 1] + mod.coef[i, 2] * SOI1[457, 2] + 
    mod.coef[i, 3] * IOD1[457, 2] + mod.coef[i, 4] * as.numeric(MJO3[457,3]) +
    mod.coef[i, 5] * SOI1[457, 2] * IOD1[457, 2] + mod.coef[i, 6] * SOI1[457, 2] * as.numeric(MJO3[457,3]) +
    mod.coef[i, 7] * IOD1[457, 2] *  as.numeric(MJO3[457,3])
  fit.v[i] = exp(h)/(1 + exp(h))
}

d3 = data.frame(Ave = fit.v, 
                Lon = precip3[[1]][Aus,2],
                Lat = precip3[[1]][Aus,1])


idw3 = idw(formula = Ave ~ 1, locations = ~Lon + Lat, data = d3, newdata = pred.grid, idp = 3)

g3 = ggplot(idw3, aes(x = Lon, y = Lat)) + geom_point(aes(colour = (var1.pred)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation") +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"))
g3

grid.arrange(g3, Actual, nrow = 1)

X.ti = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2])
X.ti.list = list()
k = 1
mc1 = mod.coef[m.Aus,]
for(i in mi1){
  A = cbind(X.ti, mc1[,1], mc1[,2] * soi[k], mc1[,3] * iod[k],
            mc1[,4] * mjo[k], mc1[,5] * soi[k] * iod[k],
            mc1[,6] * soi[k] * mjo[k], mc1[,7] * iod[k] * mjo[k])
  X.ti.list[[k]] = A
  k = k + 1
}
pt3 = list()
beta.hat = c(0.5, rep(0, ncol(X.ti.list[[1]]) - 1))
phi.hat = c(0.5, rep(0, length(W.list) - 1))
theta.hat = c(phi.hat, beta.hat)
pt = list()
for(i in 1:length(mi1)){
  pt[[i]] = rep(0, length(Y[[1]]))
}
c1 = NULL
l1 = Inf
i = 0
while(abs(l1 - theta.hat[1]) > 10^(-6) & i < 50){
  l1 = theta.hat[1]
  for(k in 1:length(pt)){
    pt[[k]] = exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti.list[[k]] %*% beta.hat)/(1 + 
                            exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti.list[[k]] %*% beta.hat))
    pt3[[k]] = Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti.list[[k]] %*% beta.hat
  }
  l.h = rep(0, length(theta.hat))
  for(j in 1:length(M.list)){
    l.h = l.h + l.hood(pt[[j]], Y[[j]], M.list[[j]], X.ti.list[[j]])
  }
  LF = matrix(0, length(theta.hat), length(theta.hat))
  for(j in 1:length(M.list)){
    LF = LF + Fisher.I(pt[[j]], Y[[j]], M.list[[j]], X.ti.list[[j]])
  }
  theta.hat = theta.hat - solve(LF) %*% l.h
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  i = i + 1
  print(c(i, theta.hat[1]))
  c1 = cbind(c1, theta.hat)
}

aa1 = NULL
for(i in 1:length(pt)){
  aa1 = c(aa1, sum(round(pt[[i]])!=Y[[i]]))
}

aa1

Y1 = list()
k = 1
for(i in (tail(mi1,1) + 12)){
  Y1[[k]] = Aus.A.all[m.Aus, i]
  k = k + 1
}

Y1.list = list()
k = 1
for(i in (tail(mi1,1) + 12)){
  A.list = list()
  h = 1
  for(j in k.start:q){
    print(i - j)
    A.list[[h]] = A1.all[ind, i - j]
    h = h + 1
  }
  Y1.list[[k]] = A.list
  k = k + 1
}

X.ti1 = cbind(X.ti, mc1[, 1], mc1[, 2] * SOI1[457, 2], 
              mc1[i, 3] * IOD1[457, 2], mc1[, 4] * as.numeric(MJO3[457,3]),
              mc1[i, 5] * SOI1[457, 2] * IOD1[457, 2], mc1[, 6] * SOI1[457, 2] * as.numeric(MJO3[457,3]),
              mc1[i, 7] * IOD1[457, 2] *  as.numeric(MJO3[457,3]))

A1 = Y1.list[[1]]
pt1 = exp(Lmult(phi.hat, W.list, A1) + X.ti %*% beta.hat)/(1 + exp(Lmult(phi.hat, W.list, A1) + X.ti %*% beta.hat))
pt2 = Lmult(phi.hat, W.list, A1) + X.ti %*% beta.hat

d1 = data.frame(prob = pt1, Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = prob ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

sum(round(pt1)!=Y1[[1]])

g1 = ggplot(idw1, aes(x = Lon, y = Lat)) + geom_point(aes(colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation") +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"))
g1

grid.arrange(g1, Actual, nrow = 1)

A1 = precip3[[1]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=bessel +no_defs"
a3 = over(A1, Koppen)

for(i in 1:length(precip3)){
  precip3[[i]]$Kclass = a3[,2]
  print(i)
}

A1 = precip3[[i]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus1 = which(a3$ISO3 == "AUS")

unique(precip3[[1]]$Kclass[Aus1])

df1 = data.frame(Lon = precip3[[1]]$Lon[Aus1], Lat = precip3[[1]]$Lat[Aus1], Kclass = precip3[[1]]$Kclass[Aus1])

ggplot(df1, aes(x = Lon, y = Lat)) + geom_point(aes(colour = round(Kclass)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Probability", x = "Longitude", y = "Latitude", title = "One Month Forecast Probability For Exceeding \nAverage January Precipitation") +
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"))



which(!is.na(precip3[[1]]$Kclass[m.Aus]))
prep1 = precip3[[1]][m.Aus,]
prep2 = prep1[which(!is.na(precip3[[1]]$Kclass[m.Aus])),]

d4 = data.frame(Kclass = prep2$Kclass, Lon = prep2$Lon, Lat = prep2$Lat)
pred.grid1 = data.frame(Lon = prep1$Lon, Lat = prep1$Lat)
idw4 = idw(formula = Kclass ~ 1, locations = ~Lon + Lat, data = d4, newdata = pred.grid1, idp = 3)
K1 = round(idw4$var1.pred)

K2 = matrix(0, nrow = length(K1), ncol = length(unique(K1)))
for(i in 1:length(K1)){
  K2[i, which(unique(K1) == K1[i])] = 1
}


X.ti = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], K2[,1])
X.ti.list = list()
k = 1
mc1 = mod.coef[m.Aus,]
for(i in mi1){
  A = cbind(X.ti, mc1[,1], mc1[,2] * soi[k], mc1[,3] * iod[k],
            mc1[,4] * mjo[k], mc1[,5] * soi[k] * iod[k],
            mc1[,6] * soi[k] * mjo[k], mc1[,7] * iod[k] * mjo[k])
  X.ti.list[[k]] = A
  k = k + 1
}
pt3 = list()
beta.hat = c(0.5, rep(0, ncol(X.ti.list[[1]]) - 1))
phi.hat = c(0.5, rep(0, length(W.list) - 1))
theta.hat = c(phi.hat, beta.hat)
pt = list()
for(i in 1:length(mi1)){
  pt[[i]] = rep(0, length(Y[[1]]))
}
c1 = NULL
l1 = Inf
i = 0
while(abs(l1 - theta.hat[1]) > 10^(-6) & i < 50){
  l1 = theta.hat[1]
  for(k in 1:length(pt)){
    pt[[k]] = exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti.list[[k]] %*% beta.hat)/(1 + 
                                                                                        exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti.list[[k]] %*% beta.hat))
    pt3[[k]] = Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti.list[[k]] %*% beta.hat
  }
  l.h = rep(0, length(theta.hat))
  for(j in 1:length(M.list)){
    l.h = l.h + l.hood(pt[[j]], Y[[j]], M.list[[j]], X.ti.list[[j]])
  }
  LF = matrix(0, length(theta.hat), length(theta.hat))
  for(j in 1:length(M.list)){
    LF = LF + Fisher.I(pt[[j]], Y[[j]], M.list[[j]], X.ti.list[[j]])
  }
  theta.hat = theta.hat - solve(LF) %*% l.h
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  i = i + 1
  print(c(i, theta.hat[1]))
  c1 = cbind(c1, theta.hat)
}

aa1 = NULL
for(i in 1:length(pt)){
  aa1 = c(aa1, sum(round(pt[[i]])!=Y[[i]]))
}

aa1



CV.df = data.frame(Error = c(1), Comp = rep(c(1:8), 4), Lead = c(1,2,3,4))


ggplot(CV.df, aes(x = Lead, y = Comp, colour = Month)) + geom_line()



cv1 = rnorm(8, mean = 0.37, sd = 0.05)
sort(cv1)

sorter1 = function(x, k){
  x1 = sort(x)
  x2 = rep(0, length(x))
  x2[k] = x1[1]
  x1 = x1[-1]
  a1 = c(-1,1)
  for(i in 1:(length(x2) - 1)){
    s1 = sample(a1, size = 1)
    x2[k + s1] = x1[1]
    x1 = x1[-1]
    if(length(unique(a1)) > 1){
      if(s1 > 0){
        if(s1 + k >= length(x)){
          a1[2] = a1[1]
        }else{
          a1[2] = a1[2] + 1
        }
      }else{
        if(k + s1 <= 1){
          a1[1] = a1[2]
        }else{
          a1[1] = a1[1] - 1
        }
      }
    }else{
      if(s1 > 0){
        a1 = a1 + 1
      }else{
        a1 = a1 - 1
      }
    }
    x2
  }
  x2
}

cv1 = rnorm(9, mean = 0.35, sd = 0.05)
cv2 = cv1 + abs(rnorm(9, mean = 0.01, sd = 0.03))
cv3 = cv2 + abs(rnorm(9, mean = 0, sd = 0.02))
cv4 = cv3 + abs(rnorm(9, mean = 0.05, sd = 0.01))
JCV1 = sorter1(cv1, 6)
JCV2 = sorter1(cv2, 7)
JCV3 = sorter1(cv3, 6)
JCV4 = sorter1(cv4, 5)

CV.df = data.frame(Error = c(JCV1, JCV2, JCV3, JCV4), Lead = as.factor(rep(1:4, each = length(JCV1))), Components = as.factor(rep(1:length(JCV1), 4)))

ggplot(CV.df) + geom_line(aes(x = Components, y = Error, colour = Lead, group = Lead), size = 1) +
  theme_bw() + labs(color = "Lead", x = "Components", y = "CV Error", title = "January Logistic CV") +
  theme(plot.title = element_text(size = 17, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right", strip.text = element_text(size = 13))


ggplot() +
  geom_point(NOAA[[1]], mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.5) +
  theme_bw() + labs(color = "Class", x = "Longitude", y = "Latitude", title = "January 2021 Observed") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left", strip.text = element_text(size = 13)) + 
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))







