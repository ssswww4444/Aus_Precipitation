"ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/2000/200004/SEMDP_GSMaP_GNRT6_0.10deg-MON_200004.nc"


# # # download data
# file1 = "ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/2000/200004/SEMDP_GSMaP_GNRT6_0.10deg-MON_200004.nc"
# download.file(file1, destfile = "NOAA/NOAA.nc2001", method = "libcurl")

ncin = nc_open("NOAA/precip.2000.nc")
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

years1 = c(rep(2000,9), rep(2001:(curr.year - 1), each = 12), if(curr.month != 1){rep(curr.year, curr.month - 1)})

months1 = c(4:12, rep(1:12, length(2001:(curr.year - 1))), if(curr.month != 1){1:(curr.month - 1)})
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

dates4 = cbind(dates1, days1)

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


JAXA = list()

for(i in 1:1){
  file1 = "ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/"
  file2 = dates4[i,1]
  file3 = dates4[i,2]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  file4 = paste0(file1, file2, "/", file2, file3, "/SEMDP_GSMaP_GNRT6_0.10deg-MON_", file2, file3, ".nc")
  download.file(file4, destfile= paste0("JAXA.nc", file2, file3), method="libcurl")
  
  ncin <- nc_open(paste0("JAXA.nc", file2, file3))
  
  lon <- ncvar_get(ncin, "lon")
  nlon <- dim(lon)
  
  lat <- ncvar_get(ncin, "lat", verbose = F)
  nlat <- dim(lat)
  
  t <- ncvar_get(ncin, "time")
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(t)
  dname = "gsmap"
  
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name")
  dunits <- ncatt_get(ncin, dname, "units")
  fillvalue <- ncatt_get(ncin, dname, "_FillValue")
  
  a1 = as.vector(tmp.array)
  WORLD[,3] = as.numeric(a1) * dates4[i,3]
  WORLD[,4] = as.numeric(rep(file2, nrow(WORLD)))
  WORLD[,5] = as.numeric(rep(file3, nrow(WORLD)))
  colnames(WORLD) = c("Latitude", "Longitude", "Precip", "Year", "Month")
  JAXA[[i]] = as.data.frame(WORLD)
  print(i)
}

JAXA = JAXA.Monthly

NJ.loc = rep(0, nrow(NOAA.aus[[1]]))
for(i in 1:nrow(NOAA.aus[[1]])){
  d1 = abs(JAXA[[1]]$Longitude - NOAA.aus[[1]]$Lon[i])
  d2 = abs(JAXA[[1]]$Latitude - NOAA.aus[[1]]$Lat[i])
  d3 = d1 + d2
  O1 = order(d3)[1]
  NJ.loc[i] = O1
}

NJ.loc

X = list()
X[[1]] = matrix(0, nrow = length(NJ.loc), ncol = 256:length(NOAA.aus))
X[[2]] = matrix(0, nrow = length(NJ.loc), ncol = 256:length(NOAA.aus))
for(i in 1:length(256:length(NOAA.aus))){
  X[[1]][,i] = JAXA[[i]]$Precip[NJ.loc]
  X[[2]][,i] = NOAA.aus[[255 + i]]$Precipitation
}

X1 = list()
X1[[1]] = t(apply(X[[1]], 1, cumsum))
X1[[1]] = t(apply(X[[2]], 1, cumsum))