####NOAA import

library(ncdf4)

download.file("https://ftp.cpc.ncep.noaa.gov/precip/PORT/SEMDP/CMORPH_BLD/DATA/1998/199801/SEMDP_CMORPH_BLD_0.25deg-MON_199801.nc",destfile="NOAA.nc",method="libcurl")
ncin <- nc_open("NOAA.nc")
print(ncin)

lon <- ncvar_get(ncin, "longitude")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin, "latitude", verbose = F)
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


  t <- ncvar_get(ncin, "time")
  tunits <- ncatt_get(ncin, "time", "units")
  nt <- dim(t)
  dname = "precip"
  
  tmp.array <- ncvar_get(ncin, dname)
  dlname <- ncatt_get(ncin, dname, "long_name")
  dunits <- ncatt_get(ncin, dname, "units")
  fillvalue <- ncatt_get(ncin, dname, "_FillValue")
  dim(tmp.array)
  