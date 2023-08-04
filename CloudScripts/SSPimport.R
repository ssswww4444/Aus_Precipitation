"http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.MSL/.pressure/%28mb%29unitconvert/DATA/2/STEP/data.nc"

download.file("http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NCEP-NCAR/.CDAS-1/.MONTHLY/.Intrinsic/.MSL/.pressure/%28mb%29unitconvert/DATA/2/STEP/data.nc", 
              destfile= "SSPnc", method="libcurl")

ncin <- nc_open("SSPnc")

lon <- ncvar_get(ncin, "X")
nlon <- dim(lon)

lat <- ncvar_get(ncin, "Y", verbose = F)
nlat <- dim(lat)

Time = ncvar_get(ncin, "T") #Months since 1960-01-01

Time = Time + 0.5

SSP.dates = NULL
SSP.months = ifelse(Time %% 12 == 1, 1, 
                    ifelse(Time %% 12 == 2, 2, 
                           ifelse(Time %% 12 == 3, 3,
                                  ifelse(Time %% 12 == 4, 4,
                                         ifelse(Time %% 12 == 5, 5,
                                                ifelse(Time %% 12 == 6, 6,
                                                       ifelse(Time %% 12 == 7, 7,
                                                              ifelse(Time %% 12 == 8, 8,
                                                                     ifelse(Time %% 12 == 9, 9,
                                                                            ifelse(Time %% 12 == 10, 10,
                                                                                   ifelse(Time %% 12 == 11, 11, 12)))))))))))


880-133
SSP.years = rep(0, length(Time))
for(i in 1:length(Time)){
  SSP.years[i] = 1960 + floor((i - 133)/12)
}

SSP.dates = cbind(SSP.years, SSP.months)

tmp.array <- ncvar_get(ncin, "pressure")

WORLD = matrix(0, nrow = nlat * nlon, ncol = length(Time))
SSP.location = NULL
k = 1
for(i in 1:nlon){
  for(j in 1:nlat){
    WORLD[k,] = tmp.array[i,j,]
    SSP.location = rbind(SSP.location, c(lon[i], lat[j]))
    k = k + 1
  }
}

SSP = WORLD

write.csv(SSP, file = "SSP", row.names = FALSE, col.names = FALSE)

colnames(SSP.location) = c("Lon", "Lat")

write.csv(SSP.location, file = "SSPLocation", row.names = FALSE)

colnames(SSP.dates) = c("Year", "Month")

write.csv(SSP.dates, file = "SSPDates", row.names = FALSE)


