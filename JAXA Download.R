"ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/2000/200004/SEMDP_GSMaP_GNRT6_0.10deg-MON_200004.nc"

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

NJ.location = JAXA[[1]][NJ.loc,3:2]

NJ.MEDQ = MEDQ(X1, p = seq(0,1, length.out = 10), weight = FALSE, scale = TRUE)
length(unique(NJ.MEDQ))


dd1 = cbind(NJ.MEDQ, seq(0,1, length.out = 10))
v1 = NULL
for(i in unique(NJ.MEDQ)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

NJ.MEDQdf = data.frame(longitude = NJ.location[unique(NJ.MEDQ),1], latitude = NJ.location[unique(NJ.MEDQ),2], Quantile = v1)
samp1 = sample(1:nrow(NJ.MEDQdf), 25, replace = FALSE)

which(NJ.MEDQdf$longitude < 135 & NJ.MEDQdf$longitude > 134 & NJ.MEDQdf$latitude > -35 & NJ.MEDQdf$latitude < -33 & NJ.MEDQdf$Quantile > 0.9)

NJ.MEDQdf= NJ.MEDQdf[-which(NJ.MEDQdf$longitude < 135 & NJ.MEDQdf$longitude > 134 & NJ.MEDQdf$latitude > -35 & NJ.MEDQdf$latitude < -33 & NJ.MEDQdf$Quantile > 0.9),]

g.EDQ1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(NJ.MEDQdf, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.EDQ1


write.csv(NJ.MEDQdf, file = "NJdf", row.names = FALSE)


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXA.monthly")
set.seed(1998)
samp1 = sort(sample(1:nrow(WORLD), 10^5))

for(i in 1:length(JAXA)){
  file1 = "JAXA.Monthly"
  file2 = JAXA[[i]][1,4]
  file3 = JAXA[[i]][1,5]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  write.csv(JAXA[[i]][samp1,], file = paste0(file1, file2, file3))
}

JAXA.Monthly = list()
for(i in 1:nrow(dates4)){
  tryCatch({
  file1 = "JAXA.Monthly"
  file2 = dates4[i,1]
  file3 = dates4[i,2]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  JAXA.Monthly[[i]] = read.csv(paste0(file1, file2, file3), row.names = NULL)
  }, error = function(e){})
}


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")


###Daily
"ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/2000/200004/SEMDP_GSMaP_GNRT6_0.10deg-DLY_20000401.nc"

WORLD1 = matrix(0, nrow = nlat * nlon, ncol = 2 + 28)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD1[k,] = c(lat[i], lon[j], rep(0, ncol(WORLD1) - 2))
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}

WORLD2 = matrix(0, nrow = nlat * nlon, ncol = 2 + 29)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD2[k,] = c(lat[i], lon[j], rep(0, ncol(WORLD2) - 2))
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}

WORLD3 = matrix(0, nrow = nlat * nlon, ncol = 2 + 30)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD3[k,] = c(lat[i], lon[j], rep(0, ncol(WORLD3) - 2))
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}

WORLD4 = matrix(0, nrow = nlat * nlon, ncol = 2 + 31)
k = 1
for(i in 1:nlat){
  for(j in 1:nlon){
    WORLD4[k,] = c(lat[i], lon[j], rep(0, ncol(WORLD4) - 2))
    k = k + 1
  }
  if(i %% 1000 == 0){
    print(i)
  }
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXA.daily")

JAXA.daily = list()
pb <- txtProgressBar(min = 1, max = nrow(dates4), style = 3)
start1 = length(JAXA.daily) + 1
for(i in length(JAXA.daily1):nrow(dates4)){
  if(dates4[i,3] == 28){
    JAXA.daily[[i]] = WORLD1
  }else if(dates4[i,3] == 29){
    JAXA.daily[[i]] = WORLD2
  }else if(dates4[i,3] == 30){
    JAXA.daily[[i]] = WORLD3
  }else{
    JAXA.daily[[i]] = WORLD4
  }
  file1 = "ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/"
  file2 = dates4[i,1]
  file3 = dates4[i,2]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  for(j in 1:dates4[i,3]){
    file4 = j
    if(nchar(file4) == 1){
      file4 = paste0("0", file4)
    }
    file5 = paste0(file1, file2, "/", file2, file3, "/SEMDP_GSMaP_GNRT6_0.10deg-DLY_", file2, file3, file4, ".nc")
    dfile = paste0("JAXA.daily.nc", file2, file3, file4)
    download.file(file5, destfile=dfile, method="libcurl")
    
    ncin <- nc_open(dfile)
    
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
    JAXA.daily[[i]][, 2 + j] = as.numeric(a1)
  }
  setTxtProgressBar(pb, i)
}
close(pb)



for(i in (length(JAXA.daily1) + 1):length(JAXA.daily)){
  if(dates4[i,3] == 28){
    JAXA.daily[[i]][,c(1,2)] = WORLD1[,c(1,2)]
  }else if(dates4[i,3] == 29){
    JAXA.daily[[i]][,c(1,2)] = WORLD2[,c(1,2)]
  }else if(dates4[i,3] == 30){
    JAXA.daily[[i]][,c(1,2)] = WORLD3[,c(1,2)]
  }else{
    JAXA.daily[[i]][,c(1,2)] = WORLD4[,c(1,2)]
  }
}

A1 = JAXA.daily1[[length(JAXA.daily1)]][,2:1]
A1 = as.data.frame(A1)
colnames(A1) = c("Lon", "Lat")
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Oceania = which(a3$ISO3 == "AUS" | a3$ISO3 == "NZL" | a3$ISO3 == "FJI" | a3$ISO3 == "PNG" | a3$ISO3 == "FSM" | a3$ISO3 == "WSM" |
                a3$ISO3 == "TON" | a3$ISO3 == "PLW" | a3$ISO3 == "VUT" | a3$ISO3 == "SLB" | a3$ISO3 == "KIR" | a3$ISO3 == "PYF" |
                a3$ISO3 == "GUM" | a3$ISO3 == "NRU" | a3$ISO3 == "MHL" | a3$ISO3 == "TUV" | a3$ISO3 == "NCL" | a3$ISO3 == "MNP" |
                a3$ISO3 == "ASM" | a3$ISO3 == "COK" | a3$ISO3 == "NIU" | a3$ISO3 == "WLF" | a3$ISO3 == "PCN" | a3$ISO3 == "NFK")

Australasia = which(a3$ISO3 == "AUS" | a3$ISO3 == "NZL" | a3$ISO3 == "PNG")

start1 = length(JAXA.daily1) + 1
JAXA.daily.A = list()
for(i in 1:length(JAXA.daily1)){
  JAXA.daily.A[[i]] = JAXA.daily1[[i]][Australasia,]
}

Australasiamap = shape[shape$ISO3 == "AUS" | shape$ISO3 == "NZL" | shape$ISO3 == "PNG",]
plot(Australasiamap)


red.lon = JAXA.daily.A[[length(JAXA.daily1)]][,2] < 180 & JAXA.daily.A[[length(JAXA.daily1)]][,2] > 90
red.lat = JAXA.daily.A[[length(JAXA.daily1)]][,1] < 5
 
for(i in 1:length(JAXA.daily1)){
  JAXA.daily1[[i]] = JAXA.daily.A[[i]][red.lat & red.lon,]
}

set.seed(1998)
samp1 = sort(sample(1:nrow(JAXA.daily1[[1]]), 2 * 10^4))

for(i in start1:length(JAXA.daily)){
  JAXA.daily1[[i]] = JAXA.daily1[[i]][samp1,]
}

JAXA.daily.A = NULL

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXA.daily1")

for(i in start1:length(JAXA.daily1)){
  file1 = "JAXA.daily"
  file2 = dates4[i,1]
  file3 = dates4[i,2]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  write.csv(JAXA.daily1[[i]], file = paste0(file1, file2, file3), row.names = FALSE)
  print(i)
}


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXA.daily1")

JAXA.daily1 = list()
for(i in 1:nrow(dates4)){
  file1 = "JAXA.daily"
  file2 = dates4[i,1]
  file3 = dates4[i,2]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  JAXA.daily1[[i]] = read.csv(paste0(file1, file2, file3), row.names = NULL)
}


for(i in 1:length(JAXA.daily1)){
  colnames(JAXA.daily1[[i]]) = c("Latitude", "longitude", paste0("Day", ifelse(nchar(3:ncol(JAXA.daily1[[i]]) - 2) == 1, paste0("0", 3:ncol(JAXA.daily1[[i]]) - 2), 3:ncol(JAXA.daily1[[i]]) - 2)))
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")


colnum = 0
colnum1 = NULL
for(i in 1:length(JAXA.daily1)){
  colnum = colnum + ncol(JAXA.daily1[[i]]) - 2
  colnum1 = c(colnum1, ncol(JAXA.daily1[[i]]) - 2)
}
colnum1 = c(0, colnum1)
colnum2 = cumsum(colnum1)


JAXA.all = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = colnum)
JAXA.3 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = colnum - 2)
k = 1
for(i in 1:length(JAXA.daily1)){
  for(j in 3:ncol(JAXA.daily1[[i]])){
    JAXA.all[,k] = JAXA.daily1[[i]][,j]
    k = k + 1
  }
}

JAXA.all[is.na(JAXA.all)] = 0

for(i in 2:(ncol(JAXA.all) - 1)){
  JAXA.3[,i - 1] = JAXA.all[, i - 1] + JAXA.all[, i] + JAXA.all[, i + 1]


which(JAXA.3 == max(JAXA.3), arr.ind = TRUE)

plot(rowSums(JAXA.3 > 100))

#########
#Days above 100mm
#########

df.1 = data.frame(Latitude = JAXA.daily1[[1]][,1], Longitude = JAXA.daily1[[1]][,2], Numb = rowSums(JAXA.3 > 100))
max(df.1$Longitude)

E.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df.1, 
             mapping = aes(x = Longitude, y = Latitude, colour = Numb), size = 0.5)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Above 100mm count", x = "Longitude", y = "Latitude",
       title = "Australasia Extreme Value Count") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
E.plot

set.seed(1998)
s1 = spsample(Australasiamap,n=200000,"random")

pred.grid = data.frame(Longitude = coordinates(s1)[,1], Latitude = coordinates(s1)[,2])
idw1 = idw(formula = Numb ~ 1, locations = ~Longitude + Latitude, data = df.1, newdata = pred.grid, idp = 3)

Ev.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Above 100mm \nCount", x = "Longitude", y = "Latitude",
       title = "Australasia Three Day Extreme Value Count") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
Ev.plot




J1 = apply(JAXA.all, 1, quantile, prob = 0.99)
JAXA.m = JAXA.all
for(i in 1:length(J1)){
  JAXA.m[i,] = (JAXA.all[i,] >= 100)
}


##############
#Any precipitation
##############

JAXA.m = matrix(0, nrow = nrow(JAXA.all), ncol = 12)
for(i in 1:nrow(dates4)){
  JAXA.m[,dates4[i,2]] = JAXA.m[,dates4[i,2]] + rowSums(JAXA.daily1[[i]][,-c(1,2)] >= 100)
  print(i)
}


JAXA.1 = (JAXA.all > 0)
sum(JAXA.1[2000,])


JAXA.a = matrix(0, nrow = nrow(JAXA.1), ncol = 366)
k = 1
l = 1
cur.year = 2000
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    JAXA.a[,k] = JAXA.a[,k] + JAXA.1[,l]
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}

Daily.Dates2 = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    mon1 = dates4[i,2]
    if(nchar(mon1) == 1){
      mon1 = paste0("0", mon1)
    }
    day1 = j
    if(nchar(day1) == 1){
      day1 = paste0("0", day1)
    }
    Daily.Dates2[k] = paste(mon1, day1, sep = "-")
    k = k + 1
  }
}

freq1 = as.numeric(table(sort(Daily.Dates2)))

Jan.Mar = cumsum(dates4[37:(37+11),3])[c(9 ,12)]
JAXA.a1 = cbind(JAXA.a[,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[,1:Jan.Mar[1]])

coefs = matrix(0, ncol = 3, nrow = nrow(JAXA.a1))
for(i in 1:nrow(JAXA.a1)){
  t1 = 1:ncol(JAXA.a1)
  y1 = JAXA.a1[i,]
  y4 = y1/freq1
  sin1 = sin(2*pi*t1/ncol(JAXA.a1))
  cos1 = cos(2*pi*t1/ncol(JAXA.a1))
  mod1 = lm(y4 ~ sin1 + cos1)
  coefs[i,] = coefficients(mod1)
  if(i %% 100 == 0){
    print(i)
  }
}

Coefs.df = data.frame(Amplitude = sqrt(coefs[,2]^2 + coefs[,3]^2), Period = asin(coefs[,3]/sqrt(coefs[,2]^2 + coefs[,3]^2)),
                      Shift = coefs[,1], Latitude = JAXA.daily1[[1]][,1], Longitude = JAXA.daily1[[1]][,2])


nc1 = sum(dates4[10:129,3])
nc2 = sum(dates4[130:249,3])

JAXA.a.1 = matrix(0, nrow = nrow(JAXA.all), ncol = nc1)
JAXA.a.2 = matrix(0, nrow = nrow(JAXA.all), ncol = nc2)
k = 1
for(i in 10:129){
  for(j in 3:ncol(JAXA.daily1[[i]])){
    JAXA.a.1[,k] = JAXA.daily1[[i]][,j] > 0 
    k = k + 1
  }
}

k = 1
for(i in 130:249){
  for(j in 3:ncol(JAXA.daily1[[i]])){
    JAXA.a.2[,k] = JAXA.daily1[[i]][,j] > 0 
    k = k + 1
  }
}


Daily.Dates2.1 = rep(0, sum(dates4[10:129,3]))
k = 1
for(i in 10:129){
  for(j in 1:dates4[i,3]){
    mon1 = dates4[i,2]
    if(nchar(mon1) == 1){
      mon1 = paste0("0", mon1)
    }
    day1 = j
    if(nchar(day1) == 1){
      day1 = paste0("0", day1)
    }
    Daily.Dates2.1[k] = paste(mon1, day1, sep = "-")
    k = k + 1
  }
}

Daily.Dates2.2 = rep(0, sum(dates4[130:249,3]))
k = 1
for(i in 130:249){
  for(j in 1:dates4[i,3]){
    mon1 = dates4[i,2]
    if(nchar(mon1) == 1){
      mon1 = paste0("0", mon1)
    }
    day1 = j
    if(nchar(day1) == 1){
      day1 = paste0("0", day1)
    }
    Daily.Dates2.2[k] = paste(mon1, day1, sep = "-")
    k = k + 1
  }
}

colnames(JAXA.a.1) = Daily.Dates2.1
colnames(JAXA.a.2) = Daily.Dates2.2


freq1 = as.numeric(table(sort(Daily.Dates2.1)))
freq2 = as.numeric(table(sort(Daily.Dates2.2)))

JAXA.a1 = matrix(0, nrow = nrow(JAXA.a.1), ncol = 366)
k = 1
l = 1
cur.year = 2001
for(i in 10:129){
  for(j in 1:dates4[i,3]){
    JAXA.a1[,k] = JAXA.a1[,k] + JAXA.a.1[,l]
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}

JAXA.a2 = matrix(0, nrow = nrow(JAXA.a.2), ncol = 366)
k = 1
l = 1
cur.year = 2011
for(i in 130:249){
  for(j in 1:dates4[i,3]){
    JAXA.a2[,k] = JAXA.a2[,k] + JAXA.a.2[,l]
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}

se1 = matrix(0, ncol = 3, nrow = nrow(JAXA.a1))
coefs1 = matrix(0, ncol = 3, nrow = nrow(JAXA.a1))
for(i in 1:nrow(JAXA.a1)){
  t1 = 1:ncol(JAXA.a1)
  y1 = JAXA.a1[i,]
  y4 = y1/freq1
  sin1 = sin(2*pi*t1/ncol(JAXA.a1))
  cos1 = cos(2*pi*t1/ncol(JAXA.a1))
  mod1 = lm(y4 ~ sin1 + cos1)
  coefs1[i,] = coefficients(mod1)
  se1[i,] = summary(mod1)$coefficients[,2]
  if(i %% 100 == 0){
    print(i)
  }
}

se2 = matrix(0, ncol = 3, nrow = nrow(JAXA.a2))
coefs2 = matrix(0, ncol = 3, nrow = nrow(JAXA.a2))
for(i in 1:nrow(JAXA.a2)){
  t1 = 1:ncol(JAXA.a2)
  y1 = JAXA.a2[i,]
  y4 = y1/freq2
  sin1 = sin(2*pi*t1/ncol(JAXA.a2))
  cos1 = cos(2*pi*t1/ncol(JAXA.a2))
  mod1 = lm(y4 ~ sin1 + cos1)
  coefs2[i,] = coefficients(mod1)
  se2[i,] = summary(mod1)$coefficients[,2]
  if(i %% 100 == 0){
    print(i)
  }
}


dim(coefs1)

sum(pnorm((coefs1[,1] - coefs2[,1])/sqrt(se1[,1]^2 + se2[,1]^2)) < 0.025 | pnorm((coefs1[,1] - coefs2[,1])/sqrt(se1[,1]^2 + se2[,1]^2)) > 0.975)
sum(pnorm((coefs1[,2] - coefs2[,2])/sqrt(se1[,2]^2 + se2[,2]^2)) < 0.025 | pnorm((coefs1[,2] - coefs2[,2])/sqrt(se1[,2]^2 + se2[,2]^2)) > 0.975)
sum(pnorm((coefs1[,3] - coefs2[,3])/sqrt(se1[,3]^2 + se2[,3]^2)) < 0.025 | pnorm((coefs1[,3] - coefs2[,3])/sqrt(se1[,3]^2 + se2[,3]^2)) > 0.975)


Diff.df = data.frame(Alpha = (pnorm((coefs1[,2] - coefs2[,2])/sqrt(se1[,2]^2 + se2[,2]^2)) < 0.025 | pnorm((coefs1[,2] - coefs2[,2])/sqrt(se1[,2]^2 + se2[,2]^2)) > 0.975),
                     Beta = (pnorm((coefs1[,3] - coefs2[,3])/sqrt(se1[,3]^2 + se2[,3]^2)) < 0.025 | pnorm((coefs1[,3] - coefs2[,3])/sqrt(se1[,3]^2 + se2[,3]^2)) > 0.975),
                     C = (pnorm((coefs1[,1] - coefs2[,1])/sqrt(se1[,1]^2 + se2[,1]^2)) < 0.025 | pnorm((coefs1[,1] - coefs2[,1])/sqrt(se1[,1]^2 + se2[,1]^2)) > 0.975),
                     Latitude = JAXA.daily1[[1]][,1], Longitude = JAXA.daily1[[1]][,2])

pred.grid = data.frame(Longitude = coordinates(s1)[,1], Latitude = coordinates(s1)[,2])
idw1 = idw(formula = Alpha ~ 1, locations = ~Longitude + Latitude, data = Diff.df, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Beta ~ 1, locations = ~Longitude + Latitude, data = Diff.df, newdata = pred.grid, idp = 3)
idw3 = idw(formula = C ~ 1, locations = ~Longitude + Latitude, data = Diff.df, newdata = pred.grid, idp = 3)


Diff1.df = data.frame(Diff = factor(round(c(idw1$var1.pred, idw2$var1.pred, idw3$var1.pred))), Lon = rep(idw1$Longitude, 3), Lat = rep(idw1$Latitude, 3),
                      Type = rep(c("alpha", "beta", "C"), each = length(idw1$var1.pred)))


Diff2.df = Diff1.df[Diff1.df$Diff == 1, ]


ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  facet_wrap(~Type, labeller = label_parsed) +
  geom_point(data = Diff2.df, 
             mapping = aes(x = Lon, y = Lat, colour = Diff), size = 1)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Coefficient Differences") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none", strip.text = element_text(size = 14))


hist(sqrt(coefs1[,2]^2 + coefs1[,3]^2))


p1 = matrix(0, nrow = nrow(JAXA.a1), ncol = ncol(JAXA.a1))
for(i in 1:nrow(JAXA.a1)){
  p1[i,] = Coefs.df$Amplitude[i] * sin(2*pi*t1/ncol(JAXA.a1) + Coefs.df$Period[i]) + Coefs.df$Shift[i]
}

q1 = matrix(0, nrow = nrow(JAXA.a1), ncol = ncol(JAXA.a1))
for(i in 1:nrow(JAXA.a1)){
  q1[i,] = cumprod(1-p1[i,])
}

T1 = 366

q2 = matrix(0, nrow = nrow(JAXA.a1), ncol = ncol(JAXA.a1))
for(i in 1:nrow(q1)){
  q2[i,] = q1[i,] * p1[i,]
}

z1 = 1:T1
q3 = matrix(0, nrow = nrow(q1), ncol = ncol(q1))
for(i in 1:nrow(q1)){
  q3[i,] = (q1[i,T1] * (T1 - z1) + z1)/(1 - q1[i,T1])^2
}

q4 = rep(0, nrow(JAXA.a1))
for(i in 1:nrow(q1)){
  q4[i] = sum(q2[i,] * q3[i,])
}

JAXA.a2 = cbind(JAXA.a1[,183:T1], JAXA.a1[,1:182])
freq2 = c(freq1[92:T1], freq1[1:91])

coefs1 = matrix(0, ncol = 3, nrow = nrow(JAXA.a))
for(i in 1:nrow(JAXA.a2)){
  t1 = 1:ncol(JAXA.a)
  y1 = JAXA.a[i,]/freq2
  sin1 = sin(2*pi*t1/ncol(JAXA.a))
  cos1 = cos(2*pi*t1/ncol(JAXA.a))
  mod1 = lm(y1 ~ sin1 + cos1)
  coefs1[i,] = coefficients(mod1)
}

Coefs1.df = data.frame(Amplitude = sqrt(coefs1[,2]^2 + coefs1[,3]^2), Period = asin(coefs1[,3]/sqrt(coefs1[,2]^2 + coefs1[,3]^2)),
                      Shift = coefs1[,1], Latitude = JAXA.daily1[[1]][,1], Longitude = JAXA.daily1[[1]][,2])


pj1 = matrix(0, nrow = nrow(JAXA.1), ncol = ncol(JAXA.1))
for(i in 1:nrow(JAXA.a2)){
  t2 = 1:ncol(JAXA.1)
  pj1[i,] = Coefs1.df$Amplitude[i] * sin(2*pi*t2/ncol(JAXA.a2) + Coefs1.df$Period[i]) + Coefs1.df$Shift[i]
}

qj1 = matrix(0, nrow = nrow(JAXA.a2), ncol = ncol(JAXA.a2))
for(i in 1:nrow(JAXA.a2)){
  qj1[i,] = cumprod(1-pj1[i,])
}

T1 = 366

qj2 = matrix(0, nrow = nrow(JAXA.a2), ncol = ncol(JAXA.a2))
for(i in 1:nrow(qj1)){
  qj2[i,] = qj1[i,] * pj1[i,]
}

z1 = 1:T1
qj3 = matrix(0, nrow = nrow(qj1), ncol = ncol(qj1))
for(i in 1:nrow(qj1)){
  qj3[i,] = (qj1[i,T1] * (T1 - z1) + z1)/(1 - qj1[i,T1])^2
}

qj4 = rep(0, nrow(JAXA.a2))
for(i in 1:nrow(qj1)){
  qj4[i] = sum(qj2[i,] * qj3[i,])
}






err1 = JAXA.1 - pj1

plot(err1[Canberra,])

Expected.df = data.frame(JanExp = q4, JulExp = qj4, Latitude = JAXA.daily1[[1]][,1], Longitude = JAXA.daily1[[1]][,2])
idwE1 = idw(formula = JanExp ~ 1, locations = ~Longitude + Latitude, data = Expected.df, newdata = pred.grid, idp = 3)
idwE2 = idw(formula = JulExp ~ 1, locations = ~Longitude + Latitude, data = Expected.df, newdata = pred.grid, idp = 3)

E1.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idwE1, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.1)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "January 1st Expectation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

E2.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idwE2, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.1)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "July 1st Expectation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")



A.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.1)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Amplitude") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

P.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.1)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         breaks = c(-1.5, 0, 1.5), labels = c("-1.5", "0", "1.5")) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Period Phase Shift") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

S.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw3, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.1)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Vertical Shift") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

grid.arrange(A.plot + labs(x = " ", y = " ", colour = " ") + theme(legend.position = "bottom"), P.plot + labs(x = " ", y = " ", colour = " ") + theme(legend.position = "bottom", axis.text.y=element_blank()),
             S.plot + labs(x = " ", y = " ", colour = " ") + theme(legend.position = "bottom", axis.text.y=element_blank()), 
             nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, hjust = -0.5),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), vjust = -8),
             right = textGrob("Estimate", gp=gpar(fontsize=18,font=8), vjust = 12.5, hjust = 16.7))


del = 0.25
Port.Moresby = which(JAXA.daily1[[1]][,1] < -9.466967 + del & JAXA.daily1[[1]][,1] > -9.466967 - del & JAXA.daily1[[1]][,2] < 147.19 + del & JAXA.daily1[[1]][,2] > 147.19 - del)[4]
Canberra = which(JAXA.daily1[[1]][,1] < -35.317 + del & JAXA.daily1[[1]][,1] > -35.317 - del & JAXA.daily1[[1]][,2] < 149.126 + del & JAXA.daily1[[1]][,2] > 149.126 - del)[5]
Wellington = which(JAXA.daily1[[1]][,1] < -41.267 + del & JAXA.daily1[[1]][,1] > -41.267 - del & JAXA.daily1[[1]][,2] < 174.768 + del & JAXA.daily1[[1]][,2] > 174.768 - del)[1]
Darwin = which(JAXA.daily1[[1]][,1] < -12.397 + del & JAXA.daily1[[1]][,1] > -12.397 - del & JAXA.daily1[[1]][,2] < 130.94 + del & JAXA.daily1[[1]][,2] > 130.94 - del)[1]
Alice = which(JAXA.daily1[[1]][,1] < -23.781 + del & JAXA.daily1[[1]][,1] > -23.781 - del & JAXA.daily1[[1]][,2] < 133.860 + del & JAXA.daily1[[1]][,2] > 133.860 - del)[1]

Jan.Mar = cumsum(dates4[37:(37+11),3])[c(9 ,12)]

Port1 = c(JAXA.a[Port.Moresby,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Port.Moresby, 1:Jan.Mar[1]])
Canb1 = c(JAXA.a[Canberra,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Canberra, 1:Jan.Mar[1]])
Well1 = c(JAXA.a[Wellington,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Wellington, 1:Jan.Mar[1]])
Dar1 = c(JAXA.a[Darwin,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Darwin, 1:Jan.Mar[1]])

k = 1
Daily.Dates = rep(0, 366)
for(i in 37:(37 + 11)){
  for(j in 1:dates4[i,3]){
    Daily.Dates[k] = paste(2000, dates4[i,2], j, sep = "-")
    k = k + 1
  }
}

cumsum(dates4[37:(37+11),3])

freq1 = as.numeric(table(Daily.Dates2))

m.dates = as.Date(Daily.Dates)
m.dates1 = format(m.dates, format="%m-%d")

d1 = data.frame(Port = Port1/freq1, Canberra = Canb1/freq1, Wellington = Well1/freq1, Dates = m.dates)
gp = ggplot(data = d1, aes(x = Dates, y = Port)) + geom_point() + scale_x_date(date_labels = "%b", date_breaks = "2 month")+ geom_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", title = "Port Moresby") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

gc = ggplot(data = d1, aes(x = Dates, y = Canberra)) + geom_point() + scale_x_date(date_labels = "%b", date_breaks = "2 month")+ geom_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", title = "Canberra") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

gw = ggplot(data = d1, aes(x = Dates, y = Wellington)) + geom_point() + scale_x_date(date_labels = "%b", date_breaks = "2 month")+ geom_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", title = "Wellington") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

d2 = data.frame(Dates = rep(d1$Dates, 3), Count = c(d1$Port, d1$Canberra, d1$Wellington), Loc = rep(c("Port Moresby", "Canberra", "Wellington"), each = nrow(d1)))

ggplot(data = d2, aes(x = Dates, y = Count)) + geom_point() + facet_wrap(~ Loc) + scale_x_date(date_labels = "%b", date_breaks = "2 month")+ geom_smooth(
  color = "blue", fill = "blue",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", x = "Date", title = "Proportion of Rain Days")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))





################
#Greater than quantile
################

JAXA.m = matrix(0, nrow = nrow(JAXA.all), ncol = 12)
for(i in 1:nrow(dates4)){
  JAXA.m[,dates4[i,2]] = JAXA.m[,dates4[i,2]] + rowSums(JAXA.daily1[[i]][,-c(1,2)] >= 100)
  print(i)
}

q1 = apply(JAXA.all, 1, quantile, p = 0.99)



JAXA.1 = (JAXA.all)
for(i in 1:nrow(JAXA.1)){
  JAXA.1[i,] = (JAXA.all[i,] > q1[i])
}

sum(JAXA.1[1,])


JAXA.a = matrix(0, nrow = nrow(JAXA.1), ncol = 366)
k = 1
l = 1
cur.year = 2000
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    JAXA.a[,k] = JAXA.a[,k] + JAXA.1[,l]
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}

Daily.Dates2 = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    mon1 = dates4[i,2]
    if(nchar(mon1) == 1){
      mon1 = paste0("0", mon1)
    }
    day1 = j
    if(nchar(day1) == 1){
      day1 = paste0("0", day1)
    }
    Daily.Dates2[k] = paste(mon1, day1, sep = "-")
    k = k + 1
  }
}

del = 0.25
Port.Moresby = which(JAXA.daily1[[1]][,1] < -9.466967 + del & JAXA.daily1[[1]][,1] > -9.466967 - del & JAXA.daily1[[1]][,2] < 147.19 + del & JAXA.daily1[[1]][,2] > 147.19 - del)[4]
Canberra = which(JAXA.daily1[[1]][,1] < -35.317 + del & JAXA.daily1[[1]][,1] > -35.317 - del & JAXA.daily1[[1]][,2] < 149.126 + del & JAXA.daily1[[1]][,2] > 149.126 - del)[5]
Wellington = which(JAXA.daily1[[1]][,1] < -41.267 + del & JAXA.daily1[[1]][,1] > -41.267 - del & JAXA.daily1[[1]][,2] < 174.768 + del & JAXA.daily1[[1]][,2] > 174.768 - del)[1]

Jan.Mar = cumsum(dates4[37:(37+11),3])[c(9 ,12)]

Port1 = c(JAXA.a[Port.Moresby,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Port.Moresby, 1:Jan.Mar[1]])
Canb1 = c(JAXA.a[Canberra,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Canberra, 1:Jan.Mar[1]])
Well1 = c(JAXA.a[Wellington,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a[Wellington, 1:Jan.Mar[1]])

k = 1
Daily.Dates = rep(0, 366)
for(i in 37:(37 + 11)){
  for(j in 1:dates4[i,3]){
    Daily.Dates[k] = paste(2000, dates4[i,2], j, sep = "-")
    k = k + 1
  }
}

cumsum(dates4[37:(37+11),3])

freq1 = as.numeric(table(Daily.Dates2))

m.dates = as.Date(Daily.Dates)
m.dates1 = format(m.dates, format="%m-%d")

d1 = data.frame(Port = Port1/freq1, Canberra = Canb1/freq1, Wellington = Well1/freq1, Dates = m.dates)
gp = ggplot(data = d1, aes(x = Dates, y = Port)) + geom_point() + scale_x_date(date_labels = "%b %d", date_breaks = "2 month")+ geom_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", title = "Port Moresby") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

gc = ggplot(data = d1, aes(x = Dates, y = Canberra)) + geom_point() + scale_x_date(date_labels = "%b %d", date_breaks = "2 month")+ geom_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", title = "Canberra") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

gw = ggplot(data = d1, aes(x = Dates, y = Wellington)) + geom_point() + scale_x_date(date_labels = "%b %d", date_breaks = "2 month")+ geom_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", title = "Wellington") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

d2 = data.frame(Dates = rep(d1$Dates, 3), Count = c(d1$Port, d1$Canberra, d1$Wellington), Loc = rep(c("Port Moresby", "Canberra", "Wellington"), each = nrow(d1)))

ggplot(data = d2, aes(x = Dates, y = Count)) + geom_point() + facet_wrap(~ Loc) + scale_x_date(date_labels = "%b", date_breaks = "2 month")+ geom_smooth(
  color = "blue", fill = "blue",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", x = "Date", title = "Proportion of Days above 0.99 Quantile")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))



df.1 = data.frame(Latitude = JAXA.daily1[[1]][,1], Longitude = JAXA.daily1[[1]][,2], Quant = q1)
max(df.1$Longitude)

E.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df.1, 
             mapping = aes(x = Longitude, y = Latitude, colour = Quant), size = 0.5)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude",
       title = "Australasia Extreme Value Count") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
E.plot

set.seed(1998)
s1 = spsample(Australasiamap,n=200000,"random")

pred.grid = data.frame(Longitude = coordinates(s1)[,1], Latitude = coordinates(s1)[,2])
idw1 = idw(formula = Quant ~ 1, locations = ~Longitude + Latitude, data = df.1, newdata = pred.grid, idp = 3)

Ev.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Precipitation \n(mm)", x = "Longitude", y = "Latitude",
       title = "Australasia 0.99 Precipitation Quantile") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
Ev.plot



alpha = rnorm(100)
beta = rnorm(100)

a = sqrt(alpha^2 + beta^2)
b = asin(beta/sqrt(alpha^2+beta^2))



seq(-pi,pi, 0.01)

plot(sin(2*pi*5/T1 + seq(-pi,pi, 0.01)))


JAXA.a3 = JAXA.a1[c(Canberra, Port.Moresby, Wellington),]

E.ts = matrix(0, nrow = 3, ncol = ncol(JAXA.a3))
for(i in 1:ncol(JAXA.a3)){
  for(j in 1:nrow(JAXA.a3)){
    t1 = 1:ncol(JAXA.a3)
    sin1 = sin(2*pi*t1/ncol(JAXA.a3))
    cos1 = cos(2*pi*t1/ncol(JAXA.a3))
    mod1 = lm(JAXA.a3[j,] ~ sin1 + cos1)
    coefs = coefficients(mod1)
    Coefs.df = data.frame(Amplitude = sqrt(coefs[2]^2 + coefs[3]^2), Period = asin(coefs[3]/sqrt(coefs[2]^2 + coefs[3]^2)),
                          Shift = coefs[1])
    
    p1 = Coefs.df$Amplitude * sin(2*pi*t1/ncol(JAXA.a3) + Coefs.df$Period) + Coefs.df$Shift 
    q1 = cumprod(1-p1)
    
    T1 = 366
    
    q2 = q1 * p1
    
    z1 = 1:T1
    
    q3 = (q1[T1] * (T1 - z1) + z1)/(1 - q1[T1])^2
    
    q4 = sum(q2 * q3)
    
    E.ts[j,i] = q4
  }
  
  JAXA.a3 = cbind(JAXA.a3[,2:T1], JAXA.a3[,1])
  
}

d2 = data.frame(Dates = rep(d1$Dates, 3), Count = c(E.ts[1,], E.ts[2,], E.ts[3,]), Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(E.ts)))

g.Exp = ggplot(data = d2, aes(x = Dates, y = Count)) + geom_line(lwd = 2) +
  facet_wrap(~ Loc) + scale_x_date(date_labels = "%b", date_breaks = "2 month") + theme_bw() + labs(y = "Number of Days", 
               x = "Date", title = "Geometric Expectation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))


V.ts = matrix(0, nrow = 3, ncol = ncol(JAXA.a3))
for(i in 1:ncol(JAXA.a3)){
  for(j in 1:nrow(JAXA.a3)){
    t1 = 1:ncol(JAXA.a3)
    sin1 = sin(2*pi*t1/ncol(JAXA.a3))
    cos1 = cos(2*pi*t1/ncol(JAXA.a3))
    mod1 = lm(JAXA.a3[j,] ~ sin1 + cos1)
    coefs = coefficients(mod1)
    Coefs.df = data.frame(Amplitude = sqrt(coefs[2]^2 + coefs[3]^2), Period = asin(coefs[3]/sqrt(coefs[2]^2 + coefs[3]^2)),
                          Shift = coefs[1])
    
    p1 = Coefs.df$Amplitude * sin(2*pi*t1/ncol(JAXA.a3) + Coefs.df$Period) + Coefs.df$Shift 
    q1 = cumprod(1-p1)
    
    T1 = 366
    
    q2 = q1 * p1/(1-q1[T1])^3
    
    z1 = 1:T1
    
    q3 = T1^2 * q1[T1] * (1 + q1[T1]) + 2 * T1 * (1 - q1[T1]) * q1[T1] * z1 + (1 - q1[T1])^2 * z1^2
    
    q4 = sum(q2 * q3)
    
    V.ts[j,i] = q4
  }
  
  JAXA.a3 = cbind(JAXA.a3[,2:T1], JAXA.a3[,1])
  
}

V.ts = V.ts - E.ts^2

d3 = data.frame(Dates = rep(d1$Dates, 3), Count = c(V.ts[1,], V.ts[2,], V.ts[3,]), Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(E.ts)))

d4 = data.frame(Dates = rep(d1$Dates, 6), Count = c(E.ts[1,], E.ts[2,], E.ts[3,], V.ts[1,], V.ts[2,], V.ts[3,]), 
                Loc = rep(rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(E.ts)), 2), Type = rep(c("Expectation", "Variance"), each = ncol(E.ts) * nrow(E.ts)))

g.Var = ggplot(data = d4, aes(x = Dates, y = (Count))) + geom_line(lwd = 2, aes(colour = Type)) + facet_wrap(~ Loc) + scale_x_date(date_labels = "%b", date_breaks = "2 month") +
  theme_bw() + labs(y = "Number of Days", x = "Date", title = "Geometric Expectation and Variance")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14)) +
  scale_color_manual(values=c("blue","red"))
g.Var

grid.arrange(g.Exp, g.Var, nrow = 2)



SOI = read.csv("https://www.ncdc.noaa.gov/teleconnections/enso/indicators/soi/data.csv", header = TRUE)
SOI = SOI[-1,]
colnames(SOI) = c("Values")
SOI.dates = rownames(SOI)[-1]
SOI.values = SOI[-1,]
SOI = data.frame(Date = as.numeric(SOI.dates), Value = as.numeric(SOI.values))

IOD = read.table("https://raw.githubusercontent.com/hinestein/SOI/master/IOD.txt")
IOD.values = as.vector(t(as.matrix(IOD[,-1])))
IOD.dates = paste0(rep(IOD[,1], each = 12), c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))
IOD = data.frame(Date = as.numeric(IOD.dates), Value = as.numeric(IOD.values))


JAXA.a.SOI = matrix(0, nrow = nrow(JAXA.1), ncol = 366)
k = 1
l = 1
cur.year = 2000
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    if(SOI2[i,2] >= 0){
      JAXA.a.SOI[,k] = JAXA.a.SOI[,k] + JAXA.1[,l]  
    }
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}



Daily.Dates3 = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    if(SOI2[i,2] >= 0){
      mon1 = dates4[i,2]
      if(nchar(mon1) == 1){
        mon1 = paste0("0", mon1)
      }
      day1 = j
      if(nchar(day1) == 1){
        day1 = paste0("0", day1)
      }
      Daily.Dates3[k] = paste(mon1, day1, sep = "-")
    }
    k = k + 1
  }
}

del = 0.25
Port.Moresby = which(JAXA.daily1[[1]][,1] < -9.466967 + del & JAXA.daily1[[1]][,1] > -9.466967 - del & JAXA.daily1[[1]][,2] < 147.19 + del & JAXA.daily1[[1]][,2] > 147.19 - del)[4]
Canberra = which(JAXA.daily1[[1]][,1] < -35.317 + del & JAXA.daily1[[1]][,1] > -35.317 - del & JAXA.daily1[[1]][,2] < 149.126 + del & JAXA.daily1[[1]][,2] > 149.126 - del)[5]
Wellington = which(JAXA.daily1[[1]][,1] < -41.267 + del & JAXA.daily1[[1]][,1] > -41.267 - del & JAXA.daily1[[1]][,2] < 174.768 + del & JAXA.daily1[[1]][,2] > 174.768 - del)[1]

Jan.Mar = cumsum(dates4[37:(37+11),3])[c(9 ,12)]

Port1 = c(JAXA.a.SOI[Port.Moresby,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a.SOI[Port.Moresby, 1:Jan.Mar[1]])
Canb1 = c(JAXA.a.SOI[Canberra,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a.SOI[Canberra, 1:Jan.Mar[1]])
Well1 = c(JAXA.a.SOI[Wellington,(Jan.Mar[1] + 1):Jan.Mar[2]], JAXA.a.SOI[Wellington, 1:Jan.Mar[1]])

k = 1
Daily.Dates = rep(0, 366)
for(i in 37:(37 + 11)){
  for(j in 1:dates4[i,3]){
    Daily.Dates[k] = paste(2000, dates4[i,2], j, sep = "-")
    k = k + 1
  }
}

cumsum(dates4[37:(37+11),3])

freq1 = as.numeric(table(Daily.Dates3))[-1]

m.dates = as.Date(Daily.Dates)
m.dates1 = format(m.dates, format="%m-%d")

d1.SOI = data.frame(Port = Port1/freq1, Canberra = Canb1/freq1, Wellington = Well1/freq1, Dates = m.dates)

d2.SOI = data.frame(Dates = rep(d1.SOI$Dates, 3), Count = c(d1.SOI$Port, d1.SOI$Canberra, d1.SOI$Wellington), Loc = rep(c("Port Moresby", "Canberra", "Wellington"), each = nrow(d1.SOI)))

ggplot(data = d2.SOI, aes(x = Dates, y = Count)) + geom_point() + facet_wrap(~ Loc) + scale_x_date(date_labels = "%b", date_breaks = "2 month")+ geom_smooth(
  color = "blue", fill = "blue",
  method = "loess"
) + theme_bw() + labs(y = "Proportion", x = "Date", title = "Proportion of Days above 0.99 Quantile")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))



##############
#Dependency
##############

JAXA.d = (JAXA.all > 0)
JAXA.d10 = matrix(0, nrow = nrow(JAXA.d), ncol = ncol(JAXA.d) - 1)
JAXA.d11 = matrix(0, nrow = nrow(JAXA.d), ncol = ncol(JAXA.d) - 1)
for(i in 1:nrow(JAXA.d)){
  for(j in 2:ncol(JAXA.d)){
    if(JAXA.d[i,j] == 1 & JAXA.d[i,j - 1] == 0){
      JAXA.d10[i,j-1] = 1
    }
    if(JAXA.d[i,j] == 1 & JAXA.d[i,j - 1] == 1){
      JAXA.d11[i,j - 1] = 1
    }
  }
}

JAXA.ad1 = matrix(0, nrow = nrow(JAXA.1), ncol = 366)
JAXA.ad2 = matrix(0, nrow = nrow(JAXA.1), ncol = 366)
k = 1
l = 1
cur.year = 2000
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    JAXA.ad1[,k] = JAXA.ad1[,k] + JAXA.d10[,l]
    JAXA.ad2[,k] = JAXA.ad2[,k] + JAXA.d11[,l]
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}


JAXA.2 = (JAXA.all == 0)
JAXA.b = matrix(0, nrow = nrow(JAXA.2), ncol = 366)
k = 1
l = 1
cur.year = 2000
for(i in 1:nrow(dates4)){
  for(j in 1:dates4[i,3]){
    JAXA.b[,k] = JAXA.b[,k] + JAXA.2[,l]
    l = l + 1
    k = k + 1
    if(dates4[i,2] == 2 & j == 28 & dates4[i,3] != 29){
      print(k)
      k = k + 1
    }
  }
  if(i %% 12 == 0){
    k = 1
  }
}


JAXA.ae1 = matrix(0, nrow = nrow(JAXA.1), ncol = 366)
JAXA.ae2 = matrix(0, nrow = nrow(JAXA.1), ncol = 366)

for(i in 1:nrow(JAXA.ae1)){
  for(j in 1:ncol(JAXA.ae1)){
    if(JAXA.b[i,j] == 0){
      JAXA.ae1[i,j] = 0
    }else{
      JAXA.ae1[i,j] = JAXA.ad1[i,j]/JAXA.b[i,j] 
    }
    if(JAXA.a[i,j] == 0){
      JAXA.ae2[i,j] = 0
    }else{
      JAXA.ae2[i,j] = JAXA.ad2[i,j]/JAXA.a[i,j]
    }
  }
}

Z.mat = matrix(0, nrow = nrow(JAXA.ae1), ncol = ncol(JAXA.ae1))
for(i in 1:nrow(JAXA.ae1)){
  for(j in 1:ncol(JAXA.ae1)){
    if(JAXA.a[i,j] <= 5 | JAXA.b[i,j] <= 5 | (JAXA.ae1[i,j] == 0 & JAXA.ae2[i,j] == 0) | (JAXA.ae1[i,j] == 1 & JAXA.ae2[i,j] == 1)){
      Z.mat[i,j] = 0
    }else{
      p1.hat = JAXA.ae1[i,j]
      p2.hat = JAXA.ae2[i,j]
      n1 = JAXA.b[i,j]
      n2 = JAXA.a[i,j]
      p.hat = (p1.hat * n1 + p2.hat * n2)/(n1 + n2)
      Z.mat[i,j] = (p1.hat - p2.hat)/sqrt(p.hat*(1-p.hat) * (1/n1 + 1/n2))  
    }
  }
}

sum(abs(Z.mat) > 1.96, na.rm = TRUE)/(nrow(Z.mat) * ncol(Z.mat))



#################
#Gamma Cumulative
#################

w1 = which(dates4[,2] == c(1))

G.c1 = 1:length(w1)
G.p1 = 1:length(w1)
G.w1 = 1:length(w1)
for(i in 1:length(w1)){
  G.c1[i] = sum(JAXA.daily1[[i]][Canberra,-c(1,2)]) + sum(JAXA.daily1[[i + 1]][Canberra,-c(1,2)]) + sum(JAXA.daily1[[i + 2]][Canberra,-c(1,2)])
  G.p1[i] = sum(JAXA.daily1[[i]][Port.Moresby,-c(1,2)]) + sum(JAXA.daily1[[i + 1]][Port.Moresby,-c(1,2)]) + sum(JAXA.daily1[[i + 2]][Port.Moresby,-c(1,2)])
  G.w1[i] = sum(JAXA.daily1[[i]][Wellington,-c(1,2)]) + sum(JAXA.daily1[[i + 1]][Wellington,-c(1,2)]) + sum(JAXA.daily1[[i + 2]][Wellington,-c(1,2)])
}


del = 0.25
Port.Moresby1 = which(precip3[[1]][,1] < -9.466967 + del & precip3[[1]][,1] > -9.466967 - del & precip3[[1]][,2] < 147.19 + del & precip3[[1]][,2] > 147.19 - del)[1]
Canberra1 = which(precip3[[1]][,1] < -35.317 + del & precip3[[1]][,1] > -35.317 - del & precip3[[1]][,2] < 149.126 + del & precip3[[1]][,2] > 149.126 - del)[1]
Wellington1 = which(precip3[[1]][,1] < -41.267 + del & precip3[[1]][,1] > -41.267 - del & precip3[[1]][,2] < 174.768 + del & precip3[[1]][,2] > 174.768 - del)[1]


w2 = which(dates2[,2] == c(1))[-length(which(dates2[,2] == c(1)))]

G.c2 = 1:(length(w2))
G.p2 = 1:(length(w2))
G.w2 = 1:(length(w2))
k = 1
for(i in w2){
  G.c2[k] = precip3[[i]][Canberra1,3] + precip3[[i + 1]][Canberra1,3] + precip3[[i + 2]][Canberra1,3]
  G.p2[k] = precip3[[i]][Port.Moresby1,3] + precip3[[i + 1]][Port.Moresby1,3] + precip3[[i + 2]][Port.Moresby1,3]
  G.w2[k] = precip3[[i]][Wellington1,3] + precip3[[i + 1]][Wellington1,3] + precip3[[i + 2]][Wellington1,3]
  k = k + 1
}

G.a1 = matrix(0, nrow = nrow(precip3[[1]]), ncol = length(w2))
G.a2 = matrix(0, ncol = 2, nrow = nrow(precip3[[1]]))
k = 1
for(j in w2){
  G.a1[,k] = precip3[[j]][,3] + precip3[[j + 1]][,3] + precip3[[j + 2]][,3]
  k = k + 1
}

A.1 = log(rowMeans(G.a1)) - rowSums(log(G.a1))/ncol(G.a1)
alpha.1 = (1/(4*A.1)) * (1 + sqrt(1 + 4 * A.1/3))
beta.1 = rowMeans(G.a1)/alpha.1
G.a2 = cbind(alpha.1, beta.1)

mlgamma(G.a1[100,])
pb <- txtProgressBar(min = 1, max =nrow(G.a1), style = 3)
for(i in 1:nrow(G.a1)){
  setTxtProgressBar(pb, i)
  if(sum(G.a1[i,]) != 0){
    G.a2[i,] = mlgamma(G.a1[i,], na.rm = TRUE)
  }
}

G.a2 = cbind(G.a2, precip3[[1]][,1:2])
colnames(G.a2) = c("Shape", "Rate", "Lat", "Lon")
G.a2 = as.data.frame(G.a2)

G.a3 = G.a1[G.a2$Shape < 100 & G.a2$Rate < 2,]

G.a2 = G.a2[G.a2$Shape < 100 & G.a2$Rate < 2,]


plot(seq(0,196.7364 + 30, 0.05), pgamma(seq(0,196.7364 + 30, 0.05), shape = G.a2$Shape[1000], rate = G.a2$Rate[1000]))


del = 0.25
Port.Moresby1 = which(G.a2[,3] < -9.466967 + del & G.a2[,3] > -9.466967 - del & G.a2[,4] < 147.19 + del & G.a2[,4] > 147.19 - del)[1]
Canberra1 = which(G.a2[,3] < -35.317 + del & G.a3[,3] > -35.317 - del & G.a2[,4] < 149.126 + del & G.a2[,4] > 149.126 - del)[1]
Wellington1 = which(G.a2[,3] < -41.267 + del & G.a3[,3] > -41.267 - del & G.a2[,4] < 174.768 + del & G.a2[,4] > 174.768 - del)[1]

plot(G.a3[Port.Moresby1,], pgamma(G.a3[Port.Moresby1,], shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))


ggplot(data.frame(x = rgamma(100000, shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1])), aes(x)) + geom_density() +
  stat_function(fun = dgamma(x, shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]), colour = "red")

plot(seq(0,1000,0.05),pgamma(seq(0,1000,0.05),shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]), type = "l")

data.frame(x = pgamma(100000, shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))

cdf1 = data.frame(x = seq(0,1000,0.05), y = pgamma(seq(0,1000,0.05),shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))
cdf2 = data.frame(x = G.a3[Port.Moresby1,], y = pgamma(G.a3[Port.Moresby1,],shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))

ggplot(cdf1) + geom_line(aes(x = x, y = y)) + stat_ecdf(data = cdf2, aes(x = x, y = y))


plot(cumsum(sort(G.a3[Port.Moresby1,]))/sum(G.a3[Port.Moresby1,]))

dd.1 = data.frame(x = c(seq(0,max(G.a3[Port.Moresby1,]) + 2*sd(G.a3[Port.Moresby1,]),length.out = 10000), 
                        seq(0,max(G.a3[Canberra1,]) + 2*sd(G.a3[Canberra1,]),length.out = 10000),
                        seq(0,max(G.a3[Wellington1,]) + 2*sd(G.a3[Wellington1,]),length.out = 10000)),
                  y = c(pgamma(seq(0,max(G.a3[Port.Moresby1,]) + 2*sd(G.a3[Port.Moresby1,]),length.out = 10000),shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]),
                        pgamma(seq(0,max(G.a3[Canberra1,]) + 2*sd(G.a3[Canberra1,]),length.out = 10000),shape = G.a2$Shape[Canberra1], rate = G.a2$Rate[Canberra1]),
                        pgamma(seq(0,max(G.a3[Wellington1,]) + 2*sd(G.a3[Wellington1,]),length.out = 10000),shape = G.a2$Shape[Wellington1], rate = G.a2$Rate[Wellington1])),
                  loc = rep(c("Port.Moresby", "Canberra", "Wellington"), each = 10000))

dd.2 = data.frame(x = c(G.a3[Port.Moresby1,], G.a3[Canberra1,], G.a3[Wellington1,]),
                 y = c(pgamma(G.a3[Port.Moresby1,], shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]),
                       pgamma(G.a3[Canberra1,], shape = G.a2$Shape[Canberra1], rate = G.a2$Rate[Canberra1]),
                       pgamma(G.a3[Wellington1,],shape = G.a2$Shape[Wellington1], rate = G.a2$Rate[Wellington1])),
                 loc = rep(c("Port.Moresby", "Canberra", "Wellington"), each = length(G.a3[Port.Moresby1,])))


ggplot(dd.1) + geom_line(aes(x = x, y = y), lwd = 1.2) + stat_ecdf(data = dd.2, aes(x = x, y = y), colour = "blue", lty = 1, lwd = 1) + facet_wrap(~loc, scale = "free") + theme_bw() +
  labs(y = "Cumulative Probability", x = "Precipitation (mm)", title = "Gamma CDF of precipitation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))



w1 = which(dates4[,2] == c(1))

G.b1 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = length(w1))
k = 1
for(i in w1){
  G.b1[,k] = rowSums(JAXA.daily1[[i]][,-c(1,2)]) + rowSums(JAXA.daily1[[i + 1]][,-c(1,2)]) + rowSums(JAXA.daily1[[i + 2]][,-c(1,2)])
  k = k + 1
}

G.b2 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = 2)
for(i in 1:nrow(G.b1)){
  if(sum(G.b1[i,] == 0) == 0){
    G.b2[i,] = mlgamma(G.b1[i,], na.rm = TRUE)
  }
}

G.b2 = cbind(G.b2, JAXA.daily1[[1]][,1:2])
colnames(G.b2) = c("Shape", "Rate", "Lat", "Lon")
G.b2 = as.data.frame(G.b2)

hist(G.b2$Rate)
hist(G.b2$Shape)

G.b3 = G.b1[G.b2$Shape < 40 & G.b2$Rate < 0.08,]

G.b2 = G.b2[G.b2$Shape < 40 & G.b2$Rate < 0.08,]

pred.grid = data.frame(Lon = coordinates(s1)[,1], Lat = coordinates(s1)[,2])
idw1 = idw(formula = Shape ~ 1, locations = ~Lon + Lat, data = G.b2, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Rate ~ 1, locations = ~Lon + Lat, data = G.b2, newdata = pred.grid, idp = 3)

Shape.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Shape") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

Rate.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Rate") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

grid.arrange(Shape.plot + labs(x = " ", y = " "), Rate.plot + labs(x = " ", y = " "),
             nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, hjust = 0.2,
                                       vjust = 2),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), vjust = -1))




dd.1 = data.frame(x = c(seq(0,max(G.b3[Port.Moresby,]) + 2*sd(G.b3[Port.Moresby,]),length.out = 10000), 
                        seq(0,max(G.b3[Canberra,]) + 2*sd(G.b3[Canberra,]),length.out = 10000),
                        seq(0,max(G.b3[Wellington,]) + 2*sd(G.b3[Wellington,]),length.out = 10000)),
                  y = c(pgamma(seq(0,max(G.b3[Port.Moresby,]) + 2*sd(G.b3[Port.Moresby,]),length.out = 10000),shape = G.b2$Shape[Port.Moresby], rate = G.b2$Rate[Port.Moresby]),
                        pgamma(seq(0,max(G.b3[Canberra,]) + 2*sd(G.b3[Canberra,]),length.out = 10000),shape = G.b2$Shape[Canberra], rate = G.b2$Rate[Canberra]),
                        pgamma(seq(0,max(G.b3[Wellington,]) + 2*sd(G.b3[Wellington,]),length.out = 10000),shape = G.b2$Shape[Wellington], rate = G.b2$Rate[Wellington])),
                  loc = rep(c("Port Moresby", "Canberra", "Wellington"), each = 10000))

dd.2 = data.frame(x = c(G.b3[Port.Moresby,], G.b3[Canberra,], G.b3[Wellington,]),
                  y = c(pgamma(G.b3[Port.Moresby,], shape = G.b2$Shape[Port.Moresby], rate = G.b2$Rate[Port.Moresby]),
                        pgamma(G.b3[Canberra,], shape = G.b2$Shape[Canberra], rate = G.b2$Rate[Canberra]),
                        pgamma(G.b3[Wellington,],shape = G.b2$Shape[Wellington], rate = G.b2$Rate[Wellington])),
                  loc = rep(c("Port Moresby", "Canberra", "Wellington"), each = length(G.b3[Port.Moresby,])))


ggplot(dd.1) + geom_line(aes(x = x, y = y), lwd = 1.2) + stat_ecdf(data = dd.2, aes(x = x, y = y), colour = "blue", lty = 1, lwd = 1) + facet_wrap(~loc, scale = "free_x") + theme_bw() +
  labs(y = "Cumulative Probability", x = "Precipitation (mm)", title = "90 Day Culumative Precipitation Gamma ECDF")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))



###############
#SPI TS
###############
##30 day

G.c1 = matrix(0, nrow = nrow(JAXA.all), ncol = ncol(JAXA.all) - 30)
for(i in 1:(ncol(JAXA.all) - 30)){
  G.c1[,i] = rowSums(JAXA.all[,i:(i + 29)])
}

dates.all = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  mon1 = dates4[i,2]
  if(nchar(mon1) == 1){
    mon1 = paste0("0", mon1)
  }
  for(j in 1:dates4[i,3]){
    j1 = j
    if(nchar(j1) == 1){
      j1 = paste0("0", j1)
    }
    dates.all[k] = paste(mon1, j1, sep = "-")
    k = k + 1
  }
}

dates.all = dates.all[1:ncol(G.c1)]

dates.all1 = unique(dates.all)

dates.all2 = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  mon1 = dates4[i,2]
  if(nchar(mon1) == 1){
    mon1 = paste0("0", mon1)
  }
  for(j in 1:dates4[i,3]){
    j1 = j
    if(nchar(j1) == 1){
      j1 = paste0("0", j1)
    }
    dates.all2[k] = paste(dates4[i,1], mon1, j1, sep = "-")
    k = k + 1
  }
}

Gamma.list = list()
for(i in 1:length(dates.all1)){
  Gamma.list[[i]] = JAXA.daily1[[1]][,1:2]
}
for(i in 1:ncol(G.c1)){
  k1 = which(dates.all1 == dates.all[i])
  x1 = matrix(G.c1[,i], ncol = 1)
  colnames(x1) = dates.all2[i]
  Gamma.list[[k1]] = cbind(Gamma.list[[k1]], x1)
}

Gamma.param = list()
for(i in 1:length(Gamma.list)){
  Gamma.param[[i]] = cbind(JAXA.daily1[[1]][,1:2], matrix(0, nrow = nrow(JAXA.daily1[[1]][,1:2]), ncol = 2))
}

for(i in 1:length(Gamma.list)){
  for(j in 1:nrow(G.c1)){
    if(sum(Gamma.list[[i]][j,] == 0) == 0){
      Gamma.param[[i]][j,3:4] = mlgamma(Gamma.list[[i]][j,-c(1,2)], na.rm = TRUE)
    }
  }
  print(i)
}

hist(qnorm(pgamma(rgamma(1000, shape = Gamma.param[[1]][1,3], rate = Gamma.param[[1]][1,4]),shape = Gamma.param[[1]][1,3], rate = Gamma.param[[1]][1,4])))
hist(rnorm(1000))

SPI.list = list()
for(i in 1:length(Gamma.list)){
  SPI.list[[i]] = Gamma.list[[i]]
}

spi.function = function(x, s, r){
  x1 = rowSums(x==0)/ncol(x)
  x2 = x1 + (1 - x1) * apply(x, 1, pgamma, shape = Gamma.param[[i]][,3], rate = Gamma.param[[i]][,4])
}

for(i in 1:length(Gamma.list)){
    x = Gamma.list[[i]][,-c(1,2)]
    x1 = rowSums(x==0)/ncol(x)
    x2 = x1 + (1 - x1) * apply(x, 1, pgamma, shape = Gamma.param[[i]][,3], rate = Gamma.param[[i]][,4])
    SPI.list[[i]][,-c(1,2)] = qnorm(x1 + (1-x1)*pgamma(x, shape = Gamma.param[[i]][,3], rate = Gamma.param[[i]][,4]))
  print(i)
}

for(i in 1:length(SPI.list)){
  colnames(SPI.list[[i]]) = colnames(Gamma.list[[i]])
}


SPI.Matrix = matrix(0, nrow = nrow(G.c1), ncol = ncol(G.c1))
SPI.dates = NULL
k = 1
for(i in 1:length(SPI.list)){
  for(j in 3:ncol(SPI.list[[i]])){
    SPI.Matrix[,k] = SPI.list[[i]][,j]
    SPI.dates = c(SPI.dates, colnames(SPI.list[[i]])[j])
    k = k + 1
  }
}

colnames(SPI.Matrix) = SPI.dates

SPI.Matrix1 = matrix(0, nrow = nrow(G.c1), ncol = ncol(G.c1))
for(i in 1:ncol(SPI.Matrix)){
  w1 = which(SPI.dates == dates.all2[i])
  SPI.Matrix1[,i] = SPI.Matrix[,w1]
}

colnames(SPI.Matrix1) = dates.all2[1:ncol(SPI.Matrix1)]


SPI.ts = data.frame(Dates = rep(as.Date(dates.all2[1:ncol(SPI.Matrix1)]), 3), SPI = c(SPI.Matrix1[Canberra,], SPI.Matrix1[Port.Moresby,], SPI.Matrix1[Wellington,]),
                    Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(SPI.Matrix1)))



ggplot(SPI.ts, aes(x = Dates, y = SPI)) + geom_line(lwd = 0.3) + facet_wrap(~ Loc, nrow = 3) + theme_bw() + labs(x = "Date", y = "SPI", title = "30 Day Standardised Precipitation Index") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))



##90 day

G.c1 = matrix(0, nrow = nrow(JAXA.all), ncol = ncol(JAXA.all) - 90)
for(i in 1:(ncol(JAXA.all) - 90)){
  G.c1[,i] = rowSums(JAXA.all[,i:(i + 89)])
}

dates.all = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  mon1 = dates4[i,2]
  if(nchar(mon1) == 1){
    mon1 = paste0("0", mon1)
  }
  for(j in 1:dates4[i,3]){
    j1 = j
    if(nchar(j1) == 1){
      j1 = paste0("0", j1)
    }
    dates.all[k] = paste(mon1, j1, sep = "-")
    k = k + 1
  }
}

dates.all = dates.all[1:ncol(G.c1)]

dates.all1 = unique(dates.all)

dates.all2 = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  mon1 = dates4[i,2]
  if(nchar(mon1) == 1){
    mon1 = paste0("0", mon1)
  }
  for(j in 1:dates4[i,3]){
    j1 = j
    if(nchar(j1) == 1){
      j1 = paste0("0", j1)
    }
    dates.all2[k] = paste(dates4[i,1], mon1, j1, sep = "-")
    k = k + 1
  }
}

Gamma.list = list()
for(i in 1:length(dates.all1)){
  Gamma.list[[i]] = JAXA.daily1[[1]][,1:2]
}
for(i in 1:ncol(G.c1)){
  k1 = which(dates.all1 == dates.all[i])
  x1 = matrix(G.c1[,i], ncol = 1)
  colnames(x1) = dates.all2[i]
  Gamma.list[[k1]] = cbind(Gamma.list[[k1]], x1)
}

Gamma.param = list()
for(i in 1:length(Gamma.list)){
  Gamma.param[[i]] = cbind(JAXA.daily1[[1]][,1:2], matrix(0, nrow = nrow(JAXA.daily1[[1]][,1:2]), ncol = 2))
}

for(i in 1:length(Gamma.list)){
  for(j in 1:nrow(G.c1)){
    if(sum(Gamma.list[[i]][j,] == 0) == 0){
      Gamma.param[[i]][j,3:4] = mlgamma(Gamma.list[[i]][j,-c(1,2)], na.rm = TRUE)
    }
  }
  print(i)
}

hist(qnorm(pgamma(rgamma(1000, shape = Gamma.param[[1]][1,3], rate = Gamma.param[[1]][1,4]),shape = Gamma.param[[1]][1,3], rate = Gamma.param[[1]][1,4])))
hist(rnorm(1000))

SPI.list = list()
for(i in 1:length(Gamma.list)){
  SPI.list[[i]] = Gamma.list[[i]]
}

for(i in 1:length(Gamma.list)){
  for(j in 1:nrow(Gamma.list[[i]])){
    x = as.numeric(Gamma.list[[i]][j,-c(1,2)])
    x1 = sum(x==0)/length(x)
    SPI.list[[i]][j,-c(1,2)] = qnorm(x1 + (1-x1)*pgamma(x, shape = Gamma.param[[i]][j,3], rate = Gamma.param[[i]][j,4]))
  }
  print(i)
}

for(i in 1:length(SPI.list)){
  colnames(SPI.list[[i]]) = colnames(Gamma.list[[i]])
}


SPI.Matrix = matrix(0, nrow = nrow(G.c1), ncol = ncol(G.c1))
SPI.dates = NULL
k = 1
for(i in 1:length(SPI.list)){
  for(j in 3:ncol(SPI.list[[i]])){
    SPI.Matrix[,k] = SPI.list[[i]][,j]
    SPI.dates = c(SPI.dates, colnames(SPI.list[[i]])[j])
    k = k + 1
  }
}

colnames(SPI.Matrix) = SPI.dates

SPI.Matrix1 = matrix(0, nrow = nrow(G.c1), ncol = ncol(G.c1))
for(i in 1:ncol(SPI.Matrix)){
  w1 = which(SPI.dates == dates.all2[i])
  SPI.Matrix1[,i] = SPI.Matrix[,w1]
}

colnames(SPI.Matrix1) = dates.all2[1:ncol(SPI.Matrix1)]

Ave90.23 = matrix(0, nrow = nrow(SPI.Matrix1), ncol = ncol(SPI.Matrix1) - 22)
k = 1
for(i in 23:(ncol(SPI.Matrix1) - 22)){
  Ave90.23[,k] = rowMeans(SPI.Matrix1[,(i - 12):(i)])
  k = k + 1
  if(k %% 100 == 0){
    print(k)
  }
}

SPI.ts = data.frame(Dates = rep(as.Date(dates.all2[1:ncol(SPI.Matrix1)]), 3), SPI = c(SPI.Matrix1[Canberra,], SPI.Matrix1[Port.Moresby,], SPI.Matrix1[Wellington,]),
                    Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(SPI.Matrix1)))

SPI.ts.ave = data.frame(Dates = rep(as.Date(dates.all2[23:(ncol(SPI.Matrix1))]), 3),
                        SPI = c(SPI.Matrix1[Canberra,23:(ncol(SPI.Matrix1))],
                                SPI.Matrix1[Port.Moresby,23:(ncol(SPI.Matrix1))], 
                                SPI.Matrix1[Wellington,23:(ncol(SPI.Matrix1))]),
                    Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(Ave90.23)),
                    Average = c(Ave90.23[Canberra,], Ave90.23[Port.Moresby,], Ave90.23[Wellington,]))

Ave90.23.sign = sign(Ave90.23)
Ave90.23.sign3 = Ave90.23.sign[,-ncol(Ave90.23.sign)] - Ave90.23.sign[,-1]



Ave90.23col = matrix(0, nrow = nrow(SPI.Matrix1), ncol = ncol(Ave90.23))
for(i in 1:nrow(SPI.Matrix1)){
  w1 = which(Ave90.23.sign3[i,] == 2)
  w2 = which(Ave90.23.sign3[i,] == -2)
  if(min(w1) > min(w2)){
    w2 = w2[-1]
  }
  for(j in 1:min(length(w1), length(w2))){
    if(min(Ave90.23[i,(w1[j] + 1):w2[j]]) < -1){
      Ave90.23col[i,(w1[j] + 1):w2[j]] = 1
    }
  }
  if(i %% 100 == 0){
    print(i)
  }
}

SPI.ts.ave$colour = c(Ave90.23col[Canberra,], Ave90.23col[Port.Moresby,], Ave90.23col[Wellington,])

g90 = ggplot(SPI.ts.ave, aes(x = Dates)) + geom_bar(stat = "identity", aes(y = SPI, fill = as.factor(colour)), width = 1.25) + geom_line(aes(y = Average)) +
  facet_wrap(~ Loc, nrow = 3) +
 theme_bw() + labs(x = "Date", y = "SPI", title = "90 Day Standardised Precipitation Index", fill = "Status") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14)) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "#3548d1"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Drought", "0" = "Normal")
  )


SOI.day = read.csv("https://data.longpaddock.qld.gov.au/SeasonalClimateOutlook/SouthernOscillationIndex/SOIDataFiles/DailySOI1933-1992Base.csv")

sum(dates2[229:231,3])

which(SOI.day$Year == 2000 & SOI.day$Day == (sum(dates2[229:231,3]) + 1))

SOI.day$scale = scale(SOI.day$SOI)

SOI.90day = NULL
for(i in which(SOI.day$Year == 2000 & SOI.day$Day == (sum(dates2[229:231,3]) + 1)):(nrow(SOI.day) - 89)){
  SOI.90day = c(SOI.90day, mean(SOI.day$scale[i:(i + 89)]))
}


SOI.90day = SOI.90day[1:ncol(SPI.Matrix1)]

SOI.cor = rep(0, length(SOI.90day))
for(i in 1:nrow(SPI.Matrix1)){
  SOI.cor[i] = cor(SPI.Matrix1[i,], SOI.90day)
}

SOI.c.d = data.frame(Latitude = Gamma.list[[1]]$Latitude, Longitude = Gamma.list[[1]]$longitude, Correlation = SOI.cor)

idw1 = idw(formula = Correlation ~ 1, locations = ~Longitude + Latitude, data = SOI.c.d, newdata = pred.grid, idp = 3)

ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "Australasian SOI-SPI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12), legend.position = "right")


SPI.Matrix2 = (SPI.Matrix1 <= -1)

SPI.ts2 = data.frame(Dates = rep(as.Date(dates.all2[1:ncol(SPI.Matrix2)]), 3), SPI = c(SPI.Matrix2[Canberra,], SPI.Matrix2[Port.Moresby,], SPI.Matrix2[Wellington,]),
                    Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(SPI.Matrix2)))

ggplot(SPI.ts2, aes(x = Dates, y = SPI)) + geom_point(size = 0.3) + facet_wrap(~ Loc, nrow = 3) +
  theme_bw() + labs(x = "Date", y = "Drought Status", title = "90 Day Standardised Precipitation Index Drought Indicator") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))


P.mat = matrix(0, nrow = nrow(Ave90.23col), ncol = 2)
pb <- txtProgressBar(min = 1, max = nrow(Ave90.23col), style = 3)
for(i in 1:nrow(Ave90.23col)){
  r1 = rle(Ave90.23col[i,])
  P.mat[i,] = c(sum(r1$values == 0)/sum(r1$lengths[r1$values == 0]), sum(r1$values == 1)/sum(r1$lengths[r1$values == 1]))
  setTxtProgressBar(pb, i)
}

P.df = data.frame(Latitude = Gamma.list[[1]]$Latitude, Longitude = Gamma.list[[1]]$longitude, P0 = P.mat[,1], P1 = P.mat[,2])

idw1 = idw(formula = P0 ~ 1, locations = ~Longitude + Latitude, data = P.df, newdata = pred.grid, idp = 3)
idw2 = idw(formula = P1 ~ 1, locations = ~Longitude + Latitude, data = P.df, newdata = pred.grid, idp = 3)

p0.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.5)+
  coord_cartesian(xlim = c(115,178), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(idw1$var1.pred, idw2$var1.pred), max(idw1$var1.pred, idw2$var1.pred))) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = expression('90 Day Estimate For p'[0])) +  theme_bw() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12), legend.position = "right")

p1.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.5)+
  coord_cartesian(xlim = c(115,180), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                        limits = c(min(idw1$var1.pred, idw2$var1.pred), max(idw1$var1.pred, idw2$var1.pred))) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = expression('90 Day Estimate For p'[1])) +  theme_bw() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12), legend.position = "right")

grid.arrange(p0.plot + labs(x = " ") + theme(legend.position = "none"), p1.plot + labs(x = " ", y = " "), nrow = 1, widths = c(1,1.5))











##1 year

G.c1 = matrix(0, nrow = nrow(JAXA.all), ncol = ncol(JAXA.all) - 365)
pb <- txtProgressBar(min = 1, max = (ncol(JAXA.all) - 365), style = 3)
for(i in 1:(ncol(JAXA.all) - 365)){
  setTxtProgressBar(pb, i)
  G.c1[,i] = rowSums(JAXA.all[,i:(i + 364)]) 
}

Gamma.param = matrix(0, nrow = nrow(G.c1), ncol = 2)
for(i in 1:nrow(G.c1)){
  Gamma.param[i,] = mlgamma(G.c1[i,], na.rm = TRUE)
}

dates.all = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  mon1 = dates4[i,2]
  if(nchar(mon1) == 1){
    mon1 = paste0("0", mon1)
  }
  for(j in 1:dates4[i,3]){
    j1 = j
    if(nchar(j1) == 1){
      j1 = paste0("0", j1)
    }
    dates.all[k] = paste(mon1, j1, sep = "-")
    k = k + 1
  }
}

dates.all1 = unique(dates.all)

dates.all2 = rep(0, sum(dates4[,3]))
k = 1
for(i in 1:nrow(dates4)){
  mon1 = dates4[i,2]
  if(nchar(mon1) == 1){
    mon1 = paste0("0", mon1)
  }
  for(j in 1:dates4[i,3]){
    j1 = j
    if(nchar(j1) == 1){
      j1 = paste0("0", j1)
    }
    dates.all2[k] = paste(dates4[i,1], mon1, j1, sep = "-")
    k = k + 1
  }
}


SPI.list = list()
SPI.list[[1]] = G.c1

pb <- txtProgressBar(min = 1, max = nrow(Gamma.list[[1]]), style = 3)
for(j in 1:nrow(Gamma.list[[1]])){
  setTxtProgressBar(pb, j)
  x = as.numeric(G.c1[j,])
  x1 = sum(x==0)/length(x)
  SPI.list[[1]][j,] = qnorm(x1 + (1-x1)*pgamma(x, shape = Gamma.param[j,1], rate = Gamma.param[j,2]))
}


SPI.Matrix = SPI.list[[1]]

colnames(SPI.Matrix) = dates.all[1:ncol(SPI.Matrix)]

SPI.dates = NULL
k = 1
for(j in 1:ncol(SPI.list[[1]])){
  SPI.Matrix[,k] = SPI.list[[1]][,j]
  SPI.dates = c(SPI.dates, colnames(SPI.Matrix)[j])
  k = k + 1
}

SPI.Matrix1 = matrix(0, nrow = nrow(G.c1), ncol = ncol(G.c1))
for(i in 1:ncol(SPI.Matrix)){
  w1 = which(SPI.dates == dates.all2[i])
  SPI.Matrix1[,i] = SPI.Matrix[,w1]
}


AveYear.90 = matrix(0, nrow = nrow(SPI.Matrix), ncol = ncol(SPI.Matrix) - 89)
k = 1
for(i in 90:(ncol(SPI.Matrix))){
  AveYear.90[,k] = rowMeans(SPI.Matrix[,(i - 89):(i)])
  k = k + 1
  if(k %% 100 == 0){
    print(k)
  }
}

Ave.list = list()
Ave.list[[1]] = AveYear.90

SPI.MEDQ = MEDQ(Ave.list, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

Can1 = AveYear.90[Wellington, ]
ar1 = auto.arima(Can1)
ar(diff(Can1))
plot(Can1, type = "l")
lines(ar1$fitted, col = "blue")

AveYear.90.1 = AveYear.90 < 0
AveYear.90.2 = matrix(0, nrow = nrow(AveYear.90.1), ncol = ncol(AveYear.90.1))
for(i in 1:nrow(AveYear.90.1)){
  diff1 = diff(AveYear.90.1[i,])
  w1 = which(diff1 == 1)
  w2 = which(diff1 == -1)
  if(w1[1] > w2[1]){
    w1 = c(1, w1)
  }
  if(max(w1) > max(w2)){
    w2 = c(w2, ncol(AveYear.90.1))
  }
  for(j in 1:length(w1)){
    m1 = AveYear.90[i,w1[j]:w2[j]]
    if(min(m1) < -1){
      AveYear.90.2[i,w1[j]:w2[j]] = 1
    }
  }
}

plot(AveYear.90[2,], col = AveYear.90.2[2,] + 1)



cdf1 = data.frame(x = seq(0,1000,0.05), y = pgamma(seq(0,1000,0.05),shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))
cdf2 = data.frame(x = G.a3[Port.Moresby1,], y = pgamma(G.a3[Port.Moresby1,],shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))

ggplot(cdf1) + geom_line(aes(x = x, y = y)) + stat_ecdf(data = cdf2, aes(x = x, y = y))




SPI.ts = data.frame(Dates = rep(as.Date(dates.all2[1:ncol(AveYear.90)]), 3),
                    SPI = c(SPI.Matrix[Canberra,1:ncol(AveYear.90)], SPI.Matrix[Port.Moresby,1:ncol(AveYear.90)], SPI.Matrix[Wellington,1:ncol(AveYear.90)]),
                    Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(AveYear.90)))

SPI.ts1 = data.frame(Dates = as.Date(dates.all2[1:ncol(AveYear.90)]),
                    SPI = c(SPI.Matrix[Canberra,1:ncol(AveYear.90)]))

SPI.ts.ave = data.frame(Dates = rep(as.Date(dates.all2[45:(ncol(SPI.Matrix) - 45)]), 3),
                        SPI = c(SPI.Matrix[Canberra,45:(ncol(SPI.Matrix) - 45)],
                                SPI.Matrix[Port.Moresby,45:(ncol(SPI.Matrix) - 45)], 
                                SPI.Matrix[Wellington,45:(ncol(SPI.Matrix) - 45)]),
                        Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(AveYear.90)),
                        Average = c(AveYear.90[Canberra,], AveYear.90[Port.Moresby,], AveYear.90[Wellington,]))

SPI.ts.ave$colour = ifelse(c(AveYear.90.2[Canberra,], AveYear.90.2[Port.Moresby,], AveYear.90.2[Wellington,]) == 1, "Drought", "Normal")


gyear = ggplot(SPI.ts.ave, aes(x = Dates)) + geom_bar(stat = "identity", aes(y = SPI, fill = colour), width = 1.25) + geom_line(aes(y = Average)) +
  facet_wrap(~ Loc, nrow = 3) +
  theme_bw() + labs(x = "Date", y = "SPI", title = "One Year Standardised Precipitation Index") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14)) +
  scale_fill_manual(values = c("Normal" = "#3548d1", "Drought" = "#ff0d23"))



for(i in 1:nrow(AveYear.90)){
  ar1 = auto.arima(AveYear.90[i,])
  
}


acf(AveYear.90[2000,], lag.max = 2000)


SOI.day = read.csv("https://data.longpaddock.qld.gov.au/SeasonalClimateOutlook/SouthernOscillationIndex/SOIDataFiles/DailySOI1933-1992Base.csv")

sum(dates2[229:231,3])

which(SOI.day$Year == 2000 & SOI.day$Day == (sum(dates2[229:231,3]) + 1))

SOI.day$scale = scale(SOI.day$SOI)

SOI.90day = NULL
for(i in which(SOI.day$Year == 2000 & SOI.day$Day == (sum(dates2[229:231,3]) + 1)):(nrow(SOI.day) - 89)){
  SOI.90day = c(SOI.90day, mean(SOI.day$scale[i:(i + 89)]))
}


SOI.90day = SOI.90day[1:ncol(SPI.Matrix1)]

SOI.cor = rep(0, length(SOI.90day))
for(i in 1:nrow(SPI.Matrix1)){
  SOI.cor[i] = cor(SPI.Matrix1[i,], SOI.90day)
}

SOI.c.d = data.frame(Lat = Gamma.list[[1]]$Latitude, Lon = Gamma.list[[1]]$longitude, Correlation = SOI.cor)

idw1 = idw(formula = Correlation ~ 1, locations = ~Lon + Lat, data = SOI.c.d, newdata = pred.grid, idp = 3)

ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "Australasian SOI-SPI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12), legend.position = "right")


SPI.Matrix2 = (SPI.Matrix1 <= -1)

SPI.ts2 = data.frame(Dates = rep(as.Date(dates.all2[1:ncol(SPI.Matrix2)]), 3), SPI = c(SPI.Matrix2[Canberra,], SPI.Matrix2[Port.Moresby,], SPI.Matrix2[Wellington,]),
                     Loc = rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(SPI.Matrix2)))

ggplot(SPI.ts2, aes(x = Dates, y = SPI)) + geom_point(size = 0.3) + facet_wrap(~ Loc, nrow = 3) + theme_bw() +
  labs(x = "Date", y = "Drought Status", title = "90 Day Standardised Precipitation Index Drought Indicator") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14))


#######################
#Wind
#######################

"ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface_gauss/uwnd.10m.gauss.2020.nc"
"ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface_gauss/uwnd.10m.gauss.1948.nc"

Wind.mat = matrix(0, nrow = nlat1 * nlon1, ncol = 4)
k = 1
for(i in 1:nlat1){
  for(j in 1:nlon1){
    Wind.mat[k,1:2] = c(lon1[j], lat1[i])
    k = k + 1
  }
}

Wind.list = list()
ye1 = 1948:2020
k = 1
for(i in 1:length(ye1)){
  if(file.exists("VWIND.nc")){
    file.remove("VWIND.nc")
  }
  if(file.exists("UWIND.nc")){
    file.remove("UWIND.nc")
  }
  file1 = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface_gauss/uwnd.10m.gauss."
  file2 = ye1[i]
  file3 = paste0(file1, file2, ".nc")
  dfile1 = "UWIND.nc"
  download.file(file3, destfile=dfile1, method="libcurl")
  file4 = "ftp://ftp2.psl.noaa.gov/Datasets/ncep.reanalysis.dailyavgs/surface_gauss/vwnd.10m.gauss."
  file5 = paste0(file4, file2, ".nc")
  dfile2 = "VWIND.nc"
  download.file(file5, destfile=dfile2, method="libcurl")
  
  ncin1 <- nc_open(dfile1)
  lon1 <- ncvar_get(ncin1, "lon")
  nlon1 <- dim(lon1)
  lat1 <- ncvar_get(ncin1, "lat", verbose = F)
  nlat1 <- dim(lat1)
  t1 <- ncvar_get(ncin1, "time")
  tunits1 <- ncatt_get(ncin1, "time", "units")
  nt1 <- dim(t1)
  dname1 = "uwnd"
  
  tmp.array1 <- ncvar_get(ncin1, dname1)
  dlname1 <- ncatt_get(ncin1, dname1, "long_name")
  dunits1 <- ncatt_get(ncin1, dname1, "units")
  fillvalue1 <- ncatt_get(ncin1, dname1, "_FillValue")
  
  ncin2 <- nc_open(dfile2)
  lon2 <- ncvar_get(ncin2, "lon")
  nlon2 <- dim(lon2)
  lat2 <- ncvar_get(ncin2, "lat", verbose = F)
  nlat2 <- dim(lat2)
  t2 <- ncvar_get(ncin2, "time")
  tunits2 <- ncatt_get(ncin2, "time", "units")
  nt2 <- dim(t2)
  dname2 = "vwnd"
  
  tmp.array2 <- ncvar_get(ncin2, dname2)
  dlname2 <- ncatt_get(ncin2, dname2, "long_name")
  dunits2 <- ncatt_get(ncin2, dname2, "units")
  fillvalue2 <- ncatt_get(ncin2, dname2, "_FillValue")
  
  for(j in 1:nt2){
    a1 = as.vector(tmp.array1[,,j])
    a2 = as.vector(tmp.array2[,,j])
    Wind.mat[,3] = a1
    Wind.mat[,4] = a2
    Wind.list[[k]] = Wind.mat
    k = k + 1
  }
  print(i)
}

for(i in 1:length(Wind.list)){
  Wind.list[[i]] = Wind.list[[i]][,c(1:4)]
}

for(i in 1:length(Wind.list)){
  colnames(Wind.list[[i]]) = c("Lon", "Lat", "U-wind", "V-wind")
}

for(i in 1:length(Wind.list)){
  Wind.list[[i]] = as.data.frame(Wind.list[[i]])
}

for(i in 1:length(Wind.list)){
  alpha.hat = sqrt(Wind.list[[i]][,3]^2 + Wind.list[[i]][,4]^2)
  theta.1 = asin(Wind.list[[i]][,4]/alpha.hat)
  theta.2 = acos(Wind.list[[i]][,3]/alpha.hat)
  Wind.list[[i]]$thetav = theta.1
  Wind.list[[i]]$thetau = theta.2
  Wind.list[[i]]$alpha = alpha.hat
  if(i %% 100 == 0){
    print(i)
  }
}

for(i in 1:length(Wind.list)){
  Wind.list[[i]]$theta = ifelse(Wind.list[[i]]$thetav > 0, Wind.list[[i]]$thetau, -Wind.list[[i]]$thetau)
}


W1 = rep(0, length(Wind.list))
for(i in 1:length(Wind.list)){
  W1[i] = Wind.list[[i]]$theta[100]
}


breaks = seq(-pi, pi, length.out = 20)
tags = rep(0, length(breaks) - 1)
for(i in 1:(length(breaks) - 1)){
  tags[i] = paste0("[", round(breaks[i],3), ":", round(breaks[i + 1],3), ")")
}

group_tags <- cut(W1, 
                  breaks=breaks, 
                  include.lowest=TRUE, 
                  right=FALSE, 
                  labels=tags)

DF.W = data.frame(Theta = 1:19, Count = as.numeric(table(group_tags)))

p <- ggplot(DF.W, aes(x=Theta, y=Count)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  ylim(-100,120) +
  
  # Custom the theme: no axis title and no cartesian grid
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-2,4), "cm")     # This remove unnecessary margin around plot
  ) +
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = 0)
p

p <- ggplot(DF.W, aes(x=Theta, y=Count)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_bar(stat="identity", fill=alpha("blue", 0.3)) + coord_polar(start = 10)
p


austasia1 = which(Wind.mat[,1] < 180 & Wind.mat[,1] > 100 & Wind.mat[,2] < 4 & Wind.mat[,2] > -55)

Wind.mat1 = Wind.mat[austasia1,]
del = 1
Port.Moresby = which(Wind.mat1[,2] < -9.466967 + del & Wind.mat1[,2] > -9.466967 - del & Wind.mat1[,1] < 147.19 + del & Wind.mat1[,1] > 147.19 - del)[1]
Canberra = which(Wind.mat1[,2] < -35.317 + del & Wind.mat1[,2] > -35.317 - del & Wind.mat1[,1] < 149.126 + del & Wind.mat1[,1] > 149.126 - del)[1]
Wellington = which(Wind.mat1[,2] < -41.267 + del & Wind.mat1[,2] > -41.267 - del & Wind.mat1[,1] < 174.768 + del & Wind.mat1[,1] > 174.768 - del)[1]

for(i in 1:length(Wind.list)){
  Wind.list[[i]] = Wind.list[[i]][austasia1,]
}

W.all = matrix(0, ncol = nrow(Wind.list[[1]]), nrow = length(Wind.list))
for(i in 1:length(Wind.list)){
  W.all[i,] = Wind.list[[i]]$theta
}

alpha.all = matrix(0, nrow = length(Wind.list), ncol = nrow(Wind.list[[1]]))
for(i in 1:length(Wind.list)){
  alpha.all[i,] = Wind.list[[i]]$alpha
}


WS = data.frame(Speed = c(alpha.all[,Canberra], alpha.all[,Port.Moresby], alpha.all[,Wellington]),
                Location = rep(c("Canberra", "Port Moresby", "Wellington"), each = nrow(alpha.all)))


ggplot(WS, aes(x = Speed)) + facet_wrap(~Location) + geom_histogram(bins = 30, fill = "white", colour = "black")  +
  theme_bw() + labs(x = "Average Daily Wind Speed (km/h)", y = "Count",
                    title = "Wind Speed") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14), axis.ticks.y = element_blank())




ac1 = cor(alpha.all)

v.all = matrix(0, nrow = length(Wind.list), ncol = nrow(Wind.list[[1]]))
for(i in 1:length(Wind.list)){
  v.all[i,] = Wind.list[[i]]$thetav
}

u.all = matrix(0, nrow = length(Wind.list), ncol = nrow(Wind.list[[1]]))
for(i in 1:length(Wind.list)){
  u.all[i,] = Wind.list[[i]]$thetau
}

alpha1 = alpha.all[alpha.all[,1] > 0,1]



rw1 = rwrappednormal(1000, mu = circular(3*pi/2), rho = NULL, sd = 1, 
               control.circular = list())

rw2 = rwrappednormal(1000, mu = circular(pi/6), rho = NULL, sd = 1, 
                     control.circular = list())

plot(density(0.5*as.numeric(rw1) + 0.5*as.numeric(rw2)))





###EM algorithm

p.func = function(mu, sigma, w, x){
  out = w * dwrappednormal(x, mu)
}




dwrappednormal(0, mu = c(circular(0)), rho = NULL, sd = c(1), 
               K = 10, min.k = 100)




wrapped = function(theta, mu, sigma){
  (1/(sigma * sqrt(2 * pi))) * sum(exp(-(theta - mu + 2 * pi * seq(-1000, 1000, 1))^2 / (2 * sigma^2)))
}

p.func = function(mu, sigma, w, x){
  denom = rep(0, length(mu))
  for(i in 1:length(mu)){
    denom[i] = w[i] * wrapped(x, mu[i], sigma[i])
  }
  out = rep(0, length(mu))
  for(i in 1:length(mu)){
    out[i] = denom[i]/sum(denom[i])
  }
}



r1 = rwrappednormal(100, mu = circular(0), rho = NULL, sd = 1, 
               control.circular = list())

data1 <- rwrappednormal(10000, mu=circular(0), rho=NULL, 
                        control.circular=list(units="degrees"))
plot(data1)


mean(as.numeric(data1) - 180)






install.packages("MCMCpack")
library("MCMCpack")


rowSums(rdirichlet(100, c(0.5,1)))



(cos(v.all[,1])^2 + sin(u.all[,1])^2)/alpha.all[,1]


r1 = rvonmises(10000, mu = 0, kappa = 1, control.circular=list("radians"))

mle.vonmises(r1)

vonmises(r1)



vdata <- data.frame(x2 = runif(nn <- 1000))
vdata <- transform(vdata, y = rnorm(nn, m = 2+x2, sd = exp(0.2)))  # Bad data!!
fit <- vglm(y  ~ x2, vonmises(zero = 2), data = vdata, trace = TRUE)
coef(fit, matrix = TRUE)
Coef(fit)
with(vdata, range(y))  # Original data
range(depvar(fit))   

rwrappednormal(100, rho = 0.5)


set.seed(1234)
x <- c(rvonmises(n=200, mu=circular(0), kappa=10), rvonmises(n=20, mu=circular(pi/2), kappa=20))
res <- mde.vonmises(x, bw=1, mu=circular(0), kappa=10)
res
plot(circular(0), type='n', xlim=c(-1, 1.75), shrink=1.2)
lines(circular(res$x), res$y)
lines(circular(res$x), res$k, col=2)
legend(1,1.5, legend=c('estimated density', 'MDE'), lty=c(1, 1), col=c(1, 2))


bin1 = rbinom(100, p = 0.3, size = 1)

theta.data = bin1 * rvonmises(100, mu = circular(0), kappa = 10) + (1 - bin1) * rvonmises(100, mu = circular(pi), kappa = 2)

W = matrix(1/2, nrow = 1, ncol = 100)
mu = c(0, 0)
kappa = c(1, 1)
for(i in 1:nrow(W)){
  n = ncol(W)
  W[i,] =  w * dvonmises(theta.data, mu = circular(mu[i]), kappa = kappa[i], log = FALSE)/sum(w * dvonmises(theta.data, mu = circular(mu[i]), kappa = kappa[i], log = FALSE))
  W1 = W[i,]
  w = sum(W1)/n
  x1 = optim(c(mu[i], kappa[i]), wle.vonmises, method = "L-BFGS-B", lower = c(-Inf, 0))
  mu[i] = x1$par[1]
  kappa[i] = x1$par[2]
}






w = 0.5
W1 = 1
theta.data = rvonmises(100, mu = circular(0), kappa = 10)
optim(c(0, 1), wle.vonmises, method = "L-BFGS-B", lower = c(-Inf, 0))




dvonmises(rvonmises(100, mu = circular(0), kappa = 1), mu = circular(0), kappa = 1)




EM.vonmises(theta.data, k = 4)




W1 = W.all[,1]
hist(W1)
plot(circular(0))

lines(circular(res$x), res$y)



W1 = as.circular(W1 + pi, type = "angles", units = "radians", zero = 0)

EM.vonmises(W1, k = 2)

#################
#Von-mises EM CV
#two dim


p = seq(0.1,0.9,0.1)
CV.list = list()
pb <- txtProgressBar(min = 1, max =nrow(alpha.mat), style = 3)
for(i in 1:length(p)){
  CV.list[[i]] = matrix(0, nrow = 100, ncol = 4)
  for(j in 1:100){
    bin1 = rbinom(100, size = 1, p = p[i])
    theta.data = bin1 * rvonmises(100, mu = circular(0), kappa = 4) + (1 - bin1) * rvonmises(100, mu = circular(pi), kappa = 1)
    for(h in 1:4){
      Em1 = EM.vonmises(theta.data, k = h, max.runs = 10000)
      CV.list[[i]][j,h] = Em1$log.lh
    }
  }
  setTxtProgressBar(pb, i)
}
close(pb)


EM.mat2 = matrix(0, nrow = ncol(W.all), ncol = 7)
for(i in 1:ncol(W.all)){
  tryCatch({
  theta.data = W.all[,i]
  Em1 = EM.vonmises(theta.data, k = 2, max.runs = 10000)
  EM.mat2[i,] = c(as.numeric(Em1$mu), as.numeric(Em1$kappa), as.numeric(Em1$alpha), as.numeric(Em1$log.lh))
  }, error = function(e){})
  print(i)
}

colnames(EM.mat2) = c("mu1", "mu2", "kappa1", "kappa2", "w1", "w2", "log.lh")
Em.mat2 = as.data.frame(EM.mat2)


plot(circular(0), type='n', xlim=c(-1.75, 1.75), shrink=1.2)
lines(circular(W.all[,Port.Moresby.wind]),
      EM.list[[Port.Moresby.wind]][7] * dvonmises(circular(W.all[,Port.Moresby.wind]), mu = circular(EM.list[[Port.Moresby.wind]][1]),
                                               kappa = EM.list[[Port.Moresby.wind]][4], log = FALSE) +
        EM.list[[Port.Moresby.wind]][8] * dvonmises(circular(W.all[,Port.Moresby.wind]), mu = circular(EM.list[[Port.Moresby.wind]][2]),
                                                     kappa = EM.list[[Port.Moresby.wind]][5], log = FALSE) +
        EM.list[[Port.Moresby.wind]][9] * dvonmises(circular(W.all[,Port.Moresby.wind]), mu = circular(EM.list[[Port.Moresby.wind]][3]),
                                                     kappa = EM.list[[Port.Moresby.wind]][6], log = FALSE))


plot(circular(0), type='n', xlim=c(-1.75, 1.75), shrink=1.2)
lines(circular(W.all[,Canberra.wind]),
      EM.list[[Canberra.wind]][7] * dvonmises(circular(W.all[,Canberra.wind]), mu = circular(EM.list[[Canberra.wind]][1]),
                                                  kappa = EM.list[[Canberra.wind]][4], log = FALSE) +
        EM.list[[Canberra.wind]][8] * dvonmises(circular(W.all[,Canberra.wind]), mu = circular(EM.list[[Canberra.wind]][2]),
                                                    kappa = EM.list[[Canberra.wind]][5], log = FALSE) +
        EM.list[[Canberra.wind]][9] * dvonmises(circular(W.all[,Canberra.wind]), mu = circular(EM.list[[Canberra.wind]][3]),
                                                    kappa = EM.list[[Canberra.wind]][6], log = FALSE))

plot(circular(0), type='n', xlim=c(-1.75, 1.75), shrink=1.2)
lines(circular(W.all[,Wellington.wind]),
      EM.list[[Wellington.wind]][7] * dvonmises(circular(W.all[,Wellington.wind]), mu = circular(EM.list[[Wellington.wind]][1]),
                                              kappa = EM.list[[Wellington.wind]][4], log = FALSE) +
        EM.list[[Wellington.wind]][8] * dvonmises(circular(W.all[,Wellington.wind]), mu = circular(EM.list[[Wellington.wind]][2]),
                                                kappa = EM.list[[Wellington.wind]][5], log = FALSE) +
        EM.list[[Wellington.wind]][9] * dvonmises(circular(W.all[,Wellington.wind]), mu = circular(EM.list[[Wellington.wind]][3]),
                                                kappa = EM.list[[Wellington.wind]][6], log = FALSE))

y1 = EM.list[[Port.Moresby.wind]][7] * dvonmises(circular(W.all[,Port.Moresby.wind]), mu = circular(EM.list[[Port.Moresby.wind]][1]),
                                                 kappa = EM.list[[Port.Moresby.wind]][4], log = FALSE) +
  EM.list[[Port.Moresby.wind]][8] * dvonmises(circular(W.all[,Port.Moresby.wind]), mu = circular(EM.list[[Port.Moresby.wind]][2]),
                                              kappa = EM.list[[Port.Moresby.wind]][5], log = FALSE) +
  EM.list[[Port.Moresby.wind]][9] * dvonmises(circular(W.all[,Port.Moresby.wind]), mu = circular(EM.list[[Port.Moresby.wind]][3]),
                                              kappa = EM.list[[Port.Moresby.wind]][6], log = FALSE)

y2 = EM.list[[Canberra.wind]][7] * dvonmises(circular(W.all[,Canberra.wind]), mu = circular(EM.list[[Canberra.wind]][1]),
                                             kappa = EM.list[[Canberra.wind]][4], log = FALSE) +
  EM.list[[Canberra.wind]][8] * dvonmises(circular(W.all[,Canberra.wind]), mu = circular(EM.list[[Canberra.wind]][2]),
                                          kappa = EM.list[[Canberra.wind]][5], log = FALSE) +
  EM.list[[Canberra.wind]][9] * dvonmises(circular(W.all[,Canberra.wind]), mu = circular(EM.list[[Canberra.wind]][3]),
                                          kappa = EM.list[[Canberra.wind]][6], log = FALSE)

y3 = EM.list[[Wellington.wind]][7] * dvonmises(circular(W.all[,Wellington.wind]), mu = circular(EM.list[[Wellington.wind]][1]),
                                               kappa = EM.list[[Wellington.wind]][4], log = FALSE) +
  EM.list[[Wellington.wind]][8] * dvonmises(circular(W.all[,Wellington.wind]), mu = circular(EM.list[[Wellington.wind]][2]),
                                            kappa = EM.list[[Wellington.wind]][5], log = FALSE) +
  EM.list[[Wellington.wind]][9] * dvonmises(circular(W.all[,Wellington.wind]), mu = circular(EM.list[[Wellington.wind]][3]),
                                            kappa = EM.list[[Wellington.wind]][6], log = FALSE)

x1 = circular(W.all[,Port.Moresby.wind])

x2 = circular(W.all[,Canberra.wind])

x3 = circular(W.all[,Wellington.wind])


Pm1 = data.frame(x = c(x1,x2,x3), y = c(y1,y2,y3), Type = rep(c("Port Moresby", "Canberra", "Wellington"), each = length(x1)))

Pm1 = Pm1[complete.cases(Pm1),]


Pm1.plot = ggplot(Pm1, aes(x = x, y = sqrt(y))) +
  geom_line()  + facet_wrap(~Type) +
  scale_x_continuous(breaks = seq(-pi, pi, length.out = 5), labels = c("-\u03c0", "-\u03c0/2", "0", "\u03c0/2", "\u03c0")) +
  coord_polar(start = pi/2, direction = -1) +
  ylim(0, max(sqrt(Pm1$y))) + theme_bw() + labs(x = " ", y = " ",
                                          title = "Wind Direction") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=14),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
Pm1.plot




legend(1,1.5, legend=c('estimated density', 'MDE'), lty=c(1, 1), col=c(1, 2))



del = 1
Melb = which(Wind.list[[1]][,2] < -37.8 + del & Wind.list[[1]][,2] > -37.8 - del & Wind.list[[1]][,1] < 144.96 + del & Wind.list[[1]][,1] > 144.96 - del)[1]
Dar =  which(Wind.list[[1]][,2] < -12.397 + del & Wind.list[[1]][,2] > -12.397 - del & Wind.list[[1]][,1] < 130.94 + del & Wind.list[[1]][,1] > 130.94 - del)[1]


del = 1
Port.Moresby.wind = which(Wind.list[[1]][,2] < -9.466967 + del & Wind.list[[1]][,2] > -9.466967 - del & Wind.list[[1]][,1] < 147.19 + del & Wind.list[[1]][,1] > 147.19 - del)[1]
Canberra.wind = which(Wind.list[[1]][,2] < -35.317 + del & Wind.list[[1]][,2] > -35.317 - del & Wind.list[[1]][,1] < 149.126 + del & Wind.list[[1]][,1] > 149.126 - del)[1]
Wellington.wind = which(Wind.list[[1]][,2] < -41.267 + del & Wind.list[[1]][,2] > -41.267 - del & Wind.list[[1]][,1] < 174.768 + del & Wind.list[[1]][,1] > 174.768 - del)[1]

plot(density(W.all[,Port.Moresby]))


mdeaths_mod <- data.frame(
  deaths = as.numeric(mdeaths),
  month = as.numeric(cycle(mdeaths))
)

# Calculate average number of deaths in each month
library(dplyr)
mdeaths_mod <- mdeaths_mod %>%
  group_by(month) %>%
  summarise(deaths = mean(deaths))

mdeaths_plot <- ggplot(mdeaths_mod, aes(x = month, y = deaths)) +
  geom_line() +
  scale_x_continuous(breaks = 1:12)

mdeaths_plot +
  coord_polar() +
  ylim(0, max(mdeaths_mod$deaths))


Pm1 = data.frame(x = circular(W.all[,1]), y = (EM.mat2[1,5] * dvonmises(circular(W.all[,1]), mu = circular(EM.mat2[1,1]), kappa = EM.mat2[1,3], log = FALSE) +
                   EM.mat2[1,6] * dvonmises(circular(W.all[,1]), mu = circular(EM.mat2[1,2]), kappa = EM.mat2[1,4], log = FALSE)))

Pm1 = Pm1[complete.cases(Pm1),]




Pm1.plot = ggplot(Pm1, aes(x = x, y = y)) +
  geom_line() +
  scale_x_continuous(breaks = seq(-pi, pi, length.out = 5), labels = c("-\u03c0", "-\u03c0/2", "0", "\u03c0/2", "\u03c0")) +
  coord_polar() +
  ylim(0, max(Pm1$y)) + theme_bw() + labs(x = " ", y = " ",
                                          title = "Wind Direction")
Pm1.plot


sum(!complete)


library(tidyverse)
library(viridis)

library(tidyverse)

# Create dataset
data <- data.frame(
  id=seq(1,60),
  individual=paste( "Mister ", seq(1,60), sep=""),
  value=sample( seq(10,100), 60, replace=T)
)

# Make the plot
p <- ggplot(Pm1, aes(x=x, y=y)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  # This add the bars with a blue color
  geom_line() +
  
  # Limits of the plot = very important. The negative value controls the size of the inner circle, the positive one is useful to add size over each bar
  
  # Custom the theme: no axis title and no cartesian grid
  
  # This makes the coordinate polar instead of cartesian.
  coord_polar(start = -pi/2) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-pi, pi, length.out = 5)) +
p


wind.samp1 = sample(1:ncol(W.all), 300, replace = FALSE)

W.all1 = W.all[,wind.samp1]
Wind.cor = rep(0, ncol(W.all1)*(ncol(W.all1) + 1)/2)
k = 1
pb <- txtProgressBar(min = 1, max = ncol(Wind.cor), style = 3)
for(i in 1:(ncol(W.all1) - 1)){
  for(j in (i + 1):ncol(W.all1)){
    setTxtProgressBar(pb, k)
    Wind.cor[k] = cor(cos(W.all1[,i]), cos(W.all1[,j]))
    k = k + 1
  }
}



direct.mean = function(theta){
  R.vec = c(sum(cos(theta + pi), na.rm = TRUE), sum(sin(theta + pi), na.rm = TRUE))
  R = sqrt(sum(R.vec^2))
  if(R.vec[1] > 0 & R.vec[2] >= 0){
    theta.hat = atan(R.vec[2]/R.vec[1])
  }else if(R.vec[1] == 0 & R.vec[2]  > 0){
    theta.hat = pi/2
  }else if(R.vec[1] < 1){
    theta.hat = atan(R.vec[2]/R.vec[1]) + pi
  }else if(R.vec[1] >= 0 & R.vec[2] < 0){
    theta.hat = atan(R.vec[1]/R.vec[2]) + 2 * pi
  }else if(R.vec[1] == 0 & R.vec[2] == 0){
    theta.hat = NA
  }
  return(theta.hat)
}

theta.mean = rep(0, ncol(W.all))
for(i in 1:ncol(W.all)){
  theta.mean[i] = direct.mean(W.all[,i])
}

wind.samp1 = sample(1:ncol(W.all), 300, replace = FALSE)

W.all1 = W.all[,wind.samp1] + pi
Wind.cor = rep(0, ncol(W.all1)*(ncol(W.all1) - 1)/2)
k = 1
pb <- txtProgressBar(min = 1, max = length(Wind.cor), style = 3)
for(i in 1:(ncol(W.all1) - 1)){
  for(j in (i + 1):ncol(W.all1)){
    setTxtProgressBar(pb, k)
    Wind.cor[k] = sum(sin(W.all1[,i] - theta.mean[wind.samp1[i]]) * sin(W.all1[,j] - theta.mean[wind.samp1[j]]))/
      sqrt(sum(sin(W.all1[,i] - theta.mean[wind.samp1[i]])^2) * sum(sin(W.all1[,j] - theta.mean[wind.samp1[j]])^2))
    k = k + 1
  }
}

max(Wind.cor, na.rm = TRUE)

Dist.1 = rep(0, length(Wind.cor))
k = 1
pb <- txtProgressBar(min = 1, max = length(Wind.cor), style = 3)
for(i in 1:(ncol(W.all1) - 1)){
  for(j in (i + 1):ncol(W.all1)){
    setTxtProgressBar(pb, k)
    Dist.1[k] = sqrt((Wind.list[[1]][wind.samp1[i],1] - Wind.list[[1]][wind.samp1[j],1])^2 + (Wind.list[[1]][wind.samp1[i],2] - Wind.list[[1]][wind.samp1[j],2])^2)
    k = k + 1
  }
}


plot(Dist.1, Wind.cor, pch = 19, cex = 0.1)

plot(circular(0))

d1 = density(W.all[,150], from = -pi, to = pi)
plot(d1)
sum(find_peaks(d1$y))

plot(circular(0), type='n', xlim=c(-1.75, 1.75), shrink=1.2)
lines(circular(d1$x + pi), sqrt(d1$y))
plot(d1)

peaks1 = rep(0, ncol(W.all))
pb <- txtProgressBar(min = 1, max = ncol(W.all), style = 3)
for(i in 1:ncol(W.all)){
  d1 = density(W.all[complete.cases(W.all[,i]),i])
  peaks1[i] = sum(find_peaks(d1$y, span = 21, ignore_threshold = 0.05))
  setTxtProgressBar(pb, i)
}
close(pb)
table(peaks1)
which(peaks1 == 5)
plot(density(W.all[,1170]))




EM.list = list()
pb <- txtProgressBar(min = 1, max = ncol(W.all), style = 3)
for(i in 1:ncol(W.all)){
  tryCatch({
    theta.data = W.all[,i]
    Em1 = EM.vonmises(theta.data[!is.na(theta.data)], k = peaks1[i], max.runs = 1000)
    EM.list[[i]] = c(as.numeric(Em1$mu), as.numeric(Em1$kappa), as.numeric(Em1$alpha), as.numeric(Em1$log.lh))
  }, error = function(e){})
  setTxtProgressBar(pb, i)
}


for(i in 1:length(EM.list)){
  n1 = length(EM.list[[i]])
  if(n1 == 4){
    names(EM.list[[i]]) = c("mu", "kappa", "w", "loglike")
  }else if(n1 == 7){
    names(EM.list[[i]]) = c("mu1", "mu2", "kappa1", "kappa2", "w1", "w2", "loglike")
  }else if(n1 == 10){
    names(EM.list[[i]]) = c("mu1", "mu2", "mu3", "kappa1", "kappa2", "kappa3", "w1", "w2", "w3", "loglike")
  }else if(n1 == 13){
    names(EM.list[[i]]) = c("mu1", "mu2", "mu3", "mu4", "kappa1", "kappa2", "kappa3", "kappa4", "w1", "w2", "w3", "w4", "loglike")
  }else if(n1 == 16){
    names(EM.list[[i]]) = c("mu1", "mu2", "mu3", "mu4", "mu5", "kappa1", "kappa2", "kappa3", "kappa4", "kappa5", "w1", "w2", "w3", "w4", "w5", "loglike")
  
}






df.1 = data.frame(Lat = Wind.list[[1]][,2], Lon = Wind.list[[1]][,1], Components = peaks1)


pred.grid = data.frame(Lon = coordinates(s1)[,1], Lat = coordinates(s1)[,2])
idw1 = idw(formula = Components ~ 1, locations = ~Lon + Lat, data = df.1, newdata = pred.grid, idp = 2)

K.plot <- ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, cFolour = factor(round(var1.pred))), size = 0.75)+
  scale_color_brewer(palette = 3, type = "seq") + guides(colour = guide_legend(override.aes = list(size=18))) +
  coord_cartesian(xlim = c(115,180), ylim = c(-50, 0)) +
  labs(color = "Number of \nComponents", x = "Longitude", y = "Latitude",
       title = "Number of von Mises Components") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
K.plot
dim(W.all)


head(W.all[,Port.Moresby.wind])
sum(W.all[,Port.Moresby.wind] > pi/3 & W.all[,Port.Moresby.wind] < 2 * pi/3)

sum(dates4[dates4[,1] < 2021,3])

dim(W.all[(nrow(W.all) - sum(dates4[dates4[,1] < 2021,3]) + 1):nrow(W.all),])


Port1 = W.all[(nrow(W.all) - sum(dates4[dates4[,1] < 2021,3]) + 1):nrow(W.all), Port.Moresby.wind]
Can1 = W.all[(nrow(W.all) - sum(dates4[dates4[,1] < 2021,3]) + 1):nrow(W.all), Canberra.wind]
Well1 = W.all[(nrow(W.all) - sum(dates4[dates4[,1] < 2021,3]) + 1):nrow(W.all), Wellington.wind]
sum(Can1 > -pi/4 & Can1 < pi/4)
sum(Can1 > pi/4 & Can1 < 3 * pi/4)
sum(Can1 < -3 * pi/ 4 | Can1 > 3 * pi/ 4)
sum(Can1 < -pi/4 & Can1 > -3 * pi/4)


sum(Can1 > 0) + sum(Can1 <= 0)
sum(Can1 > pi/4 & Can1 <= 3 * pi/ 4) + sum(Can1 <= pi/4 & Can1 > -pi/4) + sum(Can1 <= -pi/4 & Can1 > -3 * pi/4) + sum(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)

sum(Can1 > pi/4 & Can1 <= 3 * pi/ 4)
sum(Can1 <= pi/4 & Can1 > -pi/4)
sum(Can1 <= -pi/4 & Can1 > -3 * pi/4)
sum(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)

p.wind = seq(0, 1, length.out = 5)




seq(-180 + 11.25, 180 + 11.25, 22.5)

W.all2 = W.all[(nrow(W.all) - sum(dates4[dates4[,1] < 2021,3]) + 1):nrow(W.all),]
JAXA.all1 = JAXA.all[,1:sum(dates4[dates4[,1] < 2021, 3])] > 0

Port.cor.south = rep(0, nrow(JAXA.all1))
Port.cor.north = rep(0, nrow(JAXA.all1))
Canb.cor.south = rep(0, nrow(JAXA.all1))
Canb.cor.north = rep(0, nrow(JAXA.all1))
Well.cor.south = rep(0, nrow(JAXA.all1))
Well.cor.north = rep(0, nrow(JAXA.all1))
for(i in 1:nrow(JAXA.all1)){
  Port.cor.south[i] = cor(JAXA.all1[Port.Moresby, which(Port1 > 78.75/180 * pi & Port1 < 101.25/180 * pi)], JAXA.all[i, which(Port1 > 78.75/180 * pi & Port1 < 101.25/180 * pi) - 1])
  Port.cor.north[i] = cor(JAXA.all1[Port.Moresby, which(Port1 < -78.75/180 * pi & Port1 > -101.25/180 * pi)], JAXA.all[i, which(Port1 < -78.75/180 * pi & Port1 > -101.25/180 * pi) - 1])
  Canb.cor.south[i] = cor(JAXA.all1[Canberra, which(Can1 > 78.75/180 * pi & Can1 < 101.25/180 * pi)], JAXA.all[i, which(Can1 > 78.75/180 * pi & Can1 < 101.25/180 * pi) - 1])
  Canb.cor.north[i] = cor(JAXA.all1[Canberra, which(Can1 < -78.75/180 * pi & Can1 > -101.25/180 * pi)], JAXA.all[i, which(Can1 < -78.75/180 * pi & Can1 > -101.25/180 * pi) - 1])
  Well.cor.south[i] = cor(JAXA.all1[Wellington, which(Well1 > 78.75/180 * pi & Well1 < 101.25/180 * pi)], JAXA.all[i, which(Well1 > 78.75/180 * pi & Well1 < 101.25/180 * pi) - 1])
  Well.cor.north[i] = cor(JAXA.all1[Wellington, which(Well1 < -78.75/180 * pi & Well1 > -101.25/180 * pi)], JAXA.all[i, which(Well1 < -78.75/180 * pi & Well1 > -101.25/180 * pi) - 1])
}


Port.df = data.frame(Longitude = JAXA.daily1[[1]][,2], Latitude = JAXA.daily1[[1]][,1], Northerly = Port.cor.north, Southerly = Port.cor.south)
Can.df = data.frame(Longitude = JAXA.daily1[[1]][,2], Latitude = JAXA.daily1[[1]][,1], Northerly = Canb.cor.north, Southerly = Canb.cor.south)
Well.df = data.frame(Longitude = JAXA.daily1[[1]][,2], Latitude = JAXA.daily1[[1]][,1], Northerly = Well.cor.north, Southerly = Well.cor.south)
pred.grid.p = data.frame(Longitude = coordinates(s1)[,1], Latitude = coordinates(s1)[,2])

idw1 = idw(formula = Northerly ~ 1, locations = ~Longitude + Latitude, data = Port.df, newdata = pred.grid.p, idp = 3)
idw2 = idw(formula = Southerly ~ 1, locations = ~Longitude + Latitude, data = Port.df, newdata = pred.grid.p, idp = 3)

idw3 = idw(formula = Northerly ~ 1, locations = ~Longitude + Latitude, data = Can.df, newdata = pred.grid.p, idp = 3)
idw4 = idw(formula = Southerly ~ 1, locations = ~Longitude + Latitude, data = Can.df, newdata = pred.grid.p, idp = 3)

idw5 = idw(formula = Northerly ~ 1, locations = ~Longitude + Latitude, data = Well.df, newdata = pred.grid.p, idp = 3)
idw6 = idw(formula = Southerly ~ 1, locations = ~Longitude + Latitude, data = Well.df, newdata = pred.grid.p, idp = 3)


Canb.errs = err1[Canberra,]
Port.errs = err1[Port.Moresby,]
Well.errs = err1[Wellington,]

mod1 = lm(Canb.errs ~ a)



gP1 = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.5)+
  coord_cartesian(xlim = c(115,180), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "Australasia Extreme Value Count") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

gP2 = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.5)+
  coord_cartesian(xlim = c(115,180), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "Australasia Extreme Value Count") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")



Wind.cor.df = data.frame(Correlation = c(idw1$var1.pred, idw2$var1.pred, idw3$var1.pred,
                                         idw4$var1.pred, idw5$var1.pred, idw6$var1.pred),
                         Longitude = rep(idw1$Longitude, 6), Latitude = rep(idw1$Latitude, 6),
                         Location = rep(c("Port Moresby", "Canberra", "Wellington"), each = length(idw1$Longitude) * 2),
                         Direction = rep(rep(c("North Wind", "South Wind"), each = length(idw1$Longitude)), 3))


ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Wind.cor.df, 
             mapping = aes(x = Longitude, y = Latitude, colour = Correlation), size = 0.5)+ facet_grid(Location ~ Direction) +
  coord_cartesian(xlim = c(115,180), ylim = c(-50, 0)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "One Day Lagged Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 14))


sum(Can1 > pi/4 & Can1 <= 3 * pi/ 4)
sum(Can1 <= pi/4 & Can1 > -pi/4)
sum(Can1 <= -pi/4 & Can1 > -3 * pi/4)
sum(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)



Canb.list.1 = list()
Canb.list.2 = list()
Canb.list.3 = list()
Canb.list.4 = list()
for(i in 1:5){
  Canb.list.1[[i]] =  rep(0, nrow(JAXA.all1))
  Canb.list.2[[i]] =  rep(0, nrow(JAXA.all1))
  Canb.list.3[[i]] =  rep(0, nrow(JAXA.all1))
  Canb.list.4[[i]] =  rep(0, nrow(JAXA.all1))
    for(j in 1:nrow(JAXA.all1)){
      w1 = which(Can1 <= pi/4 & Can1 > -pi/4)
      w2 = which(Can1 > pi/4 & Can1 <= 3 * pi/ 4)
      w3 = which(Can1 <= -pi/4 & Can1 > -3 * pi/4)
      w4 = which(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)
      w1 = w1[w1 > i]
      w2 = w2[w2 > i]
      w3 = w3[w3 > i]
      w4 = w4[w4 > i]
      Canb.list.1[[i]][j] = cor(JAXA.all1[Canberra, w1],
                                  JAXA.all1[j, w1 - i])
      Canb.list.2[[i]][j] = cor(JAXA.all1[Canberra, w2],
                                  JAXA.all1[j, w2 - i])
      Canb.list.3[[i]][j] = cor(JAXA.all1[Canberra, w3],
                                JAXA.all1[j, w3 - i])
      Canb.list.4[[i]][j] = cor(JAXA.all1[Canberra, w4],
                                JAXA.all1[j, w4 - i])
    }
  print(i)
}

a1 = list()
a2 = list()
a3 = list()
a4 = list()
for(i in 1:length(Canb.list.1)){
  a1[[i]] = order(abs(Canb.list.1[[i]]))[1:5]
  a2[[i]] = order(abs(Canb.list.2[[i]]))[1:5]
  a3[[i]] = order(abs(Canb.list.3[[i]]))[1:5]
  a4[[i]] = order(abs(Canb.list.4[[i]]))[1:5]
}

w1 = which(Can1 <= pi/4 & Can1 > -pi/4)
w2 = which(Can1 > pi/4 & Can1 <= 3 * pi/ 4)
w3 = which(Can1 <= -pi/4 & Can1 > -3 * pi/4)
w4 = which(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)
w1 = w1[w1 > 5]
w2 = w2[w2 > 5]
w3 = w3[w3 > 5]
w4 = w4[w4 > 5]
X.1 = log((Canb.errs[w1] + 1)/(1- Canb.errs[w1]))
X.2 = log((Canb.errs[w2] + 1)/(1- Canb.errs[w2]))
X.3 = log((Canb.errs[w3] + 1)/(1- Canb.errs[w3]))
X.4 = log((Canb.errs[w4] + 1)/(1- Canb.errs[w4]))
for(i in 1:length(Canb.list.1)){
  X.1 = cbind(X.1, t(JAXA.all1[a1[[i]], w1 - i]))
  X.2 = cbind(X.2, t(JAXA.all1[a1[[i]], w2 - i]))
  X.3 = cbind(X.3, t(JAXA.all1[a1[[i]], w3 - i]))
  X.4 = cbind(X.4, t(JAXA.all1[a1[[i]], w4 - i]))
}

colnames(X.1) = c("Errs", paste0("Loc",1:25))
X.1 = as.data.frame(X.1)

colnames(X.2) = c("Errs", paste0("Loc",1:25))
X.2 = as.data.frame(X.2)

colnames(X.3) = c("Errs", paste0("Loc",1:25))
X.3 = as.data.frame(X.3)

colnames(X.4) = c("Errs", paste0("Loc",1:25))
X.4 = as.data.frame(X.4)

mod.1 = lm(Errs ~ ., data = X.1)
mod.2 = lm(Errs ~ ., data = X.2)
mod.3 = lm(Errs ~ ., data = X.3)
mod.4 = lm(Errs ~ ., data = X.4)

Canb.errs2 = Canb.errs

Canb.errs2[w1] = (exp(mod.1$fitted.values) - 1)/(exp(mod.1$fitted.values) + 1)
Canb.errs2[w2] = (exp(mod.2$fitted.values) - 1)/(exp(mod.2$fitted.values) + 1)
Canb.errs2[w3] = (exp(mod.3$fitted.values) - 1)/(exp(mod.3$fitted.values) + 1)
Canb.errs2[w4] = (exp(mod.4$fitted.values) - 1)/(exp(mod.4$fitted.values) + 1)

Canb.errs2 = Canb.errs2[6:sum(dates4[dates4[,1] < 2021, 3])]

plot(JAXA.1[Canberra,6:sum(dates4[dates4[,1] < 2021, 3])] - pj1[Canberra,6:sum(dates4[dates4[,1] < 2021, 3])] - Canb.errs2, type = "l")




plot(pj1[Canberra,6:sum(dates4[dates4[,1] < 2021, 3])] + Canb.errs2)
plot(pj1[Canberra,6:sum(dates4[dates4[,1] < 2021, 3])])

Port.list.1 = list()
Port.list.2 = list()
Port.list.3 = list()
Port.list.4 = list()
for(i in 1:5){
  Port.list.1[[i]] =  rep(0, nrow(JAXA.all1))
  Port.list.2[[i]] =  rep(0, nrow(JAXA.all1))
  Port.list.3[[i]] =  rep(0, nrow(JAXA.all1))
  Port.list.4[[i]] =  rep(0, nrow(JAXA.all1))
  for(j in 1:nrow(JAXA.all1)){
    w1 = which(Can1 <= pi/4 & Can1 > -pi/4)
    w2 = which(Can1 > pi/4 & Can1 <= 3 * pi/ 4)
    w3 = which(Can1 <= -pi/4 & Can1 > -3 * pi/4)
    w4 = which(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)
    w1 = w1[w1 > i]
    w2 = w2[w2 > i]
    w3 = w3[w3 > i]
    w4 = w4[w4 > i]
    Port.list.1[[i]][j] = cor(JAXA.all1[Port.Moresby, w1],
                              JAXA.all1[j, w1 - i])
    Port.list.2[[i]][j] = cor(JAXA.all1[Port.Moresby, w2],
                              JAXA.all1[j, w2 - i])
    Port.list.3[[i]][j] = cor(JAXA.all1[Port.Moresby, w3],
                              JAXA.all1[j, w3 - i])
    Port.list.4[[i]][j] = cor(JAXA.all1[Port.Moresby, w4],
                              JAXA.all1[j, w4 - i])
  }
  print(i)
}

a1 = list()
a2 = list()
a3 = list()
a4 = list()
for(i in 1:length(Port.list.1)){
  a1[[i]] = order(abs(Port.list.1[[i]]))[1:5]
  a2[[i]] = order(abs(Port.list.2[[i]]))[1:5]
  a3[[i]] = order(abs(Port.list.3[[i]]))[1:5]
  a4[[i]] = order(abs(Port.list.4[[i]]))[1:5]
}

w1 = which(Port1 <= pi/4 & Port1 > -pi/4)
w2 = which(Port1 > pi/4 & Port1 <= 3 * pi/ 4)
w3 = which(Port1 <= -pi/4 & Port1 > -3 * pi/4)
w4 = which(Port1 <= -3*pi/4 | Port1 > 3 * pi/4)
w1 = w1[w1 > 5]
w2 = w2[w2 > 5]
w3 = w3[w3 > 5]
w4 = w4[w4 > 5]
X.1 = log((Port.errs[w1] + 1)/(1- Port.errs[w1]))
X.2 = log((Port.errs[w2] + 1)/(1- Port.errs[w2]))
X.3 = log((Port.errs[w3] + 1)/(1- Port.errs[w3]))
X.4 = log((Port.errs[w4] + 1)/(1- Port.errs[w4]))
for(i in 1:length(Port.list.1)){
  X.1 = cbind(X.1, t(JAXA.all1[a1[[i]], w1 - i]))
  X.2 = cbind(X.2, t(JAXA.all1[a1[[i]], w2 - i]))
  X.3 = cbind(X.3, t(JAXA.all1[a1[[i]], w3 - i]))
  X.4 = cbind(X.4, t(JAXA.all1[a1[[i]], w4 - i]))
}

colnames(X.1) = c("Errs", paste0("Loc",1:25))
X.1 = as.data.frame(X.1)

colnames(X.2) = c("Errs", paste0("Loc",1:25))
X.2 = as.data.frame(X.2)

colnames(X.3) = c("Errs", paste0("Loc",1:25))
X.3 = as.data.frame(X.3)

colnames(X.4) = c("Errs", paste0("Loc",1:25))
X.4 = as.data.frame(X.4)

mod.1 = lm(Errs ~ ., data = X.1)
mod.2 = lm(Errs ~ ., data = X.2)
mod.3 = lm(Errs ~ ., data = X.3)
mod.4 = lm(Errs ~ ., data = X.4)

Port.errs2 = Port.errs

Port.errs2[w1] = (exp(mod.1$fitted.values) - 1)/(exp(mod.1$fitted.values) + 1)
Port.errs2[w2] = (exp(mod.2$fitted.values) - 1)/(exp(mod.2$fitted.values) + 1)
Port.errs2[w3] = (exp(mod.3$fitted.values) - 1)/(exp(mod.3$fitted.values) + 1)
Port.errs2[w4] = (exp(mod.4$fitted.values) - 1)/(exp(mod.4$fitted.values) + 1)

Port.errs2 = Port.errs2[6:sum(dates4[dates4[,1] < 2021, 3])]

Well.list.1 = list()
Well.list.2 = list()
Well.list.3 = list()
Well.list.4 = list()
for(i in 1:5){
  Well.list.1[[i]] =  rep(0, nrow(JAXA.all1))
  Well.list.2[[i]] =  rep(0, nrow(JAXA.all1))
  Well.list.3[[i]] =  rep(0, nrow(JAXA.all1))
  Well.list.4[[i]] =  rep(0, nrow(JAXA.all1))
  for(j in 1:nrow(JAXA.all1)){
    w1 = which(Can1 <= pi/4 & Can1 > -pi/4)
    w2 = which(Can1 > pi/4 & Can1 <= 3 * pi/ 4)
    w3 = which(Can1 <= -pi/4 & Can1 > -3 * pi/4)
    w4 = which(Can1 <= -3*pi/4 | Can1 > 3 * pi/4)
    w1 = w1[w1 > i]
    w2 = w2[w2 > i]
    w3 = w3[w3 > i]
    w4 = w4[w4 > i]
    Well.list.1[[i]][j] = cor(JAXA.all1[Wellington, w1],
                              JAXA.all1[j, w1 - i])
    Well.list.2[[i]][j] = cor(JAXA.all1[Wellington, w2],
                              JAXA.all1[j, w2 - i])
    Well.list.3[[i]][j] = cor(JAXA.all1[Wellington, w3],
                              JAXA.all1[j, w3 - i])
    Well.list.4[[i]][j] = cor(JAXA.all1[Wellington, w4],
                              JAXA.all1[j, w4 - i])
  }
  print(i)
}

a1 = list()
a2 = list()
a3 = list()
a4 = list()
for(i in 1:length(Well.list.1)){
  a1[[i]] = order(abs(Well.list.1[[i]]))[1:5]
  a2[[i]] = order(abs(Well.list.2[[i]]))[1:5]
  a3[[i]] = order(abs(Well.list.3[[i]]))[1:5]
  a4[[i]] = order(abs(Well.list.4[[i]]))[1:5]
}

w1 = which(Well1 <= pi/4 & Well1 > -pi/4)
w2 = which(Well1 > pi/4 & Well1 <= 3 * pi/ 4)
w3 = which(Well1 <= -pi/4 & Well1 > -3 * pi/4)
w4 = which(Well1 <= -3*pi/4 | Well1 > 3 * pi/4)
w1 = w1[w1 > 5]
w2 = w2[w2 > 5]
w3 = w3[w3 > 5]
w4 = w4[w4 > 5]
X.1 = log((Well.errs[w1] + 1)/(1- Well.errs[w1]))
X.2 = log((Well.errs[w2] + 1)/(1- Well.errs[w2]))
X.3 = log((Well.errs[w3] + 1)/(1- Well.errs[w3]))
X.4 = log((Well.errs[w4] + 1)/(1- Well.errs[w4]))
for(i in 1:length(Well.list.1)){
  X.1 = cbind(X.1, t(JAXA.all1[a1[[i]], w1 - i]))
  X.2 = cbind(X.2, t(JAXA.all1[a1[[i]], w2 - i]))
  X.3 = cbind(X.3, t(JAXA.all1[a1[[i]], w3 - i]))
  X.4 = cbind(X.4, t(JAXA.all1[a1[[i]], w4 - i]))
}

colnames(X.1) = c("Errs", paste0("Loc",1:25))
X.1 = as.data.frame(X.1)

colnames(X.2) = c("Errs", paste0("Loc",1:25))
X.2 = as.data.frame(X.2)

colnames(X.3) = c("Errs", paste0("Loc",1:25))
X.3 = as.data.frame(X.3)

colnames(X.4) = c("Errs", paste0("Loc",1:25))
X.4 = as.data.frame(X.4)

mod.1 = lm(Errs ~ ., data = X.1)
mod.2 = lm(Errs ~ ., data = X.2)
mod.3 = lm(Errs ~ ., data = X.3)
mod.4 = lm(Errs ~ ., data = X.4)

Well.errs2 = Well.errs

Well.errs2[w1] = (exp(mod.1$fitted.values) - 1)/(exp(mod.1$fitted.values) + 1)
Well.errs2[w2] = (exp(mod.2$fitted.values) - 1)/(exp(mod.2$fitted.values) + 1)
Well.errs2[w3] = (exp(mod.3$fitted.values) - 1)/(exp(mod.3$fitted.values) + 1)
Well.errs2[w4] = (exp(mod.4$fitted.values) - 1)/(exp(mod.4$fitted.values) + 1)

Well.errs2 = Well.errs2[6:sum(dates4[dates4[,1] < 2021, 3])]

CanAct = JAXA.1[Canberra,6:sum(dates4[dates4[,1] < 2021, 3])]
PCan = pj1[Canberra,6:sum(dates4[dates4[,1] < 2021, 3])] + Canb.errs2

PortAct = JAXA.1[Port.Moresby,6:sum(dates4[dates4[,1] < 2021, 3])]
PPort = pj1[Port.Moresby,6:sum(dates4[dates4[,1] < 2021, 3])] + Port.errs2

WellAct = JAXA.1[Wellington,6:sum(dates4[dates4[,1] < 2021, 3])]
PWell = pj1[Wellington,6:sum(dates4[dates4[,1] < 2021, 3])] + Well.errs2

plot(WellAct[100:130] - PWell[100:130], type = "l")

file5 = "ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/SPI/DATA/2000/SEMDP_SPI_GNRT6_0.25deg-MON_200005.nc"
download.file(file5, destfile="dfile", method="libcurl")
nc_in = nc_open(dfile)


for(i in 134:nrow(dates4)){
  if(dates4[i,3] == 28){
    JAXA.daily[[i]] = WORLD1
  }else if(dates4[i,3] == 29){
    JAXA.daily[[i]] = WORLD2
  }else if(dates4[i,3] == 30){
    JAXA.daily[[i]] = WORLD3
  }else{
    JAXA.daily[[i]] = WORLD4
  }
  file1 = "ftp://swcem:SEMaP+2004@hokusai.eorc.jaxa.jp/EAWP/GSMaP_GNRT/DATA/"
  file2 = dates4[i,1]
  file3 = dates4[i,2]
  if(nchar(file3) == 1){
    file3 = paste0("0", file3)
  }
  for(j in 1:dates4[i,3]){
    file4 = j
    if(nchar(file4) == 1){
      file4 = paste0("0", file4)
    }
    file5 = paste0(file1, file2, "/", file2, file3, "/SEMDP_GSMaP_GNRT6_0.10deg-DLY_", file2, file3, file4, ".nc")
    dfile = paste0("JAXA.daily.nc", file2, file3, file4)
    download.file(file5, destfile=dfile, method="libcurl")
    
    ncin <- nc_open(dfile)
    
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
    JAXA.daily[[i]][, 2 + j] = as.numeric(a1)
  }
  setTxtProgressBar(pb, i)
}
close(pb)


all.index = c(Canberra, Port.Moresby, Wellington)
G.a1 = matrix(0, nrow = 3, ncol = ncol(JAXA.all) - 365)
for(i in 1:(ncol(JAXA.all) - 365)){
  G.a1[,i] = rowSums(JAXA.all[all.index,i:(i + 364)])
}


all.param = matrix(0, nrow = 3, ncol = 2)
for(i in 1:3){
  all.param[i,] = mlgamma(G.a1[i,])
}


cdf1 = data.frame(x = seq(0,1000,0.05), y = pgamma(seq(0,1000,0.05),shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))
cdf2 = data.frame(x = G.a3[Port.Moresby1,], y = pgamma(G.a3[Port.Moresby1,],shape = G.a2$Shape[Port.Moresby1], rate = G.a2$Rate[Port.Moresby1]))

ggplot(cdf1) + geom_line(aes(x = x, y = y)) + stat_ecdf(data = cdf2, aes(x = x, y = y))


plot(cumsum(sort(G.a3[Port.Moresby1,]))/sum(G.a3[Port.Moresby1,]))

dd.1 = data.frame(x = c(seq(0,max(G.a1[1,]) + 2*sd(G.a1[1,]),length.out = 10000), 
                        seq(0,max(G.a1[2,]) + 2*sd(G.a1[2,]),length.out = 10000),
                        seq(0,max(G.a1[3,]) + 2*sd(G.a1[3,]),length.out = 10000)),
                  y = c(pgamma(seq(0,max(G.a1[1,]) + 2*sd(G.a1[1,]),length.out = 10000),shape = all.param[1,1], rate = all.param[1,2]),
                        pgamma(seq(0,max(G.a1[2,]) + 2*sd(G.a1[2,]),length.out = 10000),shape = all.param[2,1], rate = all.param[2,2]),
                        pgamma(seq(0,max(G.a1[3,]) + 2*sd(G.a1[3,]),length.out = 10000),shape = all.param[3,1], rate = all.param[3,2])),
                  loc = rep(c("Canberra", "Port.Moresby", "Wellington"), each = 10000))

ecdf.sample = sample(1:ncol(G.a1), 100)

dd.2 = data.frame(x = c(G.a1[1,ecdf.sample], G.a1[2,ecdf.sample], G.a1[3,ecdf.sample]),
                  y = c(pgamma(G.a1[1,ecdf.sample], shape = all.param[1,1], rate = all.param[1,2]),
                        pgamma(G.a1[2,ecdf.sample], shape = all.param[2,1], rate = all.param[2,2]),
                        pgamma(G.a1[3,ecdf.sample], shape = all.param[3,1], rate = all.param[3,2])),
                  loc = rep(c("Canberra", "Port.Moresby", "Wellington"), each = length(G.a1[1,ecdf.sample])))


ggplot(dd.1) + geom_line(aes(x = x, y = y), lwd = 1.2) + stat_ecdf(data = dd.2, aes(x = x, y = y), colour = "blue", lty = 1, lwd = 1) + facet_wrap(~loc, scale = "free_x") + theme_bw() +
  labs(y = "Cumulative Probability", x = "Precipitation (mm)", title = "One Year Cumulative Precipitation Gamma ECDF")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size=16))


G.list = list()
k = 1
pb <- txtProgressBar(min = 30, max = length(30:365), style = 3)
for(i in 30:365){
  temp.matrix = matrix(0, nrow = 3, ncol = ncol(JAXA.all)  - i)
  for(j in 1:(ncol(JAXA.all) - i)){
    temp.matrix[,j] = rowSums(JAXA.all[all.index, j:(j + i - 1)])
  }
  G.list[[k]] = temp.matrix
  k = k + 1
  setTxtProgressBar(pb, i)
}


w1 = which(dates.all == "01-01")
Gamma.ts.c = matrix(0, nrow = length(G.list), ncol = 2)
Gamma.ts.p = matrix(0, nrow = length(G.list), ncol = 2)
Gamma.ts.w = matrix(0, nrow = length(G.list), ncol = 2)
pb <- txtProgressBar(min = 1, max = length(G.list), style = 3)
for(i in 1:length(G.list)){
  n1 = ncol(G.list[[i]])
  w2 = w1[w1 < n1]
  Gamma.ts.c[i,] = mlgamma((G.list[[i]][1,w2])[G.list[[i]][1,w2] > 0])
  Gamma.ts.p[i,] = mlgamma((G.list[[i]][2,w2])[G.list[[i]][2,w2] > 0])
  Gamma.ts.w[i,] = mlgamma((G.list[[i]][3,w2])[G.list[[i]][3,w2] > 0])
  setTxtProgressBar(pb, i)
}

Shape.df = data.frame(Value = c(Gamma.ts.c[,1], Gamma.ts.p[,1], Gamma.ts.w[,1]), Duration = rep(30:365, 3), Location = rep(c("Canberra", "Port Moresby", "Wellington"), each = nrow(Gamma.ts.c)))


Rate.df = data.frame(Value = c(Gamma.ts.c[,2], Gamma.ts.p[,2], Gamma.ts.w[,2]), Duration = rep(30:365, 3), Location = rep(c("Canberra", "Port Moresby", "Wellington"), each = nrow(Gamma.ts.c)))


Gamma.df = rbind(Shape.df, Rate.df)

Gamma.df$Parameter = factor(rep(c("Shape", "Rate"), each = nrow(Shape.df)), levels = c("Shape", "Rate"))

ggplot(Gamma.df) + geom_line(aes(x = Duration, y = Value, colour = Location), lwd = 1) + facet_wrap(~Parameter, scales = "free_y",)  +
  labs(color = "Location", x = "Duration (Days)", y = "Value",
       title = "Cumulative Precipitation Gamma MLEs") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size=16), legend.position = "right")




Shape.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Shape") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

Rate.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Rate") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

grid.arrange(Shape.plot + labs(x = " ", y = " "), Rate.plot + labs(x = " ", y = " "),
             nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, hjust = 0.2,
                                       vjust = 2),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), vjust = -1))





Year.cum = matrix(0, nrow = nrow(JAXA.all), ncol = ncol(JAXA.all) - 364)
pb <- txtProgressBar(min = 1, max = ncol(Year.cum), style = 3)
for(i in 1:(ncol(JAXA.all) - 364)){
  Year.cum[,i] = rowSums(JAXA.all[,i:(i+364)])
  setTxtProgressBar(pb, i)
}

Gamma.year.param = matrix(0, nrow = nrow(JAXA.all), ncol = 2)
pb <- txtProgressBar(min = 1, max = ncol(Year.cum), style = 3)
for(i in 1:nrow(Year.cum)){
  Gamma.year.param[i,] = mlgamma(Year.cum[i,])
  setTxtProgressBar(pb, i)
}


Gamma.year.param = as.data.frame(Gamma.year.param)
colnames(Gamma.year.param) = c("Shape", "Rate", "Lon", "Lat")
Gamma.year.param$Lon = JAXA.daily1[[1]][,2]
Gamma.year.param$Lat = JAXA.daily1[[1]][,1]

pred.grid = data.frame(Lon = coordinates(s1)[,1], Lat = coordinates(s1)[,2])
idw1 = idw(formula = Shape ~ 1, locations = ~Lon + Lat, data = Gamma.year.param, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Rate ~ 1, locations = ~Lon + Lat, data = Gamma.year.param, newdata = pred.grid, idp = 3)



Shape.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Shape") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

Rate.plot = ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3)+
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(color = "Estimate", x = "Longitude", y = "Latitude",
       title = "Rate") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

grid.arrange(Shape.plot + labs(x = " ", y = " "), Rate.plot + labs(x = " ", y = " "),
             nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, hjust = 0.2,
                                       vjust = 2),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), vjust = -1))



########################
##Changing prob for 3 loc
########################

all.index = c(Canberra, Port.Moresby, Wellington)


p0.mat = matrix(0, nrow = 3, ncol = length(30:365))
p1.mat = matrix(0, nrow = 3, ncol = length(30:365))
Gamma.Param = list()
pb <- txtProgressBar(min = 30, max = length(30:365), style = 3)
h = 1
for(i in 30:365){
  Temp.Cum = matrix(0, nrow = 3, ncol = ncol(JAXA.all) - i)
  for(j in 1:(ncol(JAXA.all) - i)){
    Temp.Cum[,j] = rowSums(JAXA.all[all.index, j:(j + i - 1)]) 
  }
  Gamma.Param[[i]] = list()
  colnames(Temp.Cum) = dates.all[1:ncol(Temp.Cum)]
  for(j in 1:length(dates.all1)){
    w1 = which(colnames(Temp.Cum) == dates.all1[j])
    Temp.Cum1 = Temp.Cum[,w1]
    Temp.Cum1[Temp.Cum1 == 0] = 0.00001
    Gamma.Param[[i]][[j]] = t(apply(Temp.Cum1, 1, mlgamma))
  }
  SPI.temp = matrix(0, nrow = 3, ncol = ncol(Temp.Cum))
  for(j in 1:ncol(Temp.Cum)){
    w2 = which(dates.all1 == colnames(Temp.Cum)[j])
    SPI.temp[,j] = c(qnorm(pgamma(Temp.Cum[1,j], shape = Gamma.Param[[i]][[w2]][1,1], rate = Gamma.Param[[i]][[w2]][1,2])),
                     qnorm(pgamma(Temp.Cum[2,j], shape = Gamma.Param[[i]][[w2]][2,1], rate = Gamma.Param[[i]][[w2]][2,2])),
                     qnorm(pgamma(Temp.Cum[3,j], shape = Gamma.Param[[i]][[w2]][3,1], rate = Gamma.Param[[i]][[w2]][3,2])))
  }
  Average = matrix(0, nrow = 3, ncol = ncol(SPI.temp) - floor(i/4) + 1)
  k = 1
  for(j in floor(i/4):(ncol(SPI.temp))){
    Average[,k] = rowMeans(SPI.temp[,(j - floor(i/4)):j])
    k = k + 1
  }
  Average1 = Average < 0
  Average2 = matrix(0, nrow = nrow(Average1), ncol = ncol(Average1))
  for(j in 1:nrow(Average1)){
    diff1 = diff(Average1[j,])
    w1 = which(diff1 == 1)
    w2 = which(diff1 == -1)
    if(w1[1] > w2[1]){
      w1 = c(1, w1)
    }
    if(max(w1) > max(w2)){
      w2 = c(w2, ncol(Average1))
    }
    for(k in 1:length(w1)){
      m1 = Average[j,w1[k]:w2[k]]
      if(min(m1) < -1){
        Average2[j,w1[k]:w2[k]] = 1
      }
    }
  }
  for(j in 1:nrow(Average2)){
    diff1 = diff(Average2[j,])
    w1 = which(diff1 == 1)
    w2 = which(diff1 == -1)
    if(min(w1) > min(w2)){
      w3 = c(1, w1)
    }else{
      w3 = w1
    }
    if(max(w1) > max(w2)){
      w4 = c(w2, ncol(Average2))
    }else{
      w4 = w2
    }
    d0.length = rep(0, length(w3))
    for(k in 1:length(w1)){
      d0.length[k] = length((w3[k]):(w4[k]))
    }
    if(min(w1) < min(w2)){
      w6 = c(1, w2)
    }else{
      w6 = w2
    }
    if(max(w1) < max(w2)){
      w5 = c(w1, ncol(Average2))
    }else{
      w5 = w1
    }
    d1.length = rep(0, length(w6))
    for(k in 1:length(w6)){
      d1.length[k] = length((w6[k]):(w5[k]))
    }
    p0.mat[j,h] = 1/(mean(d0.length))
    p1.mat[j,h] = 1/(mean(d1.length))
  }
  h = h + 1
  setTxtProgressBar(pb, i)
}

P.df = data.frame(p = c(p0.mat[1,], p0.mat[2,], p0.mat[3,], p1.mat[1,], p1.mat[2,], p1.mat[3,]),
                  Type = rep(c("p0", "p1"), each = ncol(p0.mat) * 3),
                  Location = rep(rep(c("Canberra", "Port Moresby", "Wellington"), each = ncol(p0.mat)), 2),
                  Days = rep(30:365, 6))


strip.names = c(
  `p0` = expression('p'[0]),
  `p1` = expression('p'[1])
)

ggplot(P.df) + geom_line(aes(x = Days, y = p, colour = Location), size = 1) +
  facet_wrap(~Type, labeller = as_labeller(c(`p0` = "Drought Starting Probability", `p1` = "Drought Ending Probability"))) + theme_bw() +
  labs(color = "Location", x = "Duration (Days)", y = "Probability",
       title = "Cumulative Precipitation Gamma MLEs") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size=16), legend.position = "right")


plot(SPI.Matrix[Canberra,], type = "l")


acf(SPI.Matrix[Canberra,], lag.max = 1000, type = "correlation")
acf(SPI.Matrix[Port.Moresby,], lag.max = 1000)
acf(SPI.Matrix[Wellington,], lag.max = 1000)

bacf <- acf(SPI.Matrix[Canberra,], lag.max = 1000, plot = FALSE, type = "covariance")
bacfdf1 <- with(bacf, data.frame(lag, acf))

bacf <- acf(SPI.Matrix[Port.Moresby,], lag.max = 1000, plot = FALSE, type = "covariance")
bacfdf2 <- with(bacf, data.frame(lag, acf))

bacf <- acf(SPI.Matrix[Wellington,], lag.max = 1000, plot = FALSE, type = "covariance")
bacfdf3 <- with(bacf, data.frame(lag, acf))

bacfdf <- as.data.frame(rbind(bacfdf1, bacfdf2, bacfdf3))
bacfdf$Type = factor(c(rep("Canberra", nrow(bacfdf1)), rep("Port Moresby", nrow(bacfdf2)), rep("Wellington", nrow(bacfdf3))))

ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + facet_wrap(~Type) + theme_bw() +
  labs(x = "Lag (Days)", y = "Autocovariance Function",
       title = "One Year SPI Autocovariance") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size = 13, colour = "black"),
        legend.position = "right")



ggplot() + geom_polygon(data = Australasiamap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  coord_cartesian(xlim = c(110,180), ylim = c(-50, 5)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  labs(x = "Longitude", y = "Latitude",
       title = "Australasian Countries") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  geom_text(aes(x = 133, y = -25, label = "Australia"), size = 7) + 
  geom_segment(aes(y = -6, x = 145, yend = 1, xend = 158), size = 0.8) + 
  geom_text(aes(x = 166, y = 2, label = "Papua New\nGuinea"), size = 7) + 
  geom_segment(aes(y = -39, x = 175, yend = -30, xend = 175), size = 0.8) + 
  geom_text(aes(x = 175, y = -25, label = "New\nZealand"), size = 7) + 
  geom_segment(aes(y = -35.31, x = 149, yend = -32, xend = 158), size = 0.8) +
  geom_text(aes(x = 158, y = -30.5, label = "Canberra"), size = 4) + 
  geom_segment(aes(y = -41.3, x = 174.78, yend = -42, xend = 163), size = 0.8) +
  geom_text(aes(x = 158, y = -42, label = "Wellington"), size = 4) + 
  geom_segment(aes(y = -9.45, x = 147.19, yend = -15, xend = 155), size = 0.8) +
  geom_text(aes(x = 156, y = -16, label = "Port Moresby"), size = 4)


A1 = J1[,2:1]
A1 = as.data.frame(A1)
colnames(A1) = c("Lon", "Lat")
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

AusJ = which(a3$ISO3 == "AUS")

J1 = J1[AusJ,]

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = J1, 
             mapping = aes(x = Longitude, y = Latitude, colour = Precip), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Square Root Intercept Estimate")


write.csv(J1, file = "JAXAFC", row.names = FALSE)


JAXA.all = matrix(0, nrow = nrow(JAXA.Monthly[[1]]), ncol = length(JAXA.Monthly))
k = 1
for(i in 1:length(JAXA.Monthly)){
  JAXA.all[,i] = JAXA.Monthly[[i]]$Precip
}

A1 = JAXA.Monthly[[1]][,3:2]
A1 = as.data.frame(A1)
colnames(A1) = c("Lon", "Lat")
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

AusJ = which(a3$ISO3 == "AUS")


  
J1 = list()
J1[[1]] = t(apply(JAXA.all,1, cumsum))

J1[[1]] = J1[[1]][AusJ,]

dim(J1[[1]])


M1 = MEDQ(J1, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

dd1 = cbind(M1, seq(0,1,0.001))
v1 = NULL
for(i in unique(M1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Locations = data.frame(JAXA.Monthly[[1]][AusJ, 2], JAXA.Monthly[[1]][AusJ, 3]) 

JAXAEDQloc = data.frame(longitude = Locations[unique(M1),2], latitude = Locations[unique(M1),1], Quantile = v1)

g.EDQ1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(JAXAEDQloc, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.EDQ1


write.csv(JAXAEDQloc, file = "JAXAEDQLoc", row.names = FALSE)


nunique = rep(0, 1000)
for(i in 1:1000){
  p = seq(0,1, length.out = i)
  NJ.MEDQ = MEDQ(X1, p = p, weight = FALSE, scale = TRUE)
  nunique[i] = length(unique(NJ.MEDQ))
  print(i)
}




JAXAEDQloc = data.frame()

