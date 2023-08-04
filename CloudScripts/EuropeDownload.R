#Europe
"ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/03_Austra/"


years = c(rep(2000,10),sort(rep(seq(2001,2019),12)), rep(2020,1))
months = c(rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),20), "01")
days = c(rep(c("31","28","31","30","31","30","31","31","30","31","30","31"),20),"31")
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

getwd()
setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/Aus1")
for(i in 232:nrow(dates)){
  a = getURL(paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/03_Austra/", dates[i,1], "/", dates[i,2], "/"))
  a1 = gregexpr("gsmap",a)[[1]]
  a.1 = gregexpr("zip",a)[[1]]
  a2 = substr(a, a1[1], a.1[1] + 2)
  file = paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/03_Austra/",dates[i,1], "/", dates[i,2], "/",a2)
  temp <- tempfile()
  download.file(file,temp)
  a3 = unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
             junkpaths = FALSE, exdir = ".", unzip = "internal",
             setTimes = FALSE)
  a4 = read.csv(a3)
  unlink(substr(a3,3,nchar(a3)))
  if(class(a4[,3]) == "factor"){
    a.4 = as.numeric(as.character(a4[,3]))
    a.4[is.na(a.4)] = 0
    a4[,3] = a.4
  }
  if(class(a4[,4]) == "factor"){
    a.4 = as.numeric(as.character(a5[,4]))
    a.4[is.na(a.4)] = 0
    a4[,4] = a.4
  }
  a4[,3:4] = a4[,3:4] * 24
  a4 = cbind(a4, rep(as.numeric(dates[i,1]), nrow(a4)), rep(as.numeric(dates[i,2]), nrow(a4)))
  colnames(a4) = c("Lat", "Lon", "RainRate", "Gauge.calibratedRain", "Year", "Month")
  EU1 = list()
  EU1[[1]] = a4
  for(j in 2:as.numeric(dates[i,3])){
    a2 = substr(a, a1[j], a.1[j] + 2)
    file = paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/03_Austra/",dates[i,1], "/", dates[i,2], "/",a2)
    temp <- tempfile()
    download.file(file,temp)
    a3 = unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
               junkpaths = FALSE, exdir = ".", unzip = "internal",
               setTimes = FALSE)
    a5 = read.csv(a3)
    unlink(substr(a3,3,nchar(a3)))
    if(class(a5[,3]) == "factor"){
      a.4 = as.numeric(as.character(a5[,3]))
      a.4[is.na(a.4)] = 0
      a5[,3] = a.4
    }
    if(class(a5[,4]) == "factor"){
      a.4 = as.numeric(as.character(a4[,4]))
      a.4[is.na(a.4)] = 0
      a5[,4] = a.4
    }
    EU1[[j]] = a5
    if(nrow(a5) == nrow(a4)){
      a4[,3:4] = a4[,3:4] + a5[,3:4] * 24
    }
  }
  Austra[[i]] = a4
  write.csv(a4, file = paste0("Austra", dates[i,1], dates[i,2]), row.names = FALSE)
  print(i)
}

nrowEU1 = NULL
for(i in 1:length(EU1)){
  nrowEU1 = c(nrowEU1, nrow(EU1[[i]]))
}
unique(nrowEU1)

i = 191
a4 = cbind(EU1[[4]], rep(as.numeric(dates[i,1]), nrow(EU1[[4]])), rep(as.numeric(dates[i,2]), nrow(EU1[[4]])))
colnames(a4) = c("Lat", "Lon", "RainRate", "Gauge.calibratedRain", "Year", "Month")
a4[,3:4] = a4[,3:4]*24
for(j in 5:31){
  a4[,3:4] = a4[,3:4] + EU1[[j]][,3:4] * 24
}
Europe[[191]] = a4

i = 194
a = getURL(paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/07_Europe/", dates[i,1], "/", dates[i,2], "/"))
a1 = gregexpr("gsmap",a)[[1]]
a.1 = gregexpr("zip",a)[[1]]
a2 = substr(a, a1[1], a.1[1] + 2)
file = paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/07_Europe/",dates[i,1], "/", dates[i,2], "/",a2)
temp <- tempfile()
download.file(file,temp)
a3 = unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
           junkpaths = FALSE, exdir = ".", unzip = "internal",
           setTimes = FALSE)
a4 = read.csv(a3)
unlink(substr(a3,3,nchar(a3)))
if(class(a4[,3]) == "factor"){
  a.4 = as.numeric(as.character(a4[,3]))
  a.4[is.na(a.4)] = 0
  a4[,3] = a.4
}
if(class(a4[,4]) == "factor"){
  a.4 = as.numeric(as.character(a5[,4]))
  a.4[is.na(a.4)] = 0
  a4[,4] = a.4
}
a4[,3:4] = a4[,3:4] * 24
a4 = cbind(a4, rep(as.numeric(dates[i,1]), nrow(a4)), rep(as.numeric(dates[i,2]), nrow(a4)))
colnames(a4) = c("Lat", "Lon", "RainRate", "Gauge.calibratedRain", "Year", "Month")
for(j in 30:31){
  a2 = substr(a, a1[j], a.1[j] + 2)
  file = paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/07_Europe/",dates[i,1], "/", dates[i,2], "/",a2)
  temp <- tempfile()
  download.file(file,temp)
  a3 = unzip(temp, files = NULL, list = FALSE, overwrite = TRUE,
             junkpaths = FALSE, exdir = ".", unzip = "internal",
             setTimes = FALSE)
  a5 = read.csv(a3)
  unlink(substr(a3,3,nchar(a3)))
  if(class(a5[,3]) == "factor"){
    a.4 = as.numeric(as.character(a5[,3]))
    a.4[is.na(a.4)] = 0
    a5[,3] = a.4
  }
  if(class(a5[,4]) == "factor"){
    a.4 = as.numeric(as.character(a5[,4]))
    a.4[is.na(a.4)] = 0
    a5[,4] = a.4
  }
  if(nrow(a5) == nrow(a4)){
    a4[,3:4] = a4[,3:4] + a5[,3:4] * 24
  }
}
Europe[[i]] = a4
write.csv(a4, file = paste0("Europe", dates[i,1], dates[i,2]), row.names = FALSE)
print(i)
