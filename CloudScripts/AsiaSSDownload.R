#AsiaSS



years = c(rep(2000,10),sort(rep(seq(2001,2018),12)), rep(2019,6))
months = c(rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),19), "01", "02", "03", "04", "05", "06")
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

AsiaSS = list()
for(i in 1:nrow(dates)){
  a = getURL(paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/05_AsiaSS/", dates[i,1], "/", dates[i,2], "/"))
  a1 = gregexpr("gsmap",a)[[1]]
  a.1 = gregexpr("zip",a)[[1]]
  a2 = substr(a, a1[1], a.1[1] + 2)
  file = paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/05_AsiaSS/",dates[i,1], "/", dates[i,2], "/",a2)
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
    a.4 = as.numeric(as.character(a4[,4]))
    a.4[is.na(a.4)] = 0
    a4[,4] = a.4
  }
  a4[,3:4] = a4[,3:4] * 24
  a4 = cbind(a4, rep(as.numeric(dates[i,1]), nrow(a4)), rep(as.numeric(dates[i,2]), nrow(a4)))
  colnames(a4) = c("Lat", "Lon", "RainRate", "Gauge.calibratedRain", "Year", "Month")
  for(j in 2:as.numeric(dates[i,3])){
    a2 = substr(a, a1[j], a.1[j] + 2)
    file = paste0("ftp://rainmap:Niskur+1404@hokusai.eorc.jaxa.jp/standard/v6/txt/daily/00Z-23Z/05_AsiaSS/",dates[i,1], "/", dates[i,2], "/",a2)
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
    if(nrow(a5) == nrow(a4)){
      a4[,3:4] = a4[,3:4] + a5[,3:4] * 24
    }
  }
  AsiaSS[[i]] = a4
  print(i)
}
