#Monthly1

Stations.1 <-read.csv("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/MonthlyRainfall1/XC01X_StnDet_999999999537828.txt", header = FALSE)

library(RCurl)
Monthly.Rain.Gauge.Data1 <- list()     #Creating an empty list in which data.frames will be added to
for(i in 1:nrow(Stations.1)){
  tryCatch({
  k <- Stations.1[i,2]         #The data starts on 1/1/2017 at time 00.00 (at the earliest) and ends 15/8/2018 at 09.00 East Australia Time   
  k1 = k
  if(nchar(k) == 5){             #The observation are done in half hour intervals of Precipitation since 09.00 local time in mm
    k <- paste(0,k, sep = "")   #Need to make all station numbers 6 string characters
  }
  if(nchar(k) == 4){
    k <- paste(0,0,k, sep = "") #Defining the station number
  }
  file <- paste("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/MonthlyRainfall1/XC01X_Data_", k, "_999999999537828.txt", sep = "")
  latitude <- Stations.1[i,7]  #The Latitude coordinate of the station as a vector of length equal to the number of obs
  longitude <- Stations.1[i,8] #The Longitute coordinate of the station as a vector of length equal to the number of obs
  Start.Year <- Stations.1[i,14]
  End.Year <- Stations.1[i,15]                                
  range <- Start.Year:End.Year
  date.check <- paste(range, ", ", sep = "")
  base.text <- getURL(file)
  start.bit <- gregexpr("xc", base.text)[[1]][2]
  check.bits = gregexpr(",", base.text)[[1]]
  check.bits = c(start.bit - 1, check.bits[check.bits > start.bit])
  Temp.Matrix <- NULL
  for(j in 1:(length(check.bits) - 1)){
    Temp.Matrix = c(Temp.Matrix, substr(base.text, check.bits[j] + 1, check.bits[j + 1] - 1))
  }
  Temp.Matrix = matrix(Temp.Matrix, ncol = 11, nrow = (length(check.bits) - 1)/11, byrow = TRUE)
  row.names(Temp.Matrix) <- NULL  
  Temp.Matrix = Temp.Matrix[,c(7,8,9)]
  Year = NULL
  Month = NULL
  for(j in 1:nrow(Temp.Matrix)){
    Year = c(Year, substr(Temp.Matrix[j,1],7,10))
    Month = c(Month, substr(Temp.Matrix[j,1],4,5))
  }
  Temp.Matrix = cbind(as.numeric(Year), as.numeric(Month), as.numeric(Temp.Matrix[,3]), rep(latitude, nrow(Temp.Matrix)), rep(longitude, nrow(Temp.Matrix)), rep(k1, nrow(Temp.Matrix)))  
  colnames(Temp.Matrix) <- c("Year", "Month", "Rainfall", "Latitude", "Longitude", "Station")
  Monthly.Rain.Gauge.Data1[[i]] <- data.frame(Temp.Matrix)    #Adding the new data.frame to the list of data.frames
  print(i)
  },error = function(e){})
}

#Monthly2

Stations.2 <-read.csv("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/MonthlyRainfall2/XC01X_StnDet_999999999537827.txt", header = FALSE)


Monthly.Rain.Gauge.Data2 <- list()     #Creating an empty list in which data.frames will be added to
for(i in 1:nrow(Stations.2)){
  tryCatch({
    k <- Stations.2[i,2]         #The data starts on 1/1/2017 at time 00.00 (at the earliest) and ends 15/8/2018 at 09.00 East Australia Time   
    k1 = k
    if(nchar(k) == 5){             #The observation are done in half hour intervals of Precipitation since 09.00 local time in mm
      k <- paste(0,k, sep = "")   #Need to make all station numbers 6 string characters
    }
    if(nchar(k) == 4){
      k <- paste(0,0,k, sep = "") #Defining the station number
    }
    file <- paste("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/MonthlyRainfall2/XC01X_Data_", k, "_999999999537827.txt", sep = "")
    latitude <- Stations.2[i,7]  #The Latitude coordinate of the station as a vector of length equal to the number of obs
    longitude <- Stations.2[i,8] #The Longitute coordinate of the station as a vector of length equal to the number of obs
    Start.Year <- Stations.2[i,14]
    End.Year <- Stations.2[i,15]                                
    range <- Start.Year:End.Year
    date.check <- paste(range, ", ", sep = "")
    base.text <- getURL(file)
    start.bit <- gregexpr("xc", base.text)[[1]][2]
    check.bits = gregexpr(",", base.text)[[1]]
    check.bits = c(start.bit - 1, check.bits[check.bits > start.bit])
    Temp.Matrix <- NULL
    for(j in 1:(length(check.bits) - 1)){
      Temp.Matrix = c(Temp.Matrix, substr(base.text, check.bits[j] + 1, check.bits[j + 1] - 1))
    }
    Temp.Matrix = matrix(Temp.Matrix, ncol = 11, nrow = (length(check.bits) - 1)/11, byrow = TRUE)
    row.names(Temp.Matrix) <- NULL  
    Temp.Matrix = Temp.Matrix[,c(7,8,9)]
    Year = NULL
    Month = NULL
    for(j in 1:nrow(Temp.Matrix)){
      Year = c(Year, substr(Temp.Matrix[j,1],7,10))
      Month = c(Month, substr(Temp.Matrix[j,1],4,5))
    }
    Temp.Matrix = cbind(as.numeric(Year), as.numeric(Month), as.numeric(Temp.Matrix[,3]), rep(latitude, nrow(Temp.Matrix)), rep(longitude, nrow(Temp.Matrix)), rep(k1, nrow(Temp.Matrix)))  
    colnames(Temp.Matrix) <- c("Year", "Month", "Rainfall", "Latitude", "Longitude", "Station")
    Monthly.Rain.Gauge.Data2[[i]] <- data.frame(Temp.Matrix)    #Adding the new data.frame to the list of data.frames
    print(i)
  },error = function(e){})
}


"https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/Monthly%20Satellite%20Data/Monthly.Station.001001.Satellite"
Sat.2 = list()
for(i in 1:nrow(Stations.2)){
  tryCatch({
  k <- Stations.2[i,2]         #The data starts on 1/1/2017 at time 00.00 (at the earliest) and ends 15/8/2018 at 09.00 East Australia Time   
  k1 = k
  if(nchar(k) == 5){             #The observation are done in half hour intervals of Precipitation since 09.00 local time in mm
    k <- paste(0,k, sep = "")   #Need to make all station numbers 6 string characters
  }
  if(nchar(k) == 4){
    k <- paste(0,0,k, sep = "") #Defining the station number
  }
  Sat.2[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/Monthly%20Satellite%20Data/Monthly.Station.", k, ".Satellite"), header = TRUE)
  print(i)
  }, error = function(e){})
}

stat1 = NULL
for(i in 1:length(Sat.2)){
  tryCatch({
  stat1 = c(stat1, Sat.2[[i]][1,6])
  }, error = function(e){})
}

stat2 = NULL
for(i in 1:length(Monthly.Rain.Gauge.Data2)){
  tryCatch({
    stat2 = c(stat2, Monthly.Rain.Gauge.Data2[[i]][1,6])
  }, error = function(e){})
}

stat3 = rep(0, length(stat2))
for(i in 1:length(stat2)){
  stat3[i] = which(stat1 == stat2[i])
}







#Monthly3

Stations.3 <-read.csv("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/MonthlyRainfall3/XC01X_StnDet_999999999537826.txt", header = FALSE)


Monthly.Rain.Gauge.Data3 <- list()     #Creating an empty list in which data.frames will be added to
for(i in 1:nrow(Stations.3)){
  tryCatch({
    k <- Stations.3[i,2]         #The data starts on 1/1/2017 at time 00.00 (at the earliest) and ends 15/8/2018 at 09.00 East Australia Time   
    k1 = k
    if(nchar(k) == 5){             #The observation are done in half hour intervals of Precipitation since 09.00 local time in mm
      k <- paste(0,k, sep = "")   #Need to make all station numbers 6 string characters
    }
    if(nchar(k) == 4){
      k <- paste(0,0,k, sep = "") #Defining the station number
    }
    file <- paste("https://raw.githubusercontent.com/hinestein/MonthlyGauge/master/MonthlyRainfall3/XC01X_Data_", k, "_999999999537826.txt", sep = "")
    latitude <- Stations.3[i,7]  #The Latitude coordinate of the station as a vector of length equal to the number of obs
    longitude <- Stations.3[i,8] #The Longitute coordinate of the station as a vector of length equal to the number of obs
    Start.Year <- Stations.3[i,14]
    End.Year <- Stations.3[i,15]                                
    range <- Start.Year:End.Year
    date.check <- paste(range, ", ", sep = "")
    base.text <- getURL(file)
    start.bit <- gregexpr("xc", base.text)[[1]][2]
    check.bits = gregexpr(",", base.text)[[1]]
    check.bits = c(start.bit - 1, check.bits[check.bits > start.bit])
    Temp.Matrix <- NULL
    for(j in 1:(length(check.bits) - 1)){
      Temp.Matrix = c(Temp.Matrix, substr(base.text, check.bits[j] + 1, check.bits[j + 1] - 1))
    }
    Temp.Matrix = matrix(Temp.Matrix, ncol = 11, nrow = (length(check.bits) - 1)/11, byrow = TRUE)
    row.names(Temp.Matrix) <- NULL  
    Temp.Matrix = Temp.Matrix[,c(7,8,9)]
    Year = NULL
    Month = NULL
    for(j in 1:nrow(Temp.Matrix)){
      Year = c(Year, substr(Temp.Matrix[j,1],7,10))
      Month = c(Month, substr(Temp.Matrix[j,1],4,5))
    }
    Temp.Matrix = cbind(as.numeric(Year), as.numeric(Month), as.numeric(Temp.Matrix[,3]), rep(latitude, nrow(Temp.Matrix)), rep(longitude, nrow(Temp.Matrix)), rep(k1, nrow(Temp.Matrix)))  
    colnames(Temp.Matrix) <- c("Year", "Month", "Rainfall", "Latitude", "Longitude", "Station")
    Monthly.Rain.Gauge.Data3[[i]] <- data.frame(Temp.Matrix)    #Adding the new data.frame to the list of data.frames
    print(i)
  },error = function(e){})
}

for(i in 1:length(Monthly.Rain.Gauge.Data3)){
  tryCatch({
    colnames(Monthly.Rain.Gauge.Data3[[i]]) = c("Year", "Month", "Rainfall", "Latitude", "Longitude", "Station")
  }, error = function(e){})
}

locations = NULL
for(i in 1:length(Monthly.Rain.Gauge.Data1)){
  locations = rbind(locations, Monthly.Rain.Gauge.Data1[[i]][1,4:5])
}

for(i in 1:length(Monthly.Rain.Gauge.Data2)){
  locations = rbind(locations, Monthly.Rain.Gauge.Data2[[i]][1,4:5])
}

for(i in 1:length(Monthly.Rain.Gauge.Data3)){
  locations = rbind(locations, Monthly.Rain.Gauge.Data3[[i]][1,4:5])
}

library(plot3D)
library(rworldmap)
newmap = getMap()
plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(locations[,2], locations[,1], col = "blue", cex = 0., pch = 19, add = TRUE)

aus = readOGR(".", "COM20111216_ELB_region")

locations1 = NULL
for(i in 1:length(Monthly.Rain.Gauge.Data1)){
  locations1 = rbind(locations1, Monthly.Rain.Gauge.Data1[[i]][1,4:5])
}

coordinates(locations1) = ~Longitude + Latitude
proj4string(locations1) = "+proj=longlat +ellps=GRS80 +no_defs"
a4 = over(locations1, aus)

locations2 = NULL
for(i in 1:length(Monthly.Rain.Gauge.Data2)){
  locations2 = rbind(locations2, Monthly.Rain.Gauge.Data2[[i]][1,4:5])
}

coordinates(locations2) = ~Longitude + Latitude
proj4string(locations2) = "+proj=longlat +ellps=GRS80 +no_defs"
a5 = over(locations2, aus)

locations3 = NULL
for(i in 1:length(Monthly.Rain.Gauge.Data3)){
  locations3 = rbind(locations3, Monthly.Rain.Gauge.Data3[[i]][1,4:5])
}

coordinates(locations3) = ~Longitude + Latitude
proj4string(locations3) = "+proj=longlat +ellps=GRS80 +no_defs"
a6 = over(locations3, aus)

Monthly.Rain.Gauge.Data1 = Monthly.Rain.Gauge.Data1[which(!is.na(a4$STATE))]
Monthly.Rain.Gauge.Data2 = Monthly.Rain.Gauge.Data2[which(!is.na(a5$STATE))]
Monthly.Rain.Gauge.Data3 = Monthly.Rain.Gauge.Data3[which(!is.na(a6$STATE))]

locations = NULL
for(i in 1:length(Monthly.Rain.Gauge.Data1)){
  locations = rbind(locations, Monthly.Rain.Gauge.Data1[[i]][1,4:5])
}

for(i in 1:length(Monthly.Rain.Gauge.Data2)){
  locations = rbind(locations, Monthly.Rain.Gauge.Data2[[i]][1,4:5])
}

for(i in 1:length(Monthly.Rain.Gauge.Data3)){
  locations = rbind(locations, Monthly.Rain.Gauge.Data3[[i]][1,4:5])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(locations[,2], locations[,1], col = "blue", cex = 0.2, pch = 19, add = TRUE)

locations1 = as.data.frame(locations1)

locations2 = as.data.frame(locations2)

locations3 = as.data.frame(locations3)

B1 = NULL
for(i in 1:nrow(locations1)){
  A1 = which(Austra[[1]][,1] < locations1$Latitude[i] + 0.05 & Austra[[1]][,1] > locations1$Latitude[i] - 0.05 & Austra[[1]][,2] < locations1$Longitude[i] + 1 & Austra[[1]][,2] > locations1$Longitude[i] - 1)
  B1 = c(B1, A1)
}

c1 = which(is.na(B1))
for(i in c1){
  A1 = which(Austra[[1]][,1] < locations[i,1] + 0.08 & Austra[[1]][,1] > locations[i,1] - 0.08 & Austra[[1]][,2] < locations[i,2] + 0.08 & Austra[[1]][,2] > locations[i,2] - 0.08)
  print(A1)
}


n1 = 1:nrow(locations1)
k = 0.01
B1 = rep(0, nrow(locations1))
while(k <= 1){
  for(i in n1){
    A1 = which(Austra[[1]][,1] < locations1$Latitude[i] + k & Austra[[1]][,1] > locations1$Latitude[i] - k & Austra[[1]][,2] < locations1$Longitude[i] + k & Austra[[1]][,2] > locations1$Longitude[i] - k)[1]
    B1[i] = A1
  }
  n1 = which(is.na(B1))
  k = k + 0.01
  print(k)
}


n1 = 1:nrow(locations2)
k = 0.01
B2 = rep(0, nrow(locations2))
while(k <= 1){
  for(i in n1){
    A1 = which(Austra[[1]][,1] < locations2$Latitude[i] + k & Austra[[1]][,1] > locations2$Latitude[i] - k & Austra[[1]][,2] < locations2$Longitude[i] + k & Austra[[1]][,2] > locations2$Longitude[i] - k)[1]
    B2[i] = A1
  }
  n1 = which(is.na(B2))
  k = k + 0.01
  print(k)
}



n1 = 1:nrow(locations3)
k = 0.01
B3 = rep(0, nrow(locations3))
while(k <= 1){
  for(i in n1){
    A1 = which(Austra[[1]][,1] < locations3$Latitude[i] + k & Austra[[1]][,1] > locations3$Latitude[i] - k & Austra[[1]][,2] < locations3$Longitude[i] + k & Austra[[1]][,2] > locations3$Longitude[i] - k)[1]
    B3[i] = A1
  }
  n1 = which(is.na(B3))
  k = k + 0.01
  print(k)
}


Monthly.Rain.Gauge.Data1 = Monthly.Rain.Gauge.Data1[!is.na(B1)]
Monthly.Rain.Gauge.Data2 = Monthly.Rain.Gauge.Data2[!is.na(B2)]
Monthly.Rain.Gauge.Data3 = Monthly.Rain.Gauge.Data3[!is.na(B3)]

Monthly.Rain.Gauge.Data = c(Monthly.Rain.Gauge.Data1, Monthly.Rain.Gauge.Data2, Monthly.Rain.Gauge.Data3)
B = c(na.omit(B1), na.omit(B2), na.omit(B3))

years = c(rep(2000,10),sort(rep(seq(2001,2018),12)), rep(2019,6))
months = c(rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),19), "01", "02", "03", "04", "05", "06")
months = months[-(1:2)]
months = as.numeric(months)

Gauge.Sat = list()
for(i in 1:length(Monthly.Rain.Gauge.Data)){
  tryCatch({
    B5 = NULL
    for(j in 1:nrow(Monthly.Rain.Gauge.Data[[i]])){
      A1 = which(years == Monthly.Rain.Gauge.Data[[i]]$Year[j] & months == Monthly.Rain.Gauge.Data[[i]]$Month[j])
      B5 = c(B5, A1)
    }
    B6 = NULL
    for(j in B5){
      B6 = c(B6, Austra[[j]][B[i],3])
    }
    Sat = B6
    Gauge.Sat[[i]] = cbind(Sat, Monthly.Rain.Gauge.Data[[i]])
  }, error = function(e){})
  if(i %% 100 == 0){
    print(i)
  }
}







