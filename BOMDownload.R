a1 = readLines("https://raw.githubusercontent.com/hinestein/MonthlyGauge/fff10193171853b852c42a0ec279aeca880cffcb/MonthlyRainfall2/XC01X_StnDet_999999999537827.txt")
Gauge.Loc.2 = NULL
for(i in 1:length(a1)){
  a2 = a1[i]
  pos = gregexpr(",", a2)[[1]]
  pos = c(0, pos, nchar(a2) + 1)
  a3 = NULL
  for(j in 1:(length(pos) - 1)){
    a3 = c(a3, substr(a2, pos[j] + 1, pos[j + 1] - 1))
  }
  a3 = a3[c(2,7,8)]
  Gauge.Loc.2 = rbind(Gauge.Loc.2, a3)
}

colnames(Gauge.Loc.2) = c("Station", "Latitude", "Longitude")
row.names(Gauge.Loc.2) = NULL
Gauge.Loc.2 = as.data.frame(Gauge.Loc.2)


a1 = readLines("https://raw.githubusercontent.com/hinestein/MonthlyGauge/fff10193171853b852c42a0ec279aeca880cffcb/MonthlyRainfall1/XC01X_StnDet_999999999537828.txt")
Gauge.Loc.1 = NULL
for(i in 1:length(a1)){
  a2 = a1[i]
  pos = gregexpr(",", a2)[[1]]
  pos = c(0, pos, nchar(a2) + 1)
  a3 = NULL
  for(j in 1:(length(pos) - 1)){
    a3 = c(a3, substr(a2, pos[j] + 1, pos[j + 1] - 1))
  }
  a3 = a3[c(2,7,8)]
  Gauge.Loc.1 = rbind(Gauge.Loc.1, a3)
}

colnames(Gauge.Loc.1) = c("Station", "Latitude", "Longitude")
row.names(Gauge.Loc.1) = NULL
Gauge.Loc.1 = as.data.frame(Gauge.Loc.1)

a1 = readLines("https://raw.githubusercontent.com/hinestein/MonthlyGauge/fff10193171853b852c42a0ec279aeca880cffcb/MonthlyRainfall3/XC01X_StnDet_999999999537826.txt")
Gauge.Loc.3 = NULL
for(i in 1:length(a1)){
  a2 = a1[i]
  pos = gregexpr(",", a2)[[1]]
  pos = c(0, pos, nchar(a2) + 1)
  a3 = NULL
  for(j in 1:(length(pos) - 1)){
    a3 = c(a3, substr(a2, pos[j] + 1, pos[j + 1] - 1))
  }
  a3 = a3[c(2,7,8)]
  Gauge.Loc.3 = rbind(Gauge.Loc.3, a3)
}

colnames(Gauge.Loc.3) = c("Station", "Latitude", "Longitude")
row.names(Gauge.Loc.3) = NULL
Gauge.Loc.3 = as.data.frame(Gauge.Loc.3)



Stations = c(Gauge.Loc.1$Station, Gauge.Loc.2$Station, Gauge.Loc.3$Station)


library(RCurl)
BOM.list = list()
k1 = 1
for(k in 501:length(Stations)){
  tryCatch({
  Station = Stations[k]
  
  file = paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=", Station)
  
  a1 = getURL(file)
  
  Lon = as.numeric(substr(a1, gregexpr("Longitude", a1)[[1]][1] + 33, gregexpr("Longitude", a1)[[1]][1] + 38))
  
  Lat = -as.numeric(substr(a1, gregexpr("Latitude", a1)[[1]][1] + 33, gregexpr("Latitude", a1)[[1]][1] + 37))
  
  y1 = NULL
  for(i in 2:length(gregexpr("startYear", a1)[[1]])){
    y1 = c(y1, as.numeric(substr(a1, gregexpr("startYear", a1)[[1]][i] + 10, gregexpr("startYear", a1)[[1]][i] + 13)))
  }
  Precipdf = data.frame(Lon = rep(Lon, length(y1) * 12), Lat = rep(Lat, length(y1) * 12),
                        Year = rep(y1, each = 12), Month = rep(1:12, length(y1)), Station = rep(Station, length(y1) * 12))
  pos1 = c(gregexpr("startYear", a1)[[1]], gregexpr("startYear", a1)[[1]][length(gregexpr("startYear", a1)[[1]])] + 376)
  data2 = NULL
  t2 = NULL
  for(i in 2:(length(pos1)  - 1)){
    
    a2 = substr(a1, pos1[i], pos1[i + 1])
    if(gregexpr("scope", a2)[[1]][1] > 0){
      a2 = substr(a1, pos1[i], pos1[i] + gregexpr("scope", a2)[[1]][1])
    }
    
    data1 = NULL
    mon1 = gregexpr("<td >", a2)[[1]]
    mon2 = gregexpr("qc\">", a2)[[1]] - 1
    mon3 = gregexpr("</td>", a2)[[1]]
    mon4 = sort(c(mon1, mon2))
    mon4 = mon4[mon4 > 0]
    for(j in 1:(length(mon4) - 1)){
      data1 = c(data1, as.numeric(substr(a2, mon4[j] + 5, mon3[j] - 1)))
    }
    t2 = c(t2,length(data1))
    data2 = c(data2, data1)
  }
  
  Precipdf$Rain = data2
  
  BOM.list[[k1]] = Precipdf
  k1 = k1 + 1
  print(k1)
  }, error = function(e){})
}


for(i in 1:length(BOM.list)){
  print(nrow(BOM.list[[i]]))
}

save(BOM.list, file = "BOMlist")


B1 = rep(0, length(BOM.list))
for(i in 1:length(BOM.list)){
  B1[i] = nrow(BOM.list[[i]])
}

Lat1 = rep(0, length(BOM.list))
Lon1 = rep(0, length(BOM.list))

for(i in 1:length(BOM.list)){
  Lon1[i] = BOM.list[[i]]$Lon[1]
  Lat1[i] = BOM.list[[i]]$Lat[1]
}

BOMLoc = data.frame(Lon = Lon1, Lat = Lat1)






