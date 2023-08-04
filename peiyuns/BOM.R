setwd("~/Projects/Benjamin_code/peiyuns")

# Station list 2
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

# Station list 1
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

# Station list 3
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

# All stations
Gauge.all <- rbind(Gauge.Loc.1, Gauge.Loc.2, Gauge.Loc.3)
write.csv(Gauge.all, "Gauge_locations.csv", row.names=FALSE)
Stations <- Gauge.all$Station

# Get data
library(RCurl)
BOM.list = list()  #BOM.list should be filled using downloaded data files, not from internet.

chunks <- c(0,1000,2000,3000,4000,5036)

for(c in 2:length(chunks)) {
  for(k in (chunks[c-1]+1):chunks[c]){
    tryCatch({
      Station = Stations[k]
      
      file = paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=", Station)
      
      a1 = getURL(file)
      
      # Lon = as.numeric(substr(a1, gregexpr("Longitude", a1)[[1]][1] + 33, gregexpr("Longitude", a1)[[1]][1] + 38))
      # Lat = -as.numeric(substr(a1, gregexpr("Latitude", a1)[[1]][1] + 33, gregexpr("Latitude", a1)[[1]][1] + 37))
      
      # start years
      y1 = NULL
      for(i in 2:length(gregexpr("startYear", a1)[[1]])){
        y1 = c(y1, as.numeric(substr(a1, gregexpr("startYear", a1)[[1]][i] + 10, gregexpr("startYear", a1)[[1]][i] + 13)))
      }
      
      # df for storing data
      Precipdf = data.frame(Year = rep(y1, each = 12), Month = rep(1:12, length(y1)), Station = rep(Station, length(y1) * 12))
      pos1 = c(gregexpr("startYear", a1)[[1]], gregexpr("startYear", a1)[[1]][length(gregexpr("startYear", a1)[[1]])] + 376)
      data2 = NULL
      t2 = NULL
      
      # read data
      for(i in 2:(length(pos1)  - 1)) {
        
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
      
      BOM.list[[k]] = Precipdf
      print(k)
      
    }, error = function(e){})
  }
  save(BOM.list, file = paste0("BOM/BOMlist", chunks[c], ".RData"))
}

# Put all data together
BOM.all = list()
for(c in 2:length(chunks)) {
  # load from RData
  load(paste0("BOM/BOMlist", chunks[c], ".RData"))
  
  # put into BOM.all
  for(k in (chunks[c-1]+1):chunks[c]) {
    if(!is.null(BOM.list[[k]])) {
      df <- BOM.list[[k]]
      row.names(df) <- paste0(df[,"Year"],"-",df[,"Month"])
      colnames(df) <- c("Year", "Month", "Station", df[1,3])
      BOM.all[[k]] <- df[,4, drop=F]
    }
  }
}

# Check missing ones (link do not exist)
for(i in 1:5036) {
  if(is.null(BOM.all[[i]])) {
    print(i)
  }
}

# Merge all data into one df
df.all <- BOM.all[[1]]
for(i in 2:5036) {
  if(!is.null(BOM.all[[i]])) {
    df.all <- merge(df.all, BOM.all[[i]], 
                    by = 'row.names', all =TRUE)
    rownames(df.all) <- df.all$Row.names
    df.all <- df.all[,-1]
  }
}

write.csv(df.all, "BOM.csv")



