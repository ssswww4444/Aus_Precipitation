getwd()
install.packages("automap")
library(automap)
install.packages("spdep")
library(spdep)
setwd("smb://perses.ms.unimelb.edu.au/bhines@student")


download.file("https://www.aec.gov.au/Electorates/gis/files/national-esri-16122011.zip",destfile = "aus.shape.file")
data <- unzip("aus.shape.file", list = TRUE)
unlink(data)
aus <- readOGR(".", "COM20111216_ELB_region.shp")

"smb://perses.ms.unimelb.edu.au/bhines@student"

"https://www.aec.gov.au/Electorates/gis/files/national-esri-16122011.zip"
download.file("https://www.aec.gov.au/Electorates/gis/files/national-esri-16122011.zip", destfile = "aus.shape.file")


years <- c(rep(2000,10),sort(rep(seq(2001,2018),12)))
months <- rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),19)
days <- rep(c("31","28","31","30","31","30","31","31","30","31","30","31"),19)
months <- cbind(months, days)
months <- months[-(1:2),]
dates <- cbind(years, months)
for(i in 1:(dim(dates)[1])){
  if(dates[i,2] == "02"){
    if(dates[i,1] == "2004" | dates[i,1] == "2008" | dates[i,1] == "2012" | dates[i,1] == "2016"){
      dates[i,3] <- "29"
    }
  }
}
s1 <- split(1:n, 1:30)

base.url <- "https://sharaku.eorc.jaxa.jp/cgi-bin/trmm/GSMaP/tilemap/show_graph.cgi?flag=1&st="
Monthly.grid <- list()
for(i in (s1$`1`)){
  print(i)
  tryCatch({
    latitude <- grid.points[i,1]
    longitude <- grid.points[i,2]
    base.URL1 <- "https://sharaku.eorc.jaxa.jp/cgi-bin/trmm/GSMaP/tilemap/show_graph.cgi?flag=1&st="
    Temp.Matrix <- matrix(0, dim(dates)[1], 5)
    ptm <- proc.time()
    for(j in 2:(dim(dates)[1])){
      print(j)
      base.url2 <- paste(base.URL1, dates[j-1,1], dates[j-1,2], dates[j-1,3], "23", "&ed=", dates[j,1], dates[j,2], dates[j,3],
                         "23","&lat0=", latitude, "&lon0=", longitude, "&lang=en", sep = "")
      tryCatch({x <- readLines(base.url2)
      m <- 0
      l <- length(x)
      while(m == 0){
        if(regexpr("y:",x[l])[1]!=-1){
          x <- x[l]
          y.2 <- regexpr(" }", x)
          accum <- substr(x, regexpr("y:",x)[1] + 2, y.2 - 1)
          m <- 1
        }else{
          l <- l - 1
        }
      }
      Temp.Vector <- c(dates[j,1], dates[j,2], accum, latitude, longitude)
      Temp.Matrix[j-1,] <- Temp.Vector}, error = function(e){})
      if(j%%10==0){
        print(j)
      }
    }
    print(proc.time() - ptm)
    colnames(Temp.Matrix) <- c("Year", "Month", "Precipitation", "Latitude", "Longitude")
    rownames(Temp.Matrix) <- NULL
    Monthly.grid[[i]] <- data.frame(Temp.Matrix)
    write.csv(Monthly.grid[[i]], file = paste("Monthly.Grid.","lat=", latitude, "lon=", longitude, sep = ""), row.names = FALSE)},error = function(e){})
}