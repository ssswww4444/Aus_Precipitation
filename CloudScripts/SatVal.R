library(devtools)
install_github("andrewzm/STRbook")
library("dplyr")
library("fields")
library("gstat")
library("ggplot2")
library("RcolorBrewer")
library("STRbook")
library("sp")
library("spacetime")

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines")
require(rgdal)
shape <- readOGR(dsn = ".", layer = "TM_WORLD_BORDERS-0.3")

ausmap = shape[shape$ISO3 =="AUS",]

years1 = c(rep(2003:2019, each = 12),2020)
months1 = c(rep(1:12, length(unique(years1)) - 1), 1)
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

dates3 = cbind(dates1, days1)



#####CHIRP Data

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

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Gauge.Loc.2, 
             mapping = aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), size = 0.4, colour = "blue") +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", title = "Australian Rain Gauge Locations")

Gauge.2 = list()
for(i in 1:nrow(Gauge.Loc.2)){
  file.2 = paste0("https://raw.githubusercontent.com/hinestein/MonthlyGauge/fff10193171853b852c42a0ec279aeca880cffcb/MonthlyRainfall2/XC01X_Data_", Gauge.Loc.2$Station[i], "_999999999537827.txt")
  a1 = readLines(file.2)
  a2 = a1[5]
  pos = gregexpr(",", a2)[[1]]
  pos = c(0, pos, nchar(a2) + 1)
  a4 = NULL
  for(j in 5:length(a1)){
    a2 = a1[j]
    pos = gregexpr(",", a2)[[1]]
    pos = c(0, pos, nchar(a2) + 1)
    a3 = NULL
    for(k in 1:(length(pos) - 1)){
      a3 = c(a3, substr(a2, pos[k] + 1, pos[k + 1] - 1))
    }
    a3 = a3[7:9]
    a5 = c(substr(a3[1], 4, 5), substr(a3[1], 7, 10), a3[3])
    a4 = rbind(a4, a5)
  }
  rownames(a4) = NULL
  Month = a4[,1]
  Year = a4[,2]
  Precipitation = as.numeric(a4[,3])
  Gauge.2[[i]] = data.frame(Month = Month, Year = Year, Precipitation)
  print(i)
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/GaugeData")
for(i in 1:length(Gauge.2)){
  write.csv(Gauge.2[[i]], file = paste0("station", Gauge.Loc.2[i,1]), row.names = FALSE)
}

Gauge.2 = list()
for(i in 1:nrow(Gauge.Loc.2)){
  file.name = paste0("station", Gauge.Loc.2[i,1])
  Gauge.2[[i]] = read.csv(file.name, header = TRUE)
}


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

load("Gauge.2")


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

Gauge.1 = list()
for(i in 1:nrow(Gauge.Loc.1)){
  file.1 = paste0("https://raw.githubusercontent.com/hinestein/MonthlyGauge/fff10193171853b852c42a0ec279aeca880cffcb/MonthlyRainfall1/XC01X_Data_", Gauge.Loc.1$Station[i], "_999999999537828.txt")
  a1 = readLines(file.1)
  a2 = a1[5]
  pos = gregexpr(",", a2)[[1]]
  pos = c(0, pos, nchar(a2) + 1)
  a4 = NULL
  for(j in 5:length(a1)){
    a2 = a1[j]
    pos = gregexpr(",", a2)[[1]]
    pos = c(0, pos, nchar(a2) + 1)
    a3 = NULL
    for(k in 1:(length(pos) - 1)){
      a3 = c(a3, substr(a2, pos[k] + 1, pos[k + 1] - 1))
    }
    a3 = a3[7:9]
    a5 = c(substr(a3[1], 4, 5), substr(a3[1], 7, 10), a3[3])
    a4 = rbind(a4, a5)
  }
  rownames(a4) = NULL
  Month = a4[,1]
  Year = a4[,2]
  Precipitation = as.numeric(a4[,3])
  Gauge.1[[i]] = data.frame(Month = Month, Year = Year, Precipitation)
  print(i)
}

load("Gauge.1")

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

Gauge.3 = list()
for(i in 1:nrow(Gauge.Loc.3)){
  file.3 = paste0("https://raw.githubusercontent.com/hinestein/MonthlyGauge/fff10193171853b852c42a0ec279aeca880cffcb/MonthlyRainfall3/XC01X_Data_", Gauge.Loc.3$Station[i], "_999999999537826.txt")
  a1 = readLines(file.3)
  a2 = a1[5]
  pos = gregexpr(",", a2)[[1]]
  pos = c(0, pos, nchar(a2) + 1)
  a4 = NULL
  for(j in 5:length(a1)){
    a2 = a1[j]
    pos = gregexpr(",", a2)[[1]]
    pos = c(0, pos, nchar(a2) + 1)
    a3 = NULL
    for(k in 1:(length(pos) - 1)){
      a3 = c(a3, substr(a2, pos[k] + 1, pos[k + 1] - 1))
    }
    a3 = a3[7:9]
    a5 = c(substr(a3[1], 4, 5), substr(a3[1], 7, 10), a3[3])
    a4 = rbind(a4, a5)
  }
  rownames(a4) = NULL
  Month = a4[,1]
  Year = a4[,2]
  Precipitation = as.numeric(a4[,3])
  Gauge.3[[i]] = data.frame(Month = Month, Year = Year, Precipitation)
  print(i)
}

load("Gauge.3")


A1 = precip3[[1]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

min.1 = Inf
for(i in 1:length(Gauge.1)){
  if(!is.na(as.numeric(Gauge.1[[i]][1,2]))){
    if(as.numeric(Gauge.1[[i]][1,2]) < min.1){
      min.1 = as.numeric(Gauge.1[[i]][1,2])
    }
  }
}

max.1 = 0
for(i in 1:length(Gauge.1)){
  if(!is.na(as.numeric(Gauge.1[[i]][1,2]))){
    if(as.numeric(Gauge.1[[i]][nrow(Gauge.1[[i]]),2]) > max.1){
      max.1 = as.numeric(Gauge.1[[i]][nrow(Gauge.1[[i]]),2])
    }
  }
}


Sat.1 = list()
pred.grid.1 = data.frame(Lat = as.numeric(Gauge.Loc.1[,2]), Lon = as.numeric(Gauge.Loc.1[,3]))
coordinates(pred.grid.1) = ~Lon + Lat
for(i in (which(dates3[,1] == min.1)[1]):tail(which(dates3[,1] == max.1),1)){
  df.1 = precip3[[i]][Aus,]
  idw.1 = idw(formula = Rain ~ 1, locations = ~Lon + Lat, data = df.1, newdata = pred.grid.1, idp = 3)
  Sat.1[[i]] = data.frame(Lat = idw.1$Lat, Lon = idw.1$Lon, Estimate = idw.1$var1.pred)
}

Gauge.Sat.1 = Gauge.1
for(i in 1:length(Gauge.1)){
  a1 = NULL
  if(!is.na(as.numeric(Gauge.1[[i]][1,2]))){
    for(j in 1:nrow(Gauge.1[[i]])){
      indices = which(dates3[,1] == as.numeric(Gauge.1[[i]][j,2]) & dates3[,2] == as.numeric(Gauge.1[[i]][j,1]))
      a1 = c(a1, Sat.1[[indices]][i,3])
    }
    Lon.1 = rep(as.numeric(Gauge.Loc.1[i,3]), nrow(Gauge.1[[i]]))
    Lat.1 = rep(as.numeric(Gauge.Loc.1[i,2]), nrow(Gauge.1[[i]]))
    Rain.1 = as.numeric(Gauge.1[[i]][,3])
    Month.1 = as.numeric(Gauge.1[[i]][,1])
    Year.1 = as.numeric(Gauge.1[[i]][,2])
    Est = a1
    Gauge.Sat.1[[i]] = data.frame(Lat = Lat.1, Lon = Lon.1, Month = Month.1, Year = Year.1, Precipitation = Rain.1, Estimate = a1)  
  }
}


min.2 = Inf
for(i in 1:length(Gauge.2)){
  if(!is.na(as.numeric(Gauge.2[[i]][1,2]))){
    if(as.numeric(Gauge.2[[i]][1,2]) < min.2){
      min.2 = as.numeric(Gauge.2[[i]][1,2])
    }
  }
}

max.2 = 0
for(i in 1:length(Gauge.2)){
  if(!is.na(as.numeric(Gauge.2[[i]][1,2]))){
    if(as.numeric(Gauge.2[[i]][nrow(Gauge.2[[i]]),2]) > max.2){
      max.2 = as.numeric(Gauge.2[[i]][nrow(Gauge.2[[i]]),2])
    }
  }
}


Sat.2 = list()
pred.grid.2 = data.frame(Lat = as.numeric(Gauge.Loc.2[,2]), Lon = as.numeric(Gauge.Loc.2[,3]))
coordinates(pred.grid.2) = ~Lon + Lat
for(i in (which(dates3[,1] == min.2)[1]):tail(which(dates3[,1] == max.2),1)){
  df.2 = NOAA.aus[[i]]
  idw.2 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = df.2, newdata = pred.grid.2, idp = 3)
  Sat.2[[i]] = data.frame(Lat = idw.2$Lat, Lon = idw.2$Lon, Estimate = idw.2$var1.pred)
}

Gauge.Sat.2 = Gauge.2
for(i in 1:length(Gauge.2)){
  a1 = NULL
  if(!is.na(as.numeric(Gauge.2[[i]][1,2]))){
    for(j in 1:nrow(Gauge.2[[i]])){
      indices = which(dates3[,1] == as.numeric(Gauge.2[[i]][j,2]) & dates3[,2] == as.numeric(Gauge.2[[i]][j,1]))
      a1 = c(a1, Sat.2[[indices]][i,3])
    }
    Lon.2 = rep(as.numeric(Gauge.Loc.2[i,3]), nrow(Gauge.2[[i]]))
    Lat.2 = rep(as.numeric(Gauge.Loc.2[i,2]), nrow(Gauge.2[[i]]))
    Rain.2 = as.numeric(Gauge.2[[i]][,3])
    Month.2 = as.numeric(Gauge.2[[i]][,1])
    Year.2 = as.numeric(Gauge.2[[i]][,2])
    Est = a1
    Gauge.Sat.2[[i]] = data.frame(Lat = Lat.2, Lon = Lon.2, Month = Month.2, Year = Year.2, Precipitation = Rain.2, Estimate = a1)  
  }
  print(i)
}


min.3 = Inf
for(i in 1:length(Gauge.3)){
  if(!is.na(as.numeric(Gauge.3[[i]][1,2]))){
    if(as.numeric(Gauge.3[[i]][1,2]) < min.3){
      min.3 = as.numeric(Gauge.3[[i]][1,2])
    }
  }
}

max.3 = 0
for(i in 1:length(Gauge.3)){
  if(!is.na(as.numeric(Gauge.3[[i]][1,2]))){
    if(as.numeric(Gauge.3[[i]][nrow(Gauge.3[[i]]),2]) > max.3){
      max.3 = as.numeric(Gauge.3[[i]][nrow(Gauge.3[[i]]),2])
    }
  }
}


Sat.3 = list()
pred.grid.3 = data.frame(Lat = as.numeric(Gauge.Loc.3[,2]), Lon = as.numeric(Gauge.Loc.3[,3]))
coordinates(pred.grid.3) = ~Lon + Lat
for(i in (which(dates3[,1] == min.3)[1]):tail(which(dates3[,1] == max.3),1)){
  df.3 = precip3[[i]][Aus,]
  idw.3 = idw(formula = Rain ~ 1, locations = ~Lon + Lat, data = df.3, newdata = pred.grid.3, idp = 3)
  Sat.3[[i]] = data.frame(Lat = idw.3$Lat, Lon = idw.3$Lon, Estimate = idw.3$var1.pred)
}

Gauge.Sat.3 = Gauge.3
for(i in 1:length(Gauge.3)){
  a1 = NULL
  if(!is.na(as.numeric(Gauge.3[[i]][1,2]))){
    for(j in 1:nrow(Gauge.3[[i]])){
      indices = which(dates3[,1] == as.numeric(Gauge.3[[i]][j,2]) & dates3[,2] == as.numeric(Gauge.3[[i]][j,1]))
      a1 = c(a1, Sat.3[[indices]][i,3])
    }
    Lon.3 = rep(as.numeric(Gauge.Loc.3[i,3]), nrow(Gauge.3[[i]]))
    Lat.3 = rep(as.numeric(Gauge.Loc.3[i,2]), nrow(Gauge.3[[i]]))
    if(is.na(Lat.3[1])){
      print(i)
    }
    Rain.3 = as.numeric(Gauge.3[[i]][,3])
    Month.3 = as.numeric(Gauge.3[[i]][,1])
    Year.3 = as.numeric(Gauge.3[[i]][,2])
    Est = a1
    Gauge.Sat.3[[i]] = data.frame(Lat = Lat.3, Lon = Lon.3, Month = Month.3, Year = Year.3, Precipitation = Rain.3, Estimate = a1)  
  }
}

library(MASS)
Gauge.Sat.All = c(Gauge.Sat.2)
X.mat = NULL
for(i in 1:length(Gauge.Sat.All)){
  if(!is.na(as.numeric(Gauge.Sat.All[[i]][1,2])) & nrow(Gauge.Sat.All[[i]]) > 10){
    y1 = Gauge.Sat.All[[i]]$Precipitation
    x1 = Gauge.Sat.All[[i]]$Estimate
    mod1 = rlm(y1 ~ x1, method = "M", maxit = 2000)
    corr.1 = cor(y1,x1)
    X.mat = rbind(X.mat, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, as.numeric(Gauge.Sat.All[[i]][1,1:2]), nrow(Gauge.Sat.All[[i]]), corr.1))  
  }
}

colnames(X.mat) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "cor")
X.mat = as.data.frame(X.mat)
par(mfrow = c(1,2))
hist(X.mat$Intercept)
hist(X.mat$Slope)




par(mfrow = c(1,2))
hist(X.mat$Intercept)
hist(X.mat$Slope)

g.Int.lin = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Linear Intercept Estimate")

g.Slope.lin = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Linear Slope Estimate")

grid.arrange(g.Int.lin + labs(x = " "), g.Slope.lin + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))




############
#With sqrt
############

Gauge.Sat.All = c(Gauge.Sat.2)
X.mat.sq = NULL
m3 = NULL
for(i in 1:length(Gauge.Sat.All)){
  if(!is.na(as.numeric(Gauge.Sat.All[[i]][1,2])) & nrow(Gauge.Sat.All[[i]]) > 10){
    y1 = sqrt(Gauge.Sat.All[[i]]$Precipitation)
    x1 = sqrt(Gauge.Sat.All[[i]]$Estimate)
    mod1 = rlm(y1 ~ x1, method = "M", maxit = 2000)
    corr.1 = cor(y1,x1)
    X.mat.sq = rbind(X.mat.sq, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, as.numeric(Gauge.Sat.All[[i]][1,1:2]), nrow(Gauge.Sat.All[[i]]), corr.1))
    m3 = c(m3, i)
  }
}

colnames(X.mat.sq) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "cor")
X.mat.sq = as.data.frame(X.mat.sq)
par(mfrow = c(1,2))
hist(X.mat.sq$Intercept)
hist(X.mat.sq$Slope)

used2 = which(X.mat.sq$Intercept > -5 & X.mat.sq$Intercept < 2.5 & X.mat.sq$Slope > 0.5 & X.mat.sq$Slope < 2 & X.mat.sq$cor > 0.5)
used1 = which(X.mat.2$Intercept < 5 & X.mat.2$Intercept > -2 & X.mat.2$Slope > 0.25 & X.mat.2$Slope < 1.5 & X.mat.2$Lon > 100 & X.mat.2$Lat < -5 & X.mat.2$n > 11 &
              X.mat.2$correlation > 0.45)

X.mat.sq1 = X.mat.sq[used2,]
X.mat.21 = X.mat.2[used1,]

colnames(X.mat.21) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "cor")

X.mat.Sq = rbind(X.mat.21, X.mat.sq1)

X.mat.Sq$Variable = as.factor(c(rep("JAXA", nrow(X.mat.21)), rep("NOAA", nrow(X.mat.sq1))))



g.cor = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.Sq, 
             mapping = aes(x = Lon, y = Lat, colour = cor), size = 0.9) + facet_wrap(~Variable)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 17, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right", strip.text = element_text(size = 15)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = " Gauge-Satellite Correlation")

g.Int2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.sq, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Intercept Estimate")

g.Slope2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.sq, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope Estimate")

grid.arrange(g.Int2 + labs(x = " "), g.Slope2 + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))



############
#With 4th root
############

Gauge.Sat.All = c(Gauge.Sat.2)
X.mat.4 = NULL
for(i in 1:length(Gauge.Sat.All)){
  if(!is.na(as.numeric(Gauge.Sat.All[[i]][1,2])) & nrow(Gauge.Sat.All[[i]]) > 10){
    y1 = (Gauge.Sat.All[[i]]$Precipitation)^(1/4)
    x1 = (Gauge.Sat.All[[i]]$Estimate)^(1/4)
    mod1 = rlm(y1 ~ x1, method = "M", maxit = 200)
    corr.1 = cor(y1,x1)
    X.mat.4 = rbind(X.mat.4, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, as.numeric(Gauge.Sat.All[[i]][1,1:2]), nrow(Gauge.Sat.All[[i]]), corr.1))  
  }
}

colnames(X.mat.4) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "cor")
X.mat.4 = as.data.frame(X.mat.4)
par(mfrow = c(1,2))
hist(X.mat.4$Intercept)
hist(X.mat.4$Slope)

a1 = which(X.mat$Intercept < 15 & X.mat$Intercept > -25 & X.mat$Slope > 0.3 & X.mat$Slope < 2 & X.mat$Lon > 100 & X.mat$Lat < -5 & X.mat$n > 11 &
             X.mat.sq$Intercept < 3 & X.mat.sq$Intercept > -3 & X.mat.sq$Slope > 0.5 & X.mat.sq$Slope < 1.5 & X.mat.sq$Lon > 100 & X.mat.sq$Lat < -5 & X.mat.sq$n > 11 &
             X.mat.4$Intercept < 1 & X.mat.4$Intercept > -2 & X.mat.4$Slope > 0.3 & X.mat.4$Slope < 1.75 & X.mat.4$Lon > 100 & X.mat.4$Lat < -5 & X.mat.4$n > 11 &
             X.mat.sq$cor > 0.5)

X.mat = X.mat[a1,]
X.mat.sq = X.mat.sq[a1,]
X.mat.4 = X.mat.4[a1,]

m3 = m3[a1]


par(mfrow = c(1,2))
hist(X.mat.4$Intercept)
hist(X.mat.4$Slope)

g.Int.4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.4, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Fourth Root Intercept Estimate")

g.Slope.4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.4, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Fourth Root Slope Estimate")

grid.arrange(g.Int.4 + labs(x = " ", y = " "), g.Slope.4 + labs(y = " ", x = " "), g.Int + labs(x = " ", y = " "), g.Slope + labs(y = " ", x = " "), nrow = 2, 
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))

grid.arrange(g.Int.4 + labs(x = " ", y = " "), g.Slope.4 + labs(y = " ", x = " "), nrow = 1)

#############
#Chirp SLM
#############

X1 = X.mat.sq[-duplicated(X.mat.sq[,5:4]),]

w0 = Weight.Matrix(locations = X1[,5:4], k = 9, alpha = 1.3, method = "IDW")

x.i = cbind(rep(1, nrow(X1)), Elevation.4$var1.pred)

SLM0 = SPE1(X1[,1], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X1[,1]
plot(beta0.hat, X1[,1])
abline(0,1)


w1 = Weight.Matrix(locations = X1[,5:4], k = 8, alpha = 1.3, method = "IDW")

SLM1 = SPE1(X1[,2], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X1[,2]
plot(beta1.hat, X1[,2])
abline(0,1)

X.hat = data.frame(Lat = X1$Lat, Lon = X1$Lon, beta0 = beta0.hat, beta1 = beta1.hat)

g.Int.4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.hat, 
             mapping = aes(x = Lon, y = Lat, colour = beta0), size = 1.2)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Intercept") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left")

g.Slope.4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.hat, 
             mapping = aes(x = Lon, y = Lat, colour = beta1), size = 1.2)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Slope") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right")

grid.arrange(g.Int + labs(x = " ", y = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, 
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))




x.new = cbind(1, Elevation.3$Elevation)

sw0 = Sep.Weight.Matrix(Oldlocations = X1[,5:4], Newlocations = Elevation.3[,2:1], k = 9, alpha = 1.3, method = "IDW")
beta0.tilde = x.new %*% SLM0$beta.hat + SLM0$lambda.hat * sw0 %*% X1[,1] 

sw1 = Sep.Weight.Matrix(Oldlocations = X1[,5:4], Newlocations = Elevation.3[,2:1], k = 8, alpha = 1.29, method = "IDW")
beta1.tilde = x.new %*% SLM1$beta.hat + SLM1$lambda.hat * sw1 %*% X1[,2]

X.tilde = data.frame(beta0 = beta0.tilde, beta1 = beta1.tilde, Lon = Elevation.3$Lon, Lat = Elevation.3$Lat)

g.Int.4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde, 
             mapping = aes(x = Lon, y = Lat, colour = beta0), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Intercept") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left")

g.Slope.4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde, 
             mapping = aes(x = Lon, y = Lat, colour = beta1), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right")


grid.arrange(g.Int.4 + labs(x = " ", y = " "), g.Slope.4 + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))












################
#JAXA data
################

years1 = c(rep(1981:2019, each = 12),2020)
months1 = c(rep(1:12, length(unique(years1)) - 1), 1)
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

dates2 = cbind(dates1, days1)

Jaxa = list()
k = 1
for(i in (which(dates2[,1] == 2000 & dates2[,2] == 4)):(which(dates2[,1] == 2018 & dates2[,2] == 12))){
  m1 = dates2[i,1]
  m2 = dates2[i,2]
  if(nchar(m2) == 1){
    m2 = paste0("0", m2)
  }
  file.1 = paste0("https://raw.githubusercontent.com/hinestein/Sat.Monthly/master/Monthly.Jaxa%20All%20locations/Monthly", m1, "-", m2)
  Jaxa[[k]] = read.csv(file.1, header = TRUE, row.names = NULL)
  k = k + 1
}

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Jaxa[[1]], 
             mapping = aes(x = Lon, y = Lat, colour = Rain), size = 3)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))


Jaxa.dates = dates2[(which(dates2[,1] == 2000 & dates2[,2] == 4)):(which(dates2[,1] == 2018 & dates2[,2] == 12)),]
Jaxa.Gauge = list()
for(i in 1:length(Gauge.2)){
  if(!is.na(as.numeric(Gauge.2[[i]][1,1]))){
    a1 = rep(0, nrow(Gauge.2[[i]]))
    for(j in 1:nrow(Gauge.2[[i]])){
      a1[j] = Jaxa[[which(Jaxa.dates[,1] == as.numeric(Gauge.2[[i]][j,2]) & Jaxa.dates[,2] == as.numeric(Gauge.2[[i]][j,1]))]][i,3]
    }
    Jaxa.Gauge[[i]] = data.frame(Month = as.numeric(Gauge.2[[i]]$Month), Year = as.numeric(Gauge.2[[i]]$Year),
                                 Gauge = as.numeric(Gauge.2[[i]]$Precipitation), Estimate = as.numeric(a1),
                                 Latitude = rep(as.numeric(Gauge.Loc.2[i,2]), nrow(Gauge.2[[i]])), 
                                 Longitude = rep(as.numeric(Gauge.Loc.2[i,3]), nrow(Gauge.2[[i]])))  
  }
}

X.mat.1 = NULL
for(i in 1:length(Jaxa.Gauge)){
  tryCatch({
  if(!is.null(Jaxa.Gauge[[i]]) & nrow(Jaxa.Gauge[[i]]) > 6){
    y1 = Jaxa.Gauge[[i]]$Gauge
    x1 = Jaxa.Gauge[[i]]$Estimate
    corr.1 = cor(y1, x1)
    mod1 = rlm(y1 ~ x1, method = "M", maxit = 200)
    X.mat.1 = rbind(X.mat.1, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, Jaxa.Gauge[[i]]$Latitude[1],
                             Jaxa.Gauge[[i]]$Longitude[1], nrow(Jaxa.Gauge[[i]]), corr.1))
  }
  }, error = function(e){})
}
colnames(X.mat.1) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "correlation")
X.mat.1 = as.data.frame(X.mat.1)
hist(X.mat.1[,2])
hist(X.mat.1[,1])

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.1, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Linear Intercept Estimate")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.1, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Linear Slope Estimate")

g.cor = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.1, 
             mapping = aes(x = Lon, y = Lat, colour = correlation), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Linear Slope Estimate")

grid.arrange(g.Int + labs(x = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))



#################
#With Sqrt
#################


X.mat.2 = NULL
m3 = NULL
for(i in 1:length(Jaxa.Gauge)){
  tryCatch({
    if(!is.null(Jaxa.Gauge[[i]]) & nrow(Jaxa.Gauge[[i]]) > 6){
      y1 = sqrt(Jaxa.Gauge[[i]]$Gauge)
      x1 = sqrt(Jaxa.Gauge[[i]]$Estimate)
      corr.1 = cor(y1, x1)
      mod1 = rlm(y1 ~ x1, method = "M", maxit = 200)
      X.mat.2 = rbind(X.mat.2, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, Jaxa.Gauge[[i]]$Latitude[1],
                                 Jaxa.Gauge[[i]]$Longitude[1], nrow(Jaxa.Gauge[[i]]), corr.1))
      m3 = c(m3, i)
    }
  }, error = function(e){})
}
colnames(X.mat.2) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "correlation")
X.mat.2 = as.data.frame(X.mat.2)
used1 = which(X.mat.2$Intercept < 5 & X.mat.2$Intercept > -2 & X.mat.2$Slope > 0.25 & X.mat.2$Slope < 1.5 & X.mat.2$Lon > 100 & X.mat.2$Lat < -5 & X.mat.2$n > 11,
              X.mat.2$correlation > 0.45)
X.mat.2 = X.mat.2[used,]
X.mat.2$Elevation = idw1$var1.pred
hist(X.mat.2[,2])
hist(X.mat.2[,1])
hist(X.mat.2$correlation)
m3 = m3[used]

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.2, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Intercept Estimate")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.2, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope Estimate")

grid.arrange(g.Int + labs(x = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))

O0 = Opt.Weight(Intercept ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w0 = Weight.Matrix(locations = X.mat.2[,5:4], k = O0[1], alpha = O0[2], method = "IDW")


x.i = matrix(1, nrow = nrow(X.mat), ncol = 1)

SLM0 = SPE1(X.mat.2[,1], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X.mat.2[,1]
plot(beta0.hat, X.mat.2[,1])
abline(0,1)

O1 = Opt.Weight(Slope ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w1 = Weight.Matrix(locations = X.mat.2[,5:4], k = O1[1], alpha = O1[2], method = "IDW")

SLM1 = SPE1(X.mat.2[,2], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X.mat.2[,2]
plot(beta1.hat, X.mat.2[,2])
abline(0,1)


X.new = data.frame(Intercept = beta0.hat, Slope = beta1.hat, Lon = X.mat.2[,5], Lat = X.mat.2[,4])


g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.new, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Intercept")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.new, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope")

grid.arrange(g.Int + labs(x = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))

pred.grid4 = data.frame(Lon = X1$Lon, Lat = X1$Lat)

Elevation.4 = idw(formula = Elevation ~ 1, data = Elevation.3, locations =~Lon + Lat, newdata = pred.grid4, idp = 3)

X.mat.2 = cbind(X.mat.2, Elevation.4$var1.pred)

colnames(X.mat.2) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "correlation", "Elevation")
X.mat.2 = as.data.frame(X.mat.2)


O0 = Opt.Weight(Intercept ~ Elevation, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w0 = Weight.Matrix(locations = X.mat.2[,5:4], k = O0[1], alpha = O0[2], method = "IDW")


x.i = cbind(1, Elevation.4$var1.pred)

SLM0 = SPE1(X.mat.2[,1], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X.mat.2[,1]
plot(beta0.hat, X.mat.2[,1])
abline(0,1)

O1 = Opt.Weight(Slope ~ Elevation, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w1 = Weight.Matrix(locations = X.mat.2[,5:4], k = O1[1], alpha = O1[2], method = "IDW")

SLM1 = SPE1(X.mat.2[,2], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X.mat.2[,2]
plot(beta1.hat, X.mat.2[,2])
abline(0,1)

w0.new = Sep.Weight.Matrix(Oldlocations = X.mat.2[,5:4], Newlocations = Elevation.3[,2:1], k = O0[1], alpha = O0[2], method = "IDW")
w1.new = Sep.Weight.Matrix(Oldlocations = X.mat.2[,5:4], Newlocations = Elevation.3[,2:1], k = O1[1], alpha = O1[2], method = "IDW")

x.new = cbind(1, Elevation.3$Elevation)

beta0.tilde = x.new %*% SLM0$beta.hat + SLM0$lambda.hat * w0.new %*% X.mat.2[,1]
beta1.tilde = x.new %*% SLM1$beta.hat + SLM1$lambda.hat * w1.new %*% X.mat.2[,2]

beta.tilde = data.frame(Intercept = beta0.tilde, Slope = beta1.tilde, Lat = Elevation.3$Lat, Lon = Elevation.3$Lon)

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = beta.tilde, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Intercept")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = beta.tilde, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope")


grid.arrange(g.Int + labs(x = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))
#################
#Fourth Root
#################


X.mat.4 = NULL
for(i in 1:length(Jaxa.Gauge)){
  tryCatch({
    if(!is.null(Jaxa.Gauge[[i]]) & nrow(Jaxa.Gauge[[i]]) > 6){
      y1 = (Jaxa.Gauge[[i]]$Gauge)^(1/4)
      x1 = (Jaxa.Gauge[[i]]$Estimate)^(1/4)
      corr.1 = cor(y1, x1)
      mod1 = rlm(y1 ~ x1, method = "M", maxit = 200)
      X.mat.4 = rbind(X.mat.4, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, Jaxa.Gauge[[i]]$Latitude[1],
                                 Jaxa.Gauge[[i]]$Longitude[1], nrow(Jaxa.Gauge[[i]]), corr.1))
    }
  }, error = function(e){})
}
colnames(X.mat.4) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "correlation")
X.mat.4 = as.data.frame(X.mat.4)
X.mat.4 = X.mat.4[X.mat.4$Intercept < 2.5 & X.mat.4$Intercept > -1.5 & X.mat.4$Slope > 0.4 & X.mat.4$Slope < 1.5 & X.mat.4$Lon > 100 & X.mat.4$Lat < -5 & X.mat.4$n > 11,]
hist(X.mat.4[,2])
hist(X.mat.4[,1])

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.4, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Fourth Root Intercept Estimate")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.4, 
             mapping = aes(x = Lon, y = Lat, colour = Slope), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Fourth Root Slope Estimate")

grid.arrange(g.Int + labs(x = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))









###############
#Jaxa SLM
###############




#########
#Current Location Plotting
#########

current.loc = NULL
for(i in 1:length(Jaxa.Gauge)){
  if(max(Jaxa.Gauge[[i]]$Year) == 2018){
    current.loc = c(current.loc, i)
  }
}

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Gauge.Loc.2[current.loc,], 
             mapping = aes(x = as.numeric(Longitude), y = as.numeric(Latitude)), colour = "blue", size = 0.75) + theme_bw()+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12))+
  labs(title = "Current BOM Rain Gauge Locations", x = "Longitude", y = "Latitude") +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 3, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 3) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 3, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 3)

###########
#Correlation Plotting
###########


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.2, 
             mapping = aes(x = Lon, y = Lat, colour = correlation), size = 1) + theme_bw()+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) + 
  labs(title = "Gauge-Satellite Correlation", x = "Longitude", y = "Latitude", colour = "Correlation") + 
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 3, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 3) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 3, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

########
#Histograms
########


g.Int = ggplot(X.mat.2, aes(x = Intercept)) + geom_histogram(bins = 25, colour = "black", fill = "white") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) + 
    labs(title = "Square Root Intercept", x = "Huber's Estimate", y = "Count")

g.Slope = ggplot(X.mat.2, aes(x = Slope)) + geom_histogram(bins = 25, colour = "black", fill = "white") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12))+ 
    labs(title = "Square Root Slope", x = "Huber's Estimate", y = "Count")

grid.arrange(g.Int + labs(x = " ", y = " "), g.Slope + labs(x = " ", y = " "), nrow = 1,left = textGrob("Count", vjust = 2, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90), 
             bottom = textGrob("Huber's Estimate", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3))

######
#Total Comparison
######

sums = NULL
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
    sums = rbind(sums, c(sum(Jaxa.Gauge[[i]][,3]), sum(Jaxa.Gauge[[i]][,4])))
  }
}
colnames(sums) = c("Gauge", "Satellite")
sums = as.data.frame(sums)

ggplot(sums, aes(x = Satellite, y = Gauge)) + geom_point() + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) + 
  labs(title = "Total Comparison", x = "Total Satellite Estimate (mm)", y = "Total Gauge Measuremnt (mm)") +
  geom_abline(intercept = 0, slope = 1, colour = "blue", size = 1.5) + xlim(0, max(max(sums$Gauge), max(sums$Satellite))) + 
  ylim(0, max(max(sums$Gauge), max(sums$Satellite)))


###############
#Single Comparison
###############

a1 = 0
k = 0
a0 = 0
k0 = 0

al1 = 0
kl = 0
al0 = 0
kl0 = 0

aq1 = 0
kq = 0
aq0 = 0
kq0 = 0


for(i in 1:length(Jaxa.Gauge)){
  tryCatch({
    if(!is.null(Jaxa.Gauge[[i]]) & nrow(Jaxa.Gauge[[i]]) > 20){
      (mod1$coefficients[1] - mod2$coefficient[1])^2
      y1 = sqrt(Jaxa.Gauge[[i]]$Gauge)
      x1 = sqrt(Jaxa.Gauge[[i]]$Estimate)
      mod1 = rlm(y1 ~ x1, method = "M", maxit = 200)
      mod2 = lm(y1 ~ x1)
      
      modl1 = rlm(Jaxa.Gauge[[i]]$Gauge ~ Jaxa.Gauge[[i]]$Estimate, method = "M", maxit = 200)
      modl2 = lm(Jaxa.Gauge[[i]]$Gauge ~ Jaxa.Gauge[[i]]$Estimate)
      
      y2 = (Jaxa.Gauge[[i]]$Gauge)^(1/4)
      x2 = (Jaxa.Gauge[[i]]$Estimate)^(1/4)
      
      modq1 = rlm(y2 ~ x2, method = "M", maxit = 200)
      modq2 = lm(y2 ~ x2)
      
      a2 = (mod1$coefficients[1] - mod2$coefficient[1])^2 + (mod1$coefficients[2] - mod2$coefficient[2])^2
      a3 = (mod1$coefficients[2] - mod2$coefficient[2])^2
      
      al2 = (modl1$coefficients[1] - modl2$coefficient[1])^2 + (modl1$coefficients[2] - modl2$coefficient[2])^2
      al3 = (modl1$coefficients[2] - modl2$coefficient[2])^2
      
      aq2 = (modq1$coefficients[1] - modq2$coefficient[1])^2 + (modq1$coefficients[2] - modq2$coefficient[2])^2
      aq3 = (modq1$coefficients[2] - modq2$coefficient[2])^2
      
      if(a2 > a1){
        a1 = a2
        k = i
      }
      if(a3 > a0){
        a0 = a3
        k0 = i
      }
      
      if(al2 > al1){
        al1 = al2
        kl = i
      }
      if(al3 > al0){
        al0 = al3
        kl0 = i
      }
      
      if(aq2 > aq1){
        aq1 = aq2
        kq = i
      }
      if(aq3 > aq0){
        aq0 = aq3
        kq0 = i
      }
    }
  }, error = function(e){})
}

df1 = Jaxa.Gauge[[k]]

y1 = (Jaxa.Gauge[[k]]$Gauge)
x1 = (Jaxa.Gauge[[k]]$Estimate)
mod1 = rlm(y1 ~ x1, method = "M")
mod2 = lm(y1 ~ x1)

y3 = (Jaxa.Gauge[[k]]$Gauge)^(1/2)
x3 = (Jaxa.Gauge[[k]]$Estimate)^(1/2)

mods1 = rlm(y3 ~ x3, method = "M", maxit = 200)
mods2 = lm(y3 ~ x3)

y2 = (Jaxa.Gauge[[k]]$Gauge)^(1/4)
x2 = (Jaxa.Gauge[[k]]$Estimate)^(1/4)

modq1 = rlm(y2 ~ x2, method = "M", maxit = 200)
modq2 = lm(y2 ~ x2)

df3 = data.frame(x = c(x1, x3, x2), y = c(y1, y3, y2),
                 Type = factor(rep(c("Linear", "Square Root", "Fourth Root"), each = length(x1)),
                               levels = c("Linear", "Square Root", "Fourth Root")))

df2 = rbind(c(0,1), c(mod1$coefficients[1], mod1$coefficients[2]), c(mod2$coefficients[1], mod2$coefficients[2]),
            c(0,1), c(mods1$coefficients[1], mods1$coefficients[2]), c(mods2$coefficients[1], mods2$coefficients[2]),
            c(0,1), c(modq1$coefficients[1], modq1$coefficients[2]), c(modq2$coefficients[1], modq2$coefficients[2]))
colnames(df2) = c("Intercept", "Slope")
df2 = as.data.frame(df2)
df2$Line = rep(c("y=x", "Huber's", "Least Squares"),3)
df2$Type = factor(rep(c("Linear", "Square Root", "Fourth Root"), each = 3),
                  levels = c("Linear", "Square Root", "Fourth Root"))


ggplot(df3, aes(x = x, y = y)) + geom_abline(df2, mapping = aes(intercept = Intercept, slope = Slope, colour = Line, linetype = Line), size = 1.3) +
  geom_point() + facet_wrap(~Type, scales = "free") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12),
        strip.text = element_text(size = 14, colour = "black")) + 
  labs(title = "Rain Gauge Station 069141", x = "Satellite Estimate (mm)", y = "Gauge Measuremnt (mm)", colour = "Line")
  







#############
#Elevation Mapping
#############


library(raster)
Elevation.1 = getData("alt", country = "Aus", mask = TRUE)
coord.1 = Austra1[,1:2]
coordinates(coord.1) = ~Lon + Lat
Elevation.2 = extract(Elevation.1, coord.1)
Elevation.3 = cbind(Austra1[,1:2], Elevation.2)
colnames(Elevation.3) = c("Lat", "Lon", "Elevation")
Elevation.3 = as.data.frame(Elevation.3)
Elevation.3 = Elevation.3[complete.cases(Elevation.3),]

Elevation.3 = read.csv("https://raw.githubusercontent.com/hinestein/Elevation/main/Elevation", header = TRUE)
Elevation.3 = as.data.frame(Elevation.3)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Elevation.3, 
             mapping = aes(x = Lon, y = Lat, colour = Elevation), size = 0.5) + theme_bw() +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Elevation (m)", title = "Elevation of Australia") + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  geom_segment(aes(y = -35.579368, x = 150.165254, yend = -35.579368 + 3, xend = 150.165254 + 5), size = 0.8) +
  geom_text(aes(x = 150.165254 + 8, y = -35.579368 + 4, label = "Currowan"), size = 5) +
  geom_segment(aes(y = -36.456242, x = 148.263899, yend = -36.456242 - 3, xend = 148.263899 + 5), size = 0.8) +
  geom_text(aes(x = 148.263899 + 7, y = -36.456242 - 4, label = "Mt. Kosciuszko"), size = 5) +
  geom_segment(aes(y = -42.238884, x = 146.690422, yend = -42.238884 - 3, xend = 146.690422 - 5), size = 0.8) +
  geom_text(aes(x = 146.690422 - 5, y = -42.238884 - 4, label = "Tasmania"), size = 5) +
  geom_segment(aes(y = -27.572417, x = 151.972108, yend = -27.572417 + 3, xend = 151.972108 + 4), size = 0.8) +
  geom_text(aes(x = 151.972108 + 6, y = -27.572417 + 4, label = "Toowoomba"), size = 5) +
  geom_segment(aes(y = -29.593891, x = 128.373727, yend = -29.593891 - 8, xend = 128.373727 + 3), size = 0.8) +
  geom_text(aes(x = 128.373727 + 1.5, y = -29.593891 - 9, label = "Great Victorian Desert"), size = 5) +
  geom_segment(aes(y = -30.784662, x = 121.484262, yend = -30.784662 - 10, xend = 121.484262 - 8), size = 0.8) +
  geom_text(aes(x = 121.484262 - 8, y = -30.784662 - 11, label = "Kalgoorlie"), size = 5) +
  geom_segment(aes(y = -22.534127, x = 122.482203, yend = -22.534127 + 8, xend = 122.482203 - 5), size = 0.8) + 
  geom_text(aes(x = 122.482203 - 6.5, y = -22.534127 + 9, label = "Karlamilyi National Park"), size = 5)





#######################
#Times Series Mapping
#######################

ntot = 0
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
    ntot = ntot + nrow(Jaxa.Gauge[[i]])
  }
}

max1 = 0
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
  if(nrow(Jaxa.Gauge[[i]]) > max1){
    max1 = nrow(Jaxa.Gauge[[i]])
    print(i)
  }
  }
}

TS.dates = rep(0, max1 * length(Jaxa.Gauge))
TS.Gauge = rep(0, max1 * length(Jaxa.Gauge))
TS.Estimate = rep(0, max1 * length(Jaxa.Gauge))
TS.lm = rep(0, max1 * length(Jaxa.Gauge))
TS.ind = rep(0, max1 * length(Jaxa.Gauge))

k = 1
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
    m1 = Jaxa.Gauge[[i]][,1]
    m2 = Jaxa.Gauge[[i]][,2]
    m3 = ifelse(nchar(m1) == 1, paste0("0", m1), m1)
    m4 = paste(m2, m3, "15", sep = "-")
    TS.dates[k:(k - 1 + nrow(Jaxa.Gauge[[i]]))] = as.Date(m4)
    TS.Gauge[k:(k - 1 + nrow(Jaxa.Gauge[[i]]))] = Jaxa.Gauge[[i]]$Gauge
    TS.Estimate[k:(k - 1 + nrow(Jaxa.Gauge[[i]]))] = Jaxa.Gauge[[i]]$Estimate
    mod1 = lm(sqrt(Jaxa.Gauge[[i]]$Gauge) ~ sqrt(Jaxa.Gauge[[i]]$Estimate))
    TS.lm[k:(k - 1 + nrow(Jaxa.Gauge[[i]]))] = (mod1$fitted.values)^2
    TS.ind[k:(k - 1 + nrow(Jaxa.Gauge[[i]]))] = i
    k = k + nrow(Jaxa.Gauge[[i]])
    if(i %% 100 == 0){
      print(i)
    }
  }
}


m1 = Jaxa.Gauge[[4]][,1]
m2 = Jaxa.Gauge[[4]][,2]
m3 = ifelse(nchar(m1) == 1, paste0("0", m1), m1)
m4 = paste(m2, m3, "15", sep = "-")
TS.dates = rep(as.Date(m4), length(Jaxa.Gauge))
TS.Gauge = rep(0, max1 * length(Jaxa.Gauge))
TS.Estimate = rep(0, max1 * length(Jaxa.Gauge))
TS.lm = rep(0, max1 * length(Jaxa.Gauge))
TS.ind = rep(1:length(Jaxa.Gauge), each = max1)
k = 0
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
    m.1 = Jaxa.Gauge[[i]][,1]
    m.2 = Jaxa.Gauge[[i]][,2]
    m.3 = ifelse(nchar(m.1) == 1, paste0("0", m.1), m.1)
    m.4 = as.Date(paste(m.2, m.3, "15", sep = "-"))
    mod1 = lm(sqrt(Jaxa.Gauge[[i]]$Gauge) ~ sqrt(Jaxa.Gauge[[i]]$Estimate))
    for(j in 1:nrow(Jaxa.Gauge[[i]])){
      ind = which(m4 == m.4[j])
      TS.Gauge[k + ind] = Jaxa.Gauge[[i]]$Gauge[j]
      TS.Estimate[k + ind] = Jaxa.Gauge[[i]]$Estimate[j]
      TS.lm[k + ind] = mod1$fitted.values[j]^2
    }
  }
  if(i %% 100 == 0){
    print(i)
  }
  k = k + max1
}

TS.df = data.frame(Date = as.Date(TS.dates), Gauge = TS.Gauge, Estimate = TS.Estimate, Regression = TS.lm, Group = TS.ind)

g.Gauge = ggplot(TS.df, aes(x = Date, y = Gauge, group = Group)) +
  geom_line() +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Gauge Measurement") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) + ylim(0, max(c(TS.df$Gauge, TS.df$Estimate, TS.df$Regression)))

g.Sat = ggplot(TS.df, aes(x = Date, y = Estimate, group = Group)) +
  geom_line() +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Satllite Estimate") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) + ylim(0, max(c(TS.df$Gauge, TS.df$Estimate, TS.df$Regression)))


g.Diff = ggplot(TS.df, aes(x = Date, y = Gauge - Estimate, group = Group)) +
  geom_line() +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

g.lm = ggplot(TS.df, aes(x = Date, y = Regression, group = Group)) +
  geom_line() +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Fitted Values") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) + ylim(0, max(c(TS.df$Gauge, TS.df$Estimate, TS.df$Regression)))


grid.arrange(g.Gauge + theme(legend.position="none") + labs(y = "", x = ""), g.Sat + theme(legend.position="none") + labs(y = "", x = ""),
             g.Diff + theme(legend.position="none") + labs(y = "", x = ""), g.lm + theme(legend.position="none") + labs(y = "", x = ""),
             nrow = 2, left = textGrob("Precipitation (mm)", vjust = 2, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90), 
             bottom = textGrob("Date", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = -0.1))





########################
#Combined Loss function
#######################
pred.grid4 = data.frame(Lon = X.mat.2$Lon, Lat = X.mat.2$Lat)

Elevation.4 = idw(formula = Elevation ~ 1, data = Elevation.3, locations =~Lon + Lat, newdata = pred.grid4, idp = 3)

used = NULL
X.mat.1 = NULL
for(i in 1:length(Jaxa.Gauge)){
  tryCatch({
    if(!is.null(Jaxa.Gauge[[i]]) & nrow(Jaxa.Gauge[[i]]) > 6){
      y1 = Jaxa.Gauge[[i]]$Gauge
      x1 = Jaxa.Gauge[[i]]$Estimate
      corr.1 = cor(y1, x1)
      mod1 = rlm(y1 ~ x1, method = "M")
      X.mat.1 = rbind(X.mat.1, c(mod1$coefficients[1], mod1$coefficients[2], summary(mod1)$sigma, Jaxa.Gauge[[i]]$Latitude[1],
                                 Jaxa.Gauge[[i]]$Longitude[1], nrow(Jaxa.Gauge[[i]]), corr.1))
      used = c(used, i)
    }
  }, error = function(e){})
}
colnames(X.mat.1) = c("Intercept", "Slope", "Sigma", "Lat", "Lon", "n", "correlation")
X.mat.1 = as.data.frame(X.mat.1)
used = used[which(X.mat.1$Intercept < 50 & X.mat.1$Intercept > -10 & X.mat.1$Slope > 0 & X.mat.1$Slope < 2 & X.mat.1$Lon > 100 & X.mat.1$Lat < -5 & X.mat.1$n > 11)]
X.mat.1 = X.mat.1[X.mat.1$Intercept < 50 & X.mat.1$Intercept > -10 & X.mat.1$Slope > 0 & X.mat.1$Slope < 2 & X.mat.1$Lon > 100 & X.mat.1$Lat < -5 & X.mat.1$n > 11,]



Y.g = list()
Y.s = list()
k = 1
for(i in used){
  if(!is.null(Jaxa.Gauge[[i]])){
    Y.g[[k]] = Jaxa.Gauge[[i]]$Gauge
    Y.s[[k]] = Jaxa.Gauge[[i]]$Estimate
    k = k + 1
  }
}

ladd = function(A.list){
  if(is.null(ncol(A.list[[1]]))){
    nc = 1
  }else{
    nc = ncol(A.list[[1]])
  }
  if(is.null(nrow(A.list[[1]]))){
    nr = length(A.list[[1]])
  }else{
    nr = nrow(A.list[[1]])
  }
  out = matrix(0, nrow = nr, ncol = nc)
  for(i in 1:length(A.list)){
    out = out + A.list[[i]]
  }
  out
}


x.i = matrix(1, nrow = nrow(X.mat.4), ncol = 1)
x.i = cbind(x.i, Elevation.4$var1.pred)

w0 = Weight.Matrix(locations = X.mat.1[,5:4], k = 7, alpha = 0.92, method = "IDW")
w1 = Weight.Matrix(locations = X.mat.1[,5:4], k = 8, alpha = 0.8, method = "IDW")

beta0 = X.mat.1[,1]
beta1 = X.mat.1[,2]

#Lambda0
a.1 = list()
for(i in 1:length(Y.g)){
  a.1[[i]] = 2*(t(Y.g[[i]]) %*% rep(1, length(Y.g[[i]]))) %*% (t(w0[i,]) %*% beta0)
}

b.1 = list()
for(i in 1:length(Y.g)){
  b.1[[i]] = 2*(length(Y.g[[i]]))*t(beta0) %*% w0[i,] %*% t(x.i[i,])
}

c.1 = list()
for(i in 1:length(Y.g)){
  c.1[[i]] = (2*t(beta0)%*%(w0[i,])) %*% (rep(1, length(Y.g[[i]])) %*% Y.s[[i]] %*% t(x.i[i,]))
}

d.1 = list()
for(i in 1:length(Y.g[[i]])){
  d.1[[i]] = (2*t(beta0) %*% w0[i,]) %*% (rep(1, length(Y.g[[i]])) %*% Y.s[[i]] %*% t(w1[i,]) %*% beta1)
}

e.1 = list()
for(i in 1:length(Y.g)){
  e.1[[i]] = 2*length(Y.s[[i]])*t(beta0) %*% w0[i,] %*% t(w0[i,]) %*% beta0
}

a.1 = ladd(a.1)
b.1 = ladd(b.1)
c.1 = ladd(c.1)
d.1 = ladd(d.1)
e.1 = ladd(e.1)

#lambda1
a.2 = list()
for(i in 1:length(Y.g)){
  a.2[[i]] = 2*(t(Y.g[[i]]) %*% Y.s[[i]]) %*% (t(w1[i,]) %*% beta1)
}

b.2 = list()
for(i in 1:length(Y.g)){
  b.2[[i]] = 2*x.i[i,] %*% (rep(1, length(Y.g[[i]])) %*% Y.s[[i]]) %*% (t(w1[i,]) %*% beta1)
}

c.2 = list()
for(i in 1:length(Y.g)){
  c.2[[i]] = 2*(t(beta0) %*% w0[i,]) %*% (rep(1, length(Y.s[[i]])) %*% Y.s[[i]]) %*% (t(w1[i,]) %*% beta1)
}

d.2 = list()
for(i in 1:length(Y.g)){
  d.2[[i]] = 2*(t(beta1) %*% w1[i,]) %*% (t(Y.s[[i]]) %*% Y.s[[i]]) %*% t(x.i[i,])
}

e.2 = list()
for(i in 1:length(Y.g)){
  e.2[[i]] = 2*(t(beta1) %*% w1[i,]) %*% (t(Y.s[[i]]) %*% Y.s[[i]]) %*% (t(w1[i,]) %*% beta1)
}

a.2 = ladd(a.2)
b.2 = ladd(b.2)
c.2 = ladd(c.2)
d.2 = ladd(d.2)
e.2 = ladd(e.2)

#alpha0

a.3 = list()
for(i in 1:length(Y.g)){
  a.3[[i]] = x.i[i,] %*% (rep(1, length(Y.g[[i]])) %*% Y.g[[i]])
}

b.3 = list()
for(i in 1:length(Y.g)){
  b.3[[i]] = length(Y.g[[i]]) * x.i[i,] %*% (t(w0[i,]) %*% beta0)
}

c.3 = list()
for(i in 1:length(Y.g)){
  c.3[[i]] = x.i[i,] %*% (rep(1, length(Y.g[[i]])) %*% Y.s[[i]]) %*% t(x.i[i,])
}

d.3 = list()
for(i in 1:length(Y.g)){
  d.3[[i]] = x.i[i,] %*% (rep(1, length(Y.g[[i]])) %*% Y.s[[i]]) %*% t(w1[i,]) %*% beta1
}

e.3 = list()
for(i in 1:length(Y.g)){
  e.3[[i]] = length(Y.g[[i]]) * x.i[i,] %*% t(x.i[i,])
}

a.3 = ladd(a.3)
b.3 = ladd(b.3)
c.3 = ladd(c.3)
d.3 = ladd(d.3)
e.3 = ladd(e.3)

#alpha1

a.4 = list()
for(i in 1:length(Y.g)){
  a.4[[i]] = x.i[i,] %*% (t(Y.s[[i]]) %*% Y.g[[i]])
}

b.4 = list()
for(i in 1:length(Y.g)){
  b.4[[i]] = x.i[i,] %*% (t(Y.s[[i]]) %*% rep(1, length(Y.s[[i]]))) %*% t(x.i[i,])
}

c.4 = list()
for(i in 1:length(Y.g)){
  c.4[[i]] = x.i[i,] %*% (t(Y.s[[i]]) %*% rep(1, length(Y.s[[i]]))) %*% (t(w0[i,]) %*% beta0)
}

d.4 = list()
for(i in 1:length(Y.g)){
  d.4[[i]] = x.i[i,] %*% (t(Y.s[[i]]) %*% Y.s[[i]]) %*% (t(w1[i,]) %*% beta1)
}

e.4 = list()
for(i in 1:length(Y.g)){
  e.4[[i]] = x.i[i,] %*% (t(Y.s[[i]]) %*% Y.s[[i]]) %*% t(x.i[i,])
}

a.4 = ladd(a.4)
b.4 = ladd(b.4)
c.4 = ladd(c.4)
d.4 = ladd(d.4)
e.4 = ladd(e.4)


######
#Estimation
######

lambda0.hat = 0
lambda1.hat = 0
alpha0.hat = matrix(0, nrow = ncol(x.i), ncol = 1)
alpha1.hat = matrix(0, nrow = ncol(x.i), ncol = 1)
L1 = Inf
k = 0
while(abs(L1 - lambda0.hat) > 10^(-6)){
  L1 = lambda0.hat
  lambda0.hat = (a.1 - b.1 %*% alpha0.hat - c.1 %*% alpha1.hat - d.1 %*% lambda1.hat)/(e.1)
  lambda1.hat = (a.2 - t(alpha0.hat) %*% b.2 - c.2 %*% lambda0.hat - d.2 %*% alpha1.hat)/(e.2)
  alpha0.hat = solve(e.3) %*% (a.3 - b.3 %*% lambda0.hat - c.3 %*% alpha1.hat - d.3 %*% lambda1.hat)
  alpha1.hat = solve(e.4) %*% (a.4 - b.4 %*% alpha0.hat - c.4 %*% lambda0.hat - d.4 %*% lambda1.hat)
  k = k + 1
  print(k)
}


beta0.hat = x.i %*% alpha0.hat + lambda0.hat[1,1] * w0 %*% beta0
par(mfrow = c(1,1))
plot(beta0.hat, X.mat.1[,1])
abline(0,1)

beta1.hat = x.i %*% alpha1.hat + lambda1.hat[1,1] * w1 %*% X.mat.1[,2]
par(mfrow = c(1,1))
plot(beta1.hat, X.mat.1[,2])
abline(0,1)

sum((beta0.hat - X.mat.1[,1])^2)/length(beta0.hat)
sum((beta1.hat - X.mat.1[,2])^2)/length(beta0.hat)

X.est = cbind(X.mat.1, beta0.hat, beta1.hat)
colnames(X.est) = c(colnames(X.mat.1), "beta0.hat", "beta1.hat")
X.est = as.data.frame(X.est)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.est, 
             mapping = aes(x = Lon, y = Lat, colour = beta1.hat), size = 0.5) + theme_bw() +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Elevation (m)", title = "Elevation of Australia") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))






####################
#Single SLM
####################
O0 = Opt.Weight(Intercept ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w0 = Weight.Matrix(locations = X.mat.2[,5:4], k = 8, alpha = 1, method = "IDW")

x.i = matrix(1, nrow = nrow(X.mat.2))

SLM0 = SPE1(X.mat.2[,1], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X.mat.2[,1]
sigma.0 = sqrt(sum((X.mat.2[,1] - beta0.hat)^2)/(nrow(X.mat.2) - 2))
plot(beta0.hat, X.mat.2[,1])
abline(0,1)


O1 = Opt.Weight(Slope ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w1 = Weight.Matrix(locations = X.mat.2[,5:4], k = 12, alpha = 0.8, method = "IDW")

SLM1 = SPE1(X.mat.2[,2], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X.mat.2[,2]
sigma.1 = sqrt(sum((X.mat.2[,2] - beta1.hat)^2)/(nrow(X.mat.2) - 2))
plot(beta1.hat, X.mat.2[,2])
abline(0,1)

X.hat = data.frame(Lat = X.mat.2$Lat, Lon = X.mat.2$Lon, beta0 = beta0.hat, beta1 = beta1.hat)

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.hat, 
             mapping = aes(x = Lon, y = Lat, colour = beta0), size = 0.9)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Intercept") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.hat, 
             mapping = aes(x = Lon, y = Lat, colour = beta1), size = 0.9)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Slope") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right")

grid.arrange(g.Int + labs(x = " ", y = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, 
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))



pred.grid2 = cbind(as.numeric(Gauge.Loc.2[used, 2]), as.numeric(Gauge.Loc.2[used,3]))
colnames(pred.grid2) = c("Lat", "Lon")
pred.grid2 = as.data.frame(pred.grid2)

idw1 = idw(formula = Elevation ~ 1, locations = ~Lon + Lat, data = Elevation.3, newdata = pred.grid2, idp = 3)

x.new = cbind(1, Elevation.3$Elevation)

X.mat.2$Elevation = idw1$var1.pred

O0 = Opt.Weight(Intercept ~ Elevation, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")
w0 = Weight.Matrix(locations = X.mat.2[,5:4], k = O0[1], alpha = O0[2], method = "IDW")

SLM0 = SPE1(X.mat.2[,1], w0, cbind(1, X.mat.2$Elevation))
sigma.2 = sqrt(sum((X.mat.2[,1] - cbind(1, X.mat.2$Elevation) %*% SLM0$beta.hat -SLM0$lambda.hat * w0 %*% X.mat.2[,1])^2)/(nrow(X.mat.2) - 3))
sw0 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,5:4], Newlocations = Elevation.3[,2:1], k = O0[1], alpha = O0[2], method = "IDW")
beta0.tilde = x.new %*% SLM0$beta.hat + SLM0$lambda.hat * sw0 %*% X.mat.2[,1] 

O1 = Opt.Weight(Slope ~ Elevation, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")
w1 = Weight.Matrix(locations = X.mat.2[,5:4], k = O1[1], alpha = O1[2], method = "IDW")

SLM1 = SPE1(X.mat.2[,2], w1, cbind(1, X.mat.2$Elevation))
sigma.1 = sqrt(sum((X.mat.2[,2] - cbind(1, X.mat.2$Elevation) %*% SLM1$beta.hat -SLM1$lambda.hat * w1 %*% X.mat.2[,2])^2)/(nrow(X.mat.2) - 3))
sw1 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,5:4], Newlocations = Elevation.3[,2:1], k = O1[1], alpha = O1[2], method = "IDW")
beta1.tilde = x.new %*% SLM1$beta.hat + SLM1$lambda.hat * sw1 %*% X.mat.2[,2]

X.tilde = data.frame(beta0 = beta0.tilde, beta1 = beta1.tilde, Lon = Elevation.3$Lon, Lat = Elevation.3$Lat)

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde, 
             mapping = aes(x = Lon, y = Lat, colour = beta0), size = 0.2)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Intercept") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12), legend.position = "left")

g.Slope = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde, 
             mapping = aes(x = Lon, y = Lat, colour = beta1), size = 0.2)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  xlim(min(as.numeric(Gauge.Loc.2[current.loc,3])) - 8, max(as.numeric(Gauge.Loc.2[current.loc,3])) + 8) +
  ylim(min(as.numeric(Gauge.Loc.2[current.loc,2])) - 5, max(as.numeric(Gauge.Loc.2[current.loc,2])) + 5) + 
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Slope") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12))


grid.arrange(g.Int + labs(x = " ", y = " "), g.Slope + labs(y = " ", x = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))



##########################
#New TS
##########################

y.new = list()
k = 1
for(i in used){
  y.new[[k]] = data.frame(Gauge = Jaxa.Gauge[[i]]$Gauge, Estimate = (beta0.hat[i] + beta1.hat[i] * sqrt(Jaxa.Gauge[[i]]$Estimate))^2, Month = Jaxa.Gauge[[i]]$Month, Year = Jaxa.Gauge[[i]]$Year)
  k = k + 1
}

max1 = 0
for(i in used){
  if(!is.null(y.new[[i]])){
    if(nrow(y.new[[i]]) > max1){
      max1 = nrow(y.new[[i]])
      print(i)
    }
  }
}

TS.dates = rep(0, max1 * length(used))
TS.Gauge = rep(0, max1 * length(used))
TS.Estimate = rep(0, max1 * length(used))
TS.ind = rep(0, max1 * length(used))

m1 = y.new[[4]][,3]
m2 = y.new[[4]][,4]
m3 = ifelse(nchar(m1) == 1, paste0("0", m1), m1)
m4 = paste(m2, m3, "15", sep = "-")
TS.dates = rep(as.Date(m4), length(y.new))
TS.Gauge = rep(0, max1 * length(y.new))
TS.Estimate = rep(0, max1 * length(y.new))
TS.ind = rep(1:length(y.new), each = max1)
k = 0
for(i in 1:length(y.new)){
  if(!is.null(y.new[[i]])){
    m.1 = y.new[[i]][,3]
    m.2 = y.new[[i]][,4]
    m.3 = ifelse(nchar(m.1) == 1, paste0("0", m.1), m.1)
    m.4 = as.Date(paste(m.2, m.3, "15", sep = "-"))
    for(j in 1:nrow(y.new[[i]])){
      ind = which(m4 == m.4[j])
      TS.Gauge[k + ind] = y.new[[i]]$Gauge[j]
      TS.Estimate[k + ind] = y.new[[i]]$Estimate[j]
    }
  }
  if(i %% 100 == 0){
    print(i)
  }
  k = k + max1
}


TS.df = data.frame(Date = as.Date(TS.dates), Gauge = TS.Gauge, Estimate = TS.Estimate, Group = TS.ind)

g.Gauge = ggplot(TS.df, aes(x = Date, y = Estimate, group = Group)) +
  geom_line() +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Recreated Gauge") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

g.Sat = ggplot(TS.df, aes(x = Date, y = Gauge - Estimate, group = Group)) +
  geom_line() +
  labs(x = "Date", y = "Precipitation (mm)",
       title = "Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))


grid.arrange(g.Gauge + theme(legend.position="none") + labs(y = "", x = ""), g.Sat + theme(legend.position="none") + labs(y = "", x = ""),
             nrow = 1, left = textGrob("Precipitation (mm)", vjust = 2, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90), 
             bottom = textGrob("Date", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = -0.1))




##############
#Corr vs Time
##############

cor1 = NULL
for(i in 1:(ncol(Aus.all) - 1)){
  for(j in (i + 1):ncol(Aus.all)){
    cor1 = rbind(cor1, c(j - i, cor(Aus.all[,i], Aus.all[,j])))
  }
}


plot(cor1, cex = 0.1, pch = 19)

colnames(cor1) = c("x", "y")
cor1 = as.data.frame(cor1)

ggplot() + geom_point(cor1, mapping = aes(x = x, y = y), colour = "blue", size = 0.1) + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Time Lag", y = "Correlation", title = "Lagged Correlation")



#############
##Both
#Linear
#############


All.Gauge = list()
for(i in 1:length(Jaxa.Gauge)){
  tryCatch({
  All.Gauge[[i]] = as.data.frame(Gauge.Sat.2[[i]][,1:5])
  All.Gauge[[i]]$Jaxa = Jaxa.Gauge[[i]]$Estimate
  All.Gauge[[i]]$Chirp = Gauge.Sat.2[[i]]$Estimate
  }, error = function(e){})
}


X.mat.1 = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(!is.null(All.Gauge[[i]]) & nrow(All.Gauge[[i]]) > 12){
      y1 = (All.Gauge[[i]]$Precipitation)
      x1 = (All.Gauge[[i]]$Jaxa)
      x2 = (All.Gauge[[i]]$Chirp)
      mod1 = rlm(y1 ~ x1 + x2, method = "M", maxit = 200)
      X.mat.1 = rbind(X.mat.1, c(mod1$coefficients[1], mod1$coefficients[2], mod1$coefficients[3], summary(mod1)$sigma, All.Gauge[[i]]$Lat[1],
                                 All.Gauge[[i]]$Lon[1], nrow(All.Gauge[[i]])))
    }
  }, error = function(e){})
}
colnames(X.mat.1) = c("Intercept", "Jaxa", "Chirp", "Sigma", "Lat", "Lon", "n")
X.mat.1 = as.data.frame(X.mat.1)
hist(X.mat.1[,1])
hist(X.mat.1[,2])
hist(X.mat.1[,3])
X.mat.1 = X.mat.1[X.mat.1$Intercept < 60 & X.mat.1$Intercept > -20 & X.mat.1$Jaxa > 0 & X.mat.1$Jaxa < 2 &
                    X.mat.1$Chirp < 1 & X.mat.1$Chirp >-0.5 & X.mat.1$Lon > 100 & X.mat.1$Lat < -5 & X.mat.1$n > 11,]



g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.1, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "left") +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Intercept Estimate")

g.Jaxa = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.1, 
             mapping = aes(x = Lon, y = Lat, colour = Jaxa), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope Estimate")


g.Chirp = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.1, 
             mapping = aes(x = Lon, y = Lat, colour = Chirp), size = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(x = "Longitude", y = "Latitude", colour = "Estimate", title = "Square Root Slope Estimate")

grid.arrange(g.Int + labs(x = " "), g.Jaxa + labs(y = " ", x = " "), g.Chirp, nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))






#############
#Both
#Square Root
#############

X.mat.2 = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(!is.null(All.Gauge[[i]]) & nrow(All.Gauge[[i]]) > 12){
      y1 = (All.Gauge[[i]]$Precipitation)^(1/2)
      x1 = (All.Gauge[[i]]$Jaxa)^(1/2)
      x2 = (All.Gauge[[i]]$Chirp)^(1/2)
      mod1 = rlm(y1 ~ x1 + x2, method = "M", maxit = 200)
      X.mat.2 = rbind(X.mat.2, c(mod1$coefficients[1], mod1$coefficients[2], mod1$coefficients[3], summary(mod1)$sigma, All.Gauge[[i]]$Lat[1],
                                 All.Gauge[[i]]$Lon[1], nrow(All.Gauge[[i]])))
    }
  }, error = function(e){})
}
colnames(X.mat.2) = c("Intercept", "Jaxa", "Chirp", "Sigma", "Lat", "Lon", "n")
X.mat.2 = as.data.frame(X.mat.2)
hist(X.mat.2[,1])
hist(X.mat.2[,2])
hist(X.mat.2[,3])
X.mat.2 = X.mat.2[X.mat.2$Intercept < 2 & X.mat.2$Intercept > -3 & X.mat.2$Jaxa > -0.25 & X.mat.2$Jaxa < 1 & X.mat.2$Chirp > 0 & X.mat.2$Chirp < 1.75 &
                    X.mat.2$Lon > 100 & X.mat.2$Lat < -5 & X.mat.2$n > 11,]




g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.2, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = " ", colour = "Estimate", title = "Intercept")

g.Jaxa = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.2, 
             mapping = aes(x = Lon, y = Lat, colour = Jaxa), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA")


g.Chirp = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.mat.2, 
             mapping = aes(x = Lon, y = Lat, colour = Chirp), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA")

grid.arrange(g.Int + labs(x = " ", y = " "), g.Jaxa , g.Chirp + labs(y = " "), nrow = 3)

g.Int/g.Jaxa/g.Chirp


O0 = Opt.Weight(Intercept ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w0 = Weight.Matrix(locations = X.mat.2[,5:4], k = O0[1], alpha = O0[2], method = "IDW")


x.i = matrix(1, nrow = nrow(X.mat.2), ncol = 1)

SLM0 = SPE1(X.mat.2[,1], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X.mat.2[,1]
plot(beta0.hat, X.mat.2[,1])
abline(0,1)

O1 = Opt.Weight(Jaxa ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w1 = Weight.Matrix(locations = X.mat.2[,5:4], k = O1[1], alpha = O1[2], method = "IDW")

SLM1 = SPE1(X.mat.2[,2], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X.mat.2[,2]
plot(beta1.hat, X.mat.2[,2])
abline(0,1)

O2 = Opt.Weight(Chirp ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w2 = Weight.Matrix(locations = X.mat.2[,5:4], k = O2[1], alpha = O2[2], method = "IDW")

SLM2 = SPE1(X.mat.2[,3], w2, x.i)
beta2.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w2 %*% X.mat.2[,3]
plot(beta1.hat, X.mat.2[,3])
abline(0,1)

Sep0 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,6:5], Newlocations = NOAA.aus[[1]][,2:1], k = O0[1], alpha = O0[2], method = "IDW")
Sep1 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,6:5], Newlocations = NOAA.aus[[1]][,2:1], k = 9, alpha = 1.32, method = "IDW")
Sep2 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,6:5], Newlocations = NOAA.aus[[1]][,2:1], k = O2[1], alpha = O2[2], method = "IDW")

beta0.new =  SLM0$beta.hat[1,1] + SLM0$lambda.hat * Sep0 %*% X.mat.2[,1]
beta1.new =  SLM1$beta.hat[1,1] + SLM1$lambda.hat * Sep1 %*% X.mat.2[,2]
beta2.new =  SLM2$beta.hat[1,1] + SLM2$lambda.hat * Sep2 %*% X.mat.2[,3]


X.tilde1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Intercept = beta0.new, JAXA = beta1.new,  NOAA = beta2.new)

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde1, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = " ", colour = "Estimate", title = "Intercept")

g.Jaxa = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde1, 
             mapping = aes(x = Lon, y = Lat, colour = JAXA), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA")


g.NOAA = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = X.tilde1, 
             mapping = aes(x = Lon, y = Lat, colour = NOAA), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA")

grid.arrange(g.Int + labs(y = "Latitude") + theme(legend.position = "bottom"),
             g.Jaxa + labs(x = "Longitude", y = " ")  + theme(legend.position = "bottom"),
             g.NOAA + labs(x = " ")  + theme(legend.position = "bottom"), nrow = 1)



y.fitted = beta0.new + beta1.new * Jan2017J$Precip[NJ.loc] + beta2.new * NOAA.aus[[457]]$Precipitation


############
#Cokriging
############

library(sp)
library(gstat)
library(RColorBrewer)

data(meuse)
X.sp <- X.mat.2  #Copy the data.  It's still a data.frame
coordinates(X.sp) <- ~Lon + Lat  # Now it's SpatialPointsDataFrame, with coordinates x and y
# Create a categorical variable and plot it



X.i <- gstat(id = "Intercept", formula = Intercept ~ 1, data = X.sp, 
                 nmax = 20, beta = 0.5)
# The order=4 varible is set as per the instructions in the gstat manual.
# this tells gstat that each indicator is cumulative.  You can't be in the
# second category without also being in the first category.
X.i <- gstat(X.i, "JAXA", formula = Jaxa ~ 1, data = X.sp, 
                 nmax = 20, beta = 1)
X.i <- gstat(X.i, "NOAA", formula = Chirp ~ 1, data = X.sp, 
                 nmax = 20, beta = 1)


# Create a semivariogram model with range equal 1200, and 'dummy' partial
# sill and nugget of 1.  We will fit these later.  'One size fits all'
X.i <- gstat(X.i, model = vgm(1, "Sph", 100, 1), fill.all = T)

# Estimate the empiricalvariogram of each indicator
x <- variogram(X.i)
plot(x)

x <- variogram(X.i, width = 1, cutoff = 40)
plot(x)
X.fit = fit.lmc(x, X.i)

plot(x, model = X.fit)

X.spgrid <- Austra1
coordinates(X.spgrid) <- ~Lon + Lat
NJ.loc1 = Jan2017J[NJ.loc,]
coordinates(NJ.loc1) = ~Longitude + Latitude

X.fit


zk1 <- predict(X.fit, newdata = NJ.loc1, indicators = FALSE)

zk.df1 = data.frame(Intercept = zk1$Intercept.pred, JAXA = zk1$JAXA.pred, NOAA = zk1$NOAA.pred,
                   Lon = coordinates(zk1)[,1], Lat = coordinates(zk1)[,2], Int.Var = zk1$Intercept.var,
                   JAXA.var = zk1$JAXA.var, NOAA.var = zk1$NOAA.var)

A1 = zk.df1[,4:5]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

zk.df1 = zk.df1[Aus,]

zk.df2 = zk.df1

zk.df2$Intercept = (zk.df1$Intercept/6.5)
zk.df2$JAXA = zk.df1$JAXA + 0.2
zk.df2$NOAA = zk.df1$NOAA - 0.5

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df2, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = " ", colour = "Estimate", title = "Intercept Estimate")

g.Jaxa = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df2, 
             mapping = aes(x = Lon, y = Lat, colour = JAXA), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA Estimate")

g.NOAA = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df2, 
             mapping = aes(x = Lon, y = Lat, colour = NOAA), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA Estimate")


gauge.fitted = (zk.df2$Intercept + zk.df2$JAXA * sqrt(zk.df$JAXA) + zk.df2$NOAA * sqrt(zk.df$NOAA))^2

G1 = data.frame(Precipitation = gauge.fitted, Lat = zk.df$Lat, Lon = zk.df$Lon)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = G1, 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)", title = "Precipitation Estimate")



dim(NOAA.aus[[440]])
head(JAXA[[185]])

A1 = JAXA[[185]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Longitude + Latitude
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus.J = which(a3$ISO3 == "AUS")

J1 = JAXA[[185]][Aus.J,]
colnames(J1) = colnames(NOAA.aus[[440]])

pred.grid = zk.df[,4:5]
NOAAAug = idw(formula = Precipitation ~ 1, data = NOAA.aus[[440]], locations =~Lon + Lat, newdata = pred.grid, idp = 3)
JAXAAug = idw(formula = Precipitation ~ 1, data = J1, locations =~Lon + Lat, newdata = pred.grid, idp = 3)


gauge.Est = (zk.df2$Intercept + zk.df2$JAXA * sqrt(JAXAAug$var1.pred) + zk.df2$NOAA * sqrt(NOAAAug$var1.pred))^2

gauge.fitted = (zk.df2$Intercept + zk.df2$JAXA * sqrt(zk.df$JAXA) + zk.df2$NOAA * sqrt(zk.df$NOAA))^2

G1 = data.frame(Precipitation = gauge.fitted, Lat = zk.df$Lat, Lon = zk.df$Lon)
G2 = data.frame(Precipitation = gauge.Est, Lat = zk.df$Lat, Lon = zk.df$Lon)

g.GF = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = G1, 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(gauge.Est, gauge.fitted), max(gauge.Est, gauge.fitted))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)", title = "Forecasted Gauge Precipitation")

g.GE = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = G2, 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(gauge.Est, gauge.fitted), max(gauge.Est, gauge.fitted))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)", title = "Recreate Gauge Precipitation")


grid.arrange(g.GF + labs(x = " ") + theme(legend.position = "none"), g.GE + labs(x = " ", y = " "), nrow = 1,
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), vjust = -1, hjust = 1), widths = c(1, 1.25))


g.Int1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df1, 
             mapping = aes(x = Lon, y = Lat, colour = Int.Var), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  labs(x = " ", y = " ", colour = "Estimate", title = "Intercept Variance")

g.Jaxa1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df1, 
             mapping = aes(x = Lon, y = Lat, colour = JAXA.var), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA Variance")


g.NOAA1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df1, 
             mapping = aes(x = Lon, y = Lat, colour = NOAA.var), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA Variance")

(g.Int + g.Int1)/(g.Jaxa + g.Jaxa1)/(g.NOAA + g.NOAA1)

grid.arrange(g.Int + theme(legend.position = "left") + labs(x = " ", y = " ", colour = "Estimate"),
             g.Int1 + theme(legend.position = "right") + labs(x = " ", y = " ", colour = "Estimate"),
             g.Jaxa + theme(legend.position = "left") + labs(x = " ", y = " ", colour = "             "),
             g.Jaxa1 + theme(legend.position = "right") + labs(x = " ", y = " ", colour = "                "),
             g.NOAA + theme(legend.position = "left") + labs(x = " ", y = " ", colour = "             "),
             g.NOAA1 + theme(legend.position = "right") + labs(x = " ", y = " ", colour = "                "), nrow = 3,
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90),
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))

# $Id: cokriging.R,v 1.4 2006-02-10 19:05:02 edzer Exp $
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y

# cokriging of the four heavy metal variables
meuse.g <- gstat(id="zn", formula=log(zinc)~1, data=meuse, nmax = 10)
meuse.g <- gstat(meuse.g, "cu", log(copper)~1, meuse, nmax = 10)
meuse.g <- gstat(meuse.g, "cd", log(cadmium)~1, meuse, nmax = 10)
meuse.g <- gstat(meuse.g, "pb", log(lead)~1, meuse, nmax = 10)
meuse.g <- gstat(meuse.g, model=vgm(1, "Sph", 900, 1), fill.all=T)
x <- variogram(meuse.g, cutoff=1000)
meuse.fit = fit.lmc(x, meuse.g)
plot(x, model = meuse.fit)
z <- predict(meuse.fit, newdata = meuse.grid)

library(lattice)

pl1 <- spplot(z["zn.pred"], main="log-zinc predictions")
pl2 <- spplot(z["cu.pred"], main="log-copper predictions")
pl3 <- spplot(z["cd.pred"], main="log-cadmium predictions")
pl4 <- spplot(z["pb.pred"], main="log-lead predictions")
print(pl1, split = c(1,1,2,2), more=TRUE)
print(pl2, split = c(1,2,2,2), more=TRUE)
print(pl3, split = c(2,1,2,2), more=TRUE)
print(pl4, split = c(2,2,2,2))
z$zn.se = sqrt(z$zn.var)
z$cu.se = sqrt(z$cu.var)
z$pb.se = sqrt(z$pb.var)
z$cd.se = sqrt(z$cd.var)
pl1 <- spplot(z["zn.se"], main="log-zinc std.err.")
pl2 <- spplot(z["cu.se"], main="log-copper std.err.")
pl3 <- spplot(z["cd.se"], main="log-cadmium std.err.")
pl4 <- spplot(z["pb.se"], main="log-lead st.err.")
print(pl1, split = c(1,1,2,2), more=TRUE)
print(pl2, split = c(1,2,2,2), more=TRUE)
print(pl3, split = c(2,1,2,2), more=TRUE)
print(pl4, split = c(2,2,2,2))

rm(meuse.g, x, meuse.fit, z)

# indicator cokriging for the 9 percentiles of zinc:
q <- quantile(meuse$zinc, seq(.1,.9,.1))
meuse.i <- gstat(id = "zn1", formula = I(zinc < q[1])~1, 
                 data = meuse, nmax = 7, beta = .1, set = list(order = 4, zero = 1e-5))
meuse.i <- gstat(meuse.i, "zn2", I(zinc < q[2])~1, meuse, nmax = 7, beta=.2)
meuse.i <- gstat(meuse.i, "zn3", I(zinc < q[3])~1, meuse, nmax = 7, beta=.3)
meuse.i <- gstat(meuse.i, "zn4", I(zinc < q[4])~1, meuse, nmax = 7, beta=.4)
meuse.i <- gstat(meuse.i, "zn5", I(zinc < q[5])~1, meuse, nmax = 7, beta=.5)
meuse.i <- gstat(meuse.i, "zn6", I(zinc < q[6])~1, meuse, nmax = 7, beta=.6)
meuse.i <- gstat(meuse.i, "zn7", I(zinc < q[7])~1, meuse, nmax = 7, beta=.7)
meuse.i <- gstat(meuse.i, "zn8", I(zinc < q[8])~1, meuse, nmax = 7, beta=.8)
meuse.i <- gstat(meuse.i, "zn9", I(zinc < q[9])~1, meuse, nmax = 7, beta=.9)
meuse.i <- gstat(meuse.i, model=vgm(1, "Sph", 900, 1), fill.all=T)
x <- variogram(meuse.i, cutoff=1000)
meuse.fit = fit.lmc(x, meuse.i)
plot(x, model = meuse.fit)
z <- predict(meuse.fit, newdata = meuse.grid)
spplot(z, c(3,5,7,9,11,13,15,17,19), 
       names.attr = paste("est.Pr(Zn < ", q, ")", sep = ""))




pred.grid4 = X.mat.2[,1:2]

Elevation.4 = gstat::idw(formula = Elevation ~ 1, data = Elevation.3, locations =~Lon + Lat, newdata = pred.grid4, idp = 3)

X.mat.2$Elevation = Elevation.4$var1.pred

X = X.mat.2[-which(duplicated(X.mat.2[, 1:2])),]

O0 = Opt.Weight(Intercept ~ Elevation, data = X, locations = X[,1:2], method = "IDW")

w0 = Weight.Matrix(locations = X[,1:2], k = O0[1], alpha = O0[2], method = "IDW")

x.i = cbind(1, X$Elevation)

SLM0 = SPE1(X[,3], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X[,3]
sigma.0 = sqrt(sum((X[,3] - beta0.hat)^2)/(nrow(X) - 2))
plot(beta0.hat, X[,3])
abline(0,1)


O1 = Opt.Weight(JAXA ~ Elevation, data = X, locations = X[,1:2], method = "IDW")

w1 = Weight.Matrix(locations = X[,1:2], k = O1[1], alpha = O1[2], method = "IDW")

SLM1 = SPE1(X[,4], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X[,4]
sigma.1 = sqrt(sum((X[,4] - beta1.hat)^2)/(nrow(X) - 2))
plot(beta1.hat, X[,4])
abline(0,1)


O2 = Opt.Weight(NOAA ~ Elevation, data = X, locations = X[,1:2], method = "IDW")

w2 = Weight.Matrix(locations = X[,1:2], k = O2[1], alpha = O2[2], method = "IDW")

SLM2 = SPE1(X[,5], w2, x.i)
beta2.hat = x.i %*% SLM2$beta.hat + SLM2$lambda.hat * w2 %*% X[,5]
sigma.2 = sqrt(sum((X[,5] - beta2.hat)^2)/(nrow(X[,5]) - 2))
plot(beta2.hat, X[,5])
abline(0,1)


new.w0 =  Sep.Weight.Matrix(Oldlocations = X[,1:2], Newlocations = Elevation.3[, 2:1], k = O0[1], alpha = O0[2],method = "IDW")
new.w1 =  Sep.Weight.Matrix(Oldlocations = X[,1:2], Newlocations = Elevation.3[, 2:1], k = O1[1], alpha = O1[2],method = "IDW")
new.w2 =  Sep.Weight.Matrix(Oldlocations = X[,1:2], Newlocations = Elevation.3[, 2:1], k = O2[1], alpha = O2[2],method = "IDW")

X0 = cbind(1, Elevation.3$Elevation)

y0.hat = X0%*%SLM0$beta.hat + SLM0$lambda.hat*new.w0%*%X[,3]
y1.hat = X0%*%SLM1$beta.hat + SLM1$lambda.hat*new.w1%*%X[,4]
y2.hat = X0%*%SLM2$beta.hat + SLM2$lambda.hat*new.w2%*%X[,5]

beta.new = data.frame(Intercept = y0.hat, JAXA = y1.hat, NOAA = y2.hat, Lon = Elevation.3$Lon, Lat = Elevation.3$Lat)

g.Int = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = beta.new, 
             mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = " ", y = " ", colour = "Estimate", title = "Intercept")

g.Jaxa = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = beta.new, 
             mapping = aes(x = Lon, y = Lat, colour = JAXA), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA")


g.Chirp = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = beta.new, 
             mapping = aes(x = Lon, y = Lat, colour = NOAA), size = 0.9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA")


g.Int / g.Jaxa / g.Chirp




(beta.new$Intercept[17956] + beta.new$JAXA[17956] * (37.2)^(1/2) + beta.new$NOAA[17956] * (18.6)^(1/2))^2

cor(X.mat.2$Chirp, X.mat.2$Jaxa)
cor(X.mat.1$Chirp, X.mat.1$Jaxa)

#################
#NR
#################

Weight.Matrix = function(locations, k = NULL, alpha = 1, method = c("IDW", "Gaussian"), fun = distCosine, scale = TRUE){
  if(is.null(k)){
    k = nrow(locations) - 1
  }
  if(!is.null(k) & k %% 1 != 0){
    stop("k must be an integer")
  }
  if(alpha < 0){
    stop("alpha must be non-negative")
  }
  coords = NULL
  ds = NULL
  for(i in 1:nrow(locations)){
    newX <- distm(locations[i,], locations, fun = distCosine)
    newX1 = sort(newX[1,])[2:(k+1)]
    m3 = NULL
    if(newX1[1]!=0){
      for(j in 1:(k)){
        m3 = c(m3, which(newX[1,] == newX1[j])[1])
      }
    }else{
      newX1 = sort(newX[1,])[3:(k+2)]
      for(j in 1:k){
        m3 = c(m3, which(newX[1,] == newX1[j])[1])
      }
    }
    coords = rbind(coords, matrix(m3, nrow = 1, k))
    ds = rbind(ds, newX1)
  }
  new.W = matrix(0, nrow(locations), nrow(locations))
  if(method == "IDW"){
    for(i in 1:nrow(locations)){
      new.W[i,coords[i,]] = 1/ds[i,]^alpha
    }
  }
  if(method == "Gaussian"){
    for(i in 1:nrow(locations)){
      new.W[i,coords[i,]] = exp(-ds[i,]^2/alpha)
    }
  }
  if(scale){
    for(i in 1:nrow(new.W)){
      new.W[i,] = new.W[i,]/sum(new.W[i,])
    }
  }
  W = new.W
  W
}
n = nrow(W1)
A2 = -sum(diag(solve(diag(n) - 0 * W1))) * W1
A1 = (1/(2*1^2)) * (t(X.mat.4$Intercept) %*% (-W1 - t(W1) + 2*0 * t(W1) %*% W1) %*% X.mat.4$Intercept)

l.hood = function(Y, W, X, lambda, beta, sigma){
  n = nrow(W)
  out.1 = -sum(diag(solve(diag(n) - lambda * W) %*% W)) -
    (1/(2*sigma^2)) * (t(Y) %*% (-W - t(W) + 2 * lambda * t(W) %*% W) %*% Y) +
    t(Y) %*% t(W) %*% X %*% beta +
    t(beta) %*% t(X) %*% W %*% Y
  out.2 = -(1/sigma^2) * (-t(X) %*% (diag(n) - lambda * W) %*% Y + t(X) %*% X %*% beta)
  out.3 = -(n/sigma) + (1/(sigma^3)) * (t(Y) %*% (diag(n) - lambda * t(W)) - t(beta) %*% t(X)) %*% ((diag(n) - lambda * W) %*% Y - X %*% beta)
  out = matrix(c(out.1, out.2, out.3), ncol = 1)
  out
}


L.F = function(Y, W, X, lambda, beta, sigma){
  out1.1 = -sum(diag(-(solve(diag(n) - lambda * W) %*% W %*% solve(diag(n) - lambda * W) %*% W))) - (1/(sigma^2)) * t(Y) %*% t(W) %*% W %*% Y
  out1.2 = -(1/sigma^2) * t(X) %*% W %*% Y
  out1.3 = (1/sigma^3) * (t(Y) %*% (- W - t(W) + 2 * lambda * t(W) %*% W) %*%  Y + t(Y) %*% t(W) %*% X %*% beta + t(beta) %*% t(X) %*% W %*% Y)
  out2.2 = -(1/sigma^2) * t(X) %*% X
  out2.3 = (2/sigma^3) * (-t(X) %*% (diag(n) - lambda * W) %*% Y + t(X) %*% X %*% beta)
  out3.3 = (n/sigma^2) - (3/sigma^4) * (t(Y) %*% (diag(n) - lambda * t(W)) - t(beta) %*% t(X)) %*% ((diag(n) - lambda * W) - X %*% beta)
  out
}
library("geosphere")

W1 = Weight.Matrix(locations = X.mat.4[,5:4], k = 6, alpha = 1, method = "IDW")

l.hood(X.mat.4$Intercept, W1, matrix(1, nrow = nrow(X.mat.4)), 0, c(0), 1)



loc = NULL
for(i in 1:length(Gauge.2)){
  tryCatch({
  if(sum(Gauge.2[[i]]$Year == 2017 & Gauge.2[[i]]$Month == 1) == 1){
    loc = c(loc, i)
  }
  }, error = function(e){})
}
length(loc)

Jan2017G = NULL
k = 1
for(i in loc){
  Jan2017G = rbind(Jan2017G, as.numeric(c(as.numeric(Gauge.2[[i]][which(Gauge.2[[i]]$Year == 2017 & Gauge.2[[i]]$Month == 1),3]), as.numeric(Gauge.Loc.2[i,2:3]))))
  k = k + 1
}


colnames(Jan2017G) = c("Precipitation", "Lat", "Lon")
Jan2017G = as.data.frame(Jan2017G)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Jan2017G, 
             mapping = aes(x = Lon, y = Lat, colour = sqrt(Precipitation)), size = 1.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "Square Root Gauge Precipitation")


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = NOAA.aus[[457]], 
             mapping = aes(x = Lon, y = Lat, colour = sqrt(Precipitation)), size = 1.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "Square Root Gauge Precipitation")


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = JAXA.Monthly[[202]], 
             mapping = aes(x = Longitude, y = Latitude, colour = sqrt(Precip)), size = 1.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "Square Root Gauge Precipitation")


Jan2017J = WORLD
Jan2017J = as.data.frame(Jan2017J)


A1 = Jan2017J[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Longitude + Latitude
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus.J = which(a3$ISO3 == "AUS")

Jan2017J = Jan2017J[Aus.J,]

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Jan2017J, 
             mapping = aes(x = Longitude, y = Latitude, colour = sqrt(Precip)), size = 1.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "Square Root Gauge Precipitation")


NJ.loc = rep(0, nrow(NOAA.aus[[457]]))
for(i in 1:nrow(NOAA.aus[[457]])){
  d1 = abs(Jan2017J$Longitude - NOAA.aus[[457]]$Lon[i])
  d2 = abs(Jan2017J$Latitude - NOAA.aus[[457]]$Lat[i])
  d3 = d1 + d2
  O1 = order(d3)[1]
  NJ.loc[i] = O1
}

y.fitted = beta0.new + beta1.new * sqrt(JAXA[[202]]$Precip[NJ.loc]) + beta2.new * sqrt(NOAA.aus[[457]]$Precipitation)


Fit.df = data.frame(Fitted = y.fitted^2, Lon = NOAA.aus[[457]]$Lon, Lat = NOAA.aus[[457]]$Lat)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Fit.df, 
             mapping = aes(x = Lon, y = Lat, colour = Fitted), size = 1.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "Square Root Gauge Precipitation")

J.df = JAXA[[202]][NJ.loc,c(2,3,4)]
colnames(J.df) = c("Lat", "Lon", "Precipitation")
N.df = NOAA.aus[[457]][,1:3]
G.df = Jan2017G[,c(3,2,1)]
F.df = Fit.df[,c(3,2,1)]
colnames(F.df) = c("Lat", "Lon", "Precipitation")

J.df$Variable = "JAXA"
N.df$Variable = "NOAA"
G.df$Variable = "Gauge"
F.df$Variable = "Fitted"

idw.JAXA = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = J.df, newdata = Austra2[,1:2], idp = 3)
idw.NOAA = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N.df, newdata = Austra2[,1:2], idp = 3)
idw.Fitted = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = F.df, newdata = Austra2[,1:2], idp = 3)

idw.JAXA$Variable = "JAXA"
idw.NOAA$Variable = "NOAA"
idw.Fitted$Variable = "Fitted"

idw.JAXA = idw.JAXA[,c(1,2,3,5)]
colnames(idw.JAXA) = c("Lon", "Lat", "Precipitation", "Variable")

idw.NOAA = idw.NOAA[,c(1,2,3,5)]
colnames(idw.NOAA) = c("Lon", "Lat", "Precipitation", "Variable")

idw.Fitted = idw.Fitted[,c(1,2,3,5)]
colnames(idw.Fitted) = c("Lon", "Lat", "Precipitation", "Variable")

All.df = rbind(idw.JAXA, idw.NOAA, G.df, idw.Fitted)

All.df$Variable = factor(All.df$Variable, levels = c("JAXA", "NOAA", "Gauge", "Fitted"))


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = All.df, 
             mapping = aes(x = Lon, y = Lat, colour = (Precipitation)^(1/2)), size = 1) + facet_wrap(~Variable, nrow = 2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right",
        strip.text = element_text(size = 13)) +
  labs(x = "Longitude", y = "Latitude", colour = "Square Root\nPrecipitation\n(mm)", title = "Precipitation Mapping")

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = J.df, 
             mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 1.2) + 
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "Square Root Gauge Precipitation")






JAXA.aus1 = JAXA[[length(JAXA) - 3]]

NOAA.aus1 = NOAA.aus[[length(NOAA.aus)]]

plot(NOAA.aus1[,1:2])

write.csv(NOAA.aus1, file = "NOAA.fc", row.names = FALSE)

A1 = JAXA.aus1[,3:2]
colnames(A1) = c("Lon", "Lat")
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

JAXA.aus1 = JAXA.aus1[Aus,]

write.csv(JAXA.aus1[,-1], file = "JAXAfc", row.names = FALSE)

All.Gauge[[1]][used,]






