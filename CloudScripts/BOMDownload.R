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
for(k in 1:length(Stations)){
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
n = rep(0, length(BOM.list))

for(i in 1:length(BOM.list)){
  Lon1[i] = BOM.list[[i]]$Lon[1]
  Lat1[i] = BOM.list[[i]]$Lat[1]
  n[i] = nrow(BOM.list[[i]][complete.cases(BOM.list[[i]]),])
}

BOMLoc = data.frame(Lon = Lon1, Lat = Lat1, n = n)


load("BOMlist")


Stations = read.table("Stations", header = FALSE)[-1,]


#Set Working Directory to BOMGaugeData folder
setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/BOMGaugeData")
BOM.list = list()
k = 1
for(i in 1:length(Stations)){
  tryCatch({
  BOM.list[[k]] = read.csv(paste0("BOM", Stations[i]))
  k = k + 1
  }, error = function(e){})
}
setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

BOMLoc = NULL
for(i in 1:length(BOM.list)){
  BOMLoc = rbind(BOMLoc, BOM.list[[i]][i,1:2])
}

for(i in 1:length(BOM.list)){
  BOM.list[[i]] = BOM.list[[i]][complete.cases(BOM.list[[i]]),]
}

colnames(BOMLoc) = c("Lon", "Lat")
BOMLoc = as.data.frame(BOMLoc)

d.start = c(2022, 4)
d.end = c(1900, 1)

for(i in 1:length(BOM.list)){
  if(BOM.list[[i]]$Year[1] < d.start[1]){
    d.start = c(BOM.list[[i]]$Year[1], BOM.list[[i]]$Month[1])
  }else if(BOM.list[[i]]$Year[1] == d.start[1] & BOM.list[[i]]$Month[1] < d.start[2]){
    d.start = c(BOM.list[[i]]$Year[1], BOM.list[[i]]$Month[1])
  }
  if((BOM.list[[i]]$Year[nrow(BOM.list[[i]])] > d.end[1]) & !is.na(BOM.list[[i]]$Rain[nrow(BOM.list[[i]])])){
    d.end = c(BOM.list[[i]]$Year[nrow(BOM.list[[i]])], BOM.list[[i]]$Month[nrow(BOM.list[[i]])])
  }else if(BOM.list[[i]]$Year[nrow(BOM.list[[i]])] == d.end[1] &BOM.list[[i]]$Month[nrow(BOM.list[[i]])] > d.end[2] & !is.na(BOM.list[[i]]$Rain[nrow(BOM.list[[i]])])){
    d.end = c(BOM.list[[i]]$Year[nrow(BOM.list[[i]])], BOM.list[[i]]$Month[nrow(BOM.list[[i]])])
  }
}



years1 = c(rep(2000, 9), rep(2001:(2021), each = 12), 2022, 2022)
months1 = c(4:12, rep(1:12, length(2001:(2021))), 1, 2)
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

datesSatVal = cbind(dates1, days1)

colnames(datesSatVal) = c("Year", "Month", "Days")
datesSatVal = as.data.frame(datesSatVal)

datesSatVal$Date = as.Date(paste(datesSatVal$Year, ifelse(nchar(datesSatVal$Month) == 1, paste0("0", datesSatVal$Month), datesSatVal$Month), "15", sep = "-"))

nrow(datesSatVal)

BOM.date1 = list()
for(j in 1:nrow(datesSatVal)){
  BOM.date1[[j]] = 0
}

for(i in 1:length(BOM.list)){
  Temp = BOM.list[[i]]
  for(j in 1:nrow(datesSatVal)){
    if(sum(Temp$Year == datesSatVal[j,1] & Temp$Month == datesSatVal[j,2]) == 1){
      BOM.date1[[j]] = rbind(BOM.date1[[j]], c(Temp$Lon[j], Temp$Lat[j], i, datesSatVal[j, 1], datesSatVal[j, 2], which(Temp$Year == datesSatVal[j,1] & Temp$Month == datesSatVal[j,2])))
    }
  }
}

for(i in 1:length(BOM.date1)){
  BOM.date1[[i]] = BOM.date1[[i]][-1,]
  colnames(BOM.date1[[i]]) = c("Lon", "Lat", "Index", "Year", "Month", "rownumber")
  BOM.date1[[i]] = as.data.frame(BOM.date1[[i]])
  BOM.date1[[i]] = BOM.date1[[i]][complete.cases(BOM.date1[[i]]),]
}

BOM.monthly = list()
for(i in 1:length(BOM.date1)){
  BOM.monthly[[i]] = 0
}
for(i in 1:length(BOM.date1)){
  for(j in 1:nrow(BOM.date1[[i]])){
    BOM.monthly[[i]]= rbind(BOM.monthly[[i]], c(BOM.date1[[i]][j,1], BOM.date1[[i]][j,2], BOM.date1[[i]][j,3], BOM.list[[BOM.date1[[i]]$Index[j]]]$Rain[BOM.date1[[i]]$rownumber[j]], BOM.date1[[i]]$Year[1], BOM.date1[[i]]$Month[1]))
  }
}

for(i in 1:length(BOM.monthly)){
  BOM.monthly[[i]] = BOM.monthly[[i]][-1,]
  colnames(BOM.monthly[[i]]) = c("Lon", "Lat", "Index", "Rain", "Year", "Month")
  BOM.monthly[[i]] = as.data.frame(BOM.monthly[[i]])
  BOM.monthly[[i]] = BOM.monthly[[i]][complete.cases(BOM.monthly[[i]]),]
}


JN = list()
for(i in 1:length(BOM.monthly)){
  Temp1.df = data.frame(Lon = JAXA.Monthly[[1]]$Longitude, Lat = JAXA.Monthly[[1]]$Lat, Precipitation = JAXA.Monthly[[i]]$Precip)
  Temp1.df = Temp1.df[complete.cases(Temp1.df),]
  idw1 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = Temp1.df, newdata = BOM.monthly[[i]], idp = 3)
  Temp2.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Precipitation = NOAA.aus[[255 + i]]$Precipitation)
  Temp2.df = Temp2.df[complete.cases(Temp2.df),]
  idw2 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = Temp2.df, newdata = BOM.monthly[[i]], idp = 3)
  Temp = data.frame(Lon = idw1$Lon, Lat = idw1$Lat, JAXA = idw1$var1.pred, NOAA = idw2$var1.pred, Index = BOM.monthly[[i]]$Index, Year = BOM.monthly[[i]]$Year, Month = BOM.monthly[[i]]$Month)
  JN[[i]] = Temp
  print(i)
}


for(i in 1:length(BOM.list)){
  tryCatch({
  BOM.list[[i]]$Date = as.Date(paste(BOM.list[[i]]$Year, ifelse(nchar(BOM.list[[i]]$Month) == 1, paste0("0", BOM.list[[i]]$Month), BOM.list[[i]]$Month), "15", sep = "-"))
  }, error = function(e){})
}

BOM.list1 = list()
for(i in 1:length(BOM.list)){
  tryCatch({
  BOM.list1[[i]] = BOM.list[[i]][BOM.list[[i]]$Date >= datesSatVal$Date[1],]
  BOM.list1[[i]] = BOM.list1[[i]][complete.cases(BOM.list1[[i]]),]
  }, error = function(e){})
}


Gauge1 = list()
k = 1
for(i in 1:length(BOM.list1)){
  tryCatch({
  N1 = NULL
  J1 = NULL
  for(j in 1:nrow(BOM.list1[[i]])){
    w1 = which(datesSatVal$Date == BOM.list1[[i]]$Date[j])
    N1 = c(N1, JN[[w1]][which(JN[[w1]]$Index == i),4])
    J1 = c(J1, JN[[w1]][which(JN[[w1]]$Index == i),3])
  }
  Temp = BOM.list1[[i]]
  Temp$JAXA = J1
  Temp$NOAA = N1
  Gauge1[[k]] = Temp
  k = k + 1
  
  }, error = function(e){})
}

X.mat.2 = NULL
used = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
  y1 = sqrt(Gauge1[[i]]$Rain)
  x1 = sqrt(Gauge1[[i]]$JAXA)
  x2 = sqrt(Gauge1[[i]]$NOAA)
  mod1 = lm(y1 ~ x1 + x2)
  X.mat.2 = rbind(X.mat.2, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, nrow(Gauge1[[i]]),
                             cor(y1^2, x1^2), cor(y1^2, x2^2), summary(mod1)$coefficients[,4],
                             summary(mod1)$sigma))
  used = c(used, i)
  }, error = function(e){})
}

colnames(X.mat.2) = c("Lon", "Lat", "Intercept", "JAXA", "NOAA", "n", "JAXAcor", "NOAAcor", "InterceptP", "JAXAP", "NOAAP", "Sigma")
X.mat.2 = as.data.frame(X.mat.2)

used = used[which(X.mat.2$JAXA < 1 & X.mat.2$JAXA > -1 & X.mat.2$NOAA < 2 & X.mat.2$NOAA > -1 & X.mat.2$Intercept < 2.5 & X.mat.2$Intercept > -2.5 & X.mat.2$JAXAcor > 0.4 & X.mat.2$NOAAcor > 0.4)]

X.mat.2 = X.mat.2[X.mat.2$JAXA < 1 & X.mat.2$JAXA > -1,]
X.mat.2 = X.mat.2[X.mat.2$NOAA < 2 & X.mat.2$NOAA > -1,]
X.mat.2 = X.mat.2[X.mat.2$Intercept < 2.5 & X.mat.2$Intercept > -2.5,]
X.mat.2 = X.mat.2[X.mat.2$JAXAcor > 0.4,]
X.mat.2 = X.mat.2[X.mat.2$NOAAcor > 0.4,]


X.mat.2.1 = data.frame(Value = c(X.mat.2$Intercept, X.mat.2$JAXA, X.mat.2$NOAA),
                       Lon = rep(X.mat.2$Lon, 3),
                       Lat = rep(X.mat.2$Lat, 3),
                       Coefficient = factor(rep(c("Intercept", "JAXA", "NOAA"), each = nrow(X.mat.2))), levels = c("Intercept", "JAXA", "NOAA"))

X.mat.2cor = data.frame(Correlation = c(X.mat.2$JAXAcor, X.mat.2$NOAAcor), 
                        Lon = rep(X.mat.2$Lon, 2), Lat = rep(X.mat.2$Lat, 2),
                        Class = factor(rep(c("Gauge-JAXA Correlation", "Gauge-NOAA Correlation"), each = nrow(X.mat.2)),
                                       levels = c("Gauge-JAXA Correlation", "Gauge-NOAA Correlation")))


ggplot() + 
  geom_point(X.mat.2cor, mapping = aes(x = Lon, y = Lat, colour = Correlation), size = 0.75)+ facet_wrap(~Class, nrow = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


X.pval = data.frame(p.val = c(X.mat.2$InterceptP, X.mat.2$JAXAP, X.mat.2$NOAAP), Lon = rep(c(X.mat.2$Lon), 3), Lat = rep(c(X.mat.2$Lat), 3), Class = factor(rep(c("Intercept", "JAXA", "NOAA"), each = nrow(X.mat.2))))

X.pval$id = X.pval$p.val < 0.05

X.pval = X.pval[complete.cases(X.pval),]

X.pval$id = factor(X.pval$id, levels = c("TRUE", "FALSE"))

ggplot() + 
  geom_point(X.pval, mapping = aes(x = Lon, y = Lat, colour = id), size = 0.75) + facet_wrap(~Class, nrow = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Statistically\nSignificant")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) + scale_colour_manual(
    values = c("#82db5b","#f03c3b"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "TRUE", "0" = "FALSE")
  ) +
  guides(colour = guide_legend(override.aes = list(size=10)))






g.Int = ggplot() + 
  geom_point(X.mat.2, mapping = aes(x = Lon, y = Lat, colour = (InterceptP > 0.05)), size = 0.75)+
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Value     ", title = "Intercept")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.JAXA = ggplot() + 
  geom_point(X.mat.2, mapping = aes(x = Lon, y = Lat, colour = JAXA), size = 0.75)+
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Value", title = "JAXA")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9),
                         limits = c(min(c(X.mat.2$JAXA, X.mat.2$NOAA)), max(c(X.mat.2$JAXA, X.mat.2$NOAA))))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.NOAA = ggplot() + 
  geom_point(X.mat.2, mapping = aes(x = Lon, y = Lat, colour = NOAA), size = 0.75)+
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Value", title = "NOAA")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9),
                         limits = c(min(c(X.mat.2$JAXA, X.mat.2$NOAA)), max(c(X.mat.2$JAXA, X.mat.2$NOAA))))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

grid.arrange(g.Int + labs(x = " "), g.JAXA + labs(y = " ", colour = " "), g.NOAA + labs(x = " ", y = " ", colour = " "), nrow = 1)


Elevation.3 = read.csv("https://raw.githubusercontent.com/hinestein/Elevation/main/Elevation", header = TRUE)
Elevation.3 = as.data.frame(Elevation.3)

pred.grid4 = data.frame(Lon = X.mat.2$Lon, Lat = X.mat.2$Lat)

Elevation.4 = idw(formula = Elevation ~ 1, data = Elevation.3, locations =~Lon + Lat, newdata = pred.grid4, idp = 3)


ggplot() + 
  geom_point(X.mat.2, mapping = aes(x = Lon, y = Lat, colour = Sigma), size = 0.75) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Error  ", title = "Linear Model Residual Standard Error") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9),
                         limits = c(min(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE), max(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE)))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  annotation_custom(ggplotGrob(Pd1), xmin = 111, xmax = 137, ymin = -47, ymax = -36)

Pd1 = ggplot(X.mat.2, aes(Sigma)) +
  geom_density() + xlim(0, max(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE))+ 
  theme_bw() + labs(x = "Error", y = "Density") + ylim(0, 1.05) +
  scale_y_continuous(breaks=c(0.0,0.5,1))

Pd2 = ggplot(Errs1, aes(Sigma)) +
  geom_density() + xlim(0, max(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE))+ 
  theme_bw()+ labs(x = "Error", y = "Density") + ylim(0, 1.05)


g.Elevation = ggplot() + 
  geom_point(Elevation.3, mapping = aes(x = Lon, y = Lat, colour = Elevation), size = 0.1)+
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Elevation\n(m)")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=10), axis.text=element_text(size=12), legend.text=element_text(size=10),
        legend.key.size = unit(0.75, "cm"), axis.title=element_text(size=12), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)




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

beta.new1 = NULL
for(i in 1:nrow(X.mat.2)){
  O1 = order(abs(beta.new$Lon - X.mat.2$Lon[i]) + abs(beta.new$Lat - X.mat.2$Lat[i]))[1]
  beta.new1 = rbind(beta.new1, beta.new[O1,])
}

Errs1 = NULL
for(i in 1:length(used)){
  Temp = Gauge1[[used[i]]]
  s2 = sum((sqrt(Temp$Rain) - beta.new1$Intercept[i] - beta.new1$JAXA[i] * sqrt(Temp$JAXA) - beta.new1$NOAA[i] * sqrt(Temp$NOAA))^2/(nrow(Temp) - 3))
  s1 =  sum(abs(sqrt(Temp$Rain) - beta.new1$Intercept[i] - beta.new1$JAXA[i] * sqrt(Temp$JAXA) - beta.new1$NOAA[i] * sqrt(Temp$NOAA))/(nrow(Temp) - 3))
  Errs1 = rbind(Errs1, c(beta.new1$Lon[i], beta.new1$Lat[i], sqrt(s2), s1))
}

colnames(Errs1) = c("Lon", "Lat", "Sigma", "abs")

Errs1 = as.data.frame(Errs1)

Errs1 = Errs1[!is.nan(Errs1$Sigma),]

Errs1 = Errs1[Errs1$Sigma < Inf,]

Errs1 = Errs1[Errs1$Sigma < 4.5, ]

Pd2 = ggplot(Errs1, aes(Sigma)) +
  geom_density() + xlim(0, max(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE))+ 
  theme_bw()+ labs(x = "Error", y = "Density") + ylim(0, 1.05)

ggplot() + 
  geom_point(Errs1, mapping = aes(x = Lon, y = Lat, colour = Sigma), size = 0.75) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Error  ", title = "Spatial Model Residual Standard Error") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9),
                         limits = c(min(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE), max(c(X.mat.2$Sigma, Errs1$Sigma), na.rm = TRUE)))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  annotation_custom(ggplotGrob(Pd2), xmin = 111, xmax = 137, ymin = -47, ymax = -36)


BOM.current = NULL
for(i in 1:length(BOM.list)){
  if(sum(BOM.list[[i]]$Date == "2022-02-15") > 0){
    BOM.current = rbind(BOM.current, BOM.list[[i]][1,c(1:2, 6)])
  }
}

J1 = JAXA.Monthly[[263]]
A1 = J1[,3:2]
coordinates(A1) = ~Longitude + Latitude
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus.JAXA = which(a3$ISO3 == "AUS")
J1 = J1[Aus.JAXA,]
J1 = J1[,-1]
colnames(J1) = c("Lat", "Lon", "Precip", "Year", "Month")


N1 = NOAA.aus[[518]]

Elevation.3.3 = Elevation.3
colnames(Elevation.3.3) = c("Latitude", "Longitude", "Elevation")

idw1 = gstat::idw(formula = Precip ~ 1, locations = ~Lon + Lat, data = J1, newdata = beta.new, idp = 3)
idw2 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N1, newdata = beta.new, idp = 3)

Rain.new = data.frame(Lon = beta.new$Lon, Lat = beta.new$Lat, Precipitation = (beta.new$Intercept + beta.new$JAXA * sqrt(idw1$var1.pred) + beta.new$NOAA * sqrt(idw2$var1.pred))^2)

Rain.all = data.frame(Lon = c(BOM.monthly[[length(BOM.monthly)]]$Lon, idw1$Lon, N1$Lon, Rain.new$Lon),
                      Lat = c(BOM.monthly[[length(BOM.monthly)]]$Lat, idw1$Lat, N1$Lat, Rain.new$Lat),
                      Precipitation = c(BOM.monthly[[length(BOM.monthly)]]$Rain, idw1$var1.pred, N1$Precipitation, Rain.new$Precipitation),
                      Class = factor(rep(c("Gauge", "JAXA", "NOAA", "Blended"),
                                         c(nrow(BOM.monthly[[length(BOM.monthly)]]),
                                                nrow(idw1), nrow(N1), nrow(Rain.new))),
                      levels = c("Gauge", "JAXA", "NOAA", "Blended")))

size1 = rep(c(1,0.1,1,0.1), c(nrow(BOM.monthly[[length(BOM.monthly)]]),
                              nrow(idw1), nrow(N1), nrow(Rain.new)))

ggplot() + 
  geom_point(Rain.all, mapping = aes(x = Lon, y = Lat, colour = sqrt(Precipitation)), size = size1)+ facet_wrap(~Class, nrow = 2) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Square Root Precipitation (mm)^(1/2)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

ggplot() + 
  geom_point(Rain.new, mapping = aes(x = Lon, y = Lat, colour = sqrt(Precipitation)), size = 0.1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=12), axis.text=element_text(size=12), legend.text=element_text(size=10),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.Int = ggplot() + 
  geom_point(beta.new, mapping = aes(x = Lon, y = Lat, colour = Intercept), size = 0.1)+
  theme_bw() + labs()+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none", axis.text.x =  element_blank(),
        plot.margin = unit(c(0,0,-0.5,0), "cm")) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.JAXA = ggplot() + 
  geom_point(beta.new, mapping = aes(x = Lon, y = Lat, colour = JAXA), size = 0.1)+
  theme_bw() + labs()+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none", strip.text = element_text(size = 18), 
        axis.text.x =  element_blank(),
        plot.margin = unit(c(-0.25,0,-0.25,0), "cm")) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.NOAA = ggplot() + 
  geom_point(beta.new, mapping = aes(x = Lon, y = Lat, colour = NOAA), size = 0.1)+
  theme_bw() + labs()+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none", strip.text = element_text(size = 18),
        plot.margin = unit(c(-0.5,0,0,0), "cm")) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

grid.arrange(g.Int + labs(x = " ", y = " "), g.JAXA + labs(y = "Latitude", x = " "), g.NOAA + labs(x = "Longitude", y = " "), nrow = 3)



Total.plot = NULL
for(i in 1:length(Gauge1)){
  Total.plot = rbind(Total.plot, colSums(Gauge1[[i]][, c(6,8,9)]))
}

colnames(Total.plot) = c("Gauge", "JAXA", "NOAA")
Total.plot = as.data.frame(Total.plot)

Total.plot2 = data.frame(Gauge = c(Total.plot$Gauge, Total.plot$Gauge), Satellite = c(Total.plot$JAXA, Total.plot$NOAA),
                         Class = factor(rep(c("Gauge-JAXA Cumulative", "Gauge-NOAA Cumulative"), each = nrow(Total.plot))))

ggplot() + 
  geom_point(Total.plot2, mapping = aes(x = Satellite, y = Gauge), size = 0.5)+ facet_wrap(~ Class, nrow = 1) +
  theme_bw() + labs(x = "Satellite Estimate (mm)", y = "Gauge Measurement (mm)")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) + geom_abline(colour = "blue")


G.J1 = ggplot() + 
  geom_point(Total.plot, mapping = aes(x = JAXA, y = Gauge), size = 0.5)+
  theme_bw() + labs(x = "JAXA Estimate (mm)", y = "Gauge Measurement (mm)", colour = "Value", title = "Gauge-JAXA Cumulative")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) + geom_abline(colour = "blue")+
  xlim(min(Total.plot[,2:3]), max(Total.plot[,2:3]))

G.N1 = ggplot() + 
  geom_point(Total.plot, mapping = aes(x = NOAA, y = Gauge), size = 0.5)+
  theme_bw() + labs(x = "NOAA Estimate (mm)", y = "Gauge Measurement (mm)", colour = "Value", title = "Gauge-NOAA Cumulative")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) + geom_abline(colour = "blue") +
  xlim(min(Total.plot[,2:3]), max(Total.plot[,2:3]))

grid.arrange(G.J1, G.N1 + labs(y = " "), nrow = 1)


BOM.measure = NULL
for(i in 1:length(BOM.list)){
  BOM.measure = rbind(BOM.measure, BOM.list[[i]][BOM.list[[i]]$Date == "2021-06-15",] )
}

J1 = JAXA.Monthly[[255]]

A1 = J1[,3:2]
A1 = as.data.frame(A1)
colnames(A1) = c("Lon", "Lat")
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus.JAXA = which(a3$ISO3 == "AUS")

J2 = J1[Aus.JAXA,]

pred.aus1 = data.frame(Longitude = Austra2$Lon, Latitude = Austra2$Lat)

idw1 = gstat::idw(formula = Precip ~ 1, locations = ~Longitude + Latitude, data = J2, newdata = pred.aus1, idp = 3)

All.measure = data.frame(Precipitation = c(BOM.measure$Rain, idw1$var1.pred, NOAA.aus[[510]]$Precipitation),
                         Lon = c(BOM.measure$Lon, idw1$Longitude, NOAA.aus[[510]]$Lon),
                         Lat = c(BOM.measure$Lat, idw1$Latitude, NOAA.aus[[510]]$Lat),
                         Class = factor(rep(c("Gauge", "JAXA", "NOAA"), c(nrow(BOM.measure), nrow(idw1), nrow(NOAA.aus[[1]])))))


size1 = rep(c(1,0.1,1), c(nrow(BOM.measure), nrow(idw1), nrow(NOAA.aus[[1]])))

ggplot() + 
  geom_point(All.measure, mapping = aes(x = Lon, y = Lat, colour = sqrt(Precipitation)), size = size1)+ facet_wrap(~Class, nrow = 1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Square Root Precipitation (mm)^(1/2)", title = "June 2021 Square Root Precipitation")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

ggplot() + 
  geom_point(All.measure, mapping = aes(x = Lon, y = Lat, colour = sqrt(Precipitation)), size = size1)+ facet_wrap(~Class, nrow = 3) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=14), legend.text=element_text(size=10),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18),
        strip.background = element_blank(),
        strip.text.x = element_blank()) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


Current.loc = NULL
for(i in 1:length(BOM.list)){
  if(sum(BOM.list[[i]]$Date == "2022-01-15") > 0){
    Current.loc = rbind(Current.loc, BOM.list[[i]][1,1:2])
  }
}
BOMLoc = BOMLoc[BOMLoc$n > 0,]

ggplot() + 
  geom_point(BOMLoc, mapping = aes(x = Lon, y = Lat, colour = n), size = 0.65) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Number Of\nObservations")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)



X.mat.2.sum = NULL
X.mat.2.aut = NULL
X.mat.2.win = NULL
X.mat.2.spr = NULL
X.mat.2.season = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1.sum = Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 12|Gauge1[[i]]$Month == 1|Gauge1[[i]]$Month == 2]
    x1.sum = Gauge1[[i]]$JAXA[Gauge1[[i]]$Month == 12|Gauge1[[i]]$Month == 1|Gauge1[[i]]$Month == 2]
    x2.sum = Gauge1[[i]]$NOAA[Gauge1[[i]]$Month == 12|Gauge1[[i]]$Month == 1|Gauge1[[i]]$Month == 2]
    y1.aut = Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 3|Gauge1[[i]]$Month == 4|Gauge1[[i]]$Month == 5]
    x1.aut = Gauge1[[i]]$JAXA[Gauge1[[i]]$Month == 3|Gauge1[[i]]$Month == 4|Gauge1[[i]]$Month == 5]
    x2.aut = Gauge1[[i]]$NOAA[Gauge1[[i]]$Month == 3|Gauge1[[i]]$Month == 4|Gauge1[[i]]$Month == 5]
    y1.win = Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 6|Gauge1[[i]]$Month == 7|Gauge1[[i]]$Month == 8]
    x1.win = Gauge1[[i]]$JAXA[Gauge1[[i]]$Month == 6|Gauge1[[i]]$Month == 7|Gauge1[[i]]$Month == 8]
    x2.win = Gauge1[[i]]$NOAA[Gauge1[[i]]$Month == 6|Gauge1[[i]]$Month == 7|Gauge1[[i]]$Month == 8]
    y1.spr = Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 9|Gauge1[[i]]$Month == 10|Gauge1[[i]]$Month == 11]
    x1.spr = Gauge1[[i]]$JAXA[Gauge1[[i]]$Month == 9|Gauge1[[i]]$Month == 10|Gauge1[[i]]$Month == 11]
    x2.spr = Gauge1[[i]]$NOAA[Gauge1[[i]]$Month == 9|Gauge1[[i]]$Month == 10|Gauge1[[i]]$Month == 11]
    X.mat.2.sum = rbind(X.mat.2.sum, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                                       length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 12|Gauge1[[i]]$Month == 1|Gauge1[[i]]$Month == 2]),
                                       cor(y1.sum, x1.sum), cor(y1.sum, x2.sum)))
    X.mat.2.aut = rbind(X.mat.2.aut, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                                       length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 3|Gauge1[[i]]$Month == 4|Gauge1[[i]]$Month == 5]),
                                       cor(y1.aut, x1.aut), cor(y1.aut, x2.aut)))
    X.mat.2.win = rbind(X.mat.2.win, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                                       length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 6|Gauge1[[i]]$Month == 7|Gauge1[[i]]$Month == 8]),
                                       cor(y1.win, x1.win), cor(y1.win, x2.win)))
    X.mat.2.spr = rbind(X.mat.2.spr, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                                       length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 9|Gauge1[[i]]$Month == 10|Gauge1[[i]]$Month == 11]),
                                       cor(y1.spr, x1.spr), cor(y1.spr, x2.spr)))
    
    X.mat.2.season = rbind(X.mat.2.season, 
                           c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                                             length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 12|Gauge1[[i]]$Month == 1|Gauge1[[i]]$Month == 2]),
                                             cor(y1.sum, x1.sum), cor(y1.sum, x2.sum), "Summer"),
                           c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                             length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 3|Gauge1[[i]]$Month == 4|Gauge1[[i]]$Month == 5]),
                             cor(y1.aut, x1.aut), cor(y1.aut, x2.aut), "Autumn"),
                           c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                             length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 6|Gauge1[[i]]$Month == 7|Gauge1[[i]]$Month == 8]),
                             cor(y1.win, x1.win), cor(y1.win, x2.win), "Winter"),
                           c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1],
                             length(Gauge1[[i]]$Rain[Gauge1[[i]]$Month == 9|Gauge1[[i]]$Month == 10|Gauge1[[i]]$Month == 11]),
                             cor(y1.spr, x1.spr), cor(y1.spr, x2.spr), "Spring"))
  }, error = function(e){})
}

colnames(X.mat.2.season) = c("Lon", "Lat", "n", "JAXAcor", "NOAAcor", "Season")
X.mat.2.season = as.data.frame(X.mat.2.season)

X.mat.2.season = X.mat.2.season[as.numeric(X.mat.2.season$n) > 36 & as.numeric(X.mat.2.season$JAXAcor) > 0 & as.numeric(X.mat.2.season$NOAAcor) > 0,]

X.mat.2.seasons.1 = data.frame(Lon = as.numeric(rep(X.mat.2.season$Lon, 2)), Lat = as.numeric(rep(X.mat.2.season$Lat, 2)),
                              n = as.numeric(rep(X.mat.2.season$n, 2)), Correlation = as.numeric(c(X.mat.2.season$JAXAcor, X.mat.2.season$NOAAcor)),
                              Season = factor(rep(X.mat.2.season$Season, 2), levels = c("Summer", "Autumn", "Winter", "Spring")),
                              Class = factor(rep(c("Gauge-JAXA Correlation", "Gauge-NOAA Correlation"), each = nrow(X.mat.2.season)),
                                             levels = c("Gauge-JAXA Correlation", "Gauge-NOAA Correlation")))

X.mat.2.seasons.1 = X.mat.2.seasons.1[complete.cases(X.mat.2.seasons.1),]


summary(X.mat.2.seasons.1)


ggplot() + 
  geom_point(X.mat.2.seasons.1, mapping = aes(x = Lon, y = Lat, colour = Correlation), size = 0.75) + facet_grid(Season ~ Class) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)





X.mat.2.s = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1 = sqrt(Gauge1[[i]]$Rain)
    x1 = sqrt(Gauge1[[i]]$JAXA)
    x2 = sqrt(Gauge1[[i]]$NOAA)
    x1.1 = rep(0, length(x1))
    x1.2 = rep(0, length(x1))
    x1.3 = rep(0, length(x1))
    x1.4 = rep(0, length(x1))
    x2.1 = rep(0, length(x1))
    x2.2 = rep(0, length(x1))
    x2.3 = rep(0, length(x1))
    x2.4 = rep(0, length(x1))
    x0.1 = rep(0, length(x1))
    x0.2 = rep(0, length(x1))
    x0.3 = rep(0, length(x1))
    x0.4 = rep(0, length(x1))
    x1.1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)] = x1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)]
    x1.2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)] = x1[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)]
    x1.3[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)] = x1[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)]
    x1.4[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)] = x1[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)]
    x2.1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)] = x2[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)]
    x2.2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)] = x2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)]
    x2.3[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)] = x2[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)]
    x2.4[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)] = x2[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)]
    mod1 = lm(y1 ~ x1.1 + x1.2 + x1.3 + x1.4 + x2.1 + x2.2 + x2.3 + x2.4)
    X.mat.2.s = rbind(X.mat.2.s, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, nrow(Gauge1[[i]]), cor(y1^2, x1^2), cor(y1^2, x2^2)))
  }, error = function(e){})
}

colnames(X.mat.2.s) = c("Lon", "Lat", "Intercept", "JAXAsum", "JAXAaut", "JAXAwin", "JAXAspr", "NOAAsum", "NOAAaut", "NOAAwin", "NOAAspr", "n", "JAXAcor", "NOAAcor")

X.mat.2.s = as.data.frame(X.mat.2.s)

hist(X.mat.2.s$JAXAsum)
hist(X.mat.2.s$JAXAaut)
hist(X.mat.2.s$JAXAwin)
hist(X.mat.2.s$JAXAspr)

X.mat.2.s = X.mat.2.s[X.mat.2.s$JAXAsum < 2 & X.mat.2.s$JAXAsum > -1.5, ]
X.mat.2.s = X.mat.2.s[X.mat.2.s$JAXAaut < 2 & X.mat.2.s$JAXAaut > -1, ]
X.mat.2.s = X.mat.2.s[X.mat.2.s$JAXAwin < 1.5 & X.mat.2.s$JAXAwin > -1, ]
X.mat.2.s = X.mat.2.s[X.mat.2.s$JAXAspr < 1.5 & X.mat.2.s$JAXAspr > -1.5, ]

hist(X.mat.2.s$NOAAsum)
hist(X.mat.2.s$NOAAaut)
hist(X.mat.2.s$NOAAwin)
hist(X.mat.2.s$NOAAspr)

X.mat.2.s = X.mat.2.s[X.mat.2.s$NOAAsum < 2 & X.mat.2.s$NOAAsum > -1.5, ]
X.mat.2.s = X.mat.2.s[X.mat.2.s$NOAAaut < 2 & X.mat.2.s$NOAAaut > -1, ]
X.mat.2.s = X.mat.2.s[X.mat.2.s$NOAAwin < 2 & X.mat.2.s$NOAAwin > -0.5, ]
X.mat.2.s = X.mat.2.s[X.mat.2.s$NOAAspr < 2 & X.mat.2.s$NOAAspr > -0.5, ]


X.mat.s = data.frame(Lon = rep(X.mat.2.s$Lon, 8), Lat = rep(X.mat.2.s$Lat, 8),
                     Value = c(X.mat.2.s$JAXAsum, X.mat.2.s$JAXAaut, X.mat.2.s$JAXAwin, X.mat.2.s$JAXAspr,
                               X.mat.2.s$NOAAsum, X.mat.2.s$NOAAaut, X.mat.2.s$NOAAwin, X.mat.2.s$NOAAspr),
                     Class = factor(rep(c("JAXA", "NOAA"), each = nrow(X.mat.2.s) * 4), levels = c("JAXA", "NOAA")),
                     Season = factor(rep(c("Summer", "Autumn", "Winter", "Spring"), each = nrow(X.mat.2.s)),
                                     levels = c("Summer", "Autumn", "Winter", "Spring")))




ggplot() + 
  geom_point(X.mat.s, mapping = aes(x = Lon, y = Lat, colour = Value), size = 0.75) + facet_grid(Season ~ Class) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)





X.mat.2.s = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1 = sqrt(Gauge1[[i]]$Rain)
    x1 = sqrt(Gauge1[[i]]$JAXA)
    x2 = sqrt(Gauge1[[i]]$NOAA)
    x1.1 = rep(0, length(x1))
    x1.2 = rep(0, length(x1))
    x1.3 = rep(0, length(x1))
    x1.4 = rep(0, length(x1))
    x2.1 = rep(0, length(x1))
    x2.2 = rep(0, length(x1))
    x2.3 = rep(0, length(x1))
    x2.4 = rep(0, length(x1))
    x0.1 = rep(0, length(x1))
    x0.2 = rep(0, length(x1))
    x0.3 = rep(0, length(x1))
    x0.4 = rep(0, length(x1))
    x1.1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)] = x1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)]
    x1.2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)] = x1[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)]
    x1.3[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)] = x1[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)]
    x1.4[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)] = x1[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)]
    x2.1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)] = x2[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)]
    x2.2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)] = x2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)]
    x2.3[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)] = x2[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)]
    x2.4[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)] = x2[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)]
    x0.1[which(Gauge1[[i]]$Month == 12 | Gauge1[[i]]$Month == 1 | Gauge1[[i]]$Month == 2)] = 1
    x0.2[which(Gauge1[[i]]$Month == 3 | Gauge1[[i]]$Month == 4 | Gauge1[[i]]$Month == 5)] = 1
    x0.3[which(Gauge1[[i]]$Month == 6 | Gauge1[[i]]$Month == 7 | Gauge1[[i]]$Month == 8)] = 1
    x0.4[which(Gauge1[[i]]$Month == 9 | Gauge1[[i]]$Month == 10 | Gauge1[[i]]$Month == 11)] = 1
    mod1 = lm(y1 ~ x0.1 + x0.2 + x0.3 + x1.1 + x1.2 + x1.3 + x1.4 + x2.1 + x2.2 + x2.3 + x2.4)
    X.mat.2.s = rbind(X.mat.2.s, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, nrow(Gauge1[[i]]), cor(y1^2, x1^2), cor(y1^2, x2^2)))
  }, error = function(e){})
}


X.mat.2.s = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1 = sqrt(Gauge1[[i]]$Rain)
    x1 = sqrt(Gauge1[[i]]$JAXA)
    x2 = sqrt(Gauge1[[i]]$NOAA)
    mod1 = lm(y1 ~ x1 + x2)
    w1 = NULL
    for(j in 1:nrow(Gauge1[[i]])){
      w1 = c(w1, which(datesSatVal$Year == Gauge1[[i]]$Year[j] & datesSatVal$Month == Gauge1[[i]]$Month[j]))
    }
    da2 = rep(NA, length(w1[1]:w1[length(w1)]))
    k = 1
    for(j in 1:length(w1)){
      da2[w1[j]] = mod1$residuals[k]
      k = k + 1
    }
    r1 = ts(da2, frequency = 12)
    decompose(r1, na.rm = TRUE)
    X.mat.2.s = rbind(X.mat.2.s, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, nrow(Gauge1[[i]]), cor(y1^2, x1^2), cor(y1^2, x2^2)))
  }, error = function(e){})
}



X.mat.2.s = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1 = sqrt(Gauge1[[i]]$Rain)
    x1 = sqrt(Gauge1[[i]]$JAXA)
    x2 = sqrt(Gauge1[[i]]$NOAA)
    mod1 = lm(y1 ~ x1 + I(x1^2) + x2 + I(x2^2))
    X.mat.2.s = rbind(X.mat.2.s, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, nrow(Gauge1[[i]]), cor(y1^2, x1^2), cor(y1^2, x2^2)))
  }, error = function(e){})
}

n1 = NULL
for(i in 1:length(Gauge1)){
  n1 = c(n1, nrow(Gauge1[[i]]))
}

n2 = which(n1 == 261)

plot(Gauge1[[n2[1]]]$Rain, Gauge1[[n2[[1]]]]$JAXA)

plot(Gauge1[[243]]$Rain, Gauge1[[243]]$NOAA)



Sc.df = data.frame(y = c(Gauge1[[n2[2]]]$Rain, Gauge1[[n2[2]]]$Rain, Gauge1[[261]]$Rain, Gauge1[[261]]$Rain, Gauge1[[263]]$Rain, Gauge1[[263]]$Rain, Gauge1[[243]]$Rain, Gauge1[[243]]$Rain),
                   x = c(Gauge1[[n2[2]]]$JAXA, Gauge1[[n2[2]]]$NOAA, Gauge1[[261]]$JAXA, Gauge1[[261]]$NOAA, Gauge1[[263]]$JAXA, Gauge1[[263]]$NOAA, Gauge1[[243]]$JAXA, Gauge1[[243]]$NOAA),
                   Class = factor(c(rep(c("Gauge-JAXA", "Gauge-NOAA"), each = nrow(Gauge1[[n2[2]]])), rep(c("Gauge-JAXA", "Gauge-NOAA"), each = nrow(Gauge1[[261]])),
                                    rep(c("Gauge-JAXA", "Gauge-NOAA"), each = nrow(Gauge1[[263]])), rep(c("Gauge-JAXA", "Gauge-NOAA"), each = nrow(Gauge1[[243]])))),
                   Station = factor(rep(c(paste("Station", paste0("0",Gauge1[[n2[2]]]$Station[1])), paste("Station", paste0("00", Gauge1[[261]]$Station[1])),
                                          paste("Station", paste0("0", Gauge1[[263]]$Station[1])), paste("Station", paste0("0", Gauge1[[243]]$Station[1]))),
                                        c(nrow(Gauge1[[n2[2]]]) * 2, nrow(Gauge1[[261]]) * 2, nrow(Gauge1[[263]]) * 2, nrow(Gauge1[[243]]) * 2))))


ggplot() + geom_point(Sc.df[Sc.df$Station == "Station 009768",], mapping = aes(x = sqrt(x), y = sqrt(y)), size = 0.75) + facet_grid(Station ~ Class) +
  theme_bw() + labs(x = "Satellite Estimate (mm)", y = "Gauge Measurement (mm)")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=8), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=8), legend.position = "bottom", strip.text = element_text(size = 16),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        strip.text.y = element_blank()) + geom_abline()




head(Gauge1[[n2[2]]])


Sc.loc = rbind(Gauge1[[n2[2]]][1,1:2], Gauge1[[261]][1,1:2], Gauge1[[263]][1,1:2], Gauge1[[243]][1,1:2])

ggplot() + geom_point(Sc.df, mapping = aes(x = sqrt(x), y = sqrt(y)), size = 0.75) + facet_grid(Station ~ Class) +
  theme_bw() + labs(x = "Square Root Satellite Estimate (mm)^(1/2)", y = "Square Root Gauge Measurement (mm)^(1/2)")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 16))



g.Elevation = ggplot() + 
  geom_point(Elevation.3, mapping = aes(x = Lon, y = Lat, colour = Elevation), size = 0.1)+
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Elevation\n(m)", title = "Australian Elevation")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=18), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(108, 160) + ylim(-50,-4)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  geom_segment(aes(y = -29.54, x = 148.40, yend = -29.54 + 8.5, xend = 148.40 + 7), size = 0.8) +
  geom_text(aes(x = 150.165254 + 6, y = -29.54 + 10, label = "Collarenebri"), size = 5) +
  geom_segment(aes(y = -36.456242, x = 148.263899, yend = -36.456242 - 2, xend = 148.263899 + 5), size = 0.8) +
  geom_text(aes(x = 148.263899 + 7, y = -36.456242 - 4, label = "Mt. Kosciuszko"), size = 5) +
  geom_segment(aes(y = -34.36, x = 117.62, yend = -36.456242 - 2, xend = 117.62 - 2), size = 0.8) +
  geom_text(aes(x = 117.62 -2, y = -36.456242 - 4, label = "Mount Barker"), size = 5) +
  geom_segment(aes(y = -20.44, x = 135.26, yend = -20.44 + 10.5, xend = 135.26 - 5), size = 0.8) +
  geom_text(aes(x = 135.26 - 6, y = -20.44 + 12, label = "Canteen Creek"), size = 5) +
  geom_segment(aes(y = -25.17, x = 152.03, yend = -25.17 + 10.5, xend = 152.03 - 5), size = 0.8) +
  geom_text(aes(x = 152.03 - 2, y = -25.17 + 12, label = "Pine Creek"), size = 5)

g.Elevation


over1 = NULL
for(i in 1:length(Gauge1)){
  over1 = rbind(over1, c(mean(Gauge1[[i]]$Rain > Gauge1[[i]]$JAXA), mean(Gauge1[[i]]$Rain > Gauge1[[i]]$NOAA), Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1]))
}


colnames(over1) = c("JAXA", "NOAA", "Lon", "Lat")
over1 = as.data.frame(over1)

over2 = data.frame(Percentage = c(over1$JAXA, over1$NOAA), Lon = c(over1$Lon, over1$Lon), Lat = c(over1$Lat, over1$Lat), Class = factor(rep(c("JAXA", "NOAA"), each = nrow(over1))))

ggplot() + 
  geom_point(over2, mapping = aes(x = Lon, y = Lat, colour = Percentage * 100), size = 0.75) + facet_wrap(~Class) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Percentage")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)





X.mat.3 = NULL
used = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1 = sqrt(Gauge1[[i]]$Rain)
    x1 = sqrt(Gauge1[[i]]$JAXA)
    x2 = sqrt(Gauge1[[i]]$NOAA)
    mod1 = rlm(y1 ~ x1 - 1, method = "M", maxit = 2000)
    mod2 = rlm(y1 ~ x2 - 1, method = "M", maxit = 2000)
    X.mat.3 = rbind(X.mat.3, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, mod2$coefficients, nrow(Gauge1[[i]]), cor(y1^2, x1^2), cor(y1^2, x2^2)))
    used = c(used, i)
  }, error = function(e){})
}




X.mat.2.s = NULL
used = NULL
for(i in 1:length(Gauge1)){
  tryCatch({
    y1 = scale(sqrt(Gauge1[[i]]$Rain))
    x1 = scale(sqrt(Gauge1[[i]]$JAXA))
    x2 = scale(sqrt(Gauge1[[i]]$NOAA))
    mod1 = lm(y1 ~ x1 + x2)
    X.mat.2.s = rbind(X.mat.2.s, c(Gauge1[[i]]$Lon[1], Gauge1[[i]]$Lat[1], mod1$coefficients, nrow(Gauge1[[i]]), cor(y1^2, x1^2), cor(y1^2, x2^2)))
    used = c(used, i)
  }, error = function(e){})
}














#############################
#Joint algorithm
#############################


Z.list = list()
G.list = list()
k = 1
for(i in used){
  Z.list[[k]] = cbind(1, Gauge1[[i]]$JAXA, Gauge1[[i]]$NOAA)
  G.list[[k]] = Gauge1[[i]]$Rain
}

W.list = list()


used1 = used[sample(1:length(used), 100, replace = FALSE)]

Gauge.vid = NULL
for(i in 1:length(used1)){
  Temp = Gauge1[[used1[i]]]
  Gauge.vid = rbind(Gauge.vid, cbind(c(Temp$Rain, Temp$Rain), c(Temp$JAXA, Temp$NOAA), c(Temp$Station, Temp$Station), rep(c("Gauge-JAXA", "Gauge-NOAA"), each = nrow(Temp))))
}

Gauge.vid1 = data.frame(Gauge = as.numeric(Gauge.vid[,1]), Satellite = as.numeric(Gauge.vid[,2]), Station = Gauge.vid[,3], Class = Gauge.vid[,4])


p1 = ggplot(Gauge.vid1, aes(x = sqrt(Satellite), y = sqrt(Gauge))) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~as.factor(Class)) +
  # Here comes the gganimate specific bits
  labs(title = "Station : {closest_state}", x = 'Square Root Satellite Estimate (mm)^(1/2)', y = 'Square Root Gauge Measurement (mm)^(1/2)') +
  transition_states(Station) +
  ease_aes('linear')


animate(p1, nframes = 500)


New.Gauge1[,1]

k1 = kmeans(New.Gauge1, centers = 10)

n = 6
km1 = NULL
for(i in 2:n){
  k1 = kmeans(New.Gauge1, centers = i)
  km1 = c(km1, k1$cluster)
}


km2 = data.frame(Lon = rep(FusedLoc$Lon, n - 1), Lat = rep(FusedLoc$Lat, n - 1), Cluster = km1, k = rep(2:n, each = nrow(New.Gauge1)))




ggplot() + 
  geom_point(km2[km2$k == 6, ], mapping = aes(x = Lon, y = Lat, colour = as.factor(Cluster)), size = 0.75) + facet_wrap(~k) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Cluster")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


pred.aus1 = data.frame(Lon = Austra2$Lon, Lat = Austra2$Lat)
n = c(6, 16)
km1 = NULL
for(i in n){
  k1 = kmeans(New.Gauge1, centers = i)
  df1 = data.frame(Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Cluster = k1$cluster)
  idw1 = gstat::idw(formula = Cluster ~ 1, locations = ~Lon + Lat, data = df1, newdata = pred.aus1, idp = 10)
  idw1$k = paste0("k = ", i)
  km1 = rbind(km1, idw1)
}


ggplot() + 
  geom_point(km1, mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.01) + facet_wrap(~as.factor(k)) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Cluster")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)



k1 = kmeans(New.Gauge1, centers = 6)
df1 = data.frame(Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Cluster = k1$cluster)
idw1 = gstat::idw(formula = Cluster ~ 1, locations = ~Lon + Lat, data = df1, newdata = pred.aus1, idp = 10)
idw1$k = paste0("k = ", 6)

k2 = kmeans(New.Gauge1, centers = 16)
df2 = data.frame(Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Cluster = k2$cluster)
idw2 = gstat::idw(formula = Cluster ~ 1, locations = ~Lon + Lat, data = df2, newdata = pred.aus1, idp = 10)
idw2$k = paste0("k = ", 16)

h1 = hclust(dist(sqrt(New.Gauge1)), "ward.D")

c1 = cutree(h1, k = 6)
df1 = data.frame(Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Cluster = c1)
idw1 = gstat::idw(formula = Cluster ~ 1, locations = ~Lon + Lat, data = df1, newdata = pred.aus1, idp = 10)
idw1$k = paste0("k = ", 6)

idw1$Class = f1

kmeansplot1 = ggplot() + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = f1), size = 0.01) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Classification", title = "K means Clustering")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)+
  guides(colour = guide_legend(override.aes = list(size=10)))+ scale_colour_manual(
    values = c("#e9c6c7","#f7e697", "#c6d79f", "#d2d9e5", "#7cb2de", "#f6fae4"),
    aesthetics = c("colour", "fill")
  )+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)




k2 = kmeans(New.Gauge1, centers = 16)
df2 = data.frame(Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Cluster = k2$cluster)
idw2 = gstat::idw(formula = Cluster ~ 1, locations = ~Lon + Lat, data = df2, newdata = pred.aus1, idp = 10)
idw2$k = paste0("k = ", 16)

ggplot() + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.01) + facet_wrap(~as.factor(k)) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Cluster")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)+
  guides(colour = guide_legend(override.aes = list(size=10)))


f1 = factor(ifelse(round(idw1$var1.pred) == 3, "Arid",
            ifelse(round(idw1$var1.pred) == 2, "Summer Dominant (Medium)",
                   ifelse(round(idw1$var1.pred) == 1, "Summer Dominant (Low)",
                          ifelse(round(idw1$var1.pred) == 6, "Winter Dominant",
                                 ifelse(round(idw1$var1.pred) == 4, "Summer", "Summer Dominant (High)"))))),
            levels = c("Summer Dominant (High)", "Summer Dominant (Medium)", "Summer Dominant (Low)", "Summer", "Winter Dominant", "Arid"))


f2 = factor(ifelse(round(idw1$var1.pred) == 3, "Arid",
                   ifelse(round(idw1$var1.pred) == 2, "Summer Dominant (Medium)",
                          ifelse(round(idw1$var1.pred) == 1, "Summer Dominant (Low)",
                                 ifelse(round(idw1$var1.pred) == 6, "Winter Dominant",
                                        ifelse(round(idw1$var1.pred) == 4, "Summer", "Summer Dominant (High)"))))),
            levels = c("Summer Dominant (High)", "Summer Dominant (Medium)", "Summer Dominant (Low)", "Uniform", "Winter Dominant", "Arid"))



