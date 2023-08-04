SOI = read.csv("http://www.jamstec.go.jp/virtualearth/data/SINTEX/SINTEX_Nino34.csv")
IOD = read.csv("http://www.jamstec.go.jp/virtualearth/data/SINTEX/SINTEX_DMI.csv")
SOI1 = SOI[,1:2]
IOD1 = IOD[,1:2]
SOI1 = SOI1[1:457,]
IOD1 = IOD1[1:457,]

MJO = readLines("http://www.bom.gov.au/climate/mjo/graphics/rmm.74toRealtime.txt")
MJO2 = matrix(0, ncol = 7, nrow = length(MJO))
for(i in 3:length(MJO)){
  pos = as.vector(gregexpr(" ", MJO[i])[[1]])
  pos1 = pos[-1]
  pos2 = pos[-length(pos)]
  pos3 = which(pos1 - pos2 != 1)
  pos3 = pos3[1:7]
  vec = NULL
  for(j in 1:length(pos3)){
    vec = c(vec, as.numeric(substr(MJO[i], pos[pos3[j]] + 1, pos[pos3[j] + 1] - 1)))
  }
  MJO2[i,] = vec
  if(i %% 1000 == 0){
    print(i)
  }
}

MJO2 = MJO2[-c(1,2),]

MJO = data.frame(Year = MJO2[,1], Month = MJO2[,2], Day = MJO2[,3], RMM1 = MJO2[,4], RMM2 = MJO2[,5], Phase = MJO2[,6], Amplitude = MJO2[,7])

MJO[(nrow(MJO) - 90):nrow(MJO), ]

ggplot() + geom_line(data = MJO[(nrow(MJO) - 90):nrow(MJO), ], aes(x = RMM1, y = RMM2)) + geom_point(data = MJO[(nrow(MJO) - 90):nrow(MJO), ], aes(x = RMM1, y = RMM2))

df1 = data.frame(x = 0, y = 0, r = 1)

MJO.1 = MJO[(nrow(MJO) - 90):nrow(MJO), ]
Month1 = rep(0, nrow(MJO.1))
Month1[MJO.1$Month == 2] = "Feb"
Month1[MJO.1$Month == 3] = "Mar"
Month1[MJO.1$Month == 4] = "Apr"
Month1[MJO.1$Month == 5] = "May"

MJO.1$Month1 = as.factor(Month1)

g1 = ggplot(data = MJO.1, aes(x=RMM1, y=RMM2, colour = Amplitude, label = Day, shape = Month1)) +
  geom_point(size = 3) +
  geom_segment(aes(xend=c(tail(RMM1, n=-1), NA), yend=c(tail(RMM2, n=-1), NA))) +
  theme_bw() +
  coord_cartesian(xlim = c(-3,3), ylim = c(-3, 3)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))

circles <- data.frame(
  x0 = rep(0, 1),
  y0 = rep(0, each = 1),
  r = seq(1, 1, length.out = 1)
)

g2 = g1 + geom_circle(aes(x0 = x0, y0 = y0, r = r), colour = "black", lwd = 0.15, data = circles, inherit.aes = FALSE) + 
  geom_segment(aes(x = -1, y = 0, xend = -2.85, yend = 0), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = 1, y = 0, xend = 2.85, yend = 0), colour = "black", lty = 2, lwd = 0.15) + 
  geom_segment(aes(x = 0, y = 1, xend = 0, yend = 2.9), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = 0, y = -1, xend = 0, yend = -2.75), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = sqrt(2)/2, y = sqrt(2)/2, xend = 3.5, yend = 3.5), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = -sqrt(2)/2, y = sqrt(2)/2, xend = -3.5, yend = 3.5), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = -sqrt(2)/2, y = -sqrt(2)/2, xend = -3.5, yend = -3.5), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = sqrt(2)/2, y = -sqrt(2)/2, xend = 3.5, yend = -3.5), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = -3.5, y = 0, xend = -3.05, yend = 0), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = 3.5, y = 0, xend = 3.05, yend = 0), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = 0, y = 3.5, xend = 0, yend = 3.08), colour = "black", lty = 2, lwd = 0.15) +
  geom_segment(aes(x = 0, y = -3.5, xend = 0, yend = -3.1), colour = "black", lty = 2, lwd = 0.15) +
  annotate(geom="text", x=-3, y=-1.5, label="1", color="Black") +
  annotate(geom="text", x=-1.5, y=-3, label="2", color="Black") +
  annotate(geom="text", x=1.5, y=-3, label="3", color="Black") +
  annotate(geom="text", x=3, y=-1.5, label="4", color="Black") +
  annotate(geom="text", x=3, y=1.5, label="5", color="Black") +
  annotate(geom="text", x=1.5, y=3, label="6", color="Black") +
  annotate(geom="text", x=-1.5, y=3, label="7", color="Black") +
  annotate(geom="text", x=-3, y=1.5, label="8", color="Black")

g3 = g2 + annotate(geom="text", x=-3, y=0, label="West Hem", color="Black", angle =90, size = 5) + 
  annotate(geom="text", x=-2.9, y=0, label="and Africa", color="Black", angle =90, size = 5) +
  annotate(geom="text", x=0, y=3, label="West Pacific", color="Black", angle =0, size = 5) +
  annotate(geom="text", x=3, y=0, label="Maritime", color="Black", angle =270, size = 5) +
  annotate(geom="text", x=2.9, y=0, label="Continent", color="Black", angle =270, size = 5) +
  annotate(geom="text", x=0, y=-3, label="Ocean", color="Black", angle =0, size = 5) +
  annotate(geom="text", x=0, y=-2.8, label="Indian", color="Black", angle =0, size = 5) +
  geom_text(hjust = 1, vjust = 1.5)

g3 + labs(colour = "Amplitude", shape = "Month", title = "Madden-Julian Oscillation") + 
  theme(plot.title = element_text(size = 15, face = "bold"), legend.title=element_text(size=15),
        legend.text=element_text(size=13), legend.key.size = unit(1, "cm")) + scale_shape_discrete(breaks=c("Feb","Mar","Apr", "May"))

# Behold the some circles
ggplot() +
  geom_circle(aes(x0 = x0, y0 = y0, r = r), data = circles)

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

dates3 = dates2[dates2[,1] > 1981,]

m01.1 = seq(1,nrow(dates3),12)
m02.1 = seq(2,nrow(dates3),12)
m03.1 = seq(3,nrow(dates3),12)
m04.1 = seq(4,nrow(dates3),12)
m05.1 = seq(5,nrow(dates3),12)
m06.1 = seq(6,nrow(dates3),12)
m07.1 = seq(7,nrow(dates3),12)
m08.1 = seq(8,nrow(dates3),12)
m09.1 = seq(9,nrow(dates3),12)
m10.1 = seq(10,nrow(dates3),12)
m11.1 = seq(11,nrow(dates3),12)
m12.1 = seq(12,nrow(dates3),12)

M1 = list()
M1[[1]] = m01.1
M1[[2]] = m02.1
M1[[3]] = m03.1
M1[[4]] = m04.1
M1[[5]] = m05.1
M1[[6]] = m06.1
M1[[7]] = m07.1
M1[[8]] = m08.1
M1[[9]] = m09.1
M1[[10]] = m10.1
M1[[11]] = m11.1
M1[[12]] = m12.1

data("economics")
ggplot(economics, aes(x=date)) + 
  geom_line(aes(y = psavert), color = "darkred") + 
  geom_line(aes(y = uempmed), color="steelblue", linetype="twodash") 

date1 = as.Date(SOI1$time)
Osc = data.frame(Date = pred.SOI.sep1$Date, SOI = pred.SOI.sep1$Actual, IOD = pred.IOD.sep1$Actual)
library("tidyverse")
head(df)

ggplot(Osc, aes(x=Date)) + 
  geom_line(aes(y = SOI), color = "darkred") + 
  geom_line(aes(y = IOD), color="steelblue", linetype="twodash") +
  labs(x = "Date", y = "Index",
       title = "Total Empirical Dynamic Quantile Locations") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_color_manual(values = c("darkred", "steelblue"))

library("tidyverse")
date1 = as.Date(SOI1$time)
Osc = data.frame(Date = as.Date(date1), SOI = SOI1$Values, DMI = IOD1$Obs)
Osc1 = data.frame(Date = c(Osc$Date, Osc$Date), Oscillation = c(rep("SOI", nrow(Osc)), rep("DMI",  nrow(Osc))), 
                  Value = c(Osc$SOI, Osc$IOD))

gOsc = ggplot(Osc1, aes(x = Date, y = Value)) +
  geom_line(aes(colour = Oscillation, linetype = Oscillation), size = 0.7) + 
  scale_color_manual(values = c("darkred","blue"))  +
  labs(x = "Date", y = "Index",
       title = "Oscillation Index") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))


date1
MJOosc = data.frame(Date = date1, RMM1 = as.numeric(MJO3[1:length(date1),3]), RMM2 = as.numeric(MJO3[1:length(date1),4]),
                    phase = as.numeric(MJO3[1:length(date1),5]), amplitude = as.numeric(MJO3[1:length(date1),6]))

MJOosc1 <- MJOosc %>%
  select(Date, RMM1, RMM2) %>%
  gather(key = "variable", value = "value", -Date)
head(MJOosc1)

ggplot(MJOosc1, aes(x = Date, y = value)) +
  geom_line(aes(colour = variable, linetype = variable), size = 0.7) + 
  scale_color_manual(values = c("darkred", "steelblue"))  +
  labs(x = "Date", y = "Index",
       title = "Oscillation Index") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

plot(MJO3[,3], MJO3[,4], type = "l")

ts1 = ts(SOI[,2], frequency = 12, start = start(1950,12))
ts2 = ts(IOD[,2], frequency = 12, start = start(1982,1))
ts3 = ts(cbind(SOI1[,2], IOD1[,2]), frequency = 12, start = start(1982,1))
ar1 = ar(ts1, method = "yw", intercept = TRUE)
ar2 = ar.ols(ts2, intercept = TRUE)
ar3 = ar(ts3)

arima(ts1, order = c(4,2,0))

best.SOI = NULL
for(lag in 1:6){
  opt.phi = NULL
  for(k in 1:25){
    lphi = k
    ar.mat = NULL
    for(i in (lphi + lag):nrow(SOI1)){
      ar.mat = rbind(ar.mat, c(SOI1[i,2], SOI1[(i - lag):(i - lag - lphi + 1),2]))
    }
    set.seed(1998)
    cv.error = NULL
    for(i in 1:10){
      samp1 = sample(1:nrow(ar.mat), size = floor(nrow(ar.mat)/10))
      ar.mat1 = ar.mat[-samp1,]
      yc = ar.mat[samp1,1]
      Xc = cbind(1, ar.mat[samp1,-1])
      yphi = ar.mat1[,1]
      Xphi = cbind(1, ar.mat1[,-1])
      phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
      yc.hat = Xc %*% phi.hat
      cv.error = c(cv.error, sum((yc.hat - yc)^2)/nrow(ar.mat))
    }
    opt.phi = c(opt.phi, mean(cv.error))
  }
  best.SOI = rbind(best.SOI, c(min(opt.phi), which(opt.phi == min(opt.phi))))
}

best.IOD = NULL
for(lag in 1:6){
  opt.phi = NULL
  for(k in 1:20){
    lphi = k
    ar.mat = NULL
    for(i in (lphi + lag):nrow(IOD1)){
      ar.mat = rbind(ar.mat, c(IOD1[i,2], IOD1[(i - lag):(i - lag - lphi + 1),2]))
    }
    set.seed(1998)
    cv.error = NULL
    for(i in 1:10){
      samp1 = sample(1:nrow(ar.mat), size = floor(nrow(ar.mat)/10))
      ar.mat1 = ar.mat[-samp1,]
      yc = ar.mat[samp1,1]
      Xc = cbind(1, ar.mat[samp1,-1])
      yphi = ar.mat1[,1]
      Xphi = cbind(1, ar.mat1[,-1])
      phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
      yc.hat = Xc %*% phi.hat
      cv.error = c(cv.error, sum((yc.hat - yc)^2)/(nrow(ar.mat) - lphi))
    }
    opt.phi = c(opt.phi, mean(cv.error))
  }
  best.IOD = rbind(best.IOD, c(min(opt.phi), which(opt.phi == min(opt.phi))))
}

SOI.list = list()
for(k in 1:nrow(best.SOI)){
  lag = k
  lphi = best.SOI[k,2]
  ar.mat = NULL
  for(i in (lphi + lag):nrow(SOI1)){
    ar.mat = rbind(ar.mat, c(SOI1[i,2], SOI1[(i - lag):(i - lag - lphi + 1),2]))
  }
  yphi = ar.mat[,1]
  Xphi = cbind(1, ar.mat[,-1])
  phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
  y.hat = Xphi %*% phi.hat
  SOI.list[[k]] = y.hat
}

date1 = as.Date(paste(substr(SOI1$Date,1,4), substr(SOI1$Date, 5, 6), 15, sep = "-"))
pred.SOI1 = date1
pred.SOI2 = SOI1[,2]
pred.SOI3 = rep("Actual", nrow(SOI1))
for(i in c(1,3,6)){
  lag = best.SOI[i,2]
  pred.SOI1 = c(pred.SOI1, as.Date(date1[-(1:(length(date1) - length(SOI.list[[i]])))]))
  pred.SOI2 = c(pred.SOI2, SOI.list[[i]])
  pred.SOI3 = c(pred.SOI3, rep(paste(i, "lag", sep = "-"), nrow(SOI.list[[i]])))
}
pred.SOI = data.frame(Date = as.Date(pred.SOI1), Value = pred.SOI2, Lag = pred.SOI3)

pred.SOI.1 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[1]])))]),
                        Actual = SOI1[-(1:(length(date1) - length(SOI.list[[1]]))),2], Estimated = SOI.list[[1]])
pred.SOI.3 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[3]])))]),
                        Actual = SOI1[-(1:(length(date1) - length(SOI.list[[3]]))),2], Estimated = SOI.list[[3]])
pred.SOI.6 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[6]])))]),
                        Actual = SOI1[-(1:(length(date1) - length(SOI.list[[6]]))),2], Estimated = SOI.list[[6]])
pred.SOI.sep = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[6]])))]),
                          Actual = SOI1[-(1:(length(date1) - length(SOI.list[[6]]))),2],
                          One = SOI.list[[1]][-(1:(length(SOI.list[[1]]) - length(SOI.list[[6]])))],
                          Three = SOI.list[[3]][-(1:(length(SOI.list[[3]]) - length(SOI.list[[6]])))],
                          Six = SOI.list[[6]])

pred.SOI.sep$Date[325]
pred.SOI.sep1 = pred.SOI.sep[-c(1:324, (nrow(pred.SOI.sep) - 2):nrow(pred.SOI.sep)),]




ggplot(pred.SOI.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "yellow"), size = 0.6)+
  geom_line(aes(y = One, colour = "blue"), linetype = "dashed", size = 0.6) + 
  labs(x = "Date", y = "Index",
       title = "Three Month Lead AR(5) SOI Estimate") +  theme_bw() +
  scale_color_manual(values=c("#FF3300", "blue", "pink"), name = "Time Series", labels = c("Estimated", "Actual", "Three")) + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")


gSOI1 = ggplot(pred.SOI.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual), colour = "#FF3300", size = 1.2) +
  geom_line(aes(y = One), colour = "blue", linetype = "dashed", size = 0.6) + theme_bw() +
  labs(x = "Date", y = "Index",
       title = "One Month Lead AR(3) SOI Estimate") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
gSOI3 = ggplot(pred.SOI.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual), colour = "#FF3300", size = 1.2) +
  geom_line(aes(y = Three), colour = "blue", linetype = "dashed", size = 0.6) + theme_bw() +
  labs(x = "Date", y = "Index",
       title = "Three Month Lead AR(11) SOI Estimate") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
gSOI6 = ggplot(pred.SOI.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "#FF3300"), size = 1.2) +
  geom_line(aes(y = Six, colour = "blue"), linetype = "dashed", size = 0.6) + theme_bw() +
  labs(x = "Date", y = "Index",
       title = "Six Month Lead AR(19) SOI Estimate") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom") +
  scale_color_manual(values=c("#FF3300", "blue"), 
                     name = "Time Series:   ", labels = c("Actual      ", "Estimated"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(gSOI6) 

p3 <- grid.arrange(gSOI1 + theme(legend.position="none") + labs(y = "", x = ""),
                   gSOI3 + theme(legend.position="none") + labs(x = ""),
                   gSOI6 + theme(legend.position = "none") + labs(y = ""),
                   mylegend, nrow = 4)



library(tidyverse)


ggplot(aes(x = x, y = 10, size = y)) + 
  geom_point() +
  guides(size = FALSE) + 
  theme(plot.margin = margin(2,.8,2,.8, "cm"),
        plot.background = element_rect(fill = "darkgrey"))


IOD.list = list()
for(k in 1:nrow(best.IOD)){
  lag = k
  lphi = best.IOD[k,2]
  ar.mat = NULL
  for(i in (lphi + lag):nrow(IOD1)){
    ar.mat = rbind(ar.mat, c(IOD1[i,2], IOD1[(i - lag):(i - lag - lphi + 1),2]))
  }
  yphi = ar.mat[,1]
  Xphi = cbind(1, ar.mat[,-1])
  phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
  y.hat = Xphi %*% phi.hat
  IOD.list[[k]] = y.hat
}

date1 = as.Date(paste(substr(IOD1$Date,1,4), substr(IOD1$Date, 5, 6), 15, sep = "-"))
pred.IOD1 = date1
pred.IOD2 = IOD1[,2]
pred.IOD3 = rep("Actual", nrow(IOD1))
for(i in c(1, 3, 6)){
  lag = best.IOD[i,2]
  pred.IOD1 = c(pred.IOD1, as.Date(date1[-(1:(length(date1) - length(IOD.list[[i]])))]))
  pred.IOD2 = c(pred.IOD2, IOD.list[[i]])
  pred.IOD3 = c(pred.IOD3, rep(paste(i, "lag", sep = "-"), nrow(IOD.list[[i]])))
}
pred.IOD = data.frame(Date = as.Date(pred.IOD1), Value = pred.IOD2, Lag = pred.IOD3)

pred.IOD.1 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(IOD.list[[1]])))]),
                        Actual = IOD1[-(1:(length(date1) - length(IOD.list[[1]]))),2], Estimated = IOD.list[[1]])
pred.IOD.3 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(IOD.list[[3]])))]),
                        Actual = IOD1[-(1:(length(date1) - length(IOD.list[[3]]))),2], Estimated = IOD.list[[3]])
pred.IOD.6 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(IOD.list[[6]])))]),
                        Actual = IOD1[-(1:(length(date1) - length(IOD.list[[6]]))),2], Estimated = IOD.list[[6]])
pred.IOD.sep = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(IOD.list[[6]])))]),
                          Actual = IOD1[-(1:(length(date1) - length(IOD.list[[6]]))),2],
                          One = IOD.list[[1]][-(1:(length(IOD.list[[1]]) - length(IOD.list[[6]])))],
                          Three = IOD.list[[3]],
                          Six = IOD.list[[6]])

pred.IOD.sep$Date[1301]
pred.IOD.sep1 = pred.IOD.sep[-c(1:1301),]

gIOD1 = ggplot(pred.IOD.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual), colour = "#FF3300", size = 1.2) +
  geom_line(aes(y = One), colour = "blue", linetype = "dashed", size = 0.6) + theme_bw() +
  labs(x = "Date", y = "Index",
       title = "One Month Lead AR(4) DMI Estimate") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
gIOD3 = ggplot(pred.IOD.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual), colour = "#FF3300", size = 1.2) +
  geom_line(aes(y = Three), colour = "blue", linetype = "dashed", size = 0.6) + theme_bw() +
  labs(x = "Date", y = "Index",
       title = "Three Month Lead AR(17) DMI Estimate") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
gIOD6 = ggplot(pred.IOD.sep1, aes(x = Date)) +
  geom_line(aes(y = Actual, colour = "#FF3300"), size = 1.2) +
  geom_line(aes(y = Six, colour = "blue"), linetype = "dashed", size = 0.6) + theme_bw() +
  labs(x = "Date", y = "Index",
       title = "Six Month Lead AR(14) DMI Estimate") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom") +
  scale_color_manual(values=c("#FF3300", "blue"), 
                     name = "Time Series:   ", labels = c("Actual      ", "Estimated"))

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(gIOD6) 

p3 <- grid.arrange(gIOD1 + theme(legend.position="none") + labs(y = "", x = ""),
                   gIOD3 + theme(legend.position="none") + labs(x = ""),
                   gIOD6 + theme(legend.position = "none") + labs(y = ""),
                   mylegend, nrow = 4)




cor.A.SOI.IOD = matrix(0, nrow = nrow(Aus.A.all), ncol = length(M1))
cor.A.IOD = matrix(0, nrow = nrow(Aus.A.all), ncol = length(M1))
for(i in 1:length(M1)){
  for(j in 1:nrow(Aus.A.all)){
    cor.A.SOI[j,i] = cor(Aus.A.all[j,M1[[i]] + 12], SOI1[M1[[i]], 2])
    cor.A.IOD[j,i] = cor(Aus.A.all[j,M1[[i]] + 12], IOD1[M1[[i]], 2])
  }
  print(i)
}

cor.SOI = matrix(0, nrow = nrow(Aus.all), ncol = length(M1))
cor.IOD = matrix(0, nrow = nrow(Aus.all), ncol = length(M1))
for(i in 1:length(M1)){
  for(j in 1:nrow(Aus.A.all)){
    cor.SOI[j,i] = cor(Aus.all[j,M1[[i]] + 12], SOI1[M1[[i]], 2])
    cor.IOD[j,i] = cor(Aus.all[j,M1[[i]] + 12], IOD1[M1[[i]], 2])
  }
  print(i)
}

Cor.A.0 = data.frame(Lon = precip3[[1]]$Lon[Aus], Lat = precip3[[1]]$Lat[Aus], JanSOI = cor.A.SOI[,1],
                     FebSOI = cor.A.SOI[,2], MarSOI = cor.A.SOI[,3], AprSOI = cor.A.SOI[,4], MaySOI = cor.A.SOI[,5],
                     JunSOI = cor.A.SOI[,6], JulSOI = cor.A.SOI[,7], AugSOI = cor.A.SOI[,8], SepSOI = cor.A.SOI[,9],
                     OctSOI = cor.A.SOI[,10], NovSOI = cor.A.SOI[,11], DecSOI = cor.A.SOI[,12],
                     JanIOD = cor.A.IOD[,1],
                     FebIOD = cor.A.IOD[,2], MarIOD = cor.A.IOD[,3], AprIOD = cor.A.IOD[,4], MayIOD = cor.A.IOD[,5],
                     JunIOD = cor.A.IOD[,6], JulIOD = cor.A.IOD[,7], AugIOD = cor.A.IOD[,8], SepIOD = cor.A.IOD[,9],
                     OctIOD = cor.A.IOD[,10], NovIOD = cor.A.IOD[,11], DecIOD = cor.A.IOD[,12])

Cor.A = matrix(0, ncol = ncol(Cor.A.0), nrow = nrow(pred.grid))
for(i in 3:ncol(Cor.A.0)){
  idw1 = idw(formula = Cor.A.0[,i] ~ 1, locations =~Lon + Lat, data = Cor.A.0, newdata = pred.grid, idp = 3)
  Cor.A[,i] = idw1$var1.pred
  print(i)
}

Cor.A[,1] = pred.grid$Lon
Cor.A[,2] = pred.grid$Lat
colnames(Cor.A) = colnames(Cor.A.0)
Cor.A = as.data.frame(Cor.A)

######
#SOI World
######

g01 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = JanSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "January") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g02 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = FebSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "February") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g03 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = MarSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "March") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g04 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = AprSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "April") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g05 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = MaySOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "May") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g06 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = JunSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "June") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g07 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = JulSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "July") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g08 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = AugSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "August") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g09 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = SepSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "September") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g10 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = OctSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "October") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g11 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = NovSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "November") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g12 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = DecSOI), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI), max(cor.A.SOI))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "December") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(g12) 


my_layout = rbind(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12), c(13,13,13))

grid.arrange(g12 + theme(legend.position="none") + labs(y = "", x = ""), g01 + theme(legend.position="none") + labs(y = "", x = ""),
             g02 + theme(legend.position="none") + labs(y = "", x = ""), g03 + theme(legend.position="none") + labs(y = "", x = ""),
             g04 + theme(legend.position="none") + labs(y = "", x = ""), g05 + theme(legend.position="none") + labs(y = "", x = ""),
             g06 + theme(legend.position="none") + labs(y = "", x = ""), g07 + theme(legend.position="none") + labs(y = "", x = ""),
             g08 + theme(legend.position="none") + labs(y = "", x = ""), g09 + theme(legend.position="none") + labs(y = "", x = ""),
             g10 + theme(legend.position="none") + labs(y = "", x = ""), g11 + theme(legend.position="none") + labs(y = "", x = ""),
             mylegend, nrow = 5, top = textGrob("SOI Monthly Correlation", gp=gpar(fontsize=20,font=8)), layout_matrix = my_layout,
             left = textGrob("Latitude", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = -0.8, rot = 90), 
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -13, hjust = 0.2))


########
#IOD
########
g01 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = JanIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "January") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g02 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = FebIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "February") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g03 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = MarIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "March") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g04 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = AprIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "April") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g05 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = MayIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "May") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g06 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = JunIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "June") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g07 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = JulIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "July") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g08 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = AugIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "August") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g09 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = SepIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "September") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g10 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = OctIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "October") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g11 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = NovIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "November") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g12 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A,
             mapping = aes(x = Lon, y = Lat, colour = DecIOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.IOD), max(cor.A.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "December") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom")


g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(g12) 


my_layout = rbind(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12), c(13,13,13))

grid.arrange(g12 + theme(legend.position="none") + labs(y = "", x = ""), g01 + theme(legend.position="none") + labs(y = "", x = ""),
             g02 + theme(legend.position="none") + labs(y = "", x = ""), g03 + theme(legend.position="none") + labs(y = "", x = ""),
             g04 + theme(legend.position="none") + labs(y = "", x = ""), g05 + theme(legend.position="none") + labs(y = "", x = ""),
             g06 + theme(legend.position="none") + labs(y = "", x = ""), g07 + theme(legend.position="none") + labs(y = "", x = ""),
             g08 + theme(legend.position="none") + labs(y = "", x = ""), g09 + theme(legend.position="none") + labs(y = "", x = ""),
             g10 + theme(legend.position="none") + labs(y = "", x = ""), g11 + theme(legend.position="none") + labs(y = "", x = ""),
             mylegend, nrow = 5, top = textGrob("DMI Monthly Correlation", gp=gpar(fontsize=20,font=8)), layout_matrix = my_layout,
             left = textGrob("Latitude", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = -0.8, rot = 90), 
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -13, hjust = 0.2))




cor.A.SOI.IOD = matrix(0, nrow = nrow(Aus.A.all), ncol = length(M1))
for(i in 1:length(M1)){
  for(j in 1:nrow(Aus.A.all)){
    cor.A.SOI.IOD[j,i] = cor(Aus.A.all[j,M1[[i]] + 12], SOI1[M1[[i]], 2] * IOD1[M1[[i]], 2])
  }
  print(i)
}

cor.SOI.IOD = matrix(0, nrow = nrow(Aus.all), ncol = length(M1))
for(i in 1:length(M1)){
  for(j in 1:nrow(Aus.A.all)){
    cor.SOI.IOD[j,i] = cor(Aus.all[j,M1[[i]] + 12], SOI1[M1[[i]], 2] * IOD1[M1[[i]], 2])
  }
  print(i)
}

Cor.A.1 = data.frame(Lon = precip3[[1]]$Lon[Aus], Lat = precip3[[1]]$Lat[Aus], JanSOI.IOD = cor.A.SOI.IOD[,1],
                     FebSOI.IOD = cor.A.SOI.IOD[,2], MarSOI.IOD = cor.A.SOI.IOD[,3], AprSOI.IOD = cor.A.SOI.IOD[,4], MaySOI.IOD = cor.A.SOI.IOD[,5],
                     JunSOI.IOD = cor.A.SOI.IOD[,6], JulSOI.IOD = cor.A.SOI.IOD[,7], AugSOI.IOD = cor.A.SOI.IOD[,8], SepSOI.IOD = cor.A.SOI.IOD[,9],
                     OctSOI.IOD = cor.A.SOI.IOD[,10], NovSOI.IOD = cor.A.SOI.IOD[,11], DecSOI.IOD = cor.A.SOI.IOD[,12])

Cor.A.1.1 = matrix(0, ncol = ncol(Cor.A.1), nrow = nrow(pred.grid))
for(i in 3:ncol(Cor.A.1)){
  idw1 = idw(formula = Cor.A.1[,i] ~ 1, locations =~Lon + Lat, data = Cor.A.1, newdata = pred.grid, idp = 3)
  Cor.A.1.1[,i] = idw1$var1.pred
  print(i)
}

Cor.A.1.1[,1] = pred.grid$Lon
Cor.A.1.1[,2] = pred.grid$Lat
colnames(Cor.A.1.1) = colnames(Cor.A.1)
Cor.A.1.1 = as.data.frame(Cor.A.1.1)



g06 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Cor.A.1.1,
             mapping = aes(x = Lon, y = Lat, colour = JunSOI.IOD), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(cor.A.SOI.IOD), max(cor.A.SOI.IOD))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "June") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
g06






SOI = read.table("https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/soi", header = TRUE)
IOD = read.table("https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/sstindex/sliding_30year_period/DMI/3rmean", header = TRUE)

SOI = SOI[as.numeric(rownames(SOI)) >= 1980, ]
IOD = IOD[as.numeric(rownames(IOD)) >= 1981, ]

SOI1 = as.vector(t(as.matrix(SOI)))
SOI1 = SOI1[SOI1 < 90]
SOI.ave = NULL
for(i in 2:(length(SOI1) - 1)){
  SOI.ave = c(SOI.ave, mean(SOI1[(i-1):(i + 1)]))
}

IOD1 = as.vector(t(as.matrix(IOD)))
SOI.ave = SOI.ave[-c(1:11)]


IOD1 = IOD1[IOD1 < 90]



library(tseries)

SOI.ts = as.ts(SOI1, frequency = 12)

adf.test(IOD1, k = 10)


dates2 = rbind(dates2, c(2021, 3, 31), c(2021, 4, 30))

SOI.date = paste(dates2[,1], dates2[,2], "15", sep = "-")
SOI.date = as.Date(SOI.date)

Osc.df = data.frame(Value = c(SOI.ave, IOD1), Date = as.Date(rep(SOI.date, 2)), Oscillation = rep(c("SOI" , "IOD"), each = length(SOI.ave)))

ggplot(Osc.df, aes(x=Date, y = Value)) + 
  geom_line(aes(colour = Oscillation, linetype = Oscillation), size = 0.9) +
  labs(x = "Date", y = "Index",
       title = "Oscillation Index") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_color_manual(values = c("darkred", "steelblue"))



Aus.standard = Aus.all
for(i in 1:nrow(Aus.all)){
  for(j in 1:12){
    temp.vec = scale(Aus.all[i, M[[j]]])[,1]
    Aus.standard[i, M[[j]]] = temp.vec
  }
}




SOI.cor = rep(0, nrow(Aus.all))
IOD.cor = rep(0, nrow(Aus.all))
for(i in 1:nrow(Aus.all)){
  SOI.cor[i] = cor(Aus.standard[i,], SOI.ave)
  IOD.cor[i] = cor(Aus.standard[i,], IOD1)
  if(i %% 100 == 0){
    print(i)
  }
}
  
hist(SOI.cor)
hist(IOD.cor)

SOI.df = data.frame(Value = SOI.cor, Longitude = precip3[[1]][Aus,2], Latitude = precip3[[1]][Aus,1])
IOD.df = data.frame(Value = IOD.cor, Longitude = precip3[[1]][Aus,2], Latitude = precip3[[1]][Aus,1])

set.seed(1998)
s2 = spsample(ausmap, n=200000,"random")

pred.grid.aus = data.frame(Longitude = coordinates(s2)[,1], Latitude = coordinates(s2)[,2])


idw1 = idw(formula = Value ~ 1, locations = ~Longitude + Latitude, data = SOI.df, newdata = pred.grid.aus, idp = 3)

idw2 = idw(formula = Value ~ 1, locations = ~Longitude + Latitude, data = IOD.df, newdata = pred.grid.aus, idp = 3)


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "SOI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Longitude, y = Latitude, colour = var1.pred), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "DMI Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")





SOI = read.table("https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/soi", header = TRUE)
IOD = read.table("https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/sstindex/sliding_30year_period/DMI/3rmean", header = TRUE)


SOI1 = as.vector(t(as.matrix(SOI)))
SOI1 = SOI1[SOI1 < 90]
SOI.ave = NULL
for(i in 2:(length(SOI1) - 1)){
  SOI.ave = c(SOI.ave, mean(SOI1[(i-1):(i + 1)]))
}

IOD1 = as.vector(t(as.matrix(IOD)))
SOI.ave = SOI.ave[-c(1:11)]


IOD1 = IOD1[IOD1 < 90]




best.SOI = NULL
for(lag in 1:6){
  opt.phi = NULL
  for(k in 1:25){
    lphi = k
    ar.mat = NULL
    for(i in (lphi + lag):length(SOI1)){
      ar.mat = rbind(ar.mat, c(SOI1[i], SOI1[(i - lag):(i - lag - lphi + 1)]))
    }
    set.seed(1998)
    cv.error = NULL
    for(i in 1:10){
      samp1 = sample(1:nrow(ar.mat), size = floor(nrow(ar.mat)/10))
      ar.mat1 = ar.mat[-samp1,]
      yc = ar.mat[samp1,1]
      Xc = cbind(1, ar.mat[samp1,-1])
      yphi = ar.mat1[,1]
      Xphi = cbind(1, ar.mat1[,-1])
      phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
      yc.hat = Xc %*% phi.hat
      cv.error = c(cv.error, sum((yc.hat - yc)^2)/nrow(ar.mat))
    }
    opt.phi = c(opt.phi, mean(cv.error))
  }
  best.SOI = rbind(best.SOI, c(min(opt.phi), which(opt.phi == min(opt.phi))))
}

best.IOD = NULL
for(lag in 1:6){
  opt.phi = NULL
  for(k in 1:20){
    lphi = k
    ar.mat = NULL
    for(i in (lphi + lag):length(IOD1)){
      ar.mat = rbind(ar.mat, c(IOD1[i], IOD1[(i - lag):(i - lag - lphi + 1)]))
    }
    set.seed(1998)
    cv.error = NULL
    for(i in 1:10){
      samp1 = sample(1:nrow(ar.mat), size = floor(nrow(ar.mat)/10))
      ar.mat1 = ar.mat[-samp1,]
      yc = ar.mat[samp1,1]
      Xc = cbind(1, ar.mat[samp1,-1])
      yphi = ar.mat1[,1]
      Xphi = cbind(1, ar.mat1[,-1])
      phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
      yc.hat = Xc %*% phi.hat
      cv.error = c(cv.error, sum((yc.hat - yc)^2)/(nrow(ar.mat) - lphi))
    }
    opt.phi = c(opt.phi, mean(cv.error))
  }
  best.IOD = rbind(best.IOD, c(min(opt.phi), which(opt.phi == min(opt.phi))))
}

SOI.list = list()
for(k in 1:nrow(best.SOI)){
  lag = k
  lphi = best.SOI[k,2]
  ar.mat = NULL
  for(i in (lphi + lag):length(SOI1)){
    ar.mat = rbind(ar.mat, c(SOI1[i], SOI1[(i - lag):(i - lag - lphi + 1)]))
  }
  yphi = ar.mat[,1]
  Xphi = cbind(1, ar.mat[,-1])
  phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
  y.hat = Xphi %*% phi.hat
  SOI.list[[k]] = y.hat
}

paste(row.names(SOI), colnames(SOI), sep = "-")

date1 = as.Date(paste(substr(SOI1$Date,1,4), substr(SOI1$Date, 5, 6), 15, sep = "-"))
pred.SOI1 = date1
pred.SOI2 = SOI1[,2]
pred.SOI3 = rep("Actual", nrow(SOI1))
for(i in c(1,3,6)){
  lag = best.SOI[i,2]
  pred.SOI1 = c(pred.SOI1, as.Date(date1[-(1:(length(date1) - length(SOI.list[[i]])))]))
  pred.SOI2 = c(pred.SOI2, SOI.list[[i]])
  pred.SOI3 = c(pred.SOI3, rep(paste(i, "lag", sep = "-"), nrow(SOI.list[[i]])))
}
pred.SOI = data.frame(Date = as.Date(pred.SOI1), Value = pred.SOI2, Lag = pred.SOI3)

pred.SOI.1 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[1]])))]),
                        Actual = SOI1[-(1:(length(date1) - length(SOI.list[[1]]))),2], Estimated = SOI.list[[1]])
pred.SOI.3 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[3]])))]),
                        Actual = SOI1[-(1:(length(date1) - length(SOI.list[[3]]))),2], Estimated = SOI.list[[3]])
pred.SOI.6 = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[6]])))]),
                        Actual = SOI1[-(1:(length(date1) - length(SOI.list[[6]]))),2], Estimated = SOI.list[[6]])
pred.SOI.sep = data.frame(Date = as.Date(date1[-(1:(length(date1) - length(SOI.list[[6]])))]),
                          Actual = SOI1[-(1:(length(date1) - length(SOI.list[[6]]))),2],
                          One = SOI.list[[1]][-(1:(length(SOI.list[[1]]) - length(SOI.list[[6]])))],
                          Three = SOI.list[[3]][-(1:(length(SOI.list[[3]]) - length(SOI.list[[6]])))],
                          Six = SOI.list[[6]])







acf(SOI1)
ggAcf(SOI1)


library(ggplot2)


bacf <- acf(IOD1, plot = FALSE, type = "covariance")
bacfdf1 <- with(bacf, data.frame(lag, acf))

q1 <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocorrelation Function",
       title = "DMI Autocorrelation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q1


bacf <- acf(SOI1, plot = FALSE, type = "covariance")
bacfdf2 <- with(bacf, data.frame(lag, acf))

q2 <- ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocorrelation Function",
       title = "SOI Autocorrelation") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q2




bacfdf <- as.data.frame(rbind(bacfdf2, bacfdf1))
bacfdf$Type = factor(c(rep("SOI", nrow(bacfdf2)), rep("DMI", nrow(bacfdf1))))



ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + facet_wrap(~Type, scales = "free") + theme_bw() +
  labs(x = "Lag (months)", y = "Autocovariance Function",
       title = "Autocovariance") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size = 10, colour = "black"),
        legend.position = "right")

SOI.ar = auto.arima(SOI.ave[1:472])
IOD.ar = auto.arima(IOD1)


forecast(SOI.ar)
forecast(IOD.ar)


datesNOAA1 = NULL
for(i in 1:nrow(datesNOAA)){
  m1 = datesNOAA[i,2]
  if(nchar(m1) == 1){
    m1 = paste0("0", m1)
  }
  datesNOAA1 = c(datesNOAA1, paste(datesNOAA[i,1], m1, "15", sep = "-"))
}

Osc.df = data.frame(Osc = c(SOI1, IOD1),
                    Dates = as.Date(c(datesNOAA1[1:length(SOI1)], datesNOAA1[1:length(IOD1)])),
                    Type = rep(c("SOI", "DMI"), each = length(SOI1)))


ggplot(data = Osc.df, aes(x = Dates, y = Osc, colour = Type)) + geom_line(linetype = c(rep(1,length(SOI1)),rep(2,length(SOI1))), size = 1) +
  labs(color = "Oscillation", x = "Date", y = "Value",
       title = "Oscillation Index") +  theme_bw() + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=20), axis.text=element_text(size=14), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right") +
  scale_color_manual(values = c("darkred", "steelblue"))



























