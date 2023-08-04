#########Xinmo


XinmoHourly = read.csv("https://raw.githubusercontent.com/hinestein/XinmoRain/main/out20220605231923_st2003010118_ed2022060518_clat32.075_clon103.66667.csv")

XinmoH1 = matrix(0, nrow = nrow(XinmoHourly), ncol = 6)

for(i in 1:nrow(XinmoHourly)){
  Y1 = substr(XinmoHourly[i,1], 1, 4)
  M1 = substr(XinmoHourly[i,1], 6, 7)
  D1 = substr(XinmoHourly[i,1], 9, 10)
  H1 = substr(XinmoHourly[i,1], 12, 13)
  XinmoH1[i,] = c(Y1, M1, D1, H1, XinmoHourly[i,2], XinmoHourly[i,3])
  if(i %% 1000 == 0){
    print(i)
  }
}

XinmoH = data.frame(Year = as.numeric(XinmoH1[,1]), Month = as.numeric(XinmoH1[,2]),
                    Day = as.numeric(XinmoH1[,3]), Hour = as.numeric(XinmoH1[,4]),
                    Rain = as.numeric(XinmoH1[,5]), Satellite_flag = as.numeric(XinmoH1[,6]))

XinmoH$Rain[XinmoH$Rain < 0] = 0


XinmoCumSum = matrix(0, nrow = nrow(XinmoH[(which(XinmoH$Year == 2004)[1]):(which(XinmoH$Year == 2021)[length(which(XinmoH$Year == 2021))]),]), ncol = ncol(XinmoH) - 1)
rowc = 0
for(i in 2004:2021){
  Temp = XinmoH[XinmoH$Year == i,]
  CS1 = cumsum(Temp$Rain)
  XinmoCumSum[(rowc + 1):(rowc + length(CS1)), ] = cbind(Temp$Year, Temp$Month, Temp$Day, Temp$Hour, CS1)
  rowc = rowc + length(CS1)
}

colnames(XinmoCumSum) = c("Year", "Month", "Day", "Hour", "CumulativeRain")
XinmoCumSum = as.data.frame(XinmoCumSum)

XinmoCumSum$Date = as.POSIXct(paste0(XinmoCumSum$Month, "/", XinmoCumSum$Day, " ", XinmoCumSum$Hour, ":", "00", ":", "00"), format="%m/%d %H:%M:%S") 


ggplot(XinmoCumSum, aes(x = Date, y = CumulativeRain)) + 
  geom_line(aes(color = (Year)))


XinmoJune = XinmoH[XinmoH$Month == 6, ]

XinmoJCumSum = matrix(0, nrow = nrow(XinmoJune[XinmoJune$Year <= 2016,]), ncol = ncol(XinmoH) - 1)
rowc = 0
for(i in 2003:2016){
  Temp = XinmoJune[XinmoJune$Year == i,]
  CS1 = cumsum(Temp$Rain)
  XinmoJCumSum[(rowc + 1):(rowc + length(CS1)), ] = cbind(Temp$Year, Temp$Month, Temp$Day, Temp$Hour, CS1)
  rowc = rowc + length(CS1)
}

colnames(XinmoJCumSum) = c("Year", "Month", "Day", "Hour", "CumulativeRain")
XinmoJCumSum = as.data.frame(XinmoJCumSum)

XinmoJCumSum$Date = as.POSIXct(paste0(XinmoJCumSum$Month, "/", XinmoJCumSum$Day, " ", XinmoJCumSum$Hour, ":", "00", ":", "00"), format="%m/%d %H:%M:%S")

ggplot(XinmoJCumSum, aes(x = Date, y = CumulativeRain)) + 
  geom_line(aes(color = (Year)))


XinmoJCumSumq = matrix(0, nrow = length(unique(XinmoJCumSum$Date)), ncol = 3)
for(i in 1:length(unique(XinmoJCumSum$Date))){
  Temp = XinmoJCumSum[XinmoJCumSum$Date == unique(XinmoJCumSum$Date)[i],]
  XinmoJCumSumq[i,] = c(quantile(Temp$CumulativeRain, c(0.1, 0.9)) , mean(Temp$CumulativeRain))
}


colnames(XinmoJCumSumq) = c("Lower", "Upper", "Mean")
XinmoJCumSumq = as.data.frame(XinmoJCumSumq)

XinmoJCumSumq$Current = cumsum(XinmoJune$Rain[XinmoJune$Year == 2017])
XinmoJCumSumq$Date = XinmoJCumSum$Date[XinmoJCumSum$Year == 2016]

T1 = data.frame(x = as.POSIXct("2022/07/04 00:00:00", format="%Y/%m/%d %H:%M:%S"), y = max(XinmoJCumSumq$Current) + 5)
T2 = data.frame(x = as.POSIXct("2022/07/05 00:00:00", format="%Y/%m/%d %H:%M:%S"), y = max(XinmoJCumSumq$Upper) - 1)
T3 = data.frame(x = as.POSIXct("2022/07/05 12:00:00", format="%Y/%m/%d %H:%M:%S"), y = max(XinmoJCumSumq$Upper) - 9)
T4 = data.frame(x = as.POSIXct("2022/07/04 12:00:00", format="%Y/%m/%d %H:%M:%S"), y = max(XinmoJCumSumq$Mean))
T5 = data.frame(x = as.POSIXct("2022/07/05 00:00:00", format="%Y/%m/%d %H:%M:%S"), y = max(XinmoJCumSumq$Lower))
T6 = data.frame(x = as.POSIXct("2022/07/05 12:00:00", format="%Y/%m/%d %H:%M:%S"), y = max(XinmoJCumSumq$Lower) - 9)
T7 = data.frame(x = as.POSIXct("2022/06/28 05:38:00", format="%Y/%m/%d %H:%M:%S"), y = 25)

ggplot(XinmoJCumSumq, aes(Date)) + geom_ribbon(aes(ymin = Mean, ymax = Upper), fill = "#ff353e") + geom_ribbon(aes(ymin = Lower, ymax = Mean), fill = "#00ff8a") + 
  geom_line(aes(y = Current)) + geom_line(aes(y = Lower)) + geom_line(aes(y = Upper)) + geom_line(aes(y = Mean)) + theme_bw() +
  labs(y = "Cumulative Precipitation (mm)", x = "Date", title = "Xinmo Cumulative Precipitation") +
  scale_x_datetime(limits = as.POSIXct(c("2022/06/01 00:00:00", "2022/07/8 00:00:00"), format="%Y/%m/%d %H:%M:%S")) +
  geom_text(data = T1, aes(x = x, y = y), label = "2017", size = 5) +
  geom_text(data = T2, aes(x = x, y = y), label = "90th", size = 5) +
  geom_text(data = T3, aes(x = x, y = y), label = "Percentile", size = 5) +
  geom_text(data = T4, aes(x = x, y = y), label = "Mean", size = 5) +
  geom_text(data = T5, aes(x = x, y = y), label = "10th", size = 5) +
  geom_text(data = T6, aes(x = x, y = y), label = "Percentile", size = 5) +
  geom_vline(xintercept = as.POSIXct("2022/06/24 05:38:00", format="%Y/%m/%d %H:%M:%S"))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom", strip.text = element_text(size = 18),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm")) +
  geom_text(data = T7, aes(x = x, y = y), label = "Time of\nCollapse", size = 5)




setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")
R2.displacement = read.csv("R2Displacement", header = FALSE)
R2.locations = read.csv("R2Locations", header = FALSE)
R2.dates = read.csv("num2date.csv", header = FALSE)[,2]

Temp = NULL
for(i in 1:length(R2.dates)){
  Temp = c(Temp, paste0(substr(R2.dates[i],1,4), "-", substr(R2.dates[i],5,6), "-", substr(R2.dates[i],7,8)))
}

R2.dates = Temp


slid = function(x, s){
  out = rep(0, nrow(x))
  d1 = dist(x)
  d2 = as.matrix(d1)
  for(i in 1:nrow(x)){
    nh = order(d2[i,])[2:(s + 1)]
    d3 = d2[i,nh]
    d3[d3 == 0] = 1e-16
    out[i] = max(- ( ( 1 / s ) * sum( log( d3/max( d3 ) ) ) ) ^ ( -1 ), 0)
  }
  out = cbind(out, out > mean(out))
  return(out)
}


s.vals = c(25, 100, 250, 500, 1000, 2000)
R2.slid = matrix(0, nrow = nrow(R2.displacement) * length(s.vals) * ncol(R2.displacement), ncol = 7)
k = 1
for(i in 1:ncol(R2.displacement)){
  for(j in s.vals){
    s1 = slid(matrix(R2.displacement[,i], ncol = 1), s = j)
    R2.slid[((k - 1) * nrow(R2.locations) + 1):(k * nrow(R2.locations)),] = cbind(R2.locations[,1], R2.locations[,2], s1[,1], s1[,2], rep(j, nrow(R2.locations)), R2.displacement[,i], rep(R2.dates[i], nrow(R2.displacement)))
    k = k + 1
  }
  print(i)
}


colnames(R2.slid) = c("xloc","yloc", "sLID", "Greater", "s", "Displacement", "Time")

R2.slid = as.data.frame(R2.slid)

R2.slid.df = data.frame(xloc = as.numeric(R2.slid$xloc), yloc = as.numeric(R2.slid$yloc), sLID = as.numeric(R2.slid$sLID), Greater = as.factor(R2.slid$Greater),
                        s = factor(paste0("s = ", R2.slid$s), levels = paste0("s = ", s.vals)), Displacement = as.numeric(R2.slid$Displacement), Time = R2.slid$Time)

plot(R2.locations)

R2.slid.df2 = R2.slid.df

R2.slid.df2$sLID[R2.slid.df2$sLID > 4] = 4

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/R2vid")

v1 = ggplot(R2.slid.df2, aes(x = xloc, y = yloc, colour = sLID)) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "sLID") +
  transition_states(Time) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))

animate(v1, nframes = 500)

v1 = ggplot(R2.slid.df, aes(x = xloc, y = yloc, colour = Greater)) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "sLID") +
  transition_states(Time) +
  ease_aes('linear') + scale_colour_manual(
    values = c("#170de0", "#e1070a"),
    aesthetics = c("colour", "fill"),
    labels = c("0" = "Low", "1" = "High")
  )  +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))+
  guides(colour = guide_legend(override.aes = list(size=2)))

animate(v1)

R2.slid.df1 = R2.slid.df[R2.slid.df$s == 25,]

v1 = ggplot(R2.slid.df1, aes(x = xloc, y = yloc, colour = Displacement)) +
  geom_point(size = 0.5) +
  theme_bw() +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "Displacement") +
  transition_states(Time) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))
animate(v1, nframes = 500)


v1 =  ggplot(R2.slid.df2, aes(x = sLID)) +
  geom_histogram(binwidth=0.4, colour = "#0078b8", fill = "#0078b8") +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'sLID', y = 'Count') +
  transition_states(Time) +
  ease_aes('linear') + scale_colour_manual(
    values = c("#82db5b", "#f03c3b"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "TRUE", "0" = "FALSE")
  ) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))

animate(v1, nframes = 500)


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")


R3.slid.df2 = R3.slid.df

R3.slid.df2$sLID[R3.slid.df2$sLID > 4] = 4


ggplot(R3.slid.df2, aes(x=sLID)) + 
  geom_histogram(binwidth=1)




setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")
R3.displacement = read.csv("R3Displacement", header = FALSE)
R3.locations = read.csv("R3Locations", header = FALSE)
R3.dates = read.csv("num2date.csv", header = FALSE)[,2]

Temp = NULL
for(i in 1:length(R3.dates)){
  Temp = c(Temp, paste0(substr(R3.dates[i],1,4), "-", substr(R3.dates[i],5,6), "-", substr(R3.dates[i],7,8)))
}

R3.dates = Temp


slid = function(x, s){
  out = rep(0, nrow(x))
  d1 = dist(x)
  d2 = as.matrix(d1)
  for(i in 1:nrow(x)){
    nh = order(d2[i,])[2:(s + 1)]
    d3 = d2[i,nh]
    d3[d3 == 0] = 1e-16
    out[i] = max(- ( ( 1 / s ) * sum( log( d3/max( d3 ) ) ) ) ^ ( -1 ), 0)
  }
  out = cbind(out, out > mean(out))
  return(out)
}


s.vals = c(25, 50, 100, 200, 300, 450)
R3.slid = matrix(0, nrow = nrow(R3.displacement) * length(s.vals) * ncol(R3.displacement), ncol = 7)
k = 1
for(i in 1:ncol(R3.displacement)){
  for(j in s.vals){
    s1 = slid(matrix(R3.displacement[,i], ncol = 1), s = j)
    R3.slid[((k - 1) * nrow(R3.locations) + 1):(k * nrow(R3.locations)),] = cbind(R3.locations[,1], R3.locations[,2], s1[,1], s1[,2], rep(j, nrow(R3.locations)), R3.displacement[,i], rep(R3.dates[i], nrow(R3.displacement)))
    k = k + 1
  }
  print(i)
}


colnames(R3.slid) = c("xloc","yloc", "sLID", "Greater", "s", "Displacement", "Time")

R3.slid = as.data.frame(R3.slid)

R3.slid.df = data.frame(xloc = as.numeric(R3.slid$xloc), yloc = as.numeric(R3.slid$yloc), sLID = as.numeric(R3.slid$sLID), Greater = as.factor(R3.slid$Greater),
                        s = factor(paste0("s = ", R3.slid$s), levels = paste0("s = ", s.vals)), Displacement = as.numeric(R3.slid$Displacement), Time = R3.slid$Time)

plot(R3.locations)

R3.slid.df2 = R3.slid.df

R3.slid.df2$sLID[R3.slid.df2$sLID > 4] = 4

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/R3Vid")

v1 = ggplot(R3.slid.df2, aes(x = xloc, y = yloc, colour = sLID)) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "sLID") +
  transition_states(Time) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))

animate(v1, nframes = 500)

v1 = ggplot(R3.slid.df2, aes(x = xloc, y = yloc, colour = Class)) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "sLID") +
  transition_states(Time) +
  ease_aes('linear') + scale_colour_manual(
    values = c("#e1070a", "#170de0"),
    aesthetics = c("colour", "fill")
  ) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))+
  guides(colour = guide_legend(override.aes = list(size=2)))

animate(v1, nframes = 200)xx

R3.slid.df1 = R3.slid.df[R3.slid.df$s == 25,]

v1 = ggplot(R3.slid.df1, aes(x = xloc, y = yloc, colour = Displacement)) +
  geom_point(size = 0.5) +
  theme_bw() +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "Displacement") +
  transition_states(Time) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))
animate(v1, nframes = 500)

R3.slid.df2$Class = ifelse(R3.slid.df2$Greater == 1, "High", "Low")


v1 =  ggplot(R3.slid.df2, aes(x = sLID, fill = Class)) +
  geom_histogram(binwidth=0.4) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'sLID', y = 'Count', fill = "sLID") +
  transition_states(Time) + scale_colour_manual(
    values = c("#e1070a", "#170de0"),
    aesthetics = c("colour", "fill")
  ) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))

animate(v1, nframes = 200)


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")


ggplot(R3.slid.df2, aes(x = sLID, fill = Class)) +
  geom_histogram(binwidth=0.4) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date", x = 'sLID', y = 'Count', fill = "sLID") +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))+ scale_colour_manual(
          values = c("#e1070a", "#170de0"),
          aesthetics = c("colour", "fill")
        )



ggplot(R3.slid.df2, aes(x = xloc, y = yloc, colour = Class)) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Date: {closest_state}", x = 'x', y = 'y', colour = "sLID") + scale_colour_manual(
    values = c("#e1070a", "#170de0"),
    aesthetics = c("colour", "fill")
  )  +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12),axis.text.x = element_text(angle = 45, hjust = 1), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12))+
  guides(colour = guide_legend(override.aes = list(size=2)))



library(dplyr)
library(ggplot2)
library(magick)
library(gganimate)



A.1 = read.csv("https://raw.githubusercontent.com/hinestein/FDAworl/main/Gpathdf")

ggplot(A.1, aes(x = X, y = Y)) + geom_path() + facet_wrap(~J) + 
  transition_states(Date)

A.1 %>%
ggplot(aes(
  x = X, 
  y = Y)) + 
  geom_path() + 
  scale_color_gradient2(
    midpoint = 6, 
    low = "orange", 
    mid = "purple",
    high = "black") +
  transition_states(
    states = Date, 
    transition_length = 2, 
    state_length = 1)



R1.data = read.csv("https://raw.githubusercontent.com/hinestein/XinmoWork/main/R1Data.csv")

class(R3.data)


which(R1.data$CODE == R3.data$CODE[2])


subset(R1.data, CODE == R3.data$CODE)



R1Loc = read.csv("https://raw.githubusercontent.com/hinestein/XinmoWork/main/R1Locations.txt")
R3Loc = read.csv("https://raw.githubusercontent.com/hinestein/XinmoWork/main/R3Locations", header = FALSE)

R1.data$xloc = R1Loc$R1Locations1
R1.data$yloc = R1Loc$R1Locations2

R3.data = data.frame(xloc = R3Loc$V1, yloc = R3Loc$V2)

R2 = R1.data %>%
  filter(xloc %in% R3.data$xloc & yloc %in% R3.data$yloc)

PCord = R2[,1]
PCord1 = NULL
for(i in 1:length(PCord)){
  s1 = substr(PCord[i], 8, nchar(PCord[i]) - 1)
  g1 = gregexpr(" ", s1)[[1]][1]
  xloc = as.numeric(substr(s1, 1, g1 - 1))
  yloc = as.numeric(substr(s1, g1 + 1, nchar(s1)))
  PCord1 = rbind(PCord1, c(xloc, yloc))
}


Cum.mat = list()
for(i in 1:nrow(PCord1)){
  a1 = getURL(paste0("https://sharaku.eorc.jaxa.jp/cgi-bin/trmm/GSMaP/tilemap/show_graph.cgi?flag=1&st=2014100901&ed=2017071923&lat0=",PCord1[i,2], "&lon0=", PCord1[i,1], "&lang=en"))
  a2 = substr(a1, gregexpr("Acc", a1)[[1]][1], nchar(a1))
  a3 = substr(a2, gregexpr("data:", a2)[[1]][1], nchar(a2))
  a4 = gregexpr("}", a3)[[1]][1:length(gregexpr("}", a3)[[1]])]
  a5 = gregexpr("y:", a3)[[1]][1:length(gregexpr("y:", a3)[[1]])]
  a6 = gregexpr("x:", a3)[[1]][1:length(gregexpr("x:", a3)[[1]])]
  cumulativePrecip = rep(0, nrow = length(a5))
  Time = rep(0, nrow = length(a5))
  for(j in 1:length(a5)){
    cumulativePrecip[j] = as.numeric(substr(a3, a5[j] + 2, a4[j] - 2))
    Time[j] = substr(a3, a6[j] + 2, a5[j] - 3)
  }
  Cum.mat[[i]] = data.frame(Precip = cumulativePrecip, Time = Time)
  print(i)
}

Cum.mat[[1]]

n1 = NULL
for(i in 1:length(Cum.mat)){
  n1 = c(n1, nrow(Cum.mat[[i]]))
}


plot(Cum.mat[[1]][,1], Cum.mat[[609]][,1])

i = 1


paste0("https://sharaku.eorc.jaxa.jp/cgi-bin/trmm/GSMaP/tilemap/show_graph.cgi?flag=1&st=2014100901&ed=2017071923&lat0=",PCord1[i,2], "&lon0=", PCord1[i,1], "&lang=en")







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

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXA.daily1")

JAXA.daily = list()
pb <- txtProgressBar(min = 175, max = 207, style = 3)
start1 = 1
for(i in 175:207){
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



for(i in (start1):length(JAXA.daily)){
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

A1 = JAXA.daily[[length(JAXA.daily)]][,2:1]
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

Aus.JAXA = which(a3$ISO3 == "AUS")

JAXA.daily.A = list()
for(i in start1:length(JAXA.daily)){
  JAXA.daily.A[[i]] = JAXA.daily[[i]][Australasia,]
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
  JAXA.daily1[[i]] = JAXA.daily.A[[i]][samp1,]
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
  colnames(JAXA.daily1[[i]]) = c("Latitude", "Longitude", paste0("Day", ifelse(nchar(3:ncol(JAXA.daily1[[i]]) - 2) == 1, paste0("0", 3:ncol(JAXA.daily1[[i]]) - 2), 3:ncol(JAXA.daily1[[i]]) - 2)))
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")


DF1 = data.frame(Lon = JAXA.daily[[175]][,2], Lat = JAXA.daily[[175]][,1])
w1 = which(DF1$Lon < 105.776747 & DF1$Lon > 101.528353 & DF1$Lat < 34.124061 & DF1$Lat > 29.944708)



PCord = R1.data[,1]
PCord1 = NULL
for(i in 1:length(PCord)){
  s1 = substr(PCord[i], 8, nchar(PCord[i]) - 1)
  g1 = gregexpr(" ", s1)[[1]][1]
  xloc = as.numeric(substr(s1, 1, g1 - 1))
  yloc = as.numeric(substr(s1, g1 + 1, nchar(s1)))
  PCord1 = rbind(PCord1, c(xloc, yloc))
}


PCord1 = data.frame(PCord1)
colnames(PCord1) = c("Lon", "Lat")



Xinmo.precip = 0
for(i in 175:207){
  for(j in 3:ncol(JAXA.daily[[i]])){
    Temp.df = data.frame(Lon = JAXA.daily[[i]][w1,2], Lat = JAXA.daily[[i]][w1,1], Rain = JAXA.daily[[i]][w1, j])
    idw1 = gstat::idw(formula = Rain ~ 1, locations = ~Lon + Lat, data = Temp.df, newdata = PCord1, idp = 3)
    Xinmo.precip = Xinmo.precip + idw1$var1.pred
  }
}



Xinmo.df = data.frame(Precipitation = Xinmo.precip, Lon = PCord1$Lon, Lat = PCord1$Lat)



ggplot(Xinmo.df, aes(x = Lon, y = Lat, colour = Precipitation)) + geom_point(size = 0.5) +
  labs(x = "Longitude", y = "Latitude", colour = "Cumulative\nPrecipitation\n(mm)", title = "Xinmo Cumulative Precipitation\n(09/14/2014 - 19/06/2017)") + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 18),
        plot.margin = unit(c(0.5,0.2,0.2,0.2), "cm")) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))







