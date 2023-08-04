slid.vals = c(25, 50, 100, 250, 500, 750)
SLID.TS = NULL
k = 1
for(i in ncol(New.Gauge1):ncol(New.Gauge1)){
  for(j in slid.vals){
    s1 = slid(matrix(New.Gauge1[,i], ncol = 1), j)
    SLID.TS = rbind(SLID.TS, cbind(NOAA.aus[[1]]$Lon, NOAA.aus[[1]]$Lat, s1[,1], s1[,2], rep(j, nrow(New.Gauge1)), New.Gauge1[,i],
                                     NOAA.aus[[i + 255]]$Year, NOAA.aus[[i + 255]]$Month))
    
    k = k + 1
  }
  print(i)
}

PrecipMarch = data.frame(Precipitation = New.Gauge1[,264], Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)

idw1 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = PrecipMarch, newdata = pred.grid.aus, idp = 3)


l1 = 0.935

slid.matA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
for(i in 1:(ncol(slid.mat)/2)){
  A = slid.mat[,i]
  A1 = quantile(A, l1)
  slid.matA1[A > A1, i] = 1
}

slid.matA2 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
k = 1
for(i in (ncol(slid.mat)/2 + 1):ncol(slid.mat)){
  A = slid.mat[,i]
  A1 = quantile(A, l1)
  slid.matA2[A > A1, k] = 1
  k = k + 1
}

slid.diffA = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  slid.diffA[,i] = -(rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) - rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)]))
}

slid.diffA = slid.diffA[,c(10:12,1:9)]


slid.diffA1 = data.frame(Difference = as.vector(slid.diffA), Month = rep(1:12, each = nrow(New.Gauge1)),
                         Lon = rep(FusedLoc$Lon, 12), Lat = rep(FusedLoc$Lat, 12))


which(slid.diffA1$Difference > 0)




EXOFI = data.frame(Precip = as.vector(Precip.diff)[slid.diffA1$Difference > 0], sLID = as.vector(slid.diff.total)[slid.diffA1$Difference > 0],
                   sLIDA = as.vector(slid.diffA)[slid.diffA1$Difference > 0],
                   Month = rep(1:12, each = nrow(Precip.diff))[slid.diffA1$Difference > 0], Lon = rep(FusedLoc$Lon, 12)[slid.diffA1$Difference > 0], 
                   Lat = rep(FusedLoc$Lat, 12)[slid.diffA1$Difference > 0], id = rep(1:nrow(Precip.diff), 12)[slid.diffA1$Difference > 0])

EXOFI$Months = factor(ifelse(EXOFI$Month == 1, "January", 
                             ifelse(EXOFI$Month == 2, "February",
                                    ifelse(EXOFI$Month == 3, "March", 
                                           ifelse(EXOFI$Month == 4, "April", 
                                                  ifelse(EXOFI$Month == 5, "May", 
                                                         ifelse(EXOFI$Month == 6, "June", 
                                                                ifelse(EXOFI$Month == 7, "July", 
                                                                       ifelse(EXOFI$Month == 8, "August", 
                                                                              ifelse(EXOFI$Month == 9, "September", 
                                                                                     ifelse(EXOFI$Month == 10, "October", 
                                                                                            ifelse(EXOFI$Month == 11, "November", "December"))))))))))),
                      levels = c("January", "February", "March", "April",
                                 "May", "June", "July", "August",
                                 "September", "October", "November", "December"))



EXOFI1Pos = EXOFI

EUC1 = NULL
for(i in 1:12){
  EUC1 = c(EUC1, log(sqrt(scale(EXOFI1Pos$Precip[EXOFI1Pos$Month == i])^2 +
                        scale(EXOFI1Pos$sLID[EXOFI1Pos$Month == i])^2 +
                        scale(EXOFI1Pos$sLIDA)[EXOFI1Pos$Month == i]^2)))
}

EXOFI1Pos$Euclidean = EUC1

which(as.numeric(table(EXOFI1Pos$id)) == max(as.numeric(table(EXOFI1Pos$id))))





ggplot() + 
  geom_point(EXOFI1Pos, mapping = aes(x = Lon, y = Lat, colour = Euclidean), size = 1.25) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Positive", colour = "Index")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


EXOFI = data.frame(Precip = as.vector(Precip.diff)[slid.diffA1$Difference < 0], sLID = as.vector(slid.diff.total)[slid.diffA1$Difference < 0],
                   sLIDA = as.vector(slid.diffA)[slid.diffA1$Difference < 0],
                   Month = rep(1:12, each = nrow(Precip.diff))[slid.diffA1$Difference < 0], Lon = rep(FusedLoc$Lon, 12)[slid.diffA1$Difference < 0], 
                   Lat = rep(FusedLoc$Lat, 12)[slid.diffA1$Difference < 0], id = rep(1:nrow(Precip.diff), 12)[slid.diffA1$Difference < 0])

EXOFI$Months = factor(ifelse(EXOFI$Month == 1, "January", 
                             ifelse(EXOFI$Month == 2, "February",
                                    ifelse(EXOFI$Month == 3, "March", 
                                           ifelse(EXOFI$Month == 4, "April", 
                                                  ifelse(EXOFI$Month == 5, "May", 
                                                         ifelse(EXOFI$Month == 6, "June", 
                                                                ifelse(EXOFI$Month == 7, "July", 
                                                                       ifelse(EXOFI$Month == 8, "August", 
                                                                              ifelse(EXOFI$Month == 9, "September", 
                                                                                     ifelse(EXOFI$Month == 10, "October", 
                                                                                            ifelse(EXOFI$Month == 11, "November", "December"))))))))))),
                      levels = c("January", "February", "March", "April",
                                 "May", "June", "July", "August",
                                 "September", "October", "November", "December"))



EXOFI1Neg = EXOFI

EUC1 = NULL
for(i in 1:12){
  EUC1 = c(EUC1, log(sqrt(scale(EXOFI1Neg$Precip[EXOFI1Neg$Month == i])^2 +
                        scale(EXOFI1Neg$sLID[EXOFI1Neg$Month == i])^2 +
                        scale(EXOFI1Neg$sLIDA)[EXOFI1Neg$Month == i]^2)))
}

EXOFI1Neg$Euclidean = EUC1


ggplot() + 
  geom_point(EXOFI1, mapping = aes(x = Lon, y = Lat, colour = Euclidean), size = 1.25) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Negative", colour = "Index")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

EXOFI1PosFreq = NULL
for(i in 1:length(unique(EXOFI1Pos$id))){
  EXOFI1PosFreq = rbind(EXOFI1PosFreq, c((EXOFI1Pos[EXOFI1Pos$id == unique(EXOFI1Pos$id)[i], 5])[1], 
                                         (EXOFI1Pos[EXOFI1Pos$id == unique(EXOFI1Pos$id)[i], 6])[1],
                                         nrow(EXOFI1Pos[EXOFI1Pos$id == unique(EXOFI1Pos$id)[i], ])))
}


colnames(EXOFI1PosFreq) = c("Lon", "Lat", "Frequency")
EXOFI1PosFreq = as.data.frame(EXOFI1PosFreq)

ggplot() + 
  geom_point(EXOFI1PosFreq, mapping = aes(x = Lon, y = Lat, colour = Frequency), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Positive Frequency", colour = "Frequency")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

EXOFI1NegFreq = NULL
for(i in 1:length(unique(EXOFI1Neg$id))){
  EXOFI1NegFreq = rbind(EXOFI1NegFreq, c((EXOFI1Neg[EXOFI1Neg$id == unique(EXOFI1Neg$id)[i], 5])[1], 
                                         (EXOFI1Neg[EXOFI1Neg$id == unique(EXOFI1Neg$id)[i], 6])[1],
                                         nrow(EXOFI1Neg[EXOFI1Neg$id == unique(EXOFI1Neg$id)[i], ])))
}


colnames(EXOFI1NegFreq) = c("Lon", "Lat", "Frequency")
EXOFI1NegFreq = as.data.frame(EXOFI1NegFreq)

ggplot() + 
  geom_point(EXOFI1NegFreq, mapping = aes(x = Lon, y = Lat, colour = Frequency), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Negative Frequency", colour = "Frequency")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


Pos1 = NULL
for(i in 1:12){
  Temp = EXOFI1Pos[EXOFI1Pos$Month == i,]
  Pos1 = c(Pos1, Temp[order(Temp$Euclidean, decreasing = TRUE)[1], 7])
}

Neg1 = NULL
for(i in 1:12){
  Temp = EXOFI1Neg[EXOFI1Neg$Month == i,]
  Neg1 = c(Neg1, Temp[order(Temp$Euclidean, decreasing = TRUE)[1], 7])
}

S1 = slid.mat[Pos1,]
P1 = New.Gauge1[Pos1,]

S2 = slid.mat[Neg1,]
P2 = New.Gauge1[Neg1,]

A1 = auto.arima(S1[2,], ic = "bic")
A2 = auto.arima(as.numeric(P1[2,]))


DF1 = data.frame(Precipitation = as.numeric(P1[i,]), sLID = S1[i,], Month = factor(c(4:12, 1:3)))

mod1 = lm(Precipitation ~ sLID + Month, data = DF1)


MarchSLIDfitPos = NULL
for(i in 4:4){
  TempPos = EXOFI1Pos[EXOFI1Pos$Month == i,]
  for(j in 1:nrow(Temp)){
    id1 = TempPos$id[j]
    S1 = slid.mat[id1,]
    Ar1 = auto.arima(S1)
    MarchSLIDfitPos = rbind(MarchSLIDfitPos, Ar1$fitted)
    print(j)
  }
}


MarchSLIDfitNeg = NULL
for(i in 4:4){
  TempNeg = EXOFI1Neg[EXOFI1Neg$Month == i,]
  for(j in 1:nrow(Temp)){
    id1 = TempNeg$id[j]
    S1 = slid.mat[id1,]
    Ar1 = auto.arima(S1)
    MarchSLIDfitNeg = rbind(MarchSLIDfitNeg, Ar1$fitted)
    print(j)
  }
}

DF1 = data.frame(sLID = c(MarchSLIDfitPos[,264], MarchSLIDfitNeg[,264]), Lon = c(TempPos$Lon, TempNeg$Lon), Lat =c(TempPos$Lat, TempNeg$Lat))



ggplot() + 
  geom_point(DF1, mapping = aes(x = Lon, y = Lat, colour = sLID), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "March sLID Forecast", colour = "Value")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

month.mean = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  month.mean[,i] = rowMeans(slid.mat[,seq(i, ncol(slid.mat), 12)])
}

slid.upper = NULL
slid.lower = NULL
slid.forecast = NULL
for(i in 1:nrow(slid.mat)){
  ar1 = auto.arima(sqrt((slid.mat[i,])[1:263]))
  f1 = forecast(ar1, level = 0.95)
  slid.forecast = rbind(slid.forecast, f1$mean[1:3])
  slid.upper = rbind(slid.upper, f1$upper[1:3])
  slid.lower = rbind(slid.lower, f1$lower[1:3])
  print(i)
}




DF1 = data.frame(sLID = c(as.vector(slid.forecast),
                          as.vector(slid.upper),
                          as.vector(slid.lower)), Lon = rep(FusedLoc$Lon, 3), Lat = rep(FusedLoc$Lat, 3),
                 Months = factor(rep(rep(c("March", "April", "May"), each = nrow(slid.forecast), 3)), levels = c("March", "April", "May")), 
                 Class = rep(c("Mean", "Upper", "Lower"), each = nrow(slid.forecast) * 3), Month = rep(rep(1:3, each = nrow(slid.forecast)), 3))

ggplot() + 
  geom_point(DF1, mapping = aes(x = Lon, y = Lat, colour = sLID^2), size = 1) + facet_grid(Class ~ Months) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "March sLID Forecast", colour = "Value")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)




slid.forecast1 = NULL
for(i in 1:3){
  Temp1 = DF1[DF1$Month == i & DF1$Class == "Mean",]
  Temp2 = DF1[DF1$Month == i & DF1$Class == "Upper",]
  Temp3 = DF1[DF1$Month == i & DF1$Class == "Lower",]
  idw1 = gstat::idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = Temp1, newdata = pred.grid.aus, idp = 3)
  idw2 = gstat::idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = Temp2, newdata = pred.grid.aus, idp = 3)
  idw3 = gstat::idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = Temp3, newdata = pred.grid.aus, idp = 3)
  r1 = rbind(idw1, idw2, idw3)
  r1 = cbind(r1, rep(c("Mean", "Upper", "Lower"), each = nrow(idw1)), rep(i, each = nrow(idw1) * 3))
  slid.forecast1 = rbind(slid.forecast1, r1)
}

colnames(slid.forecast1) = c("Lon", "Lat", "sLID", "Var", "Class", "Month")
slid.forecast1$sLID = slid.forecast1$sLID^2

slid.actual1 = NULL
for(i in 262:264){
  Temp = data.frame(sLID = slid.mat[,i], Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)
  idw1 = gstat::idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  slid.actual1 = rbind(slid.actual1, idw1)
}

slid.actual1 = cbind(slid.actual1, rep("Actual", nrow(idw1) * 3), rep(1:3, each = nrow(idw1)))

colnames(slid.actual1) = c("Lon", "Lat", "sLID", "Var", "Class", "Month")

Fig9.df = rbind(slid.forecast1, slid.actual1)

Fig9.df$Months = factor(ifelse(Fig9.df$Month == 1, "January", 
                             ifelse(Fig9.df$Month == 2, "February",
                                    ifelse(Fig9.df$Month == 3, "March", 
                                           ifelse(Fig9.df$Month == 4, "April", 
                                                  ifelse(Fig9.df$Month == 5, "May", 
                                                         ifelse(Fig9.df$Month == 6, "June", 
                                                                ifelse(Fig9.df$Month == 7, "July", 
                                                                       ifelse(Fig9.df$Month == 8, "August", 
                                                                              ifelse(Fig9.df$Month == 9, "September", 
                                                                                     ifelse(Fig9.df$Month == 10, "October", 
                                                                                            ifelse(Fig9.df$Month == 11, "November", "December"))))))))))),
                      levels = c("January", "February", "March", "April",
                                 "May", "June", "July", "August",
                                 "September", "October", "November", "December"))

Fig9.df$Class = factor(Fig9.df$Class, levels = c("Actual", "Mean", "Lower", "Upper"))

ggplot() + 
  geom_point(Fig9.df, mapping = aes(x = Lon, y = Lat, colour = sLID), size = 0.1) + facet_grid(Class ~ Months) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Value")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

month.mean.p = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  month.mean.p[,i] = rowMeans(New.Gauge1[,seq(i, ncol(slid.mat), 12)])
}




precip.forecast = NULL
precip.upper = NULL
precip.lower = NULL
for(i in 1:nrow(New.Gauge1)){
  mm1 = rep(month.mean.p[i,], 22)
  df1 = data.frame(Precipitation = as.numeric(New.Gauge1[i,]), sLID = slid.mat[i,], mean = mm1)
  mod1 = lm(sqrt(Precipitation) ~ bs(sqrt(sLID)) + sqrt(mean), data = df1)
  Newdata1 = data.frame(sLID = slid.forecast[i,]^2, mean = month.mean.p[i,c(12,1,2)])
  Newdata2 = data.frame(sLID = slid.upper[i,]^2, mean = month.mean.p[i,c(12,1,2)])
  Newdata3 = data.frame(sLID = slid.lower[i,]^2, mean = month.mean.p[i,c(12,1,2)])
  precip.forecast = c(precip.forecast, predict(mod1, newdata = Newdata1)^2)
  precip.upper = c(precip.upper, predict(mod1, newdata = Newdata2)^2)
  precip.lower = c(precip.lower, predict(mod1, newdata = Newdata3)^2)
}


DF2 = data.frame(Precipitation = c(precip.forecast, precip.upper, precip.lower),
                 Month = rep(rep(3:5, nrow(New.Gauge1)), 3),
                 Class = rep(c("Mean", "Upper", "Lower"), each = nrow(New.Gauge1) * 3),
                 Lon = rep(rep(FusedLoc$Lon, each = 3), 3),
                 Lat = rep(rep(FusedLoc$Lat, each = 3), 3))

DF3 = DF2[DF2$Month == 3,]


ggplot() + 
  geom_point(DF3, mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 1) + facet_wrap( ~ Class, nrow = 3) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Value")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


Precip.forecast1 = NULL
for(i in 3:5){
  Temp1 = DF2[DF2$Month == i & DF2$Class == "Mean",]
  Temp2 = DF2[DF2$Month == i & DF2$Class == "Upper",]
  Temp3 = DF2[DF2$Month == i & DF2$Class == "Lower",]
  idw1 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = Temp1, newdata = pred.grid.aus, idp = 3)
  idw2 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = Temp2, newdata = pred.grid.aus, idp = 3)
  idw3 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = Temp3, newdata = pred.grid.aus, idp = 3)
  r2 = rbind(idw1, idw2, idw3)
  r2 = cbind(r2, rep(c("Mean", "Upper", "Lower"), each = nrow(idw1)), rep(i, each = nrow(idw1) * 3))
  Precip.forecast1 = rbind(Precip.forecast1, r2)
}

colnames(Precip.forecast1) = c("Lon", "Lat", "Precipitation", "Var", "Class", "Month")

Precip.actual1 = NULL
for(i in 262:264){
  Temp = data.frame(Precipitation = New.Gauge1[,i], Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)
  idw1 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  Precip.actual1 = rbind(Precip.actual1, idw1)
}

Precip.actual1 = cbind(Precip.actual1, rep("Actual", nrow(idw1) * 3), rep(1:3, each = nrow(idw1)))

colnames(Precip.actual1) = c("Lon", "Lat", "Precipitation", "Var", "Class", "Month")

Fig10.df = Precip.forecast1

Fig10.df$Months = factor(ifelse(Fig10.df$Month == 1, "January", 
                               ifelse(Fig10.df$Month == 2, "February",
                                      ifelse(Fig10.df$Month == 3, "March", 
                                             ifelse(Fig10.df$Month == 4, "April", 
                                                    ifelse(Fig10.df$Month == 5, "May", 
                                                           ifelse(Fig10.df$Month == 6, "June", 
                                                                  ifelse(Fig10.df$Month == 7, "July", 
                                                                         ifelse(Fig10.df$Month == 8, "August", 
                                                                                ifelse(Fig10.df$Month == 9, "September", 
                                                                                       ifelse(Fig10.df$Month == 10, "October", 
                                                                                              ifelse(Fig10.df$Month == 11, "November", "December"))))))))))),
                        levels = c("January", "February", "March", "April",
                                   "May", "June", "July", "August",
                                   "September", "October", "November", "December"))

Fig10.df$Class = factor(Fig10.df$Class, levels = c("Lower", "Mean", "Upper"))

ggplot() + 
  geom_point(Fig10.df, mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.1) + facet_grid(Class ~ Months) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)














          
          
          
          
          
          
          
          
          
          
          
