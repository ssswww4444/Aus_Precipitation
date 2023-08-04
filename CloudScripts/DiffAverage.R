#Changes in Average due to climate change

Jan20ave = apply(Aus.all[,m01[1:20]], 1, mean)
Jan40ave = apply(Aus.all[,m01], 1, mean)
plot(Jan20ave - Jan40ave, cex = 0.2, pch  = 19)

Aus20.ave = list()
Aus40.ave = list()
for(i in 1:length(M)){
  Aus20.ave[[i]] = apply(Aus.all[,M[[i]][1:20]], 1, mean)
  Aus40.ave[[i]] = apply(Aus.all[,M[[i]]], 1, mean)
}

k = 6
plot(Aus20.ave[[k]] - Aus40.ave[[k]], cex = 0.2, pch  = 19)

Aves = data.frame(Lon = precip3[[1]]$Lon[Aus1], Lat = precip3[[1]]$Lat[Aus1], Kclass = precip3[[1]]$Kclass[Aus1],
                 Jan20 = Aus20.ave[[1]], Feb20 = Aus20.ave[[2]], Mar20 = Aus20.ave[[3]], Apr20 = Aus20.ave[[4]],
                 May20 = Aus20.ave[[5]], Jun20 =Aus20.ave[[6]], Jul20 = Aus20.ave[[7]], Aug20 = Aus20.ave[[8]],
                 Sept20 = Aus20.ave[[9]], Oct20 = Aus20.ave[[10]], Nov20 = Aus20.ave[[11]], Dec20 = Aus20.ave[[12]],
                 Jan40 = Aus40.ave[[1]], Feb40 = Aus40.ave[[2]], Mar40 = Aus40.ave[[3]], Apr40 = Aus40.ave[[4]],
                 May40 = Aus40.ave[[5]], Jun40 =Aus40.ave[[6]], Jul40 = Aus40.ave[[7]], Aug40 = Aus40.ave[[8]],
                 Sept40 = Aus40.ave[[9]], Oct40 = Aus40.ave[[10]], Nov40 = Aus40.ave[[11]], Dec40 = Aus40.ave[[12]])

pred.grid = data.frame(Lon = Austra1$Lon[Aus1], Lat = Austra1$Lat[Aus1])
idwJan20 = idw(formula = Jan20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwFeb20 = idw(formula = Feb20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwMar20 = idw(formula = Mar20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwApr20 = idw(formula = Apr20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwMay20 = idw(formula = May20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwJun20 = idw(formula = Jun20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwJul20 = idw(formula = Jul20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwAug20 = idw(formula = Aug20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwSept20 = idw(formula = Sept20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwOct20 = idw(formula = Oct20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwNov20 = idw(formula = Nov20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwDec20 = idw(formula = Dec20 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)

idwJan40 = idw(formula = Jan40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwFeb40 = idw(formula = Feb40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwMar40 = idw(formula = Mar40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwApr40 = idw(formula = Apr40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwMay40 = idw(formula = May40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwJun40 = idw(formula = Jun40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwJul40 = idw(formula = Jul40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwAug40 = idw(formula = Aug40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwSept40 = idw(formula = Sept40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwOct40 = idw(formula = Oct40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwNov40 = idw(formula = Nov40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)
idwDec40 = idw(formula = Dec40 ~ 1, locations = ~Lon + Lat, data = Aves, newdata = pred.grid, idp = 3)

Aves1 = data.frame(Lon = Austra1$Lon[Aus1], Lat = Austra1$Lat[Aus1],
                  Jan20 = idwJan20$var1.pred, Feb20 = idwFeb20$var1.pred, Mar20 = idwMar20$var1.pred, Apr20 = idwApr20$var1.pred,
                  May20 = idwMay20$var1.pred, Jun20 = idwJun20$var1.pred, Jul20 = idwJul20$var1.pred, Aug20 = idwAug20$var1.pred,
                  Sept20 = idwSept20$var1.pred, Oct20 = idwOct20$var1.pred, Nov20 = idwNov20$var1.pred, Dec20 = idwDec20$var1.pred,
                  Jan40 = idwJan40$var1.pred, Feb40 = idwFeb40$var1.pred, Mar40 = idwMar40$var1.pred, Apr40 = idwApr40$var1.pred,
                  May40 = idwMay40$var1.pred, Jun40 = idwJun40$var1.pred, Jul40 = idwJul40$var1.pred, Aug40 = idwAug40$var1.pred,
                  Sept40 = idwSept40$var1.pred, Oct40 = idwOct40$var1.pred, Nov40 = idwNov40$var1.pred, Dec40 = idwDec40$var1.pred)




ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Aves1,
             mapping = aes(x = Lon, y = Lat, colour = (Jun40) - (Jun20)), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude",
       title = "Difference of June Averages") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Aves1,
             mapping = aes(x = Lon, y = Lat, colour = (Jan40) - (Jan20)), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude",
       title = "Difference of January Averages") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))



#Changes in Average Season due to climate change
M2 = list()
M2[[4]] = sort(c(m01, m02, m12))
M2[[1]] = sort(c(m03, m04, m05))
M2[[2]] = sort(c(m06, m07, m08))
M2[[3]] = sort(c(m09, m10, m11))

M2[[4]] = M2[[4]][-c(1,2,117,118)]

Aus.all1 = matrix(0, nrow = nrow(Aus.all), ncol = 156)
for(i in 1:length(M2)){
  k = i
  for(j in seq(1, length(M2[[i]]), 3)){
    Aus.all1[,k] = Aus.all[,M2[[i]][j]] + Aus.all[,M2[[i]][j + 1]] + Aus.all[,M2[[i]][j + 2]]
    k = k + 4
  }
}

M3 = list()
M3[[1]] = seq(1, ncol(Aus.all1), 4)
M3[[2]] = seq(2, ncol(Aus.all1), 4)
M3[[3]] = seq(3, ncol(Aus.all1), 4)
M3[[4]] = seq(4, ncol(Aus.all1), 4)

Aus20.ave1 = list()
Aus40.ave1 = list()
for(i in 1:length(M3)){
  Aus20.ave1[[i]] = apply(Aus.all1[,M3[[i]][1:20]], 1, mean)
  Aus40.ave1[[i]] = apply(Aus.all1[,M3[[i]]], 1, mean)
}

Aves1 = data.frame(Lon = precip3[[1]]$Lon[Aus], Lat = precip3[[1]]$Lat[Aus], Kclass = precip3[[1]]$Kclass[Aus],
                  Aut20 = Aus20.ave1[[1]], Win20 = Aus20.ave1[[2]], Spr20 = Aus20.ave1[[3]], Sum20 = Aus20.ave1[[4]],
                  Aut40 = Aus40.ave1[[1]], Win40 = Aus40.ave1[[2]], Spr40 = Aus40.ave1[[3]], Sum40 = Aus40.ave1[[4]])

pred.grid = data.frame(Lon = Austra1$Lon[Aus1], Lat = Austra1$Lat[Aus1])
idwAut20 = idw(formula = Aut20 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)
idwWin20 = idw(formula = Win20 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)
idwSpr20 = idw(formula = Spr20 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)
idwSum20 = idw(formula = Sum20 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)


idwAut40 = idw(formula = Aut40 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)
idwWin40 = idw(formula = Win40 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)
idwSpr40 = idw(formula = Spr40 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)
idwSum40 = idw(formula = Sum40 ~ 1, locations = ~Lon + Lat, data = Aves1, newdata = pred.grid, idp = 3)


Aves1 = data.frame(Lon = Austra1$Lon[Aus1], Lat = Austra1$Lat[Aus1],
                   Aut20 = idwAut20$var1.pred, Win20 = idwWin20$var1.pred, Spr20 = idwSpr20$var1.pred, Sum20 = idwSum20$var1.pred,
                   Aut40 = idwAut40$var1.pred, Win40 = idwWin40$var1.pred, Sum40 = idwSum40$var1.pred, Aug20 = idwAug20$var1.pred)




ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Aves1,
             mapping = aes(x = Lon, y = Lat, colour = (Win40) - (Win20)), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude",
       title = "Difference of Average") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))


#Oscillation difference average
Jan20ave = apply(Aus.all[,m01.1 + 12], 1, mean)
Jan40ave = apply(Aus.all[,m01.1[IOD1[m01.1,2] > 0] + 12], 1, mean)
plot(Jan20ave - Jan40ave, cex = 0.2, pch  = 19)

Aus20.ave = list()
Aus40.ave = list()
for(i in 1:length(M)){
  Aus20.ave[[i]] = apply(Aus.all[,M[[i]][1:20]], 1, mean)
  Aus40.ave[[i]] = apply(Aus.all[,M[[i]]], 1, mean)
}



ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Aves1,
             mapping = aes(x = Lon, y = Lat, colour = (Jan40)), size = 0.2) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude",
       title = "January Average Precipitation") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
