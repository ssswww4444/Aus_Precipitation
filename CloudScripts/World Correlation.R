#World correlation map
require(rgdal)
shape <- readOGR(dsn = ".", layer = "TM_WORLD_BORDERS-0.3")

ausmap = shape[shape$ISO3 =="AUS",]

df1 = as.data.frame(precip3[[1]][Aus,])
df1$Value = 1:nrow(df1)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df1, 
             mapping = aes(x = Lon, y = Lat, colour = Value), size = 1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))

df2 = as.data.frame(precip3[[1]][Aus[134],])

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df2, 
             mapping = aes(x = Lon, y = Lat), size = 3)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


#Aus[134] Melbourne
q = 6
cor.1 = rep(0, nrow(M1.all))
mi1 = m01[m01 > q]
mi2 = sort(c(m12, m01, m02))
mi3 = mi2[mi2 > 12]
for(i in 1:nrow(M1.all)){
  melb.1 = sqrt(M1.all[Aus[Melb], mi3])
  cor.1[i] = cor(melb.1, sqrt(M1.all[i, mi3 - 1]))
  if(i %% 10000 == 0){
    print(i)
  }
}

df3 = data.frame(Lon = precip3[[1]]$Lon, Lat = precip3[[1]]$Lat, Correlation = cor.1)

ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df3, 
             mapping = aes(x = Lon, y = Lat, colour = Correlation), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), 
                         values = seq(1,0, length.out = 9)) +
  theme_bw() +
  coord_cartesian(ylim = c(-55, 55)) + labs(colour = "Correlation", x = "Lon", y = "Lat",
                                            title = "Melbourne Summer One Month Lag Correlation") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12)) +
  labs(x = "Longitude", y = "Latitude")

mi2.1 = sort(c(m12.1, m01.1, m02.1))

cor.1 = rep(0, nrow(M1.all))
mi1 = m01[m01 > q]
mi2 = sort(c(m12, m01, m02))
mi2 = mi2[mi2 > 12]
mi3 = mi2[which(IOD1[sort(c(m12.1, m01.1, m02.1)),2] > 0)]
for(i in 1:nrow(M1.all)){
  melb.1 = (M1.all[Aus[Melb], mi3])
  cor.1[i] = cor(melb.1, (M1.all[i, mi3 - 1]))
  if(i %% 10000 == 0){
    print(i)
  }
}

cor.2 = rep(0, nrow(M1.all))
mi1 = m01[m01 > q]
mi2 = sort(c(m12, m01, m02))
mi2 = mi2[mi2 > 12]
mi3 = mi2[which(IOD1[sort(c(m12.1, m01.1, m02.1)),2] < 0)]
for(i in 1:nrow(M1.all)){
  melb.1 = (M1.all[Aus[Melb], mi3])
  cor.2[i] = cor(melb.1, (M1.all[i, mi3 - 1]))
  if(i %% 10000 == 0){
    print(i)
  }
}


df4 = data.frame(Lon = precip3[[1]]$Lon, Lat = precip3[[1]]$Lat, CorrelationPos = cor.1, CorrelationNeg = cor.2)

gpos = ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df4, 
             mapping = aes(x = Lon, y = Lat, colour = CorrelationPos), size = 0.6) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), 
                         values = seq(1,0, length.out = 9), limits = c(min(c(cor.1, cor.2), na.rm = TRUE),max(c(cor.1, cor.2), na.rm = TRUE))) +
  theme_bw() +
  coord_cartesian(ylim = c(-55, 55)) + labs(colour = "Correlation", x = "Lon", y = "Lat",
                                            title = "Positive SOI") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12)) +
  labs(x = "Longitude", y = "Latitude")

gneg = ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df4, 
             mapping = aes(x = Lon, y = Lat, colour = CorrelationNeg), size = 0.6) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), 
                         values = seq(1,0, length.out = 9), limits = c(min(c(cor.1, cor.2), na.rm = TRUE),max(c(cor.1, cor.2), na.rm = TRUE))) +
  theme_bw() +
  coord_cartesian(ylim = c(-55, 55)) + labs(colour = "Correlation", x = "Lon", y = "Lat",
                                            title = "Negative SOI") +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12), legend.position = "bottom") +
  labs(x = "Longitude", y = "Latitude")

g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}
mylegend <- g_legend(gneg) 

grid.arrange(gpos + theme(legend.position = "none") + labs(x = " ", y = " "),
                   gneg + theme(legend.position = "none") + labs(x = " ", y = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
                   left = textGrob("Latitude", vjust = 2, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))




grid.arrange(gpos + theme(legend.position = "none") + labs(x = " ", y = " "),
             gneg + theme(legend.position = "none") + labs(x = " ", y = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2),
             left = textGrob("Latitude", vjust = 2, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))


cor.soi = rep(0, nrow(Aus.all))
jan.soi = SOI1[m01.1, 2]
for(i in 1:nrow(Aus.all)){
  cor.soi[i] = cor(jan.soi, Aus.all[i, m01.1 + 12])
  if(i %% 1000 == 0){
    print(i)
  }
}

cor.iod = rep(0, nrow(Aus.all))
jan.iod = IOD1[m01.1, 2]
for(i in 1:nrow(Aus.all)){
  cor.iod[i] = cor(jan.iod, Aus.all[i, m01.1 + 12])
  if(i %% 1000 == 0){
    print(i)
  }
}


cor.1 = rep(0, nrow(M1.all))
mi1 = m01[m01 > q]
mi2 = 1:nrow(dates2)
mi2 = mi2[mi2 > 12]
mi3 = mi2[IOD1[, 2] > 0.3]
for(i in 1:nrow(M1.all)){
  melb.1 = sqrt(M1.all[Aus[134], mi3])
  cor.1[i] = cor(melb.1, sqrt(M1.all[i, mi3 - 1]))
  if(i %% 10000 == 0){
    print(i)
  }
}

df3 = data.frame(Lon = precip3[[1]]$Lon, Lat = precip3[[1]]$Lat, Correlation = cor.1)

ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = df3, 
             mapping = aes(x = Lon, y = Lat, colour = Correlation), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), 
                         values = seq(1,0, length.out = 9), limits = c(-0.8,0.8)) +
  theme_bw() +
  coord_cartesian(ylim = c(-52, 52))





