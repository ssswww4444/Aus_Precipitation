Aus.list = list()

p1 = seq(0,1,0.01)
set.seed(1998)
sample1 = sample(1:nrow(Aus.all), 2000)

Aus.list[[1]] = Aus.all[sample1,]

MEDQ.all = MEDQ(Aus.list, p = p1, weight = FALSE, scale = FALSE) 

Ausc.list = list()
Ausc.list[[1]] = t
MEDQ.allc = MEDQ(Ausc.list, p = p1, weight = FALSE, scale = FALSE) 

dd1 = cbind(MEDQ.all, p1)
v1 = NULL
for(i in unique(MEDQ.all)){(apply(Aus.all[sample1,], 1, cumsum))
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

dates.edq = rep(0, nrow(dates2))
for(i in 1:nrow(dates2)){
  m2 = dates2[i,2]
  if(nchar(m2) == 1){
    m2 = paste0("0", m2)
  }
  dates.edq[i] = paste(dates2[i,1], m2, "15", sep = "-")
}

TS.1 = as.vector(t(Aus.all[sample1,]))
time1 = rep(as.Date(dates.edq), nrow(Aus.all[sample1,]))
group1 = rep(1:nrow(Aus.all[sample1,]), each = ncol(Aus.all[sample1,]))

TS.df = data.frame(Time = time1, Ar1 = TS.1, Group = group1)

TS.1.1 = as.vector(t(Aus.all[sample1,][unique(MEDQ.all),]))
time1.1 = rep(as.Date(dates.edq), nrow(Aus.all[sample1,][unique(MEDQ.all),]))
group1.1 = rep(1:nrow(Aus.all[sample1,][unique(MEDQ.all),]), each = ncol(Aus.all[sample1,][unique(MEDQ.all),]))

TS.df.1 = data.frame(Time = time1.1, Ar1 = TS.1.1, Group = group1.1, Value = rep(v1, each = ncol(Aus.all[sample1,])))


TS.2 = as.vector(t(t(apply(Aus.all[sample1,], 1, cumsum))))
time2 = rep(as.Date(dates.edq), nrow(Aus.all[sample1,]))
group2 = rep(1:nrow(Aus.all[sample1,]), each = ncol(Aus.all[sample1,]))
TS.df.2 = data.frame(Time = time2, Ar1 = TS.2, Group = group2)


dd2 = cbind(MEDQ.allc, p1)
v2 = NULL
for(i in unique(MEDQ.allc)){
  v2 = c(v2, max(dd2[which(dd2[,1] == i), 2]))
}


TS.1.2 = as.vector(t(t(apply(Aus.all[sample1,], 1, cumsum))[unique(MEDQ.allc),]))
time1.2 = rep(as.Date(dates.edq), nrow(t(apply(Aus.all[sample1,], 1, cumsum))[unique(MEDQ.allc),]))
group1.2 = rep(1:nrow(t(apply(Aus.all[sample1,], 1, cumsum))[unique(MEDQ.allc),]), each = ncol(t(apply(Aus.all[sample1,], 1, cumsum))[unique(MEDQ.allc),]))

TS.df.3 = data.frame(Time = time1.2, Ar1 = TS.1.2, Group = group1.2, Value = rep(v2, each = ncol(t(apply(Aus.all[sample1,], 1, cumsum)))))


g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Precipitation (mm)",
       title = "Monthly Precipitation") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value), size = 0.8) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(1,0, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12))

g2 = ggplot(data = TS.df.2, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Precipitation (mm)",
       title = "Cumulative Monthly Precipitation") +
  geom_line(data = TS.df.3, mapping = aes(x = Time, y = Ar1, colour = Value), size = 0.8) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(1,0, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))


grid.arrange(g1 + theme(legend.position = "none") + labs(x = " "), g2 + labs(x = " ", y = " "), nrow = 1,
             bottom = textGrob("Date", gp=gpar(fontsize=16,font=8), vjust = 0, hjust = 1.35), widths = c(3,3.6))


Aus.list1 = list()
Aus.list1[[1]] = Aus.all[sample1,M[[1]]]

EDQ.1 = MEDQ(Aus.list1, p = p1, weight = FALSE, scale = FALSE) 

Aus.list2 = list()
Aus.list2[[1]] = Aus.all[sample1,M[[2]]]

EDQ.2 = MEDQ(Aus.list2, p = p1, weight = FALSE, scale = FALSE) 


Aus.list3 = list()
Aus.list3[[1]] = Aus.all[sample1,M[[3]]]

EDQ.3 = MEDQ(Aus.list3, p = p1, weight = FALSE, scale = FALSE) 

Aus.list4 = list()
Aus.list4[[1]] = Aus.all[sample1,M[[4]]]

EDQ.4 = MEDQ(Aus.list4, p = p1, weight = FALSE, scale = FALSE) 

Aus.list5 = list()
Aus.list5[[1]] = Aus.all[sample1,M[[5]]]

EDQ.5 = MEDQ(Aus.list5, p = p1, weight = FALSE, scale = FALSE) 

Aus.list6 = list()
Aus.list6[[1]] = Aus.all[sample1,M[[6]]]

EDQ.6 = MEDQ(Aus.list6, p = p1, weight = FALSE, scale = FALSE)

Aus.list7 = list()
Aus.list7[[1]] = Aus.all[sample1,M[[7]]]

EDQ.7 = MEDQ(Aus.list7, p = p1, weight = FALSE, scale = FALSE) 

Aus.list8 = list()
Aus.list8[[1]] = Aus.all[sample1,M[[8]]]

EDQ.8 = MEDQ(Aus.list8, p = p1, weight = FALSE, scale = FALSE) 

Aus.list9 = list()
Aus.list9[[1]] = Aus.all[sample1,M[[9]]]

EDQ.9 = MEDQ(Aus.list9, p = p1, weight = FALSE, scale = FALSE) 

Aus.list10 = list()
Aus.list10[[1]] = Aus.all[sample1,M[[10]]]

EDQ.10 = MEDQ(Aus.list10, p = p1, weight = FALSE, scale = FALSE) 

Aus.list11 = list()
Aus.list11[[1]] = Aus.all[sample1,M[[11]]]

EDQ.11 = MEDQ(Aus.list11, p = p1, weight = FALSE, scale = FALSE) 

Aus.list12 = list()
Aus.list12[[1]] = Aus.all[sample1,M[[12]]]

EDQ.12 = MEDQ(Aus.list12, p = p1, weight = FALSE, scale = FALSE) 




Ent.mat = matrix(0, nrow = nrow(P1), ncol = 40)
for(i in 1:nrow(P1)){
  P.mat = matrix(P1[i,], nrow = 40, ncol = 12, byrow = TRUE)
  Ent.mat[i,] = apply(P.mat, 1, entropy1)
}

Ent.corr = matrix(0, nrow = ncol(Ent.mat)*(ncol(Ent.mat) - 1)/2, ncol = 2)
k = 0
for(i in 1:(ncol(Ent.mat) - 1)){
  for(j in (i + 1):ncol(Ent.mat)){
    Ent.corr[k,] = c(cor(Ent.mat[,i], Ent.mat[,j]), j-i) 
    k = k + 1
  }
}

Ent.corr.2 = matrix(0, nrow = 39, ncol = 2)
for(i in 1:39){
  Ent.corr.2[i,] = c(mean(Ent.corr[Ent.corr[,2] == i,1]), i)
}


uniq.quantile.df = data.frame(Count = uniq.quantile, month = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))

ggplot(uniq.quantile.df) + geom_line(aes(y = Count, x = 1:12)) +
  scale_x_discrete(name ="Month", limits=uniq.quantile.df$month) +
  theme_bw() + labs(title = "Number of Unique Empirical Dynamical Quantiles")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none")



Locations = precip3[[1]][Aus,1:2]
Locations = Locations[sample1,]

dd1 = cbind(EDQ.1, p1)
v1 = NULL
for(i in unique(EDQ.1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df1 = data.frame(longitude = Locations[unique(EDQ.1),2], latitude = Locations[unique(EDQ.1),1], Quantile = v1)

g.EDQ1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df1, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
     "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ1

dd1 = cbind(EDQ.2, p1)
v1 = NULL
for(i in unique(EDQ.2)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df2 = data.frame(longitude = Locations[unique(EDQ.2),2], latitude = Locations[unique(EDQ.2),1], Quantile = v1)

g.EDQ2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df2, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "February EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ2

dd1 = cbind(EDQ.3, p1)
v1 = NULL
for(i in unique(EDQ.3)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df3 = data.frame(longitude = Locations[unique(EDQ.3),2], latitude = Locations[unique(EDQ.3),1], Quantile = v1)

g.EDQ3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df3, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "March EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ3

dd1 = cbind(EDQ.4, p1)
v1 = NULL
for(i in unique(EDQ.4)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df4 = data.frame(longitude = Locations[unique(EDQ.4),2], latitude = Locations[unique(EDQ.4),1], Quantile = v1)

g.EDQ4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df4, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "April EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ4

dd1 = cbind(EDQ.5, p1)
v1 = NULL
for(i in unique(EDQ.5)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df5 = data.frame(longitude = Locations[unique(EDQ.5),2], latitude = Locations[unique(EDQ.5),1], Quantile = v1)

g.EDQ5 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df5, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "May EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ5

dd1 = cbind(EDQ.6, p1)
v1 = NULL
for(i in unique(EDQ.6)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df6 = data.frame(longitude = Locations[unique(EDQ.6),2], latitude = Locations[unique(EDQ.6),1], Quantile = v1)

g.EDQ6 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df6, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "June EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ6

dd1 = cbind(EDQ.7, p1)
v1 = NULL
for(i in unique(EDQ.7)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df7 = data.frame(longitude = Locations[unique(EDQ.7),2], latitude = Locations[unique(EDQ.7),1], Quantile = v1)

g.EDQ7 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df7, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "July EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ7

dd1 = cbind(EDQ.8, p1)
v1 = NULL
for(i in unique(EDQ.8)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df8 = data.frame(longitude = Locations[unique(EDQ.8),2], latitude = Locations[unique(EDQ.8),1], Quantile = v1)

g.EDQ8 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df8, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "August EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ8

dd1 = cbind(EDQ.9, p1)
v1 = NULL
for(i in unique(EDQ.9)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df9 = data.frame(longitude = Locations[unique(EDQ.9),2], latitude = Locations[unique(EDQ.9),1], Quantile = v1)

g.EDQ9 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df9, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "September EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ9

dd1 = cbind(EDQ.10, p1)
v1 = NULL
for(i in unique(EDQ.10)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df10 = data.frame(longitude = Locations[unique(EDQ.10),2], latitude = Locations[unique(EDQ.10),1], Quantile = v1)

g.EDQ10 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df10, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "October EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ10

dd1 = cbind(EDQ.11, p1)
v1 = NULL
for(i in unique(EDQ.11)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df11 = data.frame(longitude = Locations[unique(EDQ.11),2], latitude = Locations[unique(EDQ.11),1], Quantile = v1)

g.EDQ11 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df11, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "November EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ11

dd1 = cbind(EDQ.12, p1)
v1 = NULL
for(i in unique(EDQ.12)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

EDQ.df12 = data.frame(longitude = Locations[unique(EDQ.12),2], latitude = Locations[unique(EDQ.12),1], Quantile = v1)

g.EDQ12 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df12, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "December EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.EDQ12


uniq.quantile = c(length(unique(EDQ.1)), length(unique(EDQ.2)), length(unique(EDQ.3)),
                  length(unique(EDQ.4)), length(unique(EDQ.5)), length(unique(EDQ.6)),
                  length(unique(EDQ.7)), length(unique(EDQ.8)), length(unique(EDQ.9)),
                  length(unique(EDQ.10)), length(unique(EDQ.11)), length(unique(EDQ.12)))


EDQ.df = rbind(EDQ.df1, EDQ.df2, EDQ.df3,
               EDQ.df4, EDQ.df5, EDQ.df6,
               EDQ.df7, EDQ.df8, EDQ.df9,
               EDQ.df10, EDQ.df11, EDQ.df12)


EDQ.df$Month = rep(1:12, uniq.quantile)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 2) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Monthly EDQ") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))+
  facet_wrap( ~ Month, nrow = 4, ncol = 3, labeller = labeller(Month = c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", "6" = "June",
                                                        "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14),
        strip.text.x = element_text(size = 10, colour = "black")) +
  xlim(113, 155) + ylim(-45,-9)




Aus.slide = matrix(0, nrow = nrow(Aus.all), ncol = ncol(Aus.all) - 12)
for(i in 1:(ncol(Aus.all) - 12)){
  Aus.slide[,i] = rowSums(Aus.all[,i:(i + 11)])
}

Aus.list.slide = list()
Aus.list.slide[[1]] = Aus.slide[sample1,]

p2 = seq(0,1,0.005)

EDQ.slide = MEDQ(Aus.list.slide, p = p2, weight = FALSE, scale = FALSE)


dd1 = cbind(EDQ.slide, p2)
v1 = NULL
for(i in unique(EDQ.slide)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

dates.edq = rep(0, nrow(dates2))
for(i in 1:nrow(dates2)){
  m2 = dates2[i,2]
  if(nchar(m2) == 1){
    m2 = paste0("0", m2)
  }
  dates.edq[i] = paste(dates2[i,1], m2, "15", sep = "-")
}

dates.slide = dates.edq[-(1:12)]

TS.1 = as.vector(t(Aus.slide[sample1,]))
time1 = rep(as.Date(dates.slide), nrow(Aus.slide[sample1,]))
group1 = rep(1:nrow(Aus.slide[sample1,]), each = ncol(Aus.slide[sample1,]))

TS.df = data.frame(Time = time1, Ar1 = TS.1, Group = group1)


TS.1.1 = as.vector(t(Aus.slide[sample1,][unique(EDQ.slide),]))
time1.1 = rep(as.Date(dates.slide), nrow(Aus.slide[sample1,][unique(EDQ.slide),]))
group1.1 = rep(1:nrow(Aus.slide[sample1,][unique(EDQ.slide),]), each = ncol(Aus.slide[sample1,][unique(EDQ.slide),]))

TS.df.1 = data.frame(Time = time1.1, Ar1 = TS.1.1, Group = group1.1, Value = rep(v1, each = ncol(Aus.slide[sample1,])))

g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Precipitation (mm)",
       title = "Twelve Month Cumulative Precipitation") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value), size = 0.8) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                             "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12))

EDQ.df.slide = data.frame(longitude = Locations[unique(EDQ.slide),2], latitude = Locations[unique(EDQ.slide),1], Quantile = v1)

g.Slide = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(EDQ.df.slide, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Slide




grid.arrange(g1 + theme(legend.position = "none"), g.Slide, nrow = 1, widths = c(3, 3.4))





nr = rep(0, length(All.Gauge))
for(i in 1:length(All.Gauge)){
  tryCatch({
    nr[i] = nrow(All.Gauge[[i]])
  }, error = function(e){})
}


nl = NULL
nu = NULL
for(i in which(nr > 100)){
  tryCatch({
    nl = c(nl, paste(All.Gauge[[i]]$Year[1], All.Gauge[[i]]$Month[1], sep = "-"))
    nu = c(nu, paste(All.Gauge[[i]]$Year[nr[i]], All.Gauge[[i]]$Month[nr[i]], sep = "-"))
  }, error = function(e){})
}

used = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 156 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2003) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2015)){
      used = c(used, i)
    }
  }, error = function(e){})
}

X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156)
X[[2]] = matrix(0, nrow = length(used), ncol = 156)
X[[3]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(i in used){
  X[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[1:156])
  X[[2]][k,] = cumsum(All.Gauge[[i]]$Jaxa[1:156])
  X[[3]][k,] = cumsum(All.Gauge[[i]]$Chirp[1:156])
  k = k + 1
}


Me1 = MEDQ(X, p = seq(0,1,0.001), scale = TRUE)

MEDQdates = as.Date(paste(datesNOAA[(which(datesNOAA[,1] == 2003)[1]):(which(datesNOAA[,1] == 2015)[12]),1],
                  datesNOAA[(which(datesNOAA[,1] == 2003)[1]):(which(datesNOAA[,1] == 2015)[12]),2],
                  "15", sep = "-"))

TS.1 = data.frame(Precip = as.vector(t(X[[1]])), ID = rep(1:nrow(X[[1]]), each = ncol(X[[1]])), Dates = rep(MEDQdates, nrow(X[[1]])))
TS.2 = data.frame(Precip = as.vector(t(X[[2]])), ID = rep(1:nrow(X[[2]]), each = ncol(X[[2]])), Dates = rep(MEDQdates, nrow(X[[2]])))
TS.3 = data.frame(Precip = as.vector(t(X[[3]])), ID = rep(1:nrow(X[[3]]), each = ncol(X[[3]])), Dates = rep(MEDQdates, nrow(X[[3]])))


dd1 = cbind(Me1, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me1)){
  v1 = c(v1, mean(dd1[which(dd1[,1] == i), 2]))
}

TS.1.1 = data.frame(Precip = as.vector(t(X[[1]][unique(Me1),])), ID = rep(1:nrow(X[[1]][unique(Me1),]), each = ncol(X[[1]][unique(Me1),])),
                    Dates = rep(MEDQdates, nrow(X[[1]][unique(Me1),])), Value = rep(v1, each = ncol(X[[1]][unique(Me1),])))
TS.2.1 = data.frame(Precip = as.vector(t(X[[2]][unique(Me1),])), ID = rep(1:nrow(X[[2]][unique(Me1),]), each = ncol(X[[2]][unique(Me1),])),
                    Dates = rep(MEDQdates, nrow(X[[2]][unique(Me1),])), Value = rep(v1, each = ncol(X[[2]][unique(Me1),])))
TS.3.1 = data.frame(Precip = as.vector(t(X[[3]][unique(Me1),])), ID = rep(1:nrow(X[[3]][unique(Me1),]), each = ncol(X[[3]][unique(Me1),])),
                    Dates = rep(MEDQdates, nrow(X[[3]][unique(Me1),])), Value = rep(v1, each = ncol(X[[3]][unique(Me1),])))



g1 = ggplot(data = TS.1, mapping = aes(x = Dates, y = Precip, group = ID)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Precipitation (mm)",
       title = "BOM") +
  geom_line(data = TS.1.1, mapping = aes(x = Dates, y = Precip, group = ID, colour = Value), size = 0.8) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12), legend.position = "none")+ ylim(0,max(X[[1]], X[[2]], X[[3]]))

g2 = ggplot(data = TS.2, mapping = aes(x = Dates, y = Precip, group = ID)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Precipitation (mm)",
       title = "JAXA") +
  geom_line(data = TS.2.1, mapping = aes(x = Dates, y = Precip, group = ID, colour = Value), size = 0.8) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12), legend.position = "none")+ ylim(0,max(X[[1]], X[[2]], X[[3]]))

g3 = ggplot(data = TS.3, mapping = aes(x = Dates, y = Precip, group = ID)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Precipitation (mm)",
       title = "NOAA") +
  geom_line(data = TS.3.1, mapping = aes(x = Dates, y = Precip, group = ID, colour = Value), size = 0.8) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12), legend.position = "none") + ylim(0,max(X[[1]], X[[2]], X[[3]]))


Locations = matrix(0, ncol = 2, nrow = length(used))
k = 1
for(i in used){
  Locations[k,] = c(All.Gauge[[i]]$Lat[1], All.Gauge[[i]]$Lon[1])
  k = k + 1
}

colnames(Locations) = c("Lat", "Lon")
Locations = as.data.frame(Locations)

MEDQ.df = data.frame(Lat = Locations[unique(Me1),1], Lon = Locations[unique(Me1),2], Value = v1)

MEDQ.df1 = MEDQ.df[-order(MEDQ.df$Lon)[1:2],]

g.Map = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Cumulative MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

order.p = NULL
for(i in 1:length(unique(Me08))){
  Y.vec = cbind(X[[1]][unique(Me08)[i],], X[[2]][unique(Me08)[i],], X[[3]][unique(Me08)[i],])
  colnames(Y.vec) = c("BOM", "JAXA", "NOAA")
  Y.vec = as.data.frame(Y.vec)
  Y.vec1 = apply(Y.vec, 2, diff)
  lagSelect = VARselect(sqrt(Y.vec1), lag.max = 20, season = 12, type = "const")
  v1 = VAR(sqrt(Y.vec1), season = 12, p = lagSelect$selection[1], type = "const")
  order.p = c(order.p, lagSelect$selection[1])
}


(fitted(v1))^2 - (Y.vec1[-c(1,2),])


Y.vec1 = cbind(X[[1]][unique(Me08)[1],], X[[2]][unique(Me08)[1],], X[[3]][unique(Me08)[1],])
colnames(Y.vec1) = c("BOM", "JAXA", "NOAA")
Y.vec1 = as.data.frame(Y.vec1)
lagSelect = VARselect(sqrt(Y.vec1), lag.max = 20, season = 12, type = "const")
v1 = VAR(sqrt(Y.vec1), season = 12, p = lagSelect$selection[1], type = "const")

Y.vec2 = cbind(X[[1]][unique(Me08)[length(unique(Me08)) - 2],], X[[2]][unique(Me08)[length(unique(Me08)) - 2],],
               X[[3]][unique(Me08)[length(unique(Me08)) - 2],])
colnames(Y.vec2) = c("BOM", "JAXA", "NOAA")
Y.vec2 = as.data.frame(Y.vec2)
lagSelect = VARselect(sqrt(Y.vec2), lag.max = 20, season = 12, type = "const")
v2 = VAR(sqrt(Y.vec2), season = 12, p = lagSelect$selection[1], type = "const")


VAR.df = data.frame(Precipitation = c(Y.vec1[,1], Y.vec1[,2], Y.vec1[,3], Y.vec2[,1], Y.vec2[,2], Y.vec2[,3]),
                    Variable = c(rep(c("BOM", "JAXA", "NOAA"), each = nrow(Y.vec1)), rep(c("BOM", "JAXA", "NOAA"), each = nrow(Y.vec2))),
                    Quantile = c(rep(c("p = 0.0535", "p = 0.9670"), each = nrow(Y.vec1) * 3),
                                 rep(c("p = 0.0535", "p = 0.9670"), each = nrow(Y.vec2) * 3)),
                    Dates = rep(MEDQdates, 6))

Fitted.df = data.frame(Precipitation = c(fitted(v1)[,1]^2, fitted(v1)[,2]^2, fitted(v1)[,3]^2, fitted(v2)[,1]^2, fitted(v2)[,2]^2, fitted(v2)[,3]^2),
                       Variable = c(rep(c("BOM", "JAXA", "NOAA"), each = nrow(fitted(v1))), rep(c("BOM", "JAXA", "NOAA"), each = nrow(fitted(v2)))),
                       Quantile = c(rep("p = 0.0535", nrow(fitted(v1)) * 3), rep("p = 0.9670", nrow(fitted(v2)) * 3)),
                       Dates = c(rep(MEDQdates[-c(1:(length(MEDQdates) - nrow(fitted(v1))))], 3),
                                 rep(MEDQdates[-c(1:(length(MEDQdates) - nrow(fitted(v2))))], 3)))

Var1.df = rbind(VAR.df, Fitted.df)

Var1.df$Type = as.factor(c(rep("Actual", nrow(VAR.df)), rep("Fitted", nrow(Fitted.df))))



ggplot(Var1.df) + geom_line(aes(x = Dates, y = Precipitation, colour = Type, linetype = Type), size = 0.9) + facet_grid(Variable~Quantile) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Cumulative MEDQ Locations")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=12), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12), strip.text = element_text(size=12)) +
  labs(x = "Date", y = "Precipitation (mm)", title = "VAR Precipitation Modelling") +
  scale_color_manual(values = c("#4798ff",
                                "#d7341d"))


f1 = NULL
for(i in 1:length(unique(Me1))){
  Y.vec = cbind(X[[1]][unique(Me1)[i],], X[[2]][unique(Me1)[i],], X[[3]][unique(Me1)[i],])
  colnames(Y.vec) = c("BOM", "JAXA", "NOAA")
  Y.vec = as.data.frame(Y.vec)
  Y.vec1 = apply(Y.vec, 2, diff)
  lagSelect = VARselect(sqrt(Y.vec1), lag.max = 20, season = 12, type = "const")
  v1 = VAR(sqrt(Y.vec1), season = 12, p = lagSelect$selection[1], type = "const")
  f1 = rbind(f1, tail(fitted(v1)^2,1))
}

D1.df = data.frame(Lon = Locations[unique(Me1),2], Lat = Locations[unique(Me1),1], Precipitation = sqrt(f1[,1]))
D2.df = data.frame(Lon = Locations[unique(Me1),2], Lat = Locations[unique(Me1),1], Precipitation = sqrt(f1[,2]))
D3.df = data.frame(Lon = Locations[unique(Me1),2], Lat = Locations[unique(Me1),1], Precipitation = sqrt(f1[,3]))

pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])

idw1 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = D1.df, newdata = pred.grid, idp = 1)
idw2 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = D2.df, newdata = pred.grid, idp = 1)
idw3 = idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = D3.df, newdata = pred.grid, idp = 1)

coordinates(D1.df) = ~Lon + Lat
coordinates(pred.grid) = ~Lon + Lat
kriging_result1 = autoKrige(Precipitation~1, D1.df, pred.grid)
plot(kriging_result1, cex = 0.2)
coordinates(D2.df) = ~Lon + Lat
coordinates(D3.df) = ~Lon + Lat
kriging_result2 = autoKrige(Precipitation~1, D2.df, pred.grid)
kriging_result3 = autoKrige(Precipitation~1, D3.df, pred.grid)

g.Map1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred^2), size = 0.5) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "BOM Forecast", colour = "Precipitation")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                                                         limits = c(min(c(idw1$var1.pred^2, idw2$var1.pred^2, idw3$var1.pred^2)),
                                                                    max(c(idw1$var1.pred^2, idw2$var1.pred^2, idw3$var1.pred^2))))

g.Map2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred^2), size = 0.5) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "JAXA Forecast", colour = "Precipitation")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                                                         limits = c(min(c(idw1$var1.pred^2, idw2$var1.pred^2, idw3$var1.pred^2)),
                                                                    max(c(idw1$var1.pred^2, idw2$var1.pred^2, idw3$var1.pred^2))))

g.Map3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw3, mapping = aes(x = Lon, y = Lat, colour = var1.pred^2), size = 0.5) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "NOAA Forecast", colour = "Precipitation")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                                                         limits = c(min(c(idw1$var1.pred^2, idw2$var1.pred^2, idw3$var1.pred^2)),
                                                                    max(c(idw1$var1.pred^2, idw2$var1.pred^2, idw3$var1.pred^2))))


g.Loc.all = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Locations, mapping = aes(x = Lon, y = Lat), size = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Australian Gauge Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

A1 = Locations[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=GRS80 +no_defs"
a3 = over(A1, VIC)

Locations.Vic = Locations[which(a3[,6] == "VIC"),]

g.Loc.Vic = ggplot() + geom_polygon(data = VIC, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Locations.Vic, mapping = aes(x = Lon, y = Lat), size = 1.5) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Victoria Gauge Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
 scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


grid.arrange(g1 + theme(legend.position = "none"), g.Map,
             g2 + theme(legend.position = "none"), g3 + theme(legend.position = "none") + labs(y = " "), nrow = 2)

(g1 | g.Map1)/(g2 | g3)



used = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 156 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2003) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2015)){
      used = c(used, i)
    }
  }, error = function(e){})
}

X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
locations1 = NULL
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(1, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(1, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(1, 156, 12)]
  locations1 = rbind(locations1, All.Gauge[[i]][1,2:1])
  k = k + 1
}


Me01 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 

X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(2, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(2, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(2, 156, 12)]
  k = k + 1
}


Me02 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(3, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(3, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(3, 156, 12)]
  k = k + 1
}


Me03 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(4, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(4, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(4, 156, 12)]
  k = k + 1
}


Me04 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(5, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(5, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(5, 156, 12)]
  k = k + 1
}


Me05 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(6, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(6, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(6, 156, 12)]
  k = k + 1
}


Me06 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(7, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(7, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(7, 156, 12)]
  k = k + 1
}


Me07 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
loc = NULL
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(8, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(8, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(8, 156, 12)]
  loc = rbind(loc, All.Gauge[[i]][1,2:1])
  k = k + 1
}


Me08 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(9, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(9, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(9, 156, 12)]
  k = k + 1
}


Me09 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(10, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(10, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(10, 156, 12)]
  k = k + 1
}


Me10 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(11, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(11, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(11, 156, 12)]
  k = k + 1
}


Me11 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[2]] = matrix(0, nrow = length(used), ncol = 156/12)
X[[3]] = matrix(0, nrow = length(used), ncol = 156/12)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])[seq(12, 156, 12)]
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])[seq(12, 156, 12)]
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])[seq(12, 156, 12)]
  k = k + 1
}


Me12 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 


MEDQ.df = rbind(Me01, Me02, Me03,
               Me04, Me05, Me06,
               Me07, Me08, Me09,
               Me10, Me11, Me12)


uniq.quantile = c(length(unique(Me01)), length(unique(Me02)), length(unique(Me03)),
                  length(unique(Me04)), length(unique(Me05)), length(unique(Me06)),
                  length(unique(Me07)), length(unique(Me08)), length(unique(Me09)),
                  length(unique(Me10)), length(unique(Me11)), length(unique(Me12)))


Locations = NULL
for(i in used){
  Locations = rbind(Locations, All.Gauge[[i]][1,1:2])
}

Locations = as.matrix(Locations)

p1 = seq(0,1,0.001)

dd1 = cbind(Me01, p1)
v1 = NULL
for(i in unique(Me01)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me01.df = data.frame(longitude = Locations[unique(Me01),2], latitude = Locations[unique(Me01),1], Quantile = v1)

g.Me01 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me01.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me01

dd1 = cbind(Me02, p1)
v1 = NULL
for(i in unique(Me02)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me02.df = data.frame(longitude = Locations[unique(Me02),2], latitude = Locations[unique(Me02),1], Quantile = v1)

g.Me02 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me02.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "February MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me02

dd1 = cbind(Me03, p1)
v1 = NULL
for(i in unique(Me03)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me03.df = data.frame(longitude = Locations[unique(Me03),2], latitude = Locations[unique(Me03),1], Quantile = v1)

g.Me03 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me03.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "March MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me03

dd1 = cbind(Me04, p1)
v1 = NULL
for(i in unique(Me04)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me04.df = data.frame(longitude = Locations[unique(Me04),2], latitude = Locations[unique(Me04),1], Quantile = v1)

g.Me04 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me04.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "April MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me04

dd1 = cbind(Me05, p1)
v1 = NULL
for(i in unique(Me05)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me05.df = data.frame(longitude = Locations[unique(Me05),2], latitude = Locations[unique(Me05),1], Quantile = v1)

g.Me05 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me05.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "May MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me05

dd1 = cbind(Me06, p1)
v1 = NULL
for(i in unique(Me06)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me06.df = data.frame(longitude = Locations[unique(Me06),2], latitude = Locations[unique(Me06),1], Quantile = v1)

g.Me06 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me06.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "June MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me06

dd1 = cbind(Me07, p1)
v1 = NULL
for(i in unique(Me07)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me07.df = data.frame(longitude = Locations[unique(Me07),2], latitude = Locations[unique(Me07),1], Quantile = v1)

g.Me07 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me07.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "July MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me07

dd1 = cbind(Me08, p1)
v1 = NULL
for(i in unique(Me08)){
  v1 = c(v1, mean(dd1[which(dd1[,1] == i), 2]))
}

Locations = loc[,2:1]

Me08.df = data.frame(longitude = Locations[unique(Me08),2], latitude = Locations[unique(Me08),1], Quantile = v1)

g.Me08 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me08.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "August MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me08

dd1 = cbind(Me09, p1)
v1 = NULL
for(i in unique(Me09)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me09.df = data.frame(longitude = Locations[unique(Me09),2], latitude = Locations[unique(Me09),1], Quantile = v1)

g.Me09 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me09.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "September MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me09

dd1 = cbind(Me10, p1)
v1 = NULL
for(i in unique(Me10)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me10.df = data.frame(longitude = Locations[unique(Me10),2], latitude = Locations[unique(Me10),1], Quantile = v1)

g.Me10 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me10.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "October MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me10

dd1 = cbind(Me11, p1)
v1 = NULL
for(i in unique(Me11)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me11.df = data.frame(longitude = Locations[unique(Me11),2], latitude = Locations[unique(Me11),1], Quantile = v1)

g.Me11 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me11.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "November MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me11

dd1 = cbind(Me12, p1)
v1 = NULL
for(i in unique(Me12)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Me12.df = data.frame(longitude = Locations[unique(Me12),2], latitude = Locations[unique(Me12),1], Quantile = v1)

g.Me12 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Me12.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "December MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Me12



VIC <- readOGR(dsn = ".", layer = "VIC_STATE_POLYGON_shp_GDA2020")


MEDQ.df = rbind(Me01.df, Me02.df, Me03.df,
                Me04.df, Me05.df, Me06.df,
                Me07.df, Me08.df, Me09.df,
                Me10.df, Me11.df, Me12.df)


uniq.quantile = c(length(unique(Me01)), length(unique(Me02)), length(unique(Me03)),
                  length(unique(Me04)), length(unique(Me05)), length(unique(Me06)),
                  length(unique(Me07)), length(unique(Me08)), length(unique(Me09)),
                  length(unique(Me10)), length(unique(Me11)), length(unique(Me12)))








MEDQ.df$Month = rep(1:12, uniq.quantile)


A1 = MEDQ.df[,1:2]
A1 = as.data.frame(A1)
coordinates(A1) =~longitude + latitude
proj4string(A1) = "+proj=longlat +ellps=GRS80 +no_defs"
a3 = over(A1, VIC)

MEDQ.df1 = MEDQ.df[which(a3[,6] == "VIC"),]

MEDQ.df1 = rbind(MEDQ.df1, c(NA,NA, NA, 2))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 3.3) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Monthly MEDQ Locations") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))+
  facet_wrap( ~ Month, nrow = 4, ncol = 3, labeller = labeller(Month = c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", "6" = "June",
                                                                         "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14),
        strip.text.x = element_text(size = 10, colour = "black")) +
  xlim(113, 155) + ylim(-45,-9)



###VAR model

ME.list = list()
ME.list[[1]] = Me01
ME.list[[2]] = Me02
ME.list[[3]] = Me03
ME.list[[4]] = Me04
ME.list[[5]] = Me05
ME.list[[6]] = Me06
ME.list[[7]] = Me07
ME.list[[8]] = Me08
ME.list[[9]] = Me09
ME.list[[10]] = Me10
ME.list[[11]] = Me11
ME.list[[12]] = Me12


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156)
X[[2]] = matrix(0, nrow = length(used), ncol = 156)
X[[3]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(j in used){
  X[[1]][k,] = (All.Gauge[[j]]$Precipitation[1:156])
  X[[2]][k,] = (All.Gauge[[j]]$Jaxa[1:156])
  X[[3]][k,] = (All.Gauge[[j]]$Chirp[1:156])
  k = k + 1
}
Order.p = list()
Fitted.list = list()
for(i in 1:length(ME.list)){
  f1 = NULL
  order.p = NULL
  for(j in 1:length(unique(ME.list[[i]]))){
    Y.vec = cbind(X[[1]][unique(ME.list[[i]])[j],], X[[2]][unique(ME.list[[i]])[j],], X[[3]][unique(ME.list[[i]])[j],])
    colnames(Y.vec) = c("BOM", "JAXA", "NOAA")
    Y.vec = as.data.frame(Y.vec)
    lagSelect = VARselect(sqrt(Y.vec), lag.max = 20, season = 12, type = "const")
    v1 = VAR(sqrt(Y.vec), season = 12, p = lagSelect$selection[1], type = "const")
    order.p = c(order.p, lagSelect$selection[1])
    f1 = rbind(f1, tail(fitted(v1)^2,1))
  }
  Order.p[[i]] = order.p
  Fitted.list[[i]] = f1
}

f1 = NULL
order.p = NULL
for(j in 1:length(unique(ME.list[[8]]))){
  Y.vec = cbind(X[[1]][unique(ME.list[[8]])[j],], X[[2]][unique(ME.list[[8]])[j],], X[[3]][unique(ME.list[[8]])[j],])
  colnames(Y.vec) = c("BOM", "JAXA", "NOAA")
  Y.vec = as.data.frame(Y.vec)
  lagSelect = VARselect(sqrt(Y.vec), lag.max = 20, season = 12, type = "const")
  v1 = VAR(sqrt(Y.vec), season = 12, p = lagSelect$selection[1], type = "const")
  order.p = c(order.p, lagSelect$selection[1])
  f1 = rbind(f1, (fitted(v1)^2)[nrow(fitted(v1)) - 6,])
}


Me08.df1 = Me08.df
Me08.df1$BOM = sqrt(f1[,1])
Me08.df1$JAXA = sqrt(f1[,2])
Me08.df1$NOAA = sqrt(f1[,3])

X.sp <- Me08.df1  #Copy the data.  It's still a data.frame
coordinates(X.sp) <- ~longitude + latitude  # Now it's SpatialPointsDataFrame, with coordinates x and y
# Create a categorical variable and plot it



X.i <- gstat(id = "BOM", formula = BOM ~ 1, data = X.sp, 
             nmax = 20, beta = 0.5)
# The order=4 varible is set as per the instructions in the gstat manual.
# this tells gstat that each indicator is cumulative.  You can't be in the
# second category without also being in the first category.
X.i <- gstat(X.i, "JAXA", formula = JAXA ~ 1, data = X.sp, 
             nmax = 20, beta = 3)
X.i <- gstat(X.i, "NOAA", formula = NOAA ~ 1, data = X.sp, 
             nmax = 20, beta = 3)


# Create a semivariogram model with range equal 1200, and 'dummy' partial
# sill and nugget of 1.  We will fit these later.  'One size fits all'
X.i <- gstat(X.i, model = vgm(2, "Sph", 1000, 1), fill.all = T)

# Estimate the empiricalvariogram of each indicator
x <- variogram(X.i)
plot(x)
X.fit = fit.lmc(x, X.i)

plot(x, model = X.fit)

X.spgrid <- Austra1[,1:2]
colnames(X.spgrid) = c("latitude", "longitude")
coordinates(X.spgrid) <- ~longitude + latitude



zk <- predict(X.fit, newdata = X.spgrid, indicators = TRUE)

zk.df = data.frame(BOM = zk$BOM.pred, JAXA = zk$JAXA.pred, NOAA = zk$NOAA.pred,
                   Lon = coordinates(zk)[,1], Lat = coordinates(zk)[,2], BOM.Var = zk$BOM.var,
                   JAXA.Var = zk$JAXA.var, NOAA.Var = zk$NOAA.var)

A1 = zk.df[,4:5]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

zk.df = zk.df[Aus,]



g.BOM = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df, 
             mapping = aes(x = Lon, y = Lat, colour = BOM^2), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(zk.df[,1:3]^2), max(zk.df[,1:3]^2))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = " ", colour = "Estimate", title = "BOM Estimate")

g.Jaxa = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df, 
             mapping = aes(x = Lon, y = Lat, colour = JAXA^2), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(zk.df[,1:3]^2), max(zk.df[,1:3]^2))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA Estimate")


g.NOAA = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df, 
             mapping = aes(x = Lon, y = Lat, colour = NOAA^2), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(zk.df[,1:3]^2), max(zk.df[,1:3]^2))) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA Estimate")


g.BOM1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df, 
             mapping = aes(x = Lon, y = Lat, colour = sqrt(BOM.Var)), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  labs(x = " ", y = " ", colour = "Estimate", title = "Intercept SD")

g.Jaxa1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df, 
             mapping = aes(x = Lon, y = Lat, colour = sqrt(JAXA.Var)), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  labs(x = " ", y = "Latitude", colour = " ", title = "JAXA SD")


g.NOAA1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = zk.df, 
             mapping = aes(x = Lon, y = Lat, colour = sqrt(NOAA.Var)), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  labs(x = "Longitude", y = " ", colour = " ", title = "NOAA SD")


grid.arrange(g.BOM + theme(legend.position = "left") + labs(x = " ", y = " ", colour = "Precipitaiton\n(mm)"),
             g.BOM1 + theme(legend.position = "right") + labs(x = " ", y = " ", colour = "Standard\nDeviation"),
             g.Jaxa + theme(legend.position = "left") + labs(x = " ", y = " ", colour = "             "),
             g.Jaxa1 + theme(legend.position = "right") + labs(x = " ", y = " ", colour = "                "),
             g.NOAA + theme(legend.position = "left") + labs(x = " ", y = " ", colour = "             "),
             g.NOAA1 + theme(legend.position = "right") + labs(x = " ", y = " ", colour = "                "), nrow = 3,
             left = textGrob("Latitude", vjust = 9, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90),
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.2))


grid.arrange(g.BOM + theme(legend.position = "none") + labs(x = " ", y = " "),
             g.Jaxa + theme(legend.position = "none") + labs(x = " ", y = " "),
             g.NOAA + theme(legend.position = "bottom") + labs(x = " ", y = " ", colour = "Precipitaiton\n(mm)"),
             g.Me08 + theme(legend.position = "bottom") + labs(x = " ", y = " "),
             nrow = 2, heights = c(1,1.25))





All.Gauge[[1]][150,]

Me08


Fitted.list[[8]]




##############
#Monthly Standardised
##############


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156)
X[[2]] = matrix(0, nrow = length(used), ncol = 156)
X[[3]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])
  X[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])
  X[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])
  k = k + 1
}


for(i in 1:12){
  X[[1]][,seq(i,156,12)] = t(apply(X[[1]][,seq(i,156,12)], 1, scale))
  X[[2]][,seq(i,156,12)] = t(apply(X[[2]][,seq(i,156,12)], 1, scale))
  X[[3]][,seq(i,156,12)] = t(apply(X[[3]][,seq(i,156,12)], 1, scale))
}

c1 = complete.cases(X[[1]]) & complete.cases(X[[2]]) & complete.cases(X[[3]])
X[[1]] = X[[1]][c1,]
X[[2]] = X[[2]][c1,]
X[[3]] = X[[3]][c1,]


ME.s = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = TRUE)

































ggplot() + geom_polygon(data = VIC, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 3.3) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Monthly MEDQ") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))+
  facet_wrap( ~ Month, nrow = 4, ncol = 3, labeller = labeller(Month = c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", "6" = "June",
                                                                         "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14),
        strip.text.x = element_text(size = 10, colour = "black")) +
  xlim(140, 152) + ylim(-40,-34)
 
ggplot() + geom_polygon(data = VIC, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df1, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 3.3, na.rm = TRUE) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Monthly MEDQ") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(0,1))+
  facet_wrap( ~ Month, nrow = 4, ncol = 3, labeller = labeller(Month = c("1" = "January", "2" = "February", "3" = "March", "4" = "April", "5" = "May", "6" = "June",
                                                                         "7" = "July", "8" = "August", "9" = "September", "10" = "October", "11" = "November", "12" = "December"))) +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=10), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14),
        strip.text.x = element_text(size = 10, colour = "black"))


################33
#Different time period
#####################


used1 = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 72 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2003) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2008)){
      used1 = c(used1, i)
    }
  }, error = function(e){})
}

used2 = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 60 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2009) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2013)){
      used2 = c(used2, i)
    }
  }, error = function(e){})
}

used3 = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2012) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2017)){
      used3 = c(used3, i)
    }
  }, error = function(e){})
}

substr(Sys.time(),0,10)
curr.year = as.numeric(substr(Sys.time(),0,4))
curr.month = as.numeric(substr(Sys.time(),6,7))
curr.day = as.numeric(substr(Sys.time(), 9,10))

years1 = c(rep(2003:(2017), each = 12), rep(2018, 7))

months1 = c(rep(1:12, length(unique(years1)) - 1), 1:7)
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

datesG = cbind(dates1, days1)



X1 = list()
X1[[1]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2003 & datesG[,2] == 1):which(datesG[,1] == 2008 & datesG[,2] == 12)))
X1[[2]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2003 & datesG[,2] == 1):which(datesG[,1] == 2008 & datesG[,2] == 12)))
X1[[3]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2003 & datesG[,2] == 1):which(datesG[,1] == 2008 & datesG[,2] == 12)))
k = 1
for(i in used1){
  X1[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[which(datesG[,1] == 2003 & datesG[,2] == 1):which(datesG[,1] == 2008 & datesG[,2] == 12)])
  X1[[2]][k,] = cumsum(All.Gauge[[i]]$Jaxa[which(datesG[,1] == 2003 & datesG[,2] == 1):which(datesG[,1] == 2008 & datesG[,2] == 12)])
  X1[[3]][k,] = cumsum(All.Gauge[[i]]$Chirp[which(datesG[,1] == 2003 & datesG[,2] == 1):which(datesG[,1] == 2008 & datesG[,2] == 12)])
  k = k + 1
}

X2 = list()
X2[[1]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2009 & datesG[,2] == 1):which(datesG[,1] == 2013 & datesG[,2] == 12)))
X2[[2]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2009 & datesG[,2] == 1):which(datesG[,1] == 2013 & datesG[,2] == 12)))
X2[[3]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2009 & datesG[,2] == 1):which(datesG[,1] == 2013 & datesG[,2] == 12)))
k = 1
for(i in used2){
  X2[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[which(datesG[,1] == 2009 & datesG[,2] == 1):which(datesG[,1] == 2013 & datesG[,2] == 12)])
  X2[[2]][k,] = cumsum(All.Gauge[[i]]$Jaxa[which(datesG[,1] == 2009 & datesG[,2] == 1):which(datesG[,1] == 2013 & datesG[,2] == 12)])
  X2[[3]][k,] = cumsum(All.Gauge[[i]]$Chirp[which(datesG[,1] == 2009 & datesG[,2] == 1):which(datesG[,1] == 2013 & datesG[,2] == 12)])
  k = k + 1
}

for(i in 1:length(X2)){
  X2[[1]] = X2[[1]][complete.cases(X2[[1]]),]
  X2[[2]] = X2[[2]][complete.cases(X2[[2]]),]
  X2[[3]] = X2[[3]][complete.cases(X2[[3]]),]
}

X3 = list()
X3[[1]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2012 & datesG[,2] == 1):which(datesG[,1] == 2017 & datesG[,2] == 12)))
X3[[2]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2012 & datesG[,2] == 1):which(datesG[,1] == 2017 & datesG[,2] == 12)))
X3[[3]] = matrix(0, nrow = length(used1), ncol = length(which(datesG[,1] == 2012 & datesG[,2] == 1):which(datesG[,1] == 2017 & datesG[,2] == 12)))
k = 1
for(i in used3){
  X3[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[which(datesG[,1] == 2012 & datesG[,2] == 1):which(datesG[,1] == 2017 & datesG[,2] == 12)])
  X3[[2]][k,] = cumsum(All.Gauge[[i]]$Jaxa[which(datesG[,1] == 2012 & datesG[,2] == 1):which(datesG[,1] == 2017 & datesG[,2] == 12)])
  X3[[3]][k,] = cumsum(All.Gauge[[i]]$Chirp[which(datesG[,1] == 2012 & datesG[,2] == 1):which(datesG[,1] == 2017 & datesG[,2] == 12)])
  k = k + 1
}

for(i in 1:length(X3)){
  X3[[1]] = X3[[1]][complete.cases(X3[[1]]),]
  X3[[2]] = X3[[2]][complete.cases(X3[[2]]),]
  X3[[3]] = X3[[3]][complete.cases(X3[[3]]),]
}

Locations1 = matrix(0, ncol = 2, nrow = length(used1))
k = 1
for(i in used1){
  Locations1[k,] = c(All.Gauge[[i]]$Lat[1], All.Gauge[[i]]$Lon[1])
  k = k + 1
}

Locations2 = matrix(0, ncol = 2, nrow = length(used2))
k = 1
for(i in used2){
  Locations2[k,] = c(All.Gauge[[i]]$Lat[1], All.Gauge[[i]]$Lon[1])
  k = k + 1
}

Locations3 = matrix(0, ncol = 2, nrow = length(used3))
k = 1
for(i in used3){
  Locations3[k,] = c(All.Gauge[[i]]$Lat[1], All.Gauge[[i]]$Lon[1])
  k = k + 1
}

ptm <- proc.time()
ME1 = MEDQ(X1, p = seq(0,1,0.001), scale = FALSE) 
proc.time() - ptm

dd1 = cbind(ME1, seq(0,1,0.001))
v1 = NULL
for(i in unique(ME1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}
ME1.df = data.frame(longitude = Locations1[unique(ME1),2], latitude = Locations1[unique(ME1),1], Quantile = v1)

g.ME1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(ME1.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "First MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))




ME2 = MEDQ(X2, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)
dd1 = cbind(ME2, seq(0,1,0.001))
v1 = NULL
for(i in unique(ME2)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}
ME2.df = data.frame(longitude = Locations2[unique(ME2),2], latitude = Locations2[unique(ME2),1], Quantile = v1)

g.ME2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(ME2.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Second MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

ME3 = MEDQ(X3, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 

dd1 = cbind(ME3, seq(0,1,0.001))
v1 = NULL
for(i in unique(ME3)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}
ME3.df = data.frame(longitude = Locations3[unique(ME3),2], latitude = Locations3[unique(ME3),1], Quantile = v1)

g.ME3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(ME3.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Third MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


grid.arrange(g.ME1, g.ME2, nrow = 1)

ME3 = MEDQ(X3, p = 0.5, weight = FALSE, scale = FALSE) 

install.packages("leaps")
library("leaps")
library("lmtest")
library("nlme")

install.packages("ape")
library("ape")
library("broom")
library("FRK")
library("purrr")

library("lattice")
library("ggplot2")
library("RColorBrewer")


library("dplyr")
library("gstat")
library("sp")
library("spacetime")
library("STRbook")
library("tidyr")

data("NOAA_df_1990", package = "STRbook")
Tmax <- filter(NOAA_df_1990, # subset the data
               proc == "Tmax" & # only max temperature
                 month == 7 & # July
                 year == 1993) # year of 1993


G <- auto_basis(data = Tmax[,c("lon","lat")] %>% # Take Tmax
                  SpatialPoints(), # To sp obj
                nres = 1, # One resolution
                type = "Gaussian") 

S <- eval_basis(basis = G, # basis functions
                s = Tmax[,c("lon","lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names

Tmax2 <- cbind(Tmax, S) %>% # append S to Tmax
  select(-year, -month, -proc, # and remove vars we
         -julian, -date) # will not be using in
# the model

Tmax2 = Tmax2[,-c(2,3,7,1)]


Tmax_no_14 <- filter(Tmax2, !(day == 14)) # remove day 14

Tmax_no_14 = Tmax_no_14[,-2]

Tmax_July_lm <- lm(z ~ (lon + lat + day)^2 + ., # model
                   data = Tmax_no_14) # omit id
Tmax_July_lm %>% summary()


X.all = list()
X.all[[1]] = matrix(0, nrow = length(used), ncol = 156)
X.all[[2]] = matrix(0, nrow = length(used), ncol = 156)
X.all[[3]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(i in used){
  X.all[[1]][k,] = (All.Gauge[[i]]$Precipitation[1:156])
  X.all[[2]][k,] = (All.Gauge[[i]]$Jaxa[1:156])
  X.all[[3]][k,] = (All.Gauge[[i]]$Chirp[1:156])
  k = k + 1
}


G8 = data.frame(Precipitation = as.vector(t(X.all[[1]][unique(Me08),])),
                Lon = rep(Locations[unique(Me08),2], each = ncol(X.all[[1]])),
                Lat = rep(Locations[unique(Me08),1], each = ncol(X.all[[1]])),
                Period = rep(1:ncol(X.all[[1]]), length(unique(Me08))))


G <- auto_basis(data = G8[,c("Lon","Lat")] %>% # Take Tmax
                  SpatialPoints(), # To sp obj
                nres = 1, # One resolution
                type = "Gaussian") 

S <- eval_basis(basis = G, # basis functions
                s = G8[,c("Lon","Lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) #

G8.1 = cbind(G8, S)

mod1 = lm(sqrt(Precipitation) ~ (Lon + Lat + Period)^2 +., data = G8.1)


X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156)
X[[2]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(i in used){
  X[[1]][k,] = (All.Gauge[[i]]$Jaxa[1:156])
  X[[2]][k,] = (All.Gauge[[i]]$Chirp[1:156])
  k = k + 1
}

Ed1 = MEDQ(X, p = seq(0,1,0.0001), weight = FALSE, scale = FALSE) 

dd1 = cbind(Ed1, seq(0,1,0.0001))
v1 = NULL
for(i in unique(Ed1)){
  v1 = c(v1, mean(dd1[which(dd1[,1] == i), 2]))
}

Ed1.df = data.frame(longitude = Locations[unique(Ed1),2], latitude = Locations[unique(Ed1),1], Quantile = v1)

g.Ed1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Ed1.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "BOM EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


X1 = list()
X2 = list()
X3 = list()
X1[[1]] = matrix(0, nrow = length(used), ncol = 156)
X2[[1]] = matrix(0, nrow = length(used), ncol = 156)
X3[[1]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(i in used){
  X1[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[1:156])
  X2[[1]][k,] = cumsum(All.Gauge[[i]]$Jaxa[1:156])
  X3[[1]][k,] = cumsum(All.Gauge[[i]]$Chirp[1:156])
  k = k + 1
}

Ed1 = MEDQ(X1, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 
Ed2 = MEDQ(X2, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 
Ed3 = MEDQ(X3, p = seq(0,1,0.001), weight = FALSE, scale = FALSE) 

dd1 = cbind(Ed1, seq(0,1,0.001))
v1 = NULL
for(i in unique(Ed1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}
dd1 = cbind(Ed2, seq(0,1,0.001))
v2 = NULL
for(i in unique(Ed2)){
  v2 = c(v2, max(dd1[which(dd1[,1] == i), 2]))
}
dd1 = cbind(Ed3, seq(0,1,0.001))
v3 = NULL
for(i in unique(Ed3)){
  v3 = c(v3, max(dd1[which(dd1[,1] == i), 2]))
}
Ed1.df = data.frame(longitude = Locations[unique(Ed1),2], latitude = Locations[unique(Ed1),1], Quantile = v1)
Ed2.df = data.frame(longitude = Locations[unique(Ed2),2], latitude = Locations[unique(Ed2),1], Quantile = v2)
Ed3.df = data.frame(longitude = Locations[unique(Ed3),2], latitude = Locations[unique(Ed3),1], Quantile = v3)

g.Ed1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Ed1.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "BOM EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

g.Ed2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Ed2.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "JAXA EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

g.Ed3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Ed3.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "NOAA EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

Ed.df = rbind(Ed1.df, Ed2.df, Ed3.df)
Ed.df$Type = c(rep("BOM", nrow(Ed1.df)), rep("JAXA", nrow(Ed2.df)), rep("NOAA", nrow(Ed3.df)))

grid.arrange(g.Ed1, g.Ed2, g.Ed3, g.Map1 + theme(plot.title = element_text(size = 18, face = "bold"),
                                                 legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
                                                 legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
               labs(title = "MEDQ"), nrow = 2)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_wrap(~Type, nrow = 1) +
  geom_point(Ed.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 3) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EDQ Locations")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom",
        strip.text = element_text(size = 10, colour = "black")) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


write.csv(Ed.df, file = "ED.df", row.names = FALSE)


MEd1 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = TRUE) 
dd1 = cbind(MEd1, seq(0,1,0.001))
v1 = NULL
for(i in unique(MEd1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}
MEd1.df = data.frame(longitude = Locations[unique(MEd1),2], latitude = Locations[unique(MEd1),1], Quantile = v1)

g.MEd1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEd1.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.MEd1




nr = rep(0, length(All.Gauge))
for(i in 1:length(All.Gauge)){
  tryCatch({
    nr[i] = nrow(All.Gauge[[i]])
  }, error = function(e){})
}


nl = NULL
nu = NULL
for(i in which(nr > 100)){
  tryCatch({
    nl = c(nl, paste(All.Gauge[[i]]$Year[1], All.Gauge[[i]]$Month[1], sep = "-"))
    nu = c(nu, paste(All.Gauge[[i]]$Year[nr[i]], All.Gauge[[i]]$Month[nr[i]], sep = "-"))
  }, error = function(e){})
}

used = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 156 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2003) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2015)){
      used = c(used, i)
    }
  }, error = function(e){})
}

n = 5

X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156 - 12 * n)
X[[2]] = matrix(0, nrow = length(used), ncol = 156 - 12 * n)
X[[3]] = matrix(0, nrow = length(used), ncol = 156 - 12 * n)
k = 1
for(i in used){
  for(j in 1:(156 - 12 * n)){
    X[[1]][k,j] = sum(All.Gauge[[i]]$Precipitation[j:(j + 12 * n - 1)])
    X[[2]][k,j] = sum(All.Gauge[[i]]$Jaxa[j:(j + 12 * n - 1)])
    X[[3]][k,j] = sum(All.Gauge[[i]]$Chirp[j:(j + 12 * n - 1)])
  }
  k = k + 1
}


Me1 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)


dd1 = cbind(Me1, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df = data.frame(Lat = Locations[unique(Me1),1], Lon = Locations[unique(Me1),2], Value = v1)

MEDQ.df1 = MEDQ.df[-4,]

g.Map = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df1, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Five Year Rolling MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Map



n = 2
MEDQ.list = list()
for(j in 1:(156 - n * 12)){
  
  X = list()
  X[[1]] = matrix(0, nrow = length(used), ncol = 12 * n)
  k = 1
  for(i in used){
    X[[1]][k,] = cumsum(All.Gauge[[i]]$Jaxa[j:(j + 12 * n - 1)])
    k = k + 1
  }
  
  
  Me1 = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)
  
  
  dd1 = cbind(Me1, seq(0,1,0.001))
  v1 = NULL
  for(i in unique(Me1)){
    v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
  }
  
  MEDQ.df = data.frame(Lat = Locations[unique(Me1),1], Lon = Locations[unique(Me1),2], Value = v1)
  
  MEDQ.list[[j]] = MEDQ.df
  
  print(j)
}

MEDQ.all = NULL
for(i in 1:length(MEDQ.list)){
  MEDQ.all = rbind(MEDQ.all, cbind(MEDQ.list[[i]], rep(i, nrow(MEDQ.list[[i]]))))
}

colnames(MEDQ.all) = c("Lat", "Lon", "Quantile", "Period")
MEDQ.all = as.data.frame(MEDQ.all)

write.csv(MEDQ.all, file = "MEDQAll3", row.names = FALSE)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.list[[21]], mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Five Year Rolling MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) 



nr = rep(0, length(All.Gauge))
for(i in 1:length(All.Gauge)){
  tryCatch({
    nr[i] = nrow(All.Gauge[[i]])
  }, error = function(e){})
}


nl = NULL
nu = NULL
for(i in which(nr > 100)){
  tryCatch({
    nl = c(nl, paste(All.Gauge[[i]]$Year[1], All.Gauge[[i]]$Month[1], sep = "-"))
    nu = c(nu, paste(All.Gauge[[i]]$Year[nr[i]], All.Gauge[[i]]$Month[nr[i]], sep = "-"))
  }, error = function(e){})
}

used = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 156 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2003) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2015)){
      used = c(used, i)
    }
  }, error = function(e){})
}

X1 = list()
X1[[1]] = matrix(0, nrow = length(used), ncol = 74)
X1[[2]] = matrix(0, nrow = length(used), ncol = 74)
X1[[3]] = matrix(0, nrow = length(used), ncol = 74)
k = 1
for(i in used){
  X1[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[83:156])
  X1[[2]][k,] = cumsum(All.Gauge[[i]]$Jaxa[83:156])
  X1[[3]][k,] = cumsum(All.Gauge[[i]]$Chirp[83:156])
  k = k + 1
}

X2 = list()
X2[[1]] = matrix(0, nrow = length(used), ncol = 82)
X2[[2]] = matrix(0, nrow = length(used), ncol = 82)
X2[[3]] = matrix(0, nrow = length(used), ncol = 82)
k = 1
for(i in used){
  X2[[1]][k,] = cumsum(All.Gauge[[i]]$Precipitation[1:82])
  X2[[2]][k,] = cumsum(All.Gauge[[i]]$Jaxa[1:82])
  X2[[3]][k,] = cumsum(All.Gauge[[i]]$Chirp[1:82])
  k = k + 1
}

Me1 = MEDQ(X1, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

Me2 = MEDQ(X2, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)


dd1 = cbind(Me1, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df1 = data.frame(Lat = Locations[unique(Me1),1], Lon = Locations[unique(Me1),2], Value = v1)


g.Map = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df1, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Five Year Rolling MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Map

dd1 = cbind(Me2, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me2)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df2 = data.frame(Lat = Locations[unique(Me2),1], Lon = Locations[unique(Me2),2], Value = v1)


g.Map = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df2, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Five Year Rolling MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Map



X.1 = list()
X.1[[1]] = t(scale(t(X[[1]])))
X.1[[2]] = t(scale(t(X[[2]])))
X.1[[3]] = t(scale(t(X[[3]])))

X.2 = list()
X.2[[1]] = X.1[[1]][,1:82]
X.2[[2]] = X.1[[2]][,1:82]
X.2[[3]] = X.1[[3]][,1:82]

X.3 = list()
X.3[[1]] = X.1[[1]][,82:156]
X.3[[2]] = X.1[[2]][,82:156]
X.3[[3]] = X.1[[3]][,82:156]

Me1 = MEDQ(X.2, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

Me2 = MEDQ(X.3, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

dd1 = cbind(Me1, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df1 = data.frame(Lat = Locations[unique(Me1),1], Lon = Locations[unique(Me1),2], Value = v1)


g.Map1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df1, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Five Year Rolling MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Map1

dd1 = cbind(Me2, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me2)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df2 = data.frame(Lat = Locations[unique(Me2),1], Lon = Locations[unique(Me2),2], Value = v1)


g.Map2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df2, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Five Year Rolling MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Map2




nr = rep(0, length(All.Gauge))
for(i in 1:length(All.Gauge)){
  tryCatch({
    nr[i] = nrow(All.Gauge[[i]])
  }, error = function(e){})
}


nl = NULL
nu = NULL
for(i in which(nr > 100)){
  tryCatch({
    nl = c(nl, paste(All.Gauge[[i]]$Year[1], All.Gauge[[i]]$Month[1], sep = "-"))
    nu = c(nu, paste(All.Gauge[[i]]$Year[nr[i]], All.Gauge[[i]]$Month[nr[i]], sep = "-"))
  }, error = function(e){})
}

used = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(nrow(All.Gauge[[i]]) >= 156 & sum(All.Gauge[[i]]$Month == 1 & All.Gauge[[i]]$Year == 2003) & sum(All.Gauge[[i]]$Month == 12 & All.Gauge[[i]]$Year == 2015)){
      used = c(used, i)
    }
  }, error = function(e){})
}

X = list()
X[[1]] = matrix(0, nrow = length(used), ncol = 156)
X[[2]] = matrix(0, nrow = length(used), ncol = 156)
k = 1
for(i in used){
  X[[1]][k,] = cumsum(All.Gauge[[i]]$Jaxa[1:156])
  X[[2]][k,] = cumsum(All.Gauge[[i]]$Chirp[1:156])
  k = k + 1
}


NJ.MEDQ = MEDQ(X, p = seq(0,1,0.001), weight = FALSE, scale = TRUE)


dd1 = cbind(NJ.MEDQ, seq(0,1,0.001))
v1 = NULL
for(i in unique(NJ.MEDQ)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

NJ.MEDQdf = data.frame(longitude = Locations[unique(NJ.MEDQ),2], latitude = Locations[unique(NJ.MEDQ),1], Quantile = v1)

g.EDQ1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(NJ.MEDQdf, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.EDQ1














