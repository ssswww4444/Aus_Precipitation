#################
#Koppen and Rainfall zones
#################

seas1 = read.csv("https://raw.githubusercontent.com/hinestein/Koppenandrainfallzones/main/seasrainmajor.txt", header = FALSE, sep = " ")

nr = 139
nc = 178

xll = 111.875
yll = -44.625

cs = 0.25

x.vec = seq(xll, length.out = nc, by = cs)
y.vec = rev(seq(yll, length.out = nr, by = cs))


seas.mat = matrix(0, ncol = 3, nrow = nr * nc)
k = 1
for(i in 1:nr){
  for(j in 1:nc){
    seas.mat[k,] = c(seas1[i,j], y.vec[i], x.vec[j])
    k = k + 1
  }
}

colnames(seas.mat) = c("Class", "Lat", "Lon")
seas.mat = as.data.frame(seas.mat)

seas.mat= seas.mat[seas.mat$Class != -9999,]



A1 = seas.mat[,3:2]
coordinates(A1) = ~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus.1 = which(a3$ISO3 == "AUS")

seas.mat = seas.mat[Aus.1,]


sc = factor(ifelse(seas.mat$Class == 1 | seas.mat$Class == 2| seas.mat$Class == 3, "Summer Dominant",
            ifelse(seas.mat$Class == 4 | seas.mat$Class == 8 | seas.mat$Class == 12 | seas.mat$Class == 20, "Arid",
                   ifelse(seas.mat$Class == 5 | seas.mat$Class == 6 | seas.mat$Class == 7, "Summer",
                          ifelse(seas.mat$Class == 9 | seas.mat$Class == 10 | seas.mat$Class == 11, "Uniform",
                                 ifelse(seas.mat$Class == 13 | seas.mat$Class == 14 | seas.mat$Class == 15, "Winter Dominant",
                                        ifelse(seas.mat$Class == 16 | seas.mat$Class == 17| seas.mat$Class == 18 | seas.mat$Class == 19, "Winter", NA)))))),
            levels = c("Summer Dominant", "Summer", "Uniform", "Winter", "Winter Dominant", "Arid"))

seas.mat$C1 = sc

ggplot() + 
  geom_point(seas.mat[seas.mat$Class == 20,], mapping = aes(x = Lon, y = Lat, colour = Class), size = 0.5)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Square Root Precipitation (mm)^(1/2)")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.SM = ggplot() + 
  geom_point(seas.mat, mapping = aes(x = Lon, y = Lat, colour = C1), size = 0.5)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "Major Rainfall Zones")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(0.6, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)+
  guides(colour = guide_legend(override.aes = list(size=8)))+ scale_colour_manual(
    values = c("#e9c6c7","#f7e697", "#c6d79f", "#d2d9e5", "#7cb2de", "#f6fae4"),
    aesthetics = c("colour", "fill")
  )







sA = read.csv("https://raw.githubusercontent.com/hinestein/Koppenandrainfallzones/main/seasrainall.txt", header = FALSE, sep = " ")


nr = 139
nc = 178

xll = 111.875
yll = -44.625

cs = 0.25

x.vec = seq(xll, length.out = nc, by = cs)
y.vec = rev(seq(yll, length.out = nr, by = cs))


sA.mat = matrix(0, ncol = 3, nrow = nr * nc)
k = 1
for(i in 1:nr){
  for(j in 1:nc){
    sA.mat[k,] = c(sA[i,j], y.vec[i], x.vec[j])
    k = k + 1
  }
}

colnames(sA.mat) = c("Class", "Lat", "Lon")
sA.mat = as.data.frame(sA.mat)

sA.mat= sA.mat[sA.mat$Class != -9999,]

A1 = sA.mat[,3:2]
coordinates(A1) = ~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus.1 = which(a3$ISO3 == "AUS")

sA.mat = sA.mat[Aus.1,]

idw1 = gstat::idw(formula = Class ~ 1, locations = ~Lon + Lat, data = sA.mat, newdata = pred.aus1, idp = 3)

sA.mat = idw1
colnames(sA.mat) = c("Lon", "Lat", "Class", "Var")
sA.mat$Class = round(sA.mat$Class)


sc = factor(ifelse(sA.mat$Class == 1, "SD: High",
                   ifelse(sA.mat$Class == 2, "SD: Medium",
                          ifelse(sA.mat$Class == 3, "SD: Low",
                                 ifelse(sA.mat$Class == 4 | sA.mat$Class == 8 | sA.mat$Class == 12 | sA.mat$Class == 20 |sA.mat$Class == 16, "Arid",
                                        ifelse(sA.mat$Class == 5, "S: High",
                                               ifelse(sA.mat$Class == 6, "S: Medium", 
                                                      ifelse(sA.mat$Class == 7, "S: Low",
                                                             ifelse(sA.mat$Class == 9, "U: High",
                                                                    ifelse(sA.mat$Class == 10, "U: Medium",
                                                                           ifelse(sA.mat$Class == 11, "U: Low",
                                                                                  ifelse(sA.mat$Class == 13, "WD: High",
                                                                                         ifelse(sA.mat$Class == 14, "WD: Medium",
                                                                                                ifelse(sA.mat$Class == 15, "WD: Low",
                                                                                                       ifelse(sA.mat$Class == 17, "W: High",
                                                                                                              ifelse(sA.mat$Class == 18, "W: Medium",
                                                                                                                     ifelse(sA.mat$Class == 19, "W: Low", NA)))))))))))))))),
            levels = c("SD: High", "SD: Medium", "SD: Low", "S: High", "S: Medium", "S: Low", "U: High", "U: Medium", "U: Low",
                       "W: High", "W: Medium", "W: Low", "WD: High", "WD: Medium", "WD: Low", "Arid"))

sA.mat$C1 = sc

g.SA = ggplot() + 
  geom_point(sA.mat, mapping = aes(x = Lon, y = Lat, colour = C1), size = 0.75)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "All Rainfall Zones")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(0.6, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)+
  guides(colour = guide_legend(override.aes = list(size=8)))+ scale_colour_manual(
    values = c("#d891a1","#ebccce", "#f2e7d7", "#f7b300", "#f8e699", "#f7f6d6", "#cdd79e",
               "#ebeed1", "#eff1e7", "#dcdee8", "#f0e9f0", "#eff0f4", "#8ab7e0", "#c7dee4", "#eaf2f5", "#f9f9e7"),
    aesthetics = c("colour", "fill")
  ) 





kop = read.csv("https://raw.githubusercontent.com/hinestein/Koppenandrainfallzones/main/kpngrp.txt", header = FALSE, sep = " ")


nr = 1361
nc = 1681

xll = 112
yll = -44

cs = 0.025

x.vec = seq(xll, length.out = nc, by = cs)
y.vec = rev(seq(yll, length.out = nr, by = cs))


kop.mat = matrix(0, ncol = 3, nrow = nr * nc)
k = 1
for(i in 1:nr){
  for(j in 1:nc){
    kop.mat[k,] = c(kop[i,j], y.vec[i], x.vec[j])
    k = k + 1
  }
}

colnames(kop.mat) = c("Class", "Lat", "Lon")
kop.mat = as.data.frame(kop.mat)

kop.mat= kop.mat[kop.mat$Class != -9999,]


A1 = kop.mat[,3:2]
coordinates(A1) = ~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus.1 = which(a3$ISO3 == "AUS")

kop.mat = kop.mat[Aus.1,]

sc = factor(ifelse(kop.mat$Class == 1, "Temperate",
                   ifelse(kop.mat$Class == 2, "Temperate",
                          ifelse(kop.mat$Class == 3, "Temperate",
                                 ifelse(kop.mat$Class == 4, "Temperate",
                                        ifelse(kop.mat$Class == 5, "Temperate",
                                               ifelse(kop.mat$Class == 6, "Temperate", 
                                                      ifelse(kop.mat$Class == 7, "Temperate",
                                                             ifelse(kop.mat$Class == 8, "Temperate",
                                                                    ifelse(kop.mat$Class == 9, "Temperate",
                                                                           ifelse(kop.mat$Class == 11, "Grassland",
                                                                                  ifelse(kop.mat$Class == 12, "Grassland",
                                                                                         ifelse(kop.mat$Class == 13, "Grassland",
                                                                                                ifelse(kop.mat$Class == 14, "Grassland",
                                                                                                       ifelse(kop.mat$Class == 15, "Grassland",
                                                                                                              ifelse(kop.mat$Class == 21, "Grassland",
                                                                                                                     ifelse(kop.mat$Class == 22, "Desert",
                                                                                                                            ifelse(kop.mat$Class == 23, "Desert",
                                                                                                                                   ifelse(kop.mat$Class == 24, "Desert",
                                                                                                                                          ifelse(kop.mat$Class == 31, "Subtropical",
                                                                                                                                                 ifelse(kop.mat$Class == 32, "Subtropical",
                                                                                                                                                        ifelse(kop.mat$Class == 33, "Subtropical",
                                                                                                                                                               ifelse(kop.mat$Class == 34, "Subtropical",
                                                                                                                                                                      ifelse(kop.mat$Class == 35, "Tropical",
                                                                                                                                                                             ifelse(kop.mat$Class == 36, "Tropical",
                                                                                                                                                                                    ifelse(kop.mat$Class == 37, "Tropical",
                                                                                                                                                                                           ifelse(kop.mat$Class == 41, "Equatorial", 
                                                                                                                                                                                                  ifelse(kop.mat$Class == 42, "Equatorial", NA))))))))))))))))))))))))))),
            levels = c("Equatorial", "Tropical", "Subtropical", "Desert", "Grassland", "Temperate"))

kop.mat$C1 = sc

g.kop = ggplot() +
  geom_point(kop.mat, mapping = aes(x = Lon, y = Lat, colour = C1), size = 0.2) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "Köppen Climate Classification")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(0.6, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)+
  guides(colour = guide_legend(override.aes = list(size=8)))+ scale_colour_manual(
    values = c("#b69534", "#8baf3f", "#d8e9b4", "#f79760", "#ffe493", "#63caf3"),
    aesthetics = c("colour", "fill")
  )


kc = ifelse(kop.mat$Class)

kmeansplot1 = ggplot() + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = f1), size = 0.1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "k Means Clustering")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=12),
        legend.key.size = unit(0.6, "cm"), axis.title=element_text(size=16), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)+
  guides(colour = guide_legend(override.aes = list(size=8)))+ scale_colour_manual(
    values = c("#d891a1","#ebccce", "#f2e7d7", "#f8e699", "#7cb2de", "#f9f9e7"),
    aesthetics = c("colour", "fill")
  )+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

g.SM = ggplot() + 
  geom_point(seas.mat, mapping = aes(x = Lon, y = Lat, colour = C1), size = 0.5)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "Major Rainfall Zones")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)+
  guides(colour = guide_legend(override.aes = list(size=10), ncol = 2))+ scale_colour_manual(
    values = c("#e9c6c7","#f7e697", "#c6d79f", "#d2d9e5", "#7cb2de", "#f6fae4"),
    aesthetics = c("colour", "fill")
  )

g.SA = ggplot() + 
  geom_point(sA.mat, mapping = aes(x = Lon, y = Lat, colour = C1), size = 0.75)+ 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "All Rainfall Zones")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)+
  guides(colour = guide_legend(override.aes = list(size=10), ncol = 3))+ scale_colour_manual(
    values = c("#d891a1","#ebccce", "#f2e7d7", "#f7b300", "#f8e699", "#f7f6d6", "#cdd79e",
               "#ebeed1", "#eff1e7", "#dcdee8", "#f0e9f0", "#eff0f4", "#8ab7e0", "#c7dee4", "#eaf2f5", "#f9f9e7"),
    aesthetics = c("colour", "fill")
  ) 

g.kop = ggplot() +
  geom_point(kop.mat, mapping = aes(x = Lon, y = Lat, colour = C1), size = 0.2) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", colour = "Class", title = "Köppen Climate Classification")+ 
  theme(plot.title = element_text(size = 22, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "bottom", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)+
  guides(colour = guide_legend(override.aes = list(size=10), ncol = 2))+ scale_colour_manual(
    values = c("#b69534", "#8baf3f", "#d8e9b4", "#f79760", "#ffe493", "#63caf3"),
    aesthetics = c("colour", "fill")
  )

grid.arrange(g.SM + labs(x = " ") + theme(plot.margin = margin(0, 0, 3, 0.2, "cm")),
             g.SA + labs(y = " ", colour = " ") + theme(plot.margin = margin(0, 0.2, 0, 0, "cm")),
             g.kop + labs(x = " ", y = " ", colour = " ") + theme(plot.margin = margin(0, 0.2, 3, 0, "cm")), nrow = 1)
















