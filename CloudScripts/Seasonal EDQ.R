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

m01 = seq(1,nrow(dates2),12)
m02 = seq(2,nrow(dates2),12)
m03 = seq(3,nrow(dates2),12)
m04 = seq(4,nrow(dates2),12)
m05 = seq(5,nrow(dates2),12)
m06 = seq(6,nrow(dates2),12)
m07 = seq(7,nrow(dates2),12)
m08 = seq(8,nrow(dates2),12)
m09 = seq(9,nrow(dates2),12)
m10 = seq(10,nrow(dates2),12)
m11 = seq(11,nrow(dates2),12)
m12 = seq(12,nrow(dates2),12)

M = list()
M[[1]] = m01
M[[2]] = m02
M[[3]] = m03
M[[4]] = m04
M[[5]] = m05
M[[6]] = m06
M[[7]] = m07
M[[8]] = m08
M[[9]] = m09
M[[10]] = m10
M[[11]] = m11
M[[12]] = m12

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/WorldPrecip")

precip2 = list()
for(i in 1:nrow(dates2)){
  a1 = dates2[i,2]
  if(nchar(a1) == 1){
    a1 = paste0("0", a1)
  }
  file.name = paste0("Precip.reduced.", dates2[i,1], ".", a1)
  precip2[[i]] = read.csv(file.name , header = TRUE)
  print(i)
}

M.all = matrix(0, nrow = nrow(precip2[[1]]), ncol = length(precip2))
for(i in 1:length(precip2)){
  M.all[,i] = precip2[[i]][,3]
  if(i %% 100 == 0){
    print(i)
  }
}

setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines")

precip3 = list()
for(i in 1:length(precip2)){
  precip3[[i]] = precip2[[i]][-which(rowSums(is.na(M.all)) > 0),]
  print(i)
}

require(rgdal)
shape <- readOGR(dsn = ".", layer = "TM_WORLD_BORDERS-0.3")

A1 = precip3[[1]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

Aus.all = matrix(0, nrow = length(Aus), ncol = length(precip3))
for(i in 1:length(precip3)){
  Aus.all[,i] = precip3[[i]][Aus,3]
  if(i %% 100 == 0){
    print(i)
  }
}


Aus.all.s = Aus.all
for(i in 1:length(M)){
  a1 = Aus.all[,M[[i]]]
  a2 = scale(t(a1))
  Aus.all.s[,M[[i]]] = t(a2)
}


X.list = list()
set.seed(1998)
samp1 = sort(sample(1:nrow(Aus.all), 1000))
X.list[[1]] = Aus.all[samp1,M[[3]]]

p = seq(0,1, 0.005)
M2 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis")
ts.plot(t(X.list[[1]]), xlab = "Years from 1980", ylab = "Precipitation (mm)")
for(i in 1:length(unique(M2))){
  lines(X.list[[1]][unique(M2)[i], ], col = i)
}

df = as.data.frame(X.list[[1]])

df1 = matrix(0, ncol = 3, nrow = ncol(df) * nrow(df))
for(i in 1:ncol(df)){
  df1[((i*1000):((i + 1)*1000 - 1)) - 999,] = cbind(df[,i], 1980 + i, 1:nrow(df))
}
colnames(df1) = c("Rain", "Year", "Group") 
df1 = as.data.frame(df1)

dd1 = cbind(M2, p)
v1 = NULL
for(i in unique(M2)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

df2 = matrix(0, nrow = ncol(df) * length(unique(M2)), ncol = 4)
for(i in 1:ncol(df)){
  df2[((i*length(unique(M2))):((i + 1)*length(unique(M2)) - 1)) - length(unique(M2)) + 1,] = cbind(df[unique(M2),i],
      1980 + i, 1:nrow(df[unique(M2), ]), v1)
}
colnames(df2) = c("Rain", "Year", "Group", "value") 
df2 = as.data.frame(df2)

g.Jan = ggplot(data = df1, mapping = aes(x = Year, y = Rain, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Precipitation (mm)",
       title = "Australian December Precipitation Time Series") +
  geom_line(data = df2, mapping = aes(x = Year, y = Rain, colour = value), size = 1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))
  
g.Jan

#ausmap = shape[shape$ISO3 =="AUS",]
#ausprec = precip3[[2]][Aus, ]
#ausprec1 = ausprec[,1:2]

ausmedq1 = data.frame(Lat = ausprec1$Lat[samp1[M2]], Lon = ausprec1$Lon[samp1[M2]], value = p, M = M2)

ausmedq2 = matrix(0, ncol = 3, nrow = length(unique(M2)))
k = 1
for(i in unique(M2)){
  a1 = ausmedq1[ausmedq1[,4] ==i, ]
  a2 = as.numeric(c(a1[1,1:2], max(a1[,3])))
  ausmedq2[k,] = a2
  k = k + 1
}
ausmedq2 = as.data.frame(ausmedq2)
colnames(ausmedq2) = c("Lat", "Lon", "Value")

medqm01 <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = ausmedq2, 
             mapping = aes(x = Lon, y = Lat, colour = Value), size = 3)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Quantile", x = "Longitude", y = "Latitude",
       title = "December Empirical Dynamic Quantile Locations") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

medqm01

#Austra1 = read.csv("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid202001")

#A1 = Austra1[,2:1]
#A1 = as.data.frame(A1)
#coordinates(A1) =~Lon + Lat
#proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#a3 = over(A1, shape)

#Aus1 = which(a3$ISO3 == "AUS")

pred.grid = data.frame(Lon = Austra1$Lon[Aus1], Lat = Austra1$Lat[Aus1])
idw1 = idw(formula = Value ~ 1, locations = ~Lon + Lat, data = ausmedq2, newdata = pred.grid, idp = 3)


medqidw <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Quantile", x = "Longitude", y = "Latitude",
       title = "March Empirical Dynamic Quantile Interpolation") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

medqidw











grid.arrange(g.Jan + theme(legend.position = "none") + labs(x = " "), medqm01 + theme(legend.position = "none") +
               labs(x = " ", title = "January Empirical Dynamic Quantile Locations"),
             g.Jun + theme(legend.position = "none"), medqm06 + theme(legend.position = "none") +
               labs(title = "June Empirical Dynamic Quantile Locations"))


map <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), fill = NA) +
  geom_point(data = ausprec, 
             mapping = aes(x = Lon, y = Lat, colour = Rain)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
map






