#Function that calculates the pth multivariate empirical dynamic quantile
#Inputs are X.list, p and method
#X.list is a list of nxT matrices for n time-series at T time intervals where each list entry is represents another dimension
#p is a point or a vector of quantiles to calculate, default is p = 0.5
#Method is determines which method of depth used. Can be Mahalanobis, Tukey, Liu or Oja.
#For dimensions greater than 2, Mahalanobis is by far the fastest
#Output is a point or vector row numbers for the pth quantile
#Can work for one dimensional time-series data
#Data set for each variable can be scaled in the function

MEDQ = function(X.list,p = 0.5, method = "Mahalanobis", scale = TRUE){
  d = length(X.list)
  if(!inherits(X.list,"list"))
  nr = NULL
  for(i in 1:d){
    nr = c(nr, nrow(X.list[[i]]))
  }
  if(length(unique(nr)) > 1){
    stop("Dimensions are not equal")
  }
  nr = nr[1]
  nc = NULL
  for(i in 1:d){
    nc = c(nc, ncol(X.list[[i]]))
  }
  nc = nc[1]
  if(length(unique(nc)) > 1){
    stop("Dimension are not equal")
  }
  if(max(p) > 1 | min(p) < 0){
    stop("p must be between 0 and 1")
  }
  if(scale){
    for(i in 1:d){
      X.list[[i]] = scale(X.list[[i]])
    }
  }
  if(method == "Mahalanobis"){
    depth3 = NULL
    for(i in 1:nc){
      my_sample <- NULL
      for(j in 1:d){
        my_sample = cbind(my_sample, X.list[[j]][,i])
      }
      mu <- apply(my_sample,2,mean)
      sigma <- cov(my_sample)
      m_dist <- mahalanobis(my_sample, mu, sigma)
      m_depth <- 1/(1 + m_dist)
      depth3 = cbind(depth3, m_depth)
    }
  }else{
    depth3 = matrix(0, nrow = nr, ncol = nc)
    for(i in 1:nc){
      my_sample <- NULL
      for(j in 1:d){
        my_sample = cbind(my_sample, X.list[[j]][,i])
      }
      for(j in 1:nr){
        depth3[j,i] = depth(my_sample[j,], my_sample, method = method)
      }
    }
  }
  depth.dash = matrix(0, nrow = nr, ncol = nc)
  for(i in 1:nc){
    x1 <- NULL
    for(j in 1:d){
      x1 = cbind(x1, X.list[[j]][,i])
    }
    zt = which(depth3[,i] == max(depth3[,i]))[1]
    sigma <- cov(x1)
    EVV=eigen(sigma)
    vec=EVV$vectors
    for(j in 1:nrow(depth3)){
      z1 = x1[zt,d]
      if(d > 1){
        for(k in 1:(d-1)){
          z1 = z1 - (vec[k,1]/vec[d,1]) * (x1[j,k] - x1[zt,k])
        }
      }else{
        z1 = z1
      }
      if(x1[j,d] < z1){
        depth.dash[j,i] = depth3[j,i]
      }else{
        depth.dash[j,i] = 2 * depth3[zt,i] - depth3[j,i]
      }
    }
  }
  dist.1 = list()
  for(i in 1:nc){
    x1 <- NULL
    for(j in 1:d){
      x1 = cbind(x1, X.list[[j]][,i])
    }
    dist.1[[i]] = as.matrix(dist(x1, upper = TRUE, diag = TRUE))
  }
  n.3 = NULL
  for(k in p){
    n.2 = NULL
    for(i in 1:nr){
      n.1 = 0
      for(j in 1:nc){
        n.1 = n.1 + k * dist.1[[j]][,i] %*% (depth.dash[,j] > depth.dash[i,j]) + (1 - k) * dist.1[[j]][,i] %*% (1 - (depth.dash[,j] > depth.dash[i,j]))
      }
      n.2 = c(n.2, n.1)
    }
    n.3 = c(n.3, which(n.2 == min(n.2)))
  }
  n.3
}

###Entropy Idea
substr(Sys.time(),0,10)
curr.year = as.numeric(substr(Sys.time(),0,4))
curr.month = as.numeric(substr(Sys.time(),6,7))
curr.day = as.numeric(substr(Sys.time(), 9,10))

years1 = c(rep(1979:(curr.year - 1), each = 12), if(curr.month != 1){rep(curr.year, curr.month - 1)})

months1 = c(rep(1:12, length(1979:(curr.year - 1))), if(curr.month != 1){1:(curr.month - 1)})
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

m01 = seq(1,nrow(dates2) - 1,12)
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


NOAA.all.aus = matrix(0, nrow = nrow(NOAA.aus[[1]]), ncol = length(NOAA.aus))
for(i in 1:length(NOAA.aus)){
  NOAA.all.aus[,i] = NOAA.aus[[i]][,3]
  if(i %% 100 == 0){
    print(i)
  }
}

library("entropy")

P.list = list()
for(i in 1:nrow(NOAA.all.aus)){
  P1 = matrix(NOAA.all.aus[i, 1:(floor(ncol(NOAA.all.aus)/12) * 12)], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12, byrow = TRUE)
  P1[P1 == 0] = 10^(-16)
  P2 = t(apply(P1,1,div1))
  P.list[[i]] = P2
}

E1 = matrix(0, nrow = length(P.list), ncol = nrow(P.list[[1]]))

for(i in 1:length(P.list)){
  E1[i,] = apply(P.list[[i]], 1, entropy1)
}

div1 = function(x){
  if(sum(x) > 0){
    a = x/sum(x)
  }else{
    a = 0
  }
  a
}

Aus.Season = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor((ncol(NOAA.all.aus)-2)/12) * 4)
Aus.1 = NOAA.all.aus[,-c(1,2)]
for(i in 1:ncol(NOAA.all.aus)){
  Aus.Season[,i] = rowSums(Aus.1[, 1 + ((i-1)*3):((i-1)*3 + 2)])
}

Melb2 = matrix(Aus.Season[Melb,], ncol = 4, byrow = TRUE)
Dar2 = matrix(Aus.Season[Dar,], ncol = 4, byrow = TRUE)

library(scales)

df.H = data.frame(Dar1 = Dar2[,2]/rowSums(Dar2), Dar2 = Dar2[,4]/rowSums(Dar2),
                  Melb1 = Melb2[,2]/rowSums(Melb2), Melb2 = Melb2[,4]/rowSums(Melb2))

g.dar1 = ggplot(df.H, mapping = aes(x = Dar1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Darwin Winter") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90)) + 
  scale_y_continuous(breaks=c(0,2.5, 5, 7.5, 10)) + xlim(0, max(df.H$Dar1))
g.dar2 = ggplot(df.H, mapping = aes(x = Dar2)) + geom_histogram(bins = 12, color="black", fill="white")+
  labs(x = "Proportion", y = "Count", title = "Darwin Summer") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb1 = ggplot(df.H, mapping = aes(x = Melb1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne Winter") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb2 = ggplot(df.H, mapping = aes(x = Melb2)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne Summer") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))

grid.arrange (g.dar1 + labs(x = "", y = ""), g.dar2 + labs(x = "", y = ""),
              g.Melb1 + labs(x = "", y = ""), g.Melb2 + labs(x = "", y = ""),
              bottom = textGrob("Proportion", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3),
              left = textGrob("Count", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))
x <- seq(64, 74, length.out=100)
df <- with(Galton, data.frame(x = x, y = dnorm(x, mean(parent), sd(parent))))

g.dar1 


E4 = matrix(0, nrow = nrow(Aus.all), ncol = 40)
for(i in 1:40){
  E2 = t(apply(Aus.Season[,((i - 1) * 4 + 1):(i * 4)], 1, div1))
  E4[,i] = apply(E2, 1, entropy)
}


E1 = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12))
for(i in 1:floor(ncol(NOAA.all.aus)/12)){
  E3 = t(apply(NOAA.all.aus[,((i - 1) * 12 + 1):(i * 12)], 1, div1))
  E1[,i] = apply(E3, 1, entropy1)
}

X.list = list()
set.seed(1998)
samp1 = sample(1:nrow(E1), 1000)
X.list[[1]] = E1[samp1,]

p = seq(0,1,0.01)
M1 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis")

m1 = 6
m2 = 1981:2019

mdates = paste(m2,m1, "15", sep = "-")

TS.dates = rep(as.Date(mdates), nrow(Aus.all[samp1,]))

TS.E = as.vector(t(E1[samp1,]))
TS.group = rep(1:nrow(E1[samp1,]), each = 39)

df.E = data.frame(Dates = TS.dates, Entropy = TS.E, Group = TS.group)

dd1 = cbind(M1, p)
v1 = NULL
for(i in unique(M1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


TS.dates1 = rep(as.Date(mdates), length(unique(M1)))
TS.E1 = as.vector(t((E1[samp1,])[unique(M1), ]))
TS.group1 = rep(1:nrow((E1[samp1,])[unique(M1), ] ), each = 39)
TS.value1 = rep(v1, each = 39)

df.E1 = data.frame(Dates = TS.dates1, Entropy = TS.E1, Group = TS.group1, Value = TS.value1)


g.E = ggplot(data = df.E, mapping = aes(x = Dates, y = Entropy, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Precipitation (mm)",
       title = "Rain Gauge") +
  geom_line(data = df.E1, mapping = aes(x = Dates, y = Entropy, colour = Value), size = 1.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))
g.E

ar.loc = precip3[[1]][Aus,1:2]
ar.loc1 = ar.loc[samp1,]

ausmedq = data.frame(Lat = ar.loc1[unique(M1),1], Lon = ar.loc1[unique(M1),2], Value = v1)
medqm01 <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = ausmedq, 
             mapping = aes(x = Lon, y = Lat, colour = Value), size = 3)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Quantile", x = "Longitude", y = "Latitude",
       title = "MEDQ Locations") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "bottom")

grid.arrange(g.Gauge + theme(legend.position = "none"), medqm01 + theme(legend.position = "none"), g.Jaxa + theme(legend.position = "none"),
             g.Chirp +theme(legend.position = "none"))

Skew = function(X){
  mu = mean(X)
  n = length(X)
  num = (1/n) * sum((X - mu)^3)
  den = (1/(n-1)) * sum((X - mu)^2)
  Skew = num/(den)^(3/2)
  return(Skew)
}


E.mean = rowMeans(E1)
E.mean1 = rowMeans(E1[,1:21])
E.mean2 = rowMeans(E1[,22:42])
E.V = apply(E1, 1, var)
Austra1 = read.csv("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid202001")

A1 = Austra1[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus1 = which(a3$ISO3 == "AUS")

d1 = data.frame(Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon, E1 = E.mean1, E2 = E.mean2, E = E.mean, V = E.V)
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = E1 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = E2 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw3 = idw(formula = E ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw4 = idw(formula = E.V ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw5 = idw(formula = E.skew ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
E.df = data.frame(E1 = idw1$var1.pred, E2 = idw2$var1.pred, E = idw3$var1.pred, V = idw4$var1.pred, Lon = idw1$Lon, Lat = idw1$Lat)

E.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = E), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mean", x = "Longitude", y = "Latitude",
       title = "Australian Entropy Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left")

V.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = V), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Variance", x = "Longitude", y = "Latitude",
       title = "Australian Entropy Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

Ed.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = E1 - E2), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Difference", x = "Longitude", y = "Latitude",
       title = "Australian Monthly Entropy Mean Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

S.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = Skew), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Skewness", x = "Longitude", y = "Latitude",
       title = "Australian Entropy Skewness") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")



grid.arrange(E.plot + labs(x = " ", y = " "), V.plot + labs(x = " ", y = " "), nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0.15))


Aus.diff = matrix(0, nrow = nrow(NOAA.all.aus), ncol = 2)
Aus.diff[,1] = rowSums(NOAA.all.aus[,1:(floor(ncol(NOAA.all.aus)/12) * 6)])/12
Aus.diff[,2] = rowSums(NOAA.all.aus[,((floor(ncol(NOAA.all.aus)/12) * 6) + 1):(floor(ncol(NOAA.all.aus)/12) * 12)])/12

Aus.diff = cbind(Aus.diff, NOAA.aus[[1]]$Lat, NOAA.aus[[1]]$Lon)
colnames(Aus.diff) = c("Precip1", "Precip2", "Lat", "Lon")
Aus.diff = as.data.frame(Aus.diff)

idw1 = idw(formula = Precip1 ~ 1, locations = ~Lon + Lat, data = Aus.diff, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Precip2 ~ 1, locations = ~Lon + Lat, data = Aus.diff, newdata = pred.grid, idp = 3)

PD.df = data.frame(Precip1 = idw1$var1.pred, Precip2 = idw2$var1.pred, Lat = idw1$Lat, Lon = idw1$Lon)

Diff.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = PD.df, 
             mapping = aes(x = Lon, y = Lat, colour = Precip1 - Precip2), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Difference\n (mm)", x = "Longitude", y = "Latitude",
       title = "Australian Mean Precipitation Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left")

grid.arrange(Diff.plot + labs(x = " ", y = " "), Ed.plot + labs(x = " ", y = " "), nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0.15))


####Darwin V Melbourne

del = 0.5
Melb = which(NOAA.aus[[1]][,1] < -37.8 + del & NOAA.aus[[1]][,1] > -37.8 - del & NOAA.aus[[1]][,2] < 144.96 + del & NOAA.aus[[1]][,2] > 144.96 - del)[1]
Dar =  which(NOAA.aus[[1]][,1] < -12.397 + del & NOAA.aus[[1]][,1] > -12.397 - del & NOAA.aus[[1]][,2] < 130.94 + del & NOAA.aus[[1]][,2] > 130.94 - del)[1]

Melb.all = matrix(Aus.all[Melb,c(1:(floor(ncol(Aus.all)/12) * 12))], ncol = 12, nrow = floor(ncol(Aus.all)/12), byrow = TRUE)
Dar.all = matrix(Aus.all[Dar,c(1:(floor(ncol(Aus.all)/12) * 12))], ncol = 12, nrow = floor(ncol(Aus.all)/12), byrow = TRUE)

Melb1 = Aus.all[Melb,]
Dar1 = Aus.all[Dar,]
m2= datesNOAA[,1]
m1 = datesNOAA[,2]

Melb2 = matrix(Melb1[1:(floor(ncol(Aus.all)/12)*12)], nrow = floor(ncol(Aus.all)/12) * 12, byrow = TRUE)

Dar2 = matrix(Dar1[1:(floor(ncol(Aus.all)/12)*12)], nrow = floor(ncol(Aus.all)/12) * 12, byrow = TRUE)

mean(rowSums(Dar2))

P1 = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12)*12)
for(i in 1:floor(ncol(NOAA.all.aus)/12)){
  P2 = t(apply(NOAA.all.aus[,((i - 1) * 12 + 1):(i * 12)], 1, div1))
  P1[,((i - 1) * 12 + 1):(i * 12)] = P2
}

Melb1 = P1[Melb,]
Dar1 = P1[Dar,]

mdates = paste(m2,m1, "15", sep = "-")
TS.dates = rep(as.Date(mdates[1:(floor(ncol(NOAA.all.aus)/12)*12)]), 2)

TS.Ent = c(rep(E1[Melb,], each = 12), rep(E1[Dar,], each = 12))

TS.P = c(Melb1, Dar1)
TS.group = c(rep("Melbourne", length(Melb1)), rep("Darwin", length(Dar1)))

df.P = data.frame(Dates = TS.dates, Precipitation = TS.P, Group = as.factor(TS.group), Value = TS.Ent)




g.P = ggplot(data = df.P, mapping = aes(x = Dates, y = Precipitation)) + geom_line(aes(colour = TS.Ent), size = 1.2) +
  labs(colour = "Entropy", x = "Year", y = "Proportion", title = "Yearly Proportion Time-Series") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 20, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 16, colour = "black", angle = -90)) + facet_grid(rows = vars(Group))
g.P


E.ts = matrix(0, nrow = nrow(E1), ncol = 3)
t1 = 1981:2020
for(i in 1:nrow(E1)){
  mod1 = lm(E1[i,] ~ t1)
  x = summary(mod1)
  E.ts[i,] = c(mod1$coefficients, x$coefficients[2,4])
}
mean(E.ts[,3] < 0.05)


E.ts1 = matrix(0, nrow = nrow(E1), ncol = 3)
t1 = 1981:2020
for(i in 1:nrow(E1)){
  mod1 = lm(E1[i,] ~ t1)
  x = summary(mod1)
  E.ts1[i,] = c(mod1$coefficients, x$coefficients[2,4])
}

mean(E.ts1[,3] < 0.05)

p.val1 = rep(0, nrow(E1))
p.val2 = rep(0, nrow(E1))
for(i in 1:nrow(E1)){
  t1 = t.test(E1[i,1:21], E1[i,22:42], alternative = "two.sided", paired = FALSE, var.equal = FALSE)
  v1 = var.test(E1[i,1:21], E1[i,22:42])
  p.val1[i] = t1$p.value
  p.val2[i] = v1$p.value
}

m.beta.e.1 = function(alpha.vec){
  out = 0.5 - (length(alpha.vec)/2) * m.beta.e(alpha.vec) + log(length(alpha.vec))
  return(out)
}

m.beta.v.1 = function(alpha.vec){
  out = length(alpha.vec)^2 / 4 * m.beta.v(alpha.vec)
  return(out)
}

p.val1 = rep(0, nrow(E1))
p.val2 = rep(0, nrow(E1))
t.stat = rep(0, nrow(E1))
for(i in 1:nrow(E1)){
  d1 = dirichlet.mle(P.list[[i]][1:21,])$alpha
  t.stat[i] = (mean(E1[i,22:42]) - m.beta.e.1(d1))/sqrt((m.beta.v.1(d1)/nrow(P.list[[i]][1:21,])))
}

mean(abs(t.stat) > qnorm(0.975), na.rm = TRUE)

Ent.z = data.frame(Ext = (abs(t.stat) > qnorm(0.975)), Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat)
Ent.z = Ent.z[complete.cases(Ent.z),]


idw1 = idw(formula = Ext ~ 1, locations = ~Lon + Lat, data = Ent.z, newdata = pred.grid, idp = 10)
idw1 = idw1[idw1$var1.pred < 0.1, ]

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw.df, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "Asymptotic Z Test") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none",
        strip.text.x = element_text(size = 14, colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size=10)))


Entdiff.df = data.frame(Mean = p.val1, Var = p.val2[!duplicated(NOAA.aus[[1]][,1:2])],
                        Lon = NOAA.aus[[1]]$Lon[!duplicated(NOAA.aus[[1]][,1:2])],
                        Lat = NOAA.aus[[1]]$Lat[!duplicated(NOAA.aus[[1]][,1:2])])


idw1 = idw(formula = Mean ~ 1, locations = ~Lon + Lat, data = Entdiff.df, newdata = pred.grid, idp = 10)
idw1 = idw1[idw1$var1.pred < 0.1, ]
idw2 = idw(formula = Var ~ 1, locations = ~Lon + Lat, data = Entdiff.df, newdata = pred.grid, idp = 10)
idw2 = idw2[idw2$var1.pred < 0.1, ]

idw.df = rbind(idw1, idw2)
idw.df$var1.pred = 1

idw.df$Type = c(rep("Mean", nrow(idw1)), rep("Variance", nrow(idw2)))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw.df, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  facet_wrap(~ Type) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "Significance Tests") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none",
        strip.text.x = element_text(size = 14, colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size=10)))


gMDiff = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "Significantly Different Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "none") +
  guides(colour = guide_legend(override.aes = list(size=10)))
gMDiff


gVDiff = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "Significantly Different Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  guides(colour = guide_legend(override.aes = list(size=10)))
gVDiff


grid.arrange(gMDiff + labs(x = " "), gVDiff + labs(x = " ", y = " "), nrow = 1, ncol = 2, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3))

mean(p.val2 < 0.05 & p.val1 < 0.05)

entropy1 = function(x){
  out = -sum(ifelse(x > 0, (x/sum(x))*log(x/sum(x)), 0))
  return(out)
}

weighted.entropy1 = function(x, w = 1){
  out = -sum(ifelse(x > 0, w * (x/sum(x))*log(x/sum(x)), 0))
  return(out)
}




library("truncnorm")

p.dist = function(n, mu.vec, sd.vec){
  m = length(mu.vec)
  out = rep(0, m)
  for(i in 1:(m - 1)){
    out[i] = rtruncnorm(1, a = 0, b = 1 - sum(out), mean = mu.vec[i], sd = sd.vec[i])
  }
  out[m] = 1 - sum(out)
  out
}

sum(p.dist(1, runif(12), runif(12)))

x = p.dist(1, runif(12), runif(12))
sum(x * log(x))

hist(P1[Dar,m01[m01 < 469]])
hist(P1[Melb,m07])


df.H = data.frame(Dar1 = P1[Dar, m01[m01 < 469]], Dar2 = P1[Dar,m07[m07 < 469]], Melb1 = P1[Melb, m01[m01 < 469]], Melb2 = P1[Melb, m07[m07 < 469]])


(g.dar1 = ggplot(df.H, mapping = aes(x = Dar1)) + geom_histogram(bins = 12, color="black", fill="white", aes(y=..density..)) +
    labs(x = "Proportion", y = "Count", title = "Darwin January") + theme_bw() + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
          axis.title=element_text(size=14),  axis.text=element_text(size=12),
          strip.text.y = element_text(size = 14, colour = "black", angle = 90)))
g.dar2 = ggplot(df.H, mapping = aes(x = Dar2)) + geom_histogram(bins = 12, color="black", fill="white")+
  labs(x = "Proportion", y = "Count", title = "Darwin July") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb1 = ggplot(df.H, mapping = aes(x = Melb1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne January") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb2 = ggplot(df.H, mapping = aes(x = Melb2)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne July") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))

grid.arrange (g.dar1 + labs(x = "", y = ""), g.dar2 + labs(x = "", y = ""),
              g.Melb1 + labs(x = "", y = ""), g.Melb2 + labs(x = "", y = ""),
              bottom = textGrob("Proportion", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3),
              left = textGrob("Count", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))

x <- seq(min(df.H$Dar1)-0.025, max(df.H$Dar1) + 0.025, length.out=100)

del = 0.25
Dar.opt =  which(pred.grid$Lat < -12.397 + del & pred.grid$Lat > -12.397 - del & pred.grid$Lon < 130.94 + del & pred.grid$Lon > 130.94 - del)[1]


df <- data.frame(x = x, y = dtruncnorm(x, a = 0, b = 1, mean = idw1$var1.pred[Dar.opt], sd = idw12$var1.pred[Dar.opt]))

g.dar1 + geom_line(data = df, aes(x = x, y = y), color = "red")



Mu = rowMeans(P1[,m01])
Sigma = apply(P1[,m01],1,sd)

df.1 = data.frame(Lat = ar.loc[,1], Lon = ar.loc[,2], Mean = Mu, SD = Sigma)
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Mean ~ 1, locations = ~Lon + Lat, data = df.1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = SD ~ 1, locations = ~Lon + Lat, data = df.1, newdata = pred.grid, idp = 3)
Prop.df = data.frame(Mean = idw1$var1.pred, SD = idw2$var1.pred, Lon = idw1$Lon, Lat = idw1$Lat)

Prop1.M <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Prop.df, 
             mapping = aes(x = Lon, y = Lat, colour = Mean), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mu", x = "Longitude", y = "Latitude",
       title = "January Proportion Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

Prop1.M


Prop1.S <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = Prop.df, 
             mapping = aes(x = Lon, y = Lat, colour = SD), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "SD", x = "Longitude", y = "Latitude",
       title = "January Proportion SD") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

Prop1.S


Mu = matrix(0, nrow = nrow(P1), ncol = length(M))
Sigma = matrix(0, nrow = nrow(P1), ncol = length(M))
for(i in 1:length(M)){
  Mu[,i] = rowMeans(P1[,M[[i]]])
  Sigma[,i] = apply(P1[,M[[i]]],1,sd)
}


p.dist = function(n, mu.vec, sd.vec){
  m = length(mu.vec)
  out = rep(0, m)
  for(i in 1:(m - 1)){
    out[i] = rtruncnorm(1, a = 0, b = 1 - sum(out), mean = mu.vec[i], sd = sd.vec[i])
  }
  out[m] = 1 - sum(out)
  out
}



n1 = 10000
X.1 = rep(0, n1)
for(i in 1:n1){
  x.1 = p.dist(1, Mu[1,], Sigma[1,])
  X.1[i] = sum(-x.1 * log(x.1))
}
plot(density(X.1))
quantile(X.1, c(0.05,0.95))

n1 = 10000
X.1 = matrix(0, ncol = n1, nrow = nrow(P1))
for(i in 1:nrow(P1)){
  for(j in 1:n1){
    x.1 = p.dist(1, Mu[i,], Sigma[i,])
    X.1[i,j] = sum(-x.1 * log(x.1))
  }
  print(i)
}




E3 = apply(Aus.all, 2, entropy)


##########
#Yearly Precipitation
##########


Y1 = matrix(0, nrow = nrow(Aus.all), ncol = floor(ncol(Aus.all)/12))
for(i in 1:(floor(ncol(Aus.all)/12))){
  Y1[,i] = rowSums(Aus.all[,(12 * (i - 1) + 1):(12*i)]) 
}

del = 0.25
Nelligen = which(precip3[[1]][Aus,1] < -35.699 + del & precip3[[1]][Aus,1] > -35.699 - del & precip3[[1]][Aus,2] < 150.086 + del & precip3[[1]][Aus,2] > 150.086 - del)[1]
Snowy = which(precip3[[1]][Aus,1] < -36.51 + del & precip3[[1]][Aus,1] > -36.51 - del & precip3[[1]][Aus,2] < 148.33 + del & precip3[[1]][Aus,2] > 148.33 - del)[1]
Blue = which(precip3[[1]][Aus,1] < -33.41 + del & precip3[[1]][Aus,1] > -33.41 - del & precip3[[1]][Aus,2] < 150.30 + del & precip3[[1]][Aus,2] > 150.3 - del)[1]

Y1[Nelligen,]
Y1[Snowy,]
Y1[Blue,]

plot(Y1[Blue,], type = "l")

plot(P1[Blue,], type = "l")
plot(P1[Blue, 457:468], type = "l")

A1 = rowMeans(Y1)

mean(Aus.all[Blue, m12])
boxplot(Aus.all[Blue, m12])


date1 = NULL
for(i in 1:468){
  if(nchar(dates2[i,2]) == 2){
    m1 = dates2[i,2]
  }else{
    m1 = paste0("0", dates2[i,2])
  }
  date1 = c(date1, paste(dates1[i,1], m1, 15, sep = "-"))
}

date1 = as.Date(date1)

Blue.df = data.frame(Precipitation = Aus.all[Blue,1:468], Month = rep(1:12, 39), Year = as.factor(rep(1981:2019, each = 12)), Date = date1)
gBlue = ggplot(Snowy.df, aes(x = Month, y = Precipitation)) +
  geom_line(aes(colour = Year), size = 0.7)  +
  labs(x = "Date", y = "Index",
       title = "Oscillation Index") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
gBlue


ggplot(Blue.df, aes(x = as.factor(Month), y = Precipitation)) +
  geom_boxplot(outlier.color = "red") + theme_bw() +
  geom_line(Snowy.df[Snowy.df$Year == c(2019),], mapping = aes(x = Month, y = Precipitation, colour = Year), linetype = 2, size = 1) +
  labs(x = "Month", y = "Precipitation (mm)", title = "Blue Mountains Monthly Precipitation") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), axis.text.x = element_text(angle = 90)) +
  scale_x_discrete(labels=c("1" = "Jan", "2" = "Feb",
                            "3" = "Mar", "4" = "Apr", "5" = "May", "6" = "Jun", "7" = "Jul",
                            "8" = "Aug", "9" = "Sep", "10" = "Oct", "11" = "Nov", "12" = "Dec")) +
  scale_color_manual(breaks = c("2019"),
                     values=c("blue"))



d1 = data.frame(Lat = ar.loc[,1], Lon = ar.loc[,2], A1 = A1)
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = A1 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred^(1/2)), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude", title = "Australia Average Yearly Precipiation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g1



A = diag(12)
for(i in 1:ncol(A)){
  for(j in 1:nrow(A)){
    if(i == j + 1){
      A[i,j] =-1
    }
  }
}



##############
#Some plots
##############

x = seq(0,1,0.001)
y=(-x*log(x) - (1-x)*log(1-x))
y1 = (1/2) * (1/2 - 2 * x^2 + 2 * x * log(2) + 1/2 - 2 * (1-x)^2 + 2 * (1-x) * log(2))
y[is.nan(y)] = 0
y1[is.nan(y1)] = 0



td = data.frame(x = c(x,x), y = c(y,y1), group = c(rep("Actual", length(x)), rep("Approximation", length(x))))

H2 = ggplot(td) + geom_line(aes(x = x, y = y, colour = group), size = 1)+ theme_bw() +
  labs(x = "x", y = "Entropy", title = "Two Dimensional Entropy", colour = " ") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16)) + scale_colour_manual(values = c("red", "blue"))
H2  



x1 = seq(0,1,0.001)
x2 = seq(0,1,0.001)
x4 = matrix(0, nrow = length(x1) * length(x2), ncol = 5)
k = 1
for(i in 1:length(x1)){
  for(j in 1:length(x2)){
    x4[k, ] = c(x1[i], x2[j], 1 - x1[i] - x2[j], i, j)
    k = k + 1
  }
}

plot(x4[,3])

x4 = x4[x4[,3] >= 0,]
nrow(x4)

Ent.3 = function(x,y){
  if(x == 0 | x == 1){
    x1 = 0
  }else{
    x1 = -x*log(x)
  }
  if(y == 0 | y == 1){
    y1 = 0
  }else{
    y1 = -y*log(y)
  }
  if(1 - x - y == 1 | 1 - x - y == 0){
    z1 = 0
  }else{
    z1 = -(1-x-y)*log(1-x-y)
  }
  out = x1 + y1 + z1
  return(out)
}
Ent.3(x4[i,1], x4[i,2])


E.1 = rep(0, nrow(x4))
for(i in 1:nrow(x4)){
  if(x4[i,3] < 0){
    E.1[i] = 0
  }else{
    E.1[i] = Ent.3(x4[i,1], x4[i,2])
  }
}


X.df = data.frame(x = x4[,1], y = x4[,2], z = E.1)


#################
#Seasonal
#################


library("entropy")

E1 = NULL

div1 = function(x){
  if(sum(x) > 0){
    a = x/sum(x)
  }else{
    a = 0
  }
  a
}

Aus.Season = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor((ncol(NOAA.all.aus)-1)/12) * 4)
Aus.1 = NOAA.all.aus[,-c(1,2)]
for(i in 1:ncol(Aus.Season)){
  Aus.Season[,i] = rowSums(Aus.1[, 1 + ((i-1)*3):((i-1)*3 + 2)])
}

P.season = matrix(0, nrow = nrow(Aus.Season), ncol = ncol(Aus.Season))
for(i in 1:floor(ncol(P.season)/4)){
  P.season[,((i - 1) * 4 + 1):(i * 4)] = t(apply(Aus.Season[,((i - 1) * 4 + 1):(i * 4)], 1, div1))
}

Melb.1 = P.season[Melb,]


E3 = matrix(0, nrow = nrow(Aus.all), ncol = 39)
for(i in 1:39){
  E4 = t(apply(Aus.Season[,((i - 1) * 4 + 1):(i * 4)], 1, div1))
  E3[,i] = apply(E4, 1, entropy)
}

sum1 = seq(4,ncol(P.season),4)
hist(P.season[Melb, sum1])


E.mean = rowMeans(E1)
E.mean1 = rowMeans(E1[,1:20])
E.mean2 = rowMeans(E1[,21:39])
E.V = apply(E1, 1, var)
Austra1 = read.csv("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid202001")

A1 = Austra1[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus1 = which(a3$ISO3 == "AUS")

d1 = data.frame(Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lat, E1 = E.mean1, E2 = E.mean2, E = E.mean, V = E.V)
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = E1 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = E2 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw3 = idw(formula = E ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw4 = idw(formula = E.V ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
E.df = data.frame(E1 = idw1$var1.pred, E2 = idw2$var1.pred, E = idw3$var1.pred, V = idw4$var1.pred, Lon = idw1$Lon, Lat = idw1$Lat)

E.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = E), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mean", x = "Longitude", y = "Latitude",
       title = "Australian Entropy Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

V.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = V), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Variance", x = "Longitude", y = "Latitude",
       title = "Australian Entropy Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")

E.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = E1 - E2), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Entropy Difference", x = "Longitude", y = "Latitude",
       title = "Australian Seasonal Entropy Mean Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
E.plot


gn = function(theta){
  mu = theta[1:3]
  sigma = theta[4:6]
  d2 = 0
  for(i in 1:nrow(P2)){
    b = c(1,1-cumsum(P2[i,1:2]))
    d2 = d2 +  sum(log((1/sqrt(2*pi*sigma^2) * exp(-(P2[i, -4] - mu)^2/(2 * sigma^2))/(pnorm(b ,mu ,sigma) - pnorm(0, mu, sigma)))))
  }
  -d2
}

colSd = function(X){
  apply(X, 2, sd)
}

Mu = colMeans(P2[,-12])
Sigma = colSd(P2[,-12])

theta = c(Mu, Sigma)

P.1 = P1[m.Aus,]

Opt1 = matrix(0, nrow = nrow(P.1), ncol = 22)
for(i in 1:nrow(P.1)){
  P2 = matrix(P.1[i,], nrow = 39, byrow = TRUE)
  
  Mu = colMeans(P2[,-12])
  Sigma = colSd(P2[,-12])
  
  o1 = optim(c(Mu, Sigma), gn)
  Opt1[i, ] = o1$par
  
  print(i)
}


d1 = data.frame(Jan.mean = Opt1[,1], Feb.mean = Opt1[,2], Mar.mean = Opt1[,3], Apr.mean = Opt1[,4],
                May.mean = Opt1[,5], Jun.mean = Opt1[,6], Jul.mean = Opt1[,7], Aug.mean = Opt1[,8],
                Sep.mean = Opt1[,9], Oct.mean = Opt1[,10], Nov.mean = Opt1[,11],
                Jan.sd = Opt1[,12], Feb.sd = Opt1[,13], Mar.sd = Opt1[,14], Apr.sd = Opt1[,15],
                May.sd = Opt1[,16], Jun.sd = Opt1[,17], Jul.sd = Opt1[,18], Aug.sd = Opt1[,19],
                Sep.sd = Opt1[,20], Oct.sd = Opt1[,21], Nov.sd = Opt1[,22], Lat = precip3[[1]][Aus[m.Aus],1], Lon = precip3[[1]][Aus[m.Aus],2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Jan.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Feb.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw3 = idw(formula = Mar.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw4 = idw(formula = Apr.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw5 = idw(formula = May.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw6 = idw(formula = Jun.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw7 = idw(formula = Jul.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw8 = idw(formula = Aug.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw9 = idw(formula = Sep.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw10 = idw(formula = Oct.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw11 = idw(formula = Nov.mean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw12 = idw(formula = Jan.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw13 = idw(formula = Feb.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw14 = idw(formula = Mar.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw15 = idw(formula = Apr.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw16 = idw(formula = May.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw17 = idw(formula = Jun.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw18 = idw(formula = Jul.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw19 = idw(formula = Aug.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw20 = idw(formula = Sep.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw21 = idw(formula = Oct.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw22 = idw(formula = Nov.sd ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)


g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(mu), x = "Longitude", y = "Latitude", title = expression(paste("January Proportion ", mu)))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g1


g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw12, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(sigma), x = "Longitude", y = "Latitude", title = expression(paste("January Proportion ", sigma)))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g2

grid.arrange(g1 + labs(x = " ", y = " "), g2 + labs(x = " ", y = " "), nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8)))


g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw8, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(mu), x = "Longitude", y = "Latitude", title = expression(paste("January Proportion ", mu)))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g1


g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw21, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(sigma), x = "Longitude", y = "Latitude", title = expression(paste("January Proportion ", sigma)))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g2



############
#Correlation
############
dim(P1)

P4 = matrix(0, nrow = nrow(P1), ncol = 11)
for(i in 1:nrow(P1)){
  P3 = matrix(P1[i,], nrow = ncol(P1)/12, ncol = 12, byrow = TRUE)
  for(j in 1:11){
    P4[i,j] = cor(rowSums(matrix(P3[,1:j], ncol = j, nrow = nrow(P3))), P3[,j+1])
  }
}

Cor1 = as.data.frame(P4)
colnames(Cor1) = c("JanFeb","FebMar","MarApr","AprMay", "MayJun","JunJul","JulyAug",
                   "AugSep","SepOct","OctNov","NovDec")
Cor1$Lat = precip3[[1]][Aus,1]
Cor1$Lon = precip3[[1]][Aus,2]

pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = JanFeb ~ 1, locations = ~Lon + Lat, data = Cor1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = JulyAug ~ 1, locations = ~Lon + Lat, data = Cor1, newdata = pred.grid, idp = 3)


g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9),limits = c(min(c(idw1$var1.pred, idw2$var1.pred)), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(rho), x = "Longitude", y = "Latitude",
                    title = "February Proportion Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g2

g3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(c(idw1$var1.pred, idw2$var1.pred)), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(rho), x = "Longitude", y = "Latitude",
                    title = "August Proportion Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g3

grid.arrange(g2 + labs(x = " ", y = " ") + theme(legend.position = "none") , g3 + labs(x = " ", y = " "), widths = c(1, 1.115), nrow = 1,
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.55),
             left = textGrob("Latitude", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))



head(P1)
plot(rowMeans(Cor1[,-c(12,13)]), cex = 0.5)

P4 = matrix(0, nrow = nrow(P1), ncol = 11)
for(i in 1:nrow(P1)){
  P3 = matrix(P1[i,], nrow = ncol(P1)/12, ncol = 12, byrow = TRUE)
  for(j in 1:11){
    P4[i,j] = mean(P3[,j] - P3[,j+1])
  }
}

plot(P4[,1])

Diff1 = as.data.frame(P4)
colnames(Diff1) = c("JanFeb","FebMar","MarApr","AprMay", "MayJun","JunJul","JulyAug",
                    "AugSep","SepOct","OctNov","NovDec")
Diff1$Lat = Cor1$Lat
Diff1$Lon = Cor1$Lon

pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = JanFeb ~ 1, locations = ~Lon + Lat, data = Diff1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = JulyAug ~ 1, locations = ~Lon + Lat, data = Diff1, newdata = pred.grid, idp = 3)


g4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(rho), x = "Longitude", y = "Latitude",
                    title = "January-February Proportion Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g4

g5 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(rho), x = "Longitude", y = "Latitude",
                    title = "July-August Proportion Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g5

g3


#############
#Summer not summer
#############

Sumnsum = matrix(0, nrow = nrow(Aus.Season), ncol = 80)
for(i in 1:nrow(Aus.Season)){
  for(j in 1:40){
    Sumnsum[i, ((j - 1) * 2 + 1):(j * 2)] = c(sum(Aus.Season[i, ((j - 1) * 4 + 1):(j * 4 - 1)]), Aus.Season[i, (j * 4)]) 
  }
}

Ent1 = matrix(0, nrow = nrow(Aus.Season), ncol = 40)
for(i in 1:nrow(Aus.Season)){
  P1 = matrix(Sumnsum[i,], ncol = 2, nrow = 40, byrow = TRUE)
  Ent1[i,] = apply(P1, 1, entropy)
}

hist(rowMeans(Ent1))

d1 = data.frame(Entropy = Ent1[,19], Lat = precip3[[1]][Aus,1], Lon = precip3[[1]][Aus,2])

idw1 = idw(formula = Entropy ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Entropy", x = "Longitude", y = "Latitude",
                    title = "2019/2020 Summer Entropy")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g1




############
#Weighted Entropy
###########

entropy1 = function(x){
  out = -sum(ifelse(x > 0, (x/sum(x))*log(x/sum(x)), 0))
  return(out)
}

weighted.entropy1 = function(x, w = 1){
  out = -sum(ifelse(x > 0, w * (x/sum(x))*log(x/sum(x)), 0))
  return(out)
}

w.ent1 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = 12)
w.ent2 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = 12)
k = 1
for(i in 238:249){
  w.ent1[,k] = apply(JAXA.daily1[[i]][,-c(1,2)], 1, entropy1)
  w.ent2[,k] = rowSums(JAXA.daily1[[i]][,-c(1,2)])
  k = k + 1
}

w.ent3 = rep(0, nrow(w.ent1))
w.ent4 = rep(0, nrow(w.ent1))
for(i in 1:nrow(w.ent1)){
  w.ent3[i] = weighted.entropy1(x = w.ent2[i,], w = w.ent1[i,]/sum(w.ent1[i,]))
  w.ent4[i] = entropy1(w.ent2[i,])
}

d1 = data.frame(Lat = JAXA.daily1[[249]][,1], Lon = JAXA.daily1[[249]][,2], Weighted = w.ent3, Unweighted = w.ent4)
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Weighted ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Unweighted ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)


g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Entropy", x = "Longitude", y = "Latitude", title = "Weighted 2020 Monthly Entropy")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9)

g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Entropy", x = "Longitude", y = "Latitude", title = "Unweighted 2020 Monthly Entropy")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9)

grid.arrange(g1 + labs(x = " ") + theme(legend.position = "left"), g2 + labs(x = " ", y = " "), widths = c(1, 1), nrow = 1,
             bottom = textGrob("Longitude", gp=gpar(fontsize=12,font=8), vjust = -1, hjust = 0.1))



##################
#Log-Odds ratio
##################

NOAA.all.aus[NOAA.all.aus == 0] = 0.0001

P1 = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12) * 12)
for(i in 1:floor(ncol(NOAA.all.aus)/12)){
  P2 = t(apply(NOAA.all.aus[,((i - 1) * 12 + 1):(i * 12)], 1, div1))
  P1[,((i - 1) * 12 + 1):(i * 12)] = P2
}


P2.Melb = matrix(P1[Melb,], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12,  byrow = TRUE)

P2.Melb = P2.Melb/P2.Melb[,12]

P2.Melb = P2.Melb[,-12]

P2.Melb = log(P2.Melb)

P2.Dar = matrix(P1[Dar,], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12,  byrow = TRUE)

P2.Dar = P2.Dar/P2.Dar[,12]

P2.Dar = P2.Dar[,-12]

P2.Dar = log(P2.Dar)




LogOdds.df = data.frame(Melb1 = P2.Melb[,1], Melb2 = P2.Melb[,7], Dar1 = P2.Dar[,1], Dar2 = P2.Dar[,7])


g.dar1 = ggplot(LogOdds.df, mapping = aes(x = Dar1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Darwin January") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90)) + 
  scale_y_continuous(breaks=c(0,2.5, 5, 7.5, 10))
g.dar2 = ggplot(LogOdds.df, mapping = aes(x = Dar2)) + geom_histogram(bins = 12, color="black", fill="white")+
  labs(x = "Proportion", y = "Count", title = "Darwin July") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb1 = ggplot(LogOdds.df, mapping = aes(x = Melb1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne January") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb2 = ggplot(LogOdds.df, mapping = aes(x = Melb2)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne July") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))

grid.arrange(g.dar1 + labs(x = "", y = ""), g.dar2 + labs(x = "", y = ""),
              g.Melb1 + labs(x = "", y = ""), g.Melb2 + labs(x = "", y = ""),
              bottom = textGrob("Value", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3),
              left = textGrob("Count", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))

rep(1:5,5)


S.Prop.Dar = matrix(0, nrow = length(1:(ncol(NOAA.all.aus) - 12)), ncol = 12)
for(i in 1:(ncol(NOAA.all.aus) - 12)){
  S.Prop.Dar[i,] = div1(NOAA.all.aus[Dar,i:(i + 11)])
}

Prop.Dar.vec1 = NULL
Prop.Dar.veclog1 = NULL
k = 1
j = 12
for(i in 1:nrow(S.Prop.Dar)){
  Prop.Dar.vec1 = c(Prop.Dar.vec1, S.Prop.Dar[i,k])
  Prop.Dar.veclog1 = c(Prop.Dar.veclog1, log(S.Prop.Dar[i,k]/S.Prop.Dar[i,j]))
  if(k == 1){
    k = 12
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 12
  }else{
    j = j - 1
  }
}

Prop.Dar.vec2 = NULL
Prop.Dar.veclog2 = NULL
k = 7
j = 12
for(i in 1:nrow(S.Prop.Dar)){
  Prop.Dar.vec2 = c(Prop.Dar.vec2, S.Prop.Dar[i,k])
  Prop.Dar.veclog2 = c(Prop.Dar.veclog2, log(S.Prop.Dar[i,k]/S.Prop.Dar[i,j]))
  if(k == 1){
    k = 12
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 12
  }else{
    j = j - 1
  }
}


S.Prop.Melb = matrix(0, nrow = length(1:(ncol(NOAA.all.aus) - 12)), ncol = 12)
for(i in 1:(ncol(NOAA.all.aus) - 12)){
  S.Prop.Melb[i,] = div1(NOAA.all.aus[Melb,i:(i + 11)])
}

Prop.Melb.vec1 = NULL
Prop.Melb.veclog1 = NULL
k = 1
j = 12
for(i in 1:nrow(S.Prop.Melb)){
  Prop.Melb.vec1 = c(Prop.Melb.vec1, S.Prop.Melb[i,k])
  Prop.Melb.veclog1 = c(Prop.Melb.veclog1, log(S.Prop.Melb[i,k]/S.Prop.Melb[i,j]))
  if(k == 1){
    k = 12
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 12
  }else{
    j = j - 1
  }
}

Prop.Melb.vec2 = NULL
Prop.Melb.veclog2 = NULL
k = 7
j = 12
for(i in 1:nrow(S.Prop.Melb)){
  Prop.Melb.vec2 = c(Prop.Melb.vec2, S.Prop.Melb[i,k])
  Prop.Melb.veclog2 = c(Prop.Melb.veclog2, log(S.Prop.Melb[i,k]/S.Prop.Melb[i,j]))
  if(k == 1){
    k = 12
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 12
  }else{
    j = j - 1
  }
}

P1 = matrix(0, nrow = nrow(Aus.Season), ncol = floor(ncol(Aus.Season)/4) * 4)
for(i in 1:floor(ncol(Aus.Season)/4)){
  P2 = t(apply(Aus.Season[,((i - 1) * 4 + 1):(i * 4)], 1, div1))
  P1[,((i - 1) * 4 + 1):(i * 4)] = P2
}


S.Prop.Dar = matrix(0, nrow = length(1:(ncol(Aus.Season) - 4)), ncol = 4)
for(i in 1:(ncol(Aus.Season) - 4)){
  S.Prop.Dar[i,] = div1(Aus.Season[Dar,i:(i + 3)])
}

Prop.Dar.vec1 = NULL
Prop.Dar.veclog1 = NULL
k = 4
j = 1
for(i in 1:nrow(S.Prop.Dar)){
  Prop.Dar.vec1 = c(Prop.Dar.vec1, S.Prop.Dar[i,k])
  Prop.Dar.veclog1 = c(Prop.Dar.veclog1, log(S.Prop.Dar[i,k]/S.Prop.Dar[i,j]))
  if(k == 1){
    k = 4
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 4
  }else{
    j = j - 1
  }
}

Prop.Dar.vec2 = NULL
Prop.Dar.veclog2 = NULL
k = 2
j = 1
for(i in 1:nrow(S.Prop.Dar)){
  Prop.Dar.vec2 = c(Prop.Dar.vec2, S.Prop.Dar[i,k])
  Prop.Dar.veclog2 = c(Prop.Dar.veclog2, log(S.Prop.Dar[i,k]/S.Prop.Dar[i,j]))
  if(k == 1){
    k = 4
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 4
  }else{
    j = j - 1
  }
}


S.Prop.Melb = matrix(0, nrow = length(1:(ncol(Aus.Season) - 4)), ncol = 4)
for(i in 1:(ncol(Aus.Season) - 4)){
  S.Prop.Melb[i,] = div1(Aus.Season[Melb,i:(i + 3)])
}

Prop.Melb.vec1 = NULL
Prop.Melb.veclog1 = NULL
k = 4
j = 1
for(i in 1:nrow(S.Prop.Melb)){
  Prop.Melb.vec1 = c(Prop.Melb.vec1, S.Prop.Melb[i,k])
  Prop.Melb.veclog1 = c(Prop.Melb.veclog1, log(S.Prop.Melb[i,k]/S.Prop.Melb[i,j]))
  if(k == 1){
    k = 4
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 4
  }else{
    j = j - 1
  }
}

Prop.Melb.vec2 = NULL
Prop.Melb.veclog2 = NULL
k = 2
j = 1
for(i in 1:nrow(S.Prop.Melb)){
  Prop.Melb.vec2 = c(Prop.Melb.vec2, S.Prop.Melb[i,k])
  Prop.Melb.veclog2 = c(Prop.Melb.veclog2, log(S.Prop.Melb[i,k]/S.Prop.Melb[i,j]))
  if(k == 1){
    k = 4
  }else{
    k = k - 1
  }
  if(j == 1){
    j = 4
  }else{
    j = j - 1
  }
}



Slide.Prop = data.frame(Melb1 = Prop.Melb.vec1, Melb2 = Prop.Melb.vec2, Dar1 = Prop.Dar.vec1, Dar2 = Prop.Dar.vec2,
                        LogMelb1 = Prop.Melb.veclog1, LogMelb2 = Prop.Melb.veclog2, LogDar1 = Prop.Dar.veclog1, LogDar2 = Prop.Dar.veclog2)



g.dar1 = ggplot(Slide.Prop, mapping = aes(x = Dar1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Darwin January") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.dar2 = ggplot(Slide.Prop, mapping = aes(x = Dar2)) + geom_histogram(bins = 12, color="black", fill="white")+
  labs(x = "Proportion", y = "Count", title = "Darwin July") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb1 = ggplot(Slide.Prop, mapping = aes(x = Melb1)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne January") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melb2 = ggplot(Slide.Prop, mapping = aes(x = Melb2)) + geom_histogram(bins = 12, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne July") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))

grid.arrange (g.dar1 + labs(x = "", y = ""), g.dar2 + labs(x = "", y = ""),
              g.Melb1 + labs(x = "", y = ""), g.Melb2 + labs(x = "", y = ""),
              bottom = textGrob("Proportion", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3),
              left = textGrob("Count", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))

g.darlog1 = ggplot(Slide.Prop, mapping = aes(x = LogDar1)) + geom_histogram(bins = 15, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Darwin Summer") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.darlog2 = ggplot(Slide.Prop, mapping = aes(x = LogDar2)) + geom_histogram(bins = 15, color="black", fill="white")+
  labs(x = "Proportion", y = "Count", title = "Darwin Winter") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melblog1 = ggplot(Slide.Prop, mapping = aes(x = LogMelb1)) + geom_histogram(bins = 15, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne Summer") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))
g.Melblog2 = ggplot(Slide.Prop, mapping = aes(x = LogMelb2)) + geom_histogram(bins = 15, color="black", fill="white") +
  labs(x = "Proportion", y = "Count", title = "Melbourne Winter") + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 14, colour = "black", angle = 90))

grid.arrange (g.darlog1 + labs(x = "", y = ""), g.darlog2 + labs(x = "", y = ""),
              g.Melblog1 + labs(x = "", y = ""), g.Melblog2 + labs(x = "", y = ""),
              bottom = textGrob("Value", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3),
              left = textGrob("Count", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.3, rot = 90))



P.list = list()
for(i in 1:nrow(P1)){
  p1 = matrix(P1[i,], nrow = 40, ncol = 12, byrow = TRUE)
  d1 = p1/p1[,12]
  d1 = d1[,-12]
  P.list[[i]] = log(d1)
}


LO.mu = matrix(0, nrow = length(P.list), ncol = 11)
LO.var = matrix(0, nrow = length(P.list), ncol = 11)
for(i in 1:length(P.list)){
  LO.mu[i,] = colMeans(P.list[[i]])
  LO.var[i,] = apply(P.list[[i]], 2, var)
}


LO.df = data.frame(Mean1 = LO.mu[,1], Mean2 = LO.mu[,7], Var1 = LO.var[,1], Var2 = LO.var[,2], Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon)

pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Mean1 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Mean2 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)
idw3 = idw(formula = Var1 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)
idw4 = idw(formula = Var2 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)

g.Mean1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mean", x = "Longitude", y = "Latitude",
       title = "January Log-Odds Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                            "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred, idw2$var1.pred)), max(idw1$var1.pred, idw2$var1.pred)))

g.Mean2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mean     ", x = "Longitude", y = "Latitude",
       title = "July Log-Odds Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred, idw2$var1.pred)), max(idw1$var1.pred, idw2$var1.pred)))

g.Var1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw3, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Variance", x = "Longitude", y = "Latitude",
       title = "January Log-Odds Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw3$var1.pred, idw4$var1.pred)), max(idw3$var1.pred, idw4$var1.pred)))

g.Var2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw4, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Variance", x = "Longitude", y = "Latitude",
       title = "July Log-Odds Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw3$var1.pred, idw4$var1.pred)), max(idw3$var1.pred, idw4$var1.pred)))

grid.arrange(g.Mean1 + theme(legend.position = "none") + labs(x = " ", y = " "), g.Mean2 + labs(x = " ", y = " "),
             g.Var1 + theme(legend.position = "none") + labs(x = " ", y = " "), g.Var2 + labs(x = " ", y = " "),
             nrow = 2, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.75),
             left = textGrob("Latitude", gp=gpar(fontsize=16,font=8), vjust = 1.5, hjust = 0.3, rot = 90), widths = c(3, 3.7))

P.lists = list()
for(i in 1:nrow(Aus.Season)){
  P1 = matrix(Aus.Season[i, 1:(floor(ncol(Aus.Season)/4) * 4)], nrow = floor(ncol(Aus.Season)/4), ncol = 4, byrow = TRUE)
  P1[P1 == 0] = 0.001
  P2 = t(apply(P1,1,div1))
  for(j in 1:nrow(P2)){
    P2[j,] = P2[j,]/P2[j,3]
  }
  P.lists[[i]] = log(P2[,-3])
}


LO.mu = matrix(0, nrow = length(P.lists), ncol = 3)
LO.var = matrix(0, nrow = length(P.lists), ncol = 3)
for(i in 1:length(P.lists)){
  LO.mu[i,] = colMeans(P.lists[[i]])
  LO.var[i,] = apply(P.lists[[i]], 2, var)
}


LO.df = data.frame(Mean1 = LO.mu[,3], Mean2 = LO.mu[,2], Var1 = LO.var[,3], Var2 = LO.var[,2], Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon)

pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Mean1 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Mean2 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)
idw3 = idw(formula = Var1 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)
idw4 = idw(formula = Var2 ~ 1, locations = ~Lon + Lat, data = LO.df, newdata = pred.grid, idp = 3)

g.Mean1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mean", x = "Longitude", y = "Latitude",
       title = "Summer Log-Odds Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred, idw2$var1.pred)), max(idw1$var1.pred, idw2$var1.pred)))

g.Mean2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Mean     ", x = "Longitude", y = "Latitude",
       title = "Winter Log-Odds Mean") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred, idw2$var1.pred)), max(idw1$var1.pred, idw2$var1.pred)))

g.Var1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw3, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Variance", x = "Longitude", y = "Latitude",
       title = "Summer Log-Odds Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw3$var1.pred, idw4$var1.pred)), max(idw3$var1.pred, idw4$var1.pred)))

g.Var2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw4, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Variance", x = "Longitude", y = "Latitude",
       title = "Winter Log-Odds Variance") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw3$var1.pred, idw4$var1.pred)), max(idw3$var1.pred, idw4$var1.pred)))

grid.arrange(g.Mean1 + theme(legend.position = "none") + labs(x = " ", y = " "), g.Mean2 + labs(x = " ", y = " "),
             g.Var1 + theme(legend.position = "none") + labs(x = " ", y = " "), g.Var2 + labs(x = " ", y = " "),
             nrow = 2, bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.75),
             left = textGrob("Latitude", gp=gpar(fontsize=16,font=8), vjust = 1.5, hjust = 0.3, rot = 90), widths = c(3, 3.7))


install_github("RfastOfficial/Rfast")


colMeans(log(d1))
((t(d1) - colMeans(d1)) %*% t(t(d1) - colMeans(d1)))/11


dim((t(d1) - colMeans(d1)) %*% t(t(d1) - colMeans(d1)))


mu.vec = colMeans(d1)
Sigma = matrix(0, 11, 11)
for(i in 1:nrow(d1)){
  Sigma = Sigma + (d1[i,] - mu.vec) %*% t(d1[i,] - mu.vec)/40
}

solve(Sigma)


g.func = function(x){
  numer = exp(x)
  denom = 1 + sum(exp(x))
  out = -(sum((numer / denom) * log(numer / denom)) + 1/denom * log(1/denom))
  return(out)
}


g.diff.func = function(x){
  denom = 1 + sum(exp(x))
  out = rep(0, length(x))
  for(i in 1:length(x)){
    denom1 = 1 + sum(exp(x[-i]))
    out[i] = exp(x[i]) * (log(denom) + (denom1) * log(exp(x[i] / denom)) - sum(exp(x[-i]) * log(exp(x[-i]) / denom)))/denom^2
  }
  return(out)
}

g.diff.func(rnorm(mu.vec)) %*% (Sigma) %*% g.diff.func(rnorm(mu.vec))

mu.1 = rep(0, length(P.list))
sigma.1 = rep(0, length(P.list))
for(i in 1:length(P.list)){
  mu.vec = colMeans(P.list[[i]])
  Sigma = matrix(0, length(mu.vec), length(mu.vec))
  for(j in 1:nrow(P.list[[i]])){
    Sigma = Sigma + (P.list[[i]][j,] - mu.vec) %*% t(P.list[[i]][j,] - mu.vec)/nrow(P.list[[i]])
  }
  mu.1[i] = g.func(mu.vec)
  sigma.1[i] = g.diff.func(mu.vec) %*% Sigma %*% g.diff.func(mu.vec)
}


plot(seq(0,2.5, 0.0001), dnorm(seq(0,2.5, 0.0001), mean = mu.1[Melb], sd = sqrt(sigma.1[Melb])/40),type = "l")





~ -x1/(1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11) * log(x1/(1 + x1 + x2 + x3 + x4 + x5 + x6 + x7 + x8 + x9 + x10 + x11))




deltamethod (~ -exp(x1)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x1)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x2)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x2)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x3)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x3)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x4)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x4)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x5)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x5)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x6)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x6)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x7)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x7)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x8)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x8)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x9)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x9)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x10)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x10)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               exp(x11)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(exp(x11)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
               1/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
               log(1/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))),
             mu.vec, Sigma) 



P.list = list()
for(i in 1:nrow(NOAA.all.aus)){
  P1 = matrix(NOAA.all.aus[i, 1:(floor(ncol(NOAA.all.aus)/12) * 12)], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12, byrow = TRUE)
  P1[P1 == 0] = 0.001
  P2 = t(apply(P1,1,div1))
  for(j in 1:nrow(P2)){
    P2[j,] = P2[j,]/P2[j,12]
  }
  P.list[[i]] = log(P2[,-12])
}



mu.1 = rep(0, length(P.list))
sigma.1 = rep(0, length(P.list))
pb <- txtProgressBar(min = 1, max = length(P.list), style = 3)
for(i in 1:length(P.list)){
  mu.vec = colMeans(P.list[[i]])
  Sigma = matrix(0, length(mu.vec), length(mu.vec))
  for(j in 1:nrow(P.list[[i]])){
    Sigma = Sigma + (P.list[[i]][j,] - mu.vec) %*% t(P.list[[i]][j,] - mu.vec)/nrow(P.list[[i]])
  }
  mu.1[i] = g.func(mu.vec)
  sigma.1[i] = deltamethod (~ -exp(x1)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x1)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x2)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x2)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x3)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x3)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x4)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x4)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x5)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x5)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x6)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x6)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x7)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x7)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x8)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x8)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x9)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x9)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x10)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x10)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              exp(x11)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(exp(x11)/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))) -
                              1/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11)) *
                              log(1/(1 + exp(x1) + exp(x2) + exp(x3) + exp(x4) + exp(x5) + exp(x6) + exp(x7) + exp(x8) + exp(x9) + exp(x10) + exp(x11))),
                            mu.vec, Sigma) 
  setTxtProgressBar(pb, i)
}

pval = matrix(0, nrow = length(P.list), ncol = nrow(P.list[[1]]))
for(i in 1:length(P.list)){
  samp.ent = apply(matrix(NOAA.all.aus[i, 1:(floor(ncol(NOAA.all.aus)/12) * 12)], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12, byrow = TRUE), 1, entropy1)
  pval[i,] = pnorm(samp.ent, mean = mu.1[i], sd = sigma.1[i])
}

mean(pval[,41] < 0.01)

EntExt.df = data.frame(Ext = pval[,41], Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat)

pred.grid = data.frame(spsample(ausmap, n = 50000, type = "random"))
colnames(pred.grid) = c("Lon", "Lat")
idw1 = idw(formula = Ext ~ 1, locations = ~Lon + Lat, data = EntExt.df, newdata = pred.grid, idp = 5)
idw1 = idw1[idw1$var1.pred < 0.01, ]

idw1$var1.pred = (idw1$var1.pred < 0.01)

idw.df = rbind(idw.entext, idw1)
idw.df$var1.pred = 1
idw.df$Type = c(rep("Approximate", nrow(idw.entext)), rep("Delta Method", nrow(idw1)))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "red", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  facet_wrap(~ Type) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "2019 Extreme Entropy") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none",
        strip.text.x = element_text(size = 12, colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size=10)))


gExt = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred) )), size = 0.9) +
  scale_colour_manual(
    values = c("1" = "red", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "2019 Extreme Entropy") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  guides(colour = guide_legend(override.aes = list(size=10)))
gExt

gExt + g1

grid.arrange(gExt + labs(x = " "), g1 + labs(x = " ", y = " "), nrow = 1,
             bottom = textGrob("Longitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3),
             left = textGrob("Latitude", gp=gpar(fontsize=16,font=8), vjust = -1, hjust = 0.3, rot = 90))



####ARIMA modelling

fit = auto.arima(as.ts(E1[Dar,]), ic = "aicc")

plot(forecast(fit, h = 1))

fit.ar = matrix(0, nrow = nrow(E1), ncol = length(fit$arma))
pb <- txtProgressBar(min = 1, max = nrow(E1), style = 3)
for(i in 1:nrow(E1)){
  setTxtProgressBar(pb, i)
  fit = auto.arima(as.ts(E1[i,]))
  fit.ar[i,] = fit$arma
}
close(pb)



fit <- auto.arima(WWWusage)
plot(forecast(fit,h=20))


fit = arima(as.ts(E1[Da,]), order = c(1,0,0), include.mean = FALSE)
fit$arma
sum(fit$residuals^2/(40 - 1))


plot(E1[Dar,], type = "l")


fit$loglik


prop.mat = matrix(P1[1,], ncol = 12, nrow = 40, byrow = TRUE) 

fit1 = ar(P1[1,])


fit2 = auto.arima(P1[1,])

fit1$ar

plot(P1[1,], type = "l")
plot(fit1$resid, type = "l")
plot(fit2$residuals, type = "l")
plot(P1[1,] - fit1$resid, type = "l")




###Sliding Entropy
dim(P1)
S.Ent = matrix(0, nrow = nrow(NOAA.all.aus), ncol = length(1:(ncol(NOAA.all.aus) - 12)))
for(i in 1:(ncol(NOAA.all.aus) - 12)){
  S.Ent[,i] = apply(NOAA.all.aus[,i:(i + 11)], 1, entropy1)
}

a1 = auto.arima(S.Ent[1,], d = 0)

plot(S.Ent[1,], type = "l")

lines(a1$fitted)

forecast(a1, 12)


ar1 = auto.arima(S.Ent[100,])

pb <- txtProgressBar(min = 1, max = nrow(S.Ent), style = 3)
ARMA.Comp = matrix(0, nrow = nrow(S.Ent), ncol = 3)
for(i in 1:nrow(S.Ent)){
  a1 = auto.arima(S.Ent[i,])
  if(!identical(a1$model[[1]], numeric(0))){
    ar.comp = length(a1$model[[1]])
  }else{
    ar.comp = 0
  }
  if(!identical(a1$model[[2]], numeric(0))){
    ma.comp = length(a1$model[[2]])
  }else{
    ma.comp = 0
  }
  if(!identical(a1$model[[3]], numeric(0))){
    d.comp = length(a1$model[[3]])
  }else{
    d.comp = 0
  }
  ARMA.Comp[i,1] = ar.comp
  ARMA.Comp[i,2] = ma.comp
  ARMA.Comp[i,3] = d.comp
  setTxtProgressBar(pb, i)
}



ARMA.Comp


table(ARMA.Comp[,3])


d1 = data.frame(Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon, AR = ARMA.Comp[,1], MA = ARMA.Comp[,2], D = ARMA.Comp[,3])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = AR ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 10)
idw2 = idw(formula = MA ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 10)
idw3 = idw(formula = D ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 10)


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw2, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "2019 Extreme Entropy") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  guides(colour = guide_legend(override.aes = list(size=10)))


S.Ent.list = list()
S.Ent.list[[1]] = S.Ent

Slide.EDQ = MEDQ(S.Ent.list, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)


S.1 = S.Ent.list[[1]][unique(Slide.EDQ),]

S.2 = t(S.1)
colnames(S.2) = paste0("Loc", 1:ncol(S.2))
S.2 = as.data.frame(S.2)

L1 = VARselect(S.2, lag.max = 20, type = "both")

v1 = VAR(S.2, p = 3, type = "const", ic = "AIC")

sum((fitted(v1) - S.2)^2)/(nrow(S.2) * ncol(S.2))


matplot(S.2 - fitted(v1), type = "l")

matplot(fitted(v1), type = "l")


Err.df = data.frame(Error = as.vector(as.matrix(S.2 - fitted(v1))),
                    Location = rep(1:30, each = nrow(S.2)),
                    Date = rep(as.Date(Ent.Dates)), ncol(S.2))

ggplot(data = Err.df, aes(x = Date, y = Error, colour = as.factor(Location))) + geom_line() +
  labs(color = "Location", x = "Date", y = "Prediction Error",
       title = "VAR Prediction Error") +  theme_bw()  + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))




Ent.Dates = rep(0, ncol(S.Ent))

for(i in 13:nrow(datesNOAA)){
  m2 = datesNOAA[i,2]
  if(nchar(m2) == 1){
    m2 = paste0("0", m2)
  }
  Ent.Dates[i - 12] = paste(datesNOAA[i,1], m2, "15", sep = "-")
}


S.Ent.df = data.frame(Entropy = c(S.Ent[Dar,], S.Ent[Melb,]), Location = rep(c("Darwin", "Melbourne"), each = ncol(S.Ent)), Dates = rep(as.Date(Ent.Dates), 2))

ggplot(S.Ent.df) + geom_line(aes(x = Dates, y = Entropy, colour = Location), size = 1) +
  labs(color = "Location", x = "Date", y = "Entropy",
       title = "Sliding Entropy Time-Series") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=15), legend.position = "right") + scale_color_manual(values = c("blue",
                                                                                                                                      "red"))


dd1 = cbind(Slide.EDQ, seq(0,1,0.001))
v1 = NULL
for(i in unique(Slide.EDQ)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Slide.df1 = data.frame(longitude = NOAA.aus[[1]][sample2,2][unique(Slide.EDQ)], latitude = NOAA.aus[[1]][sample2,1][unique(Slide.EDQ)], Quantile = v1)

g.Slide.NOAA = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Slide.df1, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "NOAA Entropy EDQ Locations")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Slide.NOAA



S.Ent.list1 = list()
S.Ent.list1[[1]] = J1[sample2,]

Slide.EDQ1 = MEDQ(S.Ent.list1, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

dd1 = cbind(Slide.EDQ1, seq(0,1, length.out = length(Slide.EDQ1)))
v1 = NULL
for(i in unique(Slide.EDQ1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

Slide.df2 = data.frame(longitude = ((JAXA[[1]][NJ.loc,2])[sample2])[unique(Slide.EDQ1)], latitude = ((JAXA[[1]][NJ.loc,1])[sample2])[unique(Slide.EDQ1)], Quantile = v1)

g.Slide.JAXA = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Slide.df2, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "JAXA Entropy EDQ Locations")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))
g.Slide.JAXA

Slide.df = rbind(Slide.df1, Slide.df2)

Slide.df$Type = factor(c(rep("NOAA", nrow(Slide.df1)), rep("JAXA", nrow(Slide.df2))), levels = c("NOAA", "JAXA"))


g.Slide = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Slide.df, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) + facet_wrap(~Type) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Entropy EDQ Locations")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))

dates.edq = rep(0, nrow(dates2))
for(i in 1:nrow(dates2)){
  m2 = dates2[i,2]
  if(nchar(m2) == 1){
    m2 = paste0("0", m2)
  }
  dates.edq[i] = paste(dates2[i,1], m2, "15", sep = "-")
}

dates.slide = dates.edq[-(1:12)]


TS.1 = as.vector(t(S.Ent))
time1 = rep(as.Date(dates.slide), nrow(S.Ent))
group1 = rep(1:nrow(S.Ent), each = ncol(S.Ent))

TS.df = data.frame(Time = time1, Ar1 = TS.1, Group = group1)

TS.1.1 = as.vector(t(S.Ent[unique(Slide.EDQ),]))
time1.1 = rep(as.Date(dates.slide), nrow(S.Ent[unique(Slide.EDQ),]))
group1.1 = rep(1:nrow(S.Ent[unique(Slide.EDQ),]), each = ncol(S.Ent[unique(Slide.EDQ),]))

dd1 = cbind(Slide.EDQ, p1)
v1 = NULL
for(i in unique(Slide.EDQ)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

TS.df.1 = data.frame(Time = time1.1, Ar1 = TS.1.1, Group = group1.1, Value = rep(v1, each = ncol(S.Ent)))


g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Date", y = "Entropy",
       title = "Sliding Monthly Entropy Time Series") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value), size = 0.8) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
g1

grid.arrange(g1 + theme(legend.position = "none"), g.Slide, nrow = 1, widths = c(3,3.5))


library(geosphere)

help(distm)


distm(c(0.05,0.05), c(0.025,0.025), fun = distCosine)

distm(c(145.000439,-37.759651), c(145.001734,-37.752290), fun = distCosine)




r1 = rdirichlet(10^6, alpha.mat[Melb,])
Melb.delta = rnorm(10^6, mean = mu.1[Melb], sd = sigma.1[Melb])

Melb.sim = apply(r1, 1, entropy1)

Melb.approx = 0.5 - 6 * rowSums(r1^2) + log(12)

r2 = rdirichlet(10^6, alpha.mat[Dar,])
Dar.delta = rnorm(10^6, mean = mu.1[Dar], sd = sigma.1[Dar])

Dar.sim = apply(r2, 1, entropy1)

Dar.approx = 0.5 - 6 * rowSums(r2^2) + log(12)


Dist.df = data.frame(Value = c(Melb.sim, Melb.approx, Melb.delta, Dar.sim, Dar.approx, Dar.delta),
                     Location = rep(c("Melbourne", "Darwin"), each = 3 * 10^6),
                     Type = factor(rep(c("Simulated", "Taylor's Approximation", "Delta Method"), each = 10^6),
                                      levels = c("Simulated", "Taylor's Approximation", "Delta Method")))

ggplot(Dist.df) + geom_density(aes(x = Value), size = 0.8) + facet_grid(Type ~ Location) +
  labs(x = "Value", y = "Density", title = "Entropy Distributions") + theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 12, colour = "black", angle = 90),
        strip.text.x = element_text(size = 12))




Dist.df = data.frame(Value = c(Melb.sim, Melb.approx, Dar.sim, Dar.approx),
                     Location = rep(c("Melbourne", "Darwin"), each = 2 * 10^6),
                     Type = factor(rep(c("Simulated", "Approximated"), each = 10^6),
                                   levels = c("Simulated", "Approximated")))

ggplot(Dist.df) + geom_density(aes(x = Value), size = 0.8) + facet_grid(Type ~ Location) +
  labs(x = "Value", y = "Density", title = "Entropy Distributions") + theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 12, colour = "black", angle = -90),
        strip.text.x = element_text(size = 12))



################
#Generalised Dirichlet
################

install.packages("MGLM")
library("MGLM")

n <- 2000
p <- 5
d <- 4
m <- rep(20, n)
set.seed(1234)
X <- 0.1* matrix(rnorm(n*p),n, p)
alpha <- matrix(1, p, d-1)
beta <- matrix(1, p, d-1)
Alpha <- exp(X %*% alpha)
Beta <- exp(X %*% beta)
gdm.Y <- rgdirmn(n, m, Alpha, Beta)

##----------------------------------------##
## Regression
gdm.reg <- MGLMreg(gdm.Y~X, dist="GDM", LRT=FALSE)

MGLMreg(P.list[[Melb]][,-12] ~ ., dist="GDM", LRT=FALSE)

MGLMfit(matrix(NOAA.all.aus[Melb, 1:(41*12)], nrow = 41, ncol = 12, byrow = TRUE), dist = "GDM")

data(rnaseq)
Y <- as.matrix(rnaseq[, 1:6])
fit <- MGLMfit(data=Y, dist="GDM") 


E2 = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12))
for(i in 1:nrow(NOAA.all.aus)){
  P.temp = matrix(NOAA.all.aus[i,1:(floor(ncol(NOAA.all.aus)/12)* 12)], ncol = 12, nrow = floor(ncol(NOAA.all.aus)/12))
  E2[i,] = apply(P.temp, 1, entropy1)
}

Cor.Ent = matrix(0, nrow = nrow(E2) * (nrow(E2) - 1)/2, ncol = 2)
Cor.Mat = matrix(0, nrow(E2), nrow(E2))
k = 1
for(i in 1:(nrow(E2) - 1)){
  for(j in (i + 1):nrow(E2)){
    Cor.Ent[k, ] = c(cor(E2[i,], E2[j,]), distm(NOAA.aus[[1]][i,2:1], NOAA.aus[[1]][j,2:1]))
    Cor.Mat[i,j] = cor(E2[i,], E2[j,])
    k = k + 1
  }
}


colnames(Cor.Ent) = c("Correlation", "Distance")
Cor.Ent = as.data.frame(Cor.Ent)

Cor.Ent1 = Cor.Ent[sample(1:nrow(Cor.Ent), 2000),]

g.scor = ggplot(Cor.Ent1) + geom_point(aes(x = Distance, y = Correlation))  +
  labs(x = "Distance (m)", y = "Correlation", title = "Entropy Spatial Correlation") + theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 12, colour = "black", angle = -90),
        strip.text.x = element_text(size = 12))


Cor.Ent2 = matrix(0, nrow = ncol(E2) * (ncol(E2) - 1)/2, ncol = 2)
k = 1
for(i in 1:(ncol(E2) - 1)){
  for(j in (i + 1):ncol(E2)){
    Cor.Ent2[k, ] = c(cor(E2[,i], E2[,j]), abs(i - j))
    k = k + 1
  }
}

colnames(Cor.Ent2) = c("Correlation", "Lag")
Cor.Ent2 = as.data.frame(Cor.Ent2)

g.tcor = ggplot(Cor.Ent2) + geom_point(aes(x = Lag, y = Correlation))  +
  labs(x = "Lag (Years)", y = " ", title = "Entropy Temporal Correlation") + theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 12, colour = "black", angle = -90),
        strip.text.x = element_text(size = 12))

grid.arrange(g.scor, g.tcor, nrow = 1)


E3 = matrix(0, nrow = nrow(NOAA.all.aus), ncol = ncol(NOAA.all.aus) - 12)
for(i in 1:nrow(NOAA.all.aus)){
  P.temp = matrix(0,nrow = ncol(NOAA.all.aus) - 12, ncol = 12)
  for(j in 1:(ncol(NOAA.all.aus) - 12)){
    P.temp[j,] = NOAA.all.aus[i,j:(j+11)]
  }
  E3[i,] = apply(P.temp, 1, entropy1)
}

Cor.Ent3 = matrix(0, nrow = ncol(E3) * (ncol(E3) - 1)/2, ncol = 2)
k = 1
for(i in 1:(ncol(E3) - 1)){
  for(j in (i + 1):ncol(E3)){
    Cor.Ent3[k, ] = c(cor(E3[,i], E3[,j]), abs(i - j))
    k = k + 1
  }
}

colnames(Cor.Ent3) = c("Correlation", "Lag")
Cor.Ent3 = as.data.frame(Cor.Ent3)

Cor.Ent4 = Cor.Ent3[sample(1:nrow(Cor.Ent3), 5000),]
ggplot(Cor.Ent4) + geom_point(aes(x = Lag, y = Correlation))  +
  labs(x = "Lag (Months)", y = "Correlation", title = "Entropy Temporal Correlation") + theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 12, colour = "black", angle = -90),
        strip.text.x = element_text(size = 12))




E1
dim(E1)
E1[Dar,]

dim(P.list[[Dar]])

cor(P.list[[Dar]][-1,12], E1[Dar,-42])

cor.d = rep(0, 10)
for(i in 1:10){
  cor.d[i] = cor(P.list[[Dar]][-c(1:i),7], E1[Dar,-c(42:(43 - i))])
}

cor.m = rep(0, 10)
for(i in 1:10){
  cor.m[i] = cor(P.list[[Melb]][,7], E1[Melb,])
}



bacf <- acf(E1[Dar,], plot = FALSE, type = "covariance")
bacfdf1 <- with(bacf, data.frame(lag, acf))

q1 <- ggplot(data = bacfdf1, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocovariance Function",
       title = "Darwin Entropy Autocovariance") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q1


bacf <- acf(E1[Melb,], plot = FALSE, type = "covariance")
bacfdf2 <- with(bacf, data.frame(lag, acf))

q2 <- ggplot(data = bacfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocovariance Function",
       title = "Melbourne Entropy Autocovariance") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q2

bacfdf <- as.data.frame(rbind(bacfdf2, bacfdf1))
bacfdf$Type = factor(c(rep("Darwin", nrow(bacfdf1)), rep("Melbourne", nrow(bacfdf2))))



ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + facet_wrap(~Type, scales = "free") + theme_bw() +
  labs(x = "Lag (Years)", y = "Autocovariance Function",
       title = "Entropy Autocovariance") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size = 13, colour = "black"),
        legend.position = "right")



library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos) 




###ST Ent Kriging

library("sp")
library("spacetime")
library("ggplot2")
library("dplyr")
library("gstat")
library("RColorBrewer")
library("STRbook")
library("tidyr")

data("STObj3", package = "STRbook")
STObj4 <- STObj3[, "1993-07-01::1993-07-31"]
vv <- variogram(object = z ~ 1 + lat, # fixed effect component
                data = STObj4, # July data
                width = 80, # spatial bin (80 km)
                cutoff = 1000, # consider pts < 1000 km apart
                tlags = 0.01:6.01) # 0 days to 6 days

sepVgm <- vgmST(stModel = "separable",
                space = vgm(10, "Exp", 400, nugget = 0.1),
                time = vgm(10, "Exp", 1, nugget = 0.1),
                sill = 20)
sepVgm <- fit.StVariogram(vv, sepVgm)

metricVgm <- vgmST(stModel = "metric",
                   joint = vgm(100, "Exp", 400, nugget = 0.1),
                   sill = 10,
                   stAni = 100)
metricVgm <- fit.StVariogram(vv, metricVgm)



metricMSE <- attr(metricVgm, "optim")$value
sepMSE <- attr(sepVgm, "optim")$value

plot(vv, list(sepVgm, metricVgm), main = "Semi-variance")

spat_pred_grid <- expand.grid(
  lon = seq(-100, -80, length = 20),
  lat = seq(32, 46, length = 20)) %>%
  SpatialPoints(proj4string = CRS(proj4string(STObj3)))
gridded(spat_pred_grid) <- TRUE

temp_pred_grid <- as.Date("1993-07-01") + seq(3, 28, length = 6)

DE_pred <- STF(sp = spat_pred_grid, # spatial part
               time = temp_pred_grid) # temporal part

STObj5 <- as(STObj4[, -14], "STIDF") # convert to STIDF
STObj5 <- subset(STObj5, !is.na(STObj5$z)) # remove missing data

pred_kriged <- krigeST(z ~ 1 + lat, # latitude trend
                       data = STObj5, # data set w/o 14 July
                       newdata = DE_pred, # prediction grid
                       modelList = sepVgm, # semivariogram
                       computeVar = TRUE) 

color_pal <- rev(colorRampPalette(brewer.pal(11, "Spectral"))(16))

stplot(pred_kriged,
       main = "Predictions (degrees Fahrenheit)",
       layout = c(3, 2),
       col.regions = color_pal)

pred_kriged$se <- sqrt(pred_kriged$var1.var)
stplot(pred_kriged[, , "se"],
       main = "Prediction std. errors (degrees Fahrenheit)",
       layout = c(3, 2),
       col.regions = color_pal)



sp = cbind(x = c(0,0,1), y = c(0,1,1))
row.names(sp) = paste("point", 1:nrow(sp), sep="")
library(sp)
sp = SpatialPoints(sp)
time = as.POSIXct("2010-08-05")+3600*(10:13)
m = c(10,20,30) # means for each of the 3 point locations
mydata = rnorm(length(sp)*length(time),mean=rep(m, 4))
IDs = paste("ID",1:length(mydata))
mydata = data.frame(values = signif(mydata,3), ID=IDs)
stfdf = STFDF(sp, time, mydata)



#####
#Temporal Correlation


P.cor = matrix(0, 12, 12)
for(i in 1:length(P.list)){
  P.cor = P.cor + cov(P.list[[i]])
}



P.list = list()
for(i in 1:nrow(Aus.Season)){
  P1 = matrix(Aus.Season[i, ], nrow = floor(ncol(Aus.Season)/4), ncol = 4, byrow = TRUE)
  P1[P1 == 0] = 0.001
  P2 = t(apply(P1,1,div1))
  P.list[[i]] = P2
}


for(i in 1:length(P.list)){
  P1 = cor(P.list[[i]])
  diag(P1) = 0
  if(sum(P1 > 0) > 0){
    print(i)
  }
}

P.cor = matrix(0, 4, 4)
for(i in 1:length(P.list)){
  P.cor = P.cor + cor(P.list[[i]])
}

P.cor/1391



Y.mat = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12))
for(i in 1:nrow(NOAA.all.aus)){
  Temp = matrix(NOAA.all.aus[i,1:(floor(ncol(NOAA.all.aus)/12) * 12 )], nrow = 42, ncol = 12, byrow = TRUE)
  Y.mat[i,] = rowSums(Temp)
}

Cor.Ent = rep(0, nrow(E1))
for(i in 1:nrow(E1)){
  Cor.Ent[i] = cor(E1[i,], Y.mat[i,])
}


CE.df = data.frame(Correlation = Cor.Ent, Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon)

idw1 = idw(formula = Correlation ~ 1, locations = ~Lon + Lat, data = CE.df, newdata = Austra2[,2:1], idp = 3)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Correlation", x = "Longitude", y = "Latitude",
       title = "Australian Entropy Precipitation Correlation") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")




####
#FlexDir


FD.estimation(data, normalize = F, iter.initial.SEM = 50,
              iter.final.EM = 100, verbose = T)

data <- FD.generate(n=20,a=c(12,7,15),p=c(0.3,0.4,0.3),t=8)
data
results <- FD.estimation(data, normalize=TRUE,iter.initial.SEM = 5,iter.final.EM = 10)
results
summary(results)








