#################
##k-means on Fused precipitation Data
New.Gauge1


Partitions = seq(1, ncol(New.Gauge1) + 1, 12)

X = list()
for(i in 1:(length(Partitions) - 1)){
}



MEDQ50 = MEDQdf[MEDQdf$Type == 50,]
Mloc = rep(0, nrow(MEDQ50))
for(i in 1:nrow(MEDQ50)){
  Mloc[i]= order(abs(NOAA.aus[[1]]$Lon - MEDQ50$Lon[i]) + abs(NOAA.aus[[1]]$Lat - MEDQ50$Lat[i]))[1]
}

fviz_nbclust(scale(X[[8]]), kmeans, method = "silhouette")

X1 = X[[1]]
colnames(X1) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
X1 = as.data.frame(X1)

row.names(X1) = paste0("Loc", 1:nrow(X1))

X2 = scale(X1)

fviz1(X[[1]], kmeans, nstart = 25, method = "gap_stat", nboot = 100, k.max = 10)


X1 = list()
for(i in 1:(length(Partitions) - 1)){
  X1[[i]] = New.Gauge1[Mloc,Partitions[i]:(Partitions[i + 1] - 1)]
}

X2 = data.frame(X[[17]][,3])

fviz_nbclust(X2, kmeans, method = "silhouette")

k1 = kmeans(X2, 2, iter.max = 100)


Mar1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Cluster = k1$cluster)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Mar1, mapping = aes(x = Lon, y = Lat, colour = as.factor(Cluster)), size = 2) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)

k.opt.month = NULL
for(i in 1:ncol(New.Gauge1)){
  f1 = fviz_nbclust(matrix(New.Gauge1[,i], ncol = 1), kmeans, method = "silhouette")
  k.opt.month = c(k.opt.month, which(f1$data$y == max(f1$data$y))[1])
  print(i)
}


p1 = PRclust()

kmax = 100
nb = 100
SE1 = list()
B1 = rep(0, kmax)
for(i in 1:kmax){
  SE1[[i]] = rep(0, nb)
}

for(i in 1:nb){
  B = matrix(runif(nrow(New.Gauge1) * 12, min = 0, max = 1), nrow = nrow(New.Gauge1), ncol = 12)
  d1 = as.matrix(dist(B))
  for(k in 1:kmax){
    k1 = kmeans(B, k)
    Se1 = 0
    for(j in 1:k){
      Se1 = Se1 + sum(d1[k1$cluster == j, k1$cluster == j])/sum(k1$cluster == j)
    }
    SE1[[k]][i] = Se1
  }
  print(i)
}

F1 = list()
for(i in 1:length(X)){
  F1[[i]] = matrix(0, nrow = kmax, ncol = 3)
  J2 = X[[i]]
  d1 = as.matrix(dist(J2))
  Wk = rep(0, kmax)
  EK = rep(0, kmax)
  for(k in 1:kmax){
    k1 = kmeans(J2, centers = k, iter.max = 50)
    for(j in 1:k){
      Wk[k] = Wk[k] + sum(d1[k1$cluster == j, k1$cluster == j])/sum(k1$cluster == j)
    }
    F1[[i]][k,] = c(log(Wk[k]), mean(log(SE1[[k]])), sd(SE1[[k]]/nb))
  }
}

F1[[3]][-nrow(F1[[2]]),2] - F1[[3]][-nrow(F1[[2]]),1] >= F1[[3]][-1,2] - F1[[3]][-1,1] - F1[[3]][-1,3]

k2 = kmeans(X, 3)
kmin = rep(0, 50)
for(i in 1:50){
  kmin[i] = GCV(t(X),lambda1=1,lambda2=1,tau=i,sigma=0.25, B = 10)[2]
}

c1 = GCV(t(X),lambda1=1,lambda2=1,tau=i,sigma=0.25, B = 10)$GCV
set.seed(1)
library("prclust")
data = matrix(NA,2,50)
data[1,1:25] = rnorm(25,0,0.33)
data[2,1:25] = rnorm(25,0,0.33)
data[1,26:50] = rnorm(25,1,0.33)
data[2,26:50] = rnorm(25,1,0.33)
#case 1
gcv1 = GCV(data,lambda1=1,lambda2=1,tau=1.5,sigma=0.25)
gcv1


df1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Precip = X[,6])

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(df1, mapping = aes(x = Lon, y = Lat, colour = Precip), size = 1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13), strip.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))




gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)











####################
#K-means on daily JAXA data

for(i in 1:length(JAXA.daily1)){
  J2 = JAXA.daily1[[i]][,-c(1,2)]
  f1 = fviz_nbclust(J2, kmeans, k.max = 5,  method = "gap_stat", nboot = 20)
  k2 = kmeans(J2, j, iter.max = 50)
  J3 = k2$centers[k2$cluster,]
  sse[j - 9] = sum((J2 - J3)^2)/(nrow(J2) - ncol(J2)) + 3*j
}


# Standardize the data
df <- (USArrests)
head(df)

set.seed(123)
fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
  labs(subtitle = "Gap statistic method")

f1 = fviz_nbclust(df, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)


k2 = kmeans(J2, 2)

sum(dist(df[k2$cluster == 1,]))

d2 = dist(df)

d1 = dist(J2)

d2 = as.matrix(d1)
d3 = d2[k2$cluster == 1, k2$cluster == 1]

k3 = kmeans(df, 2)

SE.sim = NULL
for(i in 1:30){
  B = matrix(runif(nrow(df) * ncol(df)), nrow = nrow(df), ncol = ncol(df))
  
  k4 = kmeans(B, 2)
  
  d4 = as.matrix(dist(B))
  
  d5 = d4[k4$cluster == 1, k4$cluster == 1]
  
  d6 = d4[k4$cluster == 2, k4$cluster == 2]
  
  log(sum(d5)/sum(k4$cluster == 1) + sum(d6)/sum(k4$cluster == 2))
  SE.sim = c(SE.sim, log(sum(d5)/sum(k4$cluster == 1) + sum(d6)/sum(k4$cluster == 2)))
}


kmax = 5
nb = 100
SE1 = list()
B1 = rep(0, kmax)
for(i in 1:kmax){
  SE1[[i]] = rep(0, nb)
}

for(i in 1:nb){
  B = matrix(runif(nrow(J2) * ncol(J2)), nrow = nrow(J2), ncol = ncol(J2))
  d1 = as.matrix(dist(B))
  for(k in 1:kmax){
    k1 = kmeans(B, k)
    Se1 = 0
    for(j in 1:k){
      Se1 = Se1 + sum(d1[k1$cluster == j, k1$cluster == j])/sum(k1$cluster == j)
    }
    SE1[[k]][i] = Se1
  }
  print(i)
}

for(i in 1:length(JAXA.daily1)){
  J2 = JAXA.daily1[[i]][,-c(1,2)]
  d1 = as.matrix(dist(J2))
  Wk = rep(0, kmax)
  EK = rep(0, kmax)
  for(k in 1:kmax){
    k1 = kmeans(J2, centers = k)
    for(j in 1:k){
      Wk[k] = Wk[k] + sum(d1[k1$cluster == j, k1$cluster == j])/sum(k1$cluster == j)
    }
  }
}


for(i in 1:length(NOAA.aus)){
  date1 = NOAA.aus[[i]]$Month[1]
  if(nchar(date1) == 1){
    date1 = paste0("0", date1)
  }
  write.csv(NOAA.aus[[i]], file = paste0("NOAA.aus", NOAA.aus[[i]]$Year[1], date1), row.names = FALSE)
}

for(i in 1:length(NOAA)){
  NOAA[[i]] = NOAA[[i]][!duplicated(NOAA[[i]][,1:2]),]
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/NOAAWorld")

for(i in 1:length(NOAA)){
  date1 = NOAA[[i]]$Month[1]
  if(nchar(date1) == 1){
    date1 = paste0("0", date1)
  }
  write.csv(NOAA[[i]], file = paste0("NOAA", NOAA[[i]]$Year[1], date1), row.names = FALSE)
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXAMonthly")
for(i in 1:length(JAXA.Monthly)){
  date1 = JAXA.Monthly[[i]]$Month[1]
  if(nchar(date1) == 1){
    date1 = paste0("0", date1)
  }
  write.csv(JAXA.Monthly[[i]], file = paste0("JAXA", JAXA.Monthly[[i]]$Year[1], date1), row.names = FALSE)
}



ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(JAXA.aus[[1]], mapping = aes(x = Longitude, y = Latitude, colour = Precip), size = 1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13), strip.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))

JAXA.aus = list()
A1 = JAXA.Monthly[[1]][,3:2]
A1 = as.data.frame(A1)
coordinates(A1) =~Longitude + Latitude
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus = which(a3$ISO3 == "AUS")

for(i in 1:length(JAXA.Monthly)){
  JAXA.aus[[i]] = JAXA.Monthly[[i]][Aus,]
}


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/JAXAMonthly")
for(i in 1:length(JAXA.Monthly)){
  date1 = JAXA.Monthly[[i]]$Month[1]
  if(nchar(date1) == 1){
    date1 = paste0("0", date1)
  }
  write.csv(JAXA.Monthly[[i]], file = paste0("JAXA", JAXA.Monthly[[i]]$Year[1], date1), row.names = FALSE)
}









k1 = list()
k2 = matrix(0, nrow = nrow(X[[1]]) * length(X) * length(2:10), ncol = 5)
j = 1
for(i in 1:length(X1)){
  for(k in 2:10){
    k2[((j - 1) * nrow(X[[1]]) + 1):(j * nrow(X[[1]])),] = cbind(NOAA.aus[[1]]$Lon,
                                                                 NOAA.aus[[1]]$Lat,
                                                                 kmeans(scale(X[[i]]), k, iter.max = 100)$cluster,
                                                                 rep(2000 + i, nrow(NOAA.aus[[1]])),
                                                                 rep(k, nrow(NOAA.aus[[1]])))
    j = j + 1
  }
}

colnames(k2) = c("Lon", "Lat", "Cluster", "Year", "Number")

k2 = as.data.frame(k2)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(k1[[2]][[9]], mapping = aes(x = Lon, y = Lat, colour = as.factor(Cluster)), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "January EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_fill_brewer(palette="Dark2") 


write.csv(k2, file = "Clustering", row.names = FALSE)



######
#s-LID code
######


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




s2 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, sLID = s1[,1], Class = s1[,2])


slid.vals = c(25, 50, 100, 250, 500, 750)
s2 = NULL
for(i in slid.vals){
  s1 = slid(X2, i)
  s2 = rbind(s2, cbind(NOAA.aus[[1]]$Lon, NOAA.aus[[1]]$Lat, s1, rep(i, nrow(s1)), X2))
}

colnames(s2) = c("Lon", "Lat", "sLID", "Class", "s", "Precipitation")
s2 = as.data.frame(s2)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(s2, mapping = aes(x = Lon, y = Lat, colour = as.factor(Class)), size = 1) + facet_wrap(~s) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "sLID March 2017")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  )

ggplot(s2, mapping = aes(x = Precipitation, y = sLID), size = 1) + 
  geom_point() + facet_wrap(~s) +
  theme_bw() + labs(x = "Precipitation", y = "sLID", title = "sLID March 2017")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  )


pred.grid = data.frame(Lon = Austra2[,2], Lat = Austra2[,1])


s25 = s2[s2$s = 25,]

idw1 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2[s2$s == 25,], newdata = pred.grid, idp = 3)
idw2 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2[s2$s == 50,], newdata = pred.grid, idp = 3)
idw3 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2[s2$s == 100,], newdata = pred.grid, idp = 3)
idw4 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2[s2$s == 250,], newdata = pred.grid, idp = 3)
idw5 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2[s2$s == 500,], newdata = pred.grid, idp = 3)
idw6 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2[s2$s == 750,], newdata = pred.grid, idp = 3)

s3 = rbind(idw1, idw2, idw3, idw4, idw5, idw6)

s3$s = rep(slid.vals, each = nrow(idw1))

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(s2, mapping = aes(x = Lon, y = Lat, colour = sLID), size = 2) + facet_wrap(~s) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "sLID March 2017")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(s3, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~s) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "sLID March 2017")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))


slid.vals = c(750)
SLID.TS = matrix(0, nrow = nrow(New.Gauge1) * ncol(New.Gauge1) * length(slid.vals), ncol = 8)
k = 1
SLID.list = list()
for(i in 1:length(slid.vals)){
  SLID.list[[i]] = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
}
for(i in 1:ncol(New.Gauge1)){
  for(j in slid.vals){
    s1 = slid(matrix(New.Gauge1[,i], ncol = 1), j)
    SLID.TS[((k - 1) * nrow(New.Gauge1) + 1):(k * nrow(New.Gauge1)),] = cbind(NOAA.aus[[1]]$Lon, NOAA.aus[[1]]$Lat,
                                                                              s1[,1], s1[,2], rep(j, nrow(New.Gauge1)), New.Gauge1[,i],
                                                                              NOAA.aus[[i + 255]]$Year, NOAA.aus[[i + 255]]$Month)
    SLID.list[[which(slid.vals == j)]][,i] = s1[,1]
    
    k = k + 1
  }
  print(i)
}

colnames(SLID.TS) = c("Lon", "Lat", "sLID", "Class", "s", "Precipitation", "Year", "Month")
SLID.TS = as.data.frame(SLID.TS)

SLID.TS$time = rep(1:ncol(New.Gauge1), each = nrow(New.Gauge1) * length(slid.vals))

SLID.TS$Date = as.Date(paste(SLID.TS$Year, ifelse(nchar(SLID.TS$Month) == 1, paste0("0", SLID.TS$Month), SLID.TS$Month), "15", sep = "-"))

dateSLID = unique(SLID.TS$Date)

splinecoefs = matrix(0, nrow = length(slid.vals) * length(dateSLID), ncol = 4)
r.val = matrix(0, nrow = length(slid.vals) * length(dateSLID), ncol = 2)
k = 1
for(i in 1:length(dateSLID)){
  for(j in 1:length(slid.vals)){
    temp.data = SLID.TS[which(SLID.TS$Date == dateSLID[i] & SLID.TS$s == slid.vals[j]),]
    mod1 = lm(Precipitation ~ bs(sLID), data = temp.data)
    s1 = summary(mod1)
    r.val[k,] = c(s1$r.squared, slid.vals[j])
    splinecoefs[k,] = mod1$coefficients
    k = k + 1
  }
}





which(SLID.TS$Year == 2021 & SLID.TS$Month == 3 & SLID.TS$s == 25)

SLID.TS1 = SLID.TS[-c(1:(which(SLID.TS$Year == 2021 & SLID.TS$Month == 3)[1] - 1)),]

datesidw = datesNOAA[c(448:459),]


IDW.mat = matrix(0, dim(Austra2) * 12 * 6, 6)
pred.grid.aus = data.frame(Lon = Austra2$Lon, Lat = Austra2$Lat)
k = 1
for(i in 1:nrow(datesidw)){
  for(j in 1:6){
    s2 = SLID.TS[which(SLID.TS$Year == datesidw[i,1] & SLID.TS$Month == datesidw[i,2] & SLID.TS$s == slid.vals[j]), ]
    idw1 = idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = s2, newdata = pred.grid.aus, idp = 3)
    IDW.mat[((k - 1) * nrow(Austra2) + 1):(k * nrow(Austra2)),] = cbind(Austra2$Lon, Austra2$Lat, 
                                                                        idw1$var1.pred, rep(slid.vals[j], nrow(Austra2)),
                                                                        rep(datesidw[i,1], nrow(Austra2)),
                                                                        rep(datesidw[i,2], nrow(Austra2)))
    k = k + 1
  }
}





colnames(IDW.mat) = c("Lon", "Lat", "sLID", "s", "Year", "Month")
IDW.mat = as.data.frame(IDW.mat)

write.csv(IDW.mat, file = "IDWmat", row.names = FALSE)

#############
#sLID for Daily
#############


dates.Daily = NULL
for(i in 1:nrow(dates4)){
  dates.Daily = rbind(dates.Daily, cbind(rep(dates4[i,1], dates4[i,3]), rep(dates4[i,2], dates4[i,3]), 1:dates4[i,3]))
}


A1 = JAXA.daily1[[length(JAXA.daily1)]][,2:1]
A1 = as.data.frame(A1)
colnames(A1) = c("Lon", "Lat")
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus.JAXA = which(a3$ISO3 == "AUS")

JAXA.all.aus = JAXA.all[Aus.JAXA,]


slid.vals = c(25, 50, 100, 500, 1000, 5000)
SLID.TS = matrix(0, nrow = length(samp2) * ncol(JAXA.all.aus) * length(slid.vals), ncol = 9)
k = 1
set.seed(1998)
samp2 = sample(1:nrow(JAXA.all.aus), 5000)
for(i in 1:ncol(JAXA.all.aus)){
  for(j in slid.vals){
    s1 = slid(matrix(JAXA.all.aus[samp2,i], ncol = 1), j)
    SLID.TS[((k - 1) * length(samp2) + 1):(k * length(samp2)),] = cbind(JAXA.daily1[[1]]$longitude[samp2], JAXA.daily1[[1]]$Latitude[samp2],
                                                                              s1[,1], s1[,2], rep(j, length(samp2)), JAXA.all.aus[samp2,i],
                                                                              rep(dates.Daily[i,1], length(samp2)), 
                                                                          rep(dates.Daily[i,2], length(samp2)),
                                                                          rep(dates.Daily[i,3], length(samp2)))
    k = k + 1
  }
  print(i)
}



pr1 = prcomp(SLID.list[[1]])

ts.plot(t(pr1$rotation))


ts.plot(t(SLID.list[[1]]))

which(SLID.list[[1]] == max(SLID.list[[1]]), arr.ind = TRUE)


ar1 = auto.arima(SLID.list[[2]][1389,])

sLID.cor = matrix(0, nrow = nrow(SLID.list[[1]]), ncol = 6)

for(i in 1:nrow(SLID.list[[1]])){
  for(j in 1:length(SLID.list)){
    sLID.cor[i,j] = cor(New.Gauge1[i,], SLID.list[[j]][i,])
  }
}

colnames(sLID.cor) = c("s1", "s2", "s3", "s4", "s5", "s6")
sLID.cor = as.data.frame(sLID.cor)

sLID.cor$Lon = NOAA.aus[[1]]$Lon
sLID.cor$Lat = NOAA.aus[[1]]$Lat


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(sLID.cor, mapping = aes(x = Lon, y = Lat, colour = s6), size = 2) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))


idw1 = idw(formula = s1 ~ 1, locations = ~Lon + Lat, data = sLID.cor, newdata = pred.grid, idp = 3)
idw2 = idw(formula = s2 ~ 1, locations = ~Lon + Lat, data = sLID.cor, newdata = pred.grid, idp = 3)
idw3 = idw(formula = s3 ~ 1, locations = ~Lon + Lat, data = sLID.cor, newdata = pred.grid, idp = 3)
idw4 = idw(formula = s4 ~ 1, locations = ~Lon + Lat, data = sLID.cor, newdata = pred.grid, idp = 3)
idw5 = idw(formula = s5 ~ 1, locations = ~Lon + Lat, data = sLID.cor, newdata = pred.grid, idp = 3)
idw6 = idw(formula = s6 ~ 1, locations = ~Lon + Lat, data = sLID.cor, newdata = pred.grid, idp = 3)

sLID.cor1 = rbind(idw1, idw2, idw3, idw4, idw5, idw6)

sLID.cor1$s = rep(slid.vals, each = nrow(idw1))

ggplot() + 
  geom_point(sLID.cor1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~s) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "sLID-Precipitation Correlation", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)



bSpline(SLID.list[[6]][,2])



bs1 = bs(SLID.list[[1]][,2])

bs1 = list()
for(i in 1:ncol(SLID.list[[1]])){
  bs1[[i]] = bs(SLID.list[[1]][,i])
}

mod1 = lm(New.Gauge1[,1] ~ bs1[[1]])

bs2 = list()
for(i in 1:nrow(New.Gauge1)){
  bs2[[i]] = matrix(0, nrow = length(bs1), ncol = 4)
}

for(i in 1:length(bs2)){
  for(j in 1:length(bs1)){
    bs2[[i]][j,] = c(New.Gauge1[i,j], bs1[[j]][i,])
  }
}


auto.arima(bs2[[1]][,1], xreg = bs2[[1]][,-1])



arim.out = NULL
for(i in 1:length(bs2)){
  ar1 = auto.arima(bs2[[i]][,1], xreg = bs2[[i]][,-1])
  arim.out = c(arim.out, sum(ar1$residuals^2)/(length(ar1$residuals)))
}



Season.adj = New.Gauge1
for(i in 1:12){
  Season.adj[,seq(i, ncol(New.Gauge1), 12)] = Season.adj[,seq(i, ncol(New.Gauge1), 12)] - rowMeans(Season.adj[,seq(i, ncol(New.Gauge1), 12)])
}


precip.var.t = NULL
for(i in 1:ncol(New.Gauge1)){
  precip.var.t = c(precip.var.t, var(New.Gauge1[,i]))
}

cor.spat = NULL
for(i in 1:nrow(New.Gauge1)){
  cor.spat = c(cor.spat, cor(precip.var.t, New.Gauge1[i,]))
}

cor.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Cor = cor.spat)

idw1 = idw(formula = Cor ~ 1, locations = ~Lon + Lat, data = cor.df, newdata = pred.grid, idp = 3)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Spatial Variation-Precipitation Correlation", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))





k.opt.month = NULL
dist2 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
for(i in 1:ncol(New.Gauge1)){
  f1 = fviz_nbclust(matrix(New.Gauge1[,i], ncol = 1), kmeans, method = "silhouette")
  k1 = kmeans(matrix(New.Gauge1[,i], ncol = 1), centers = which(f1$data$y == max(f1$data$y))[1], iter.max = 50)
  dist1 = abs(New.Gauge1[,i] - k1$centers[k1$cluster])
  dist2[,i] = dist1
  k.opt.month = c(k.opt.month, which(f1$data$y == max(f1$data$y))[1])
  print(i)
}


cor.kdist = NULL
for(i in 1:nrow(New.Gauge1)){
  cor.kdist = c(cor.kdist, cor(dist2[i,], New.Gauge1[i,]))
}


cor.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Cor = cor.kdist)

idw1 = idw(formula = Cor ~ 1, locations = ~Lon + Lat, data = cor.df, newdata = pred.grid, idp = 3)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "kmeans dist-Precipitation\nCorrelation", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))

install.packages("DDoutlier")
library("DDoutlier")


###COF

COF.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = COF(X)
  COF.mat[,i] = c1
  print(i)
}

####DB

DB.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = DB(X)
  DB.mat[,i] = c1$neighbors
}

###INFLO

INFLO.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = INFLO(X)
  INFLO.mat[,i] = c1
}

###KDEOS 

KDEOS.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = KDEOS(X)
  KDEOS.mat[,i] = c1
}

###KNN_AGG

KNN_AGG.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = KNN_AGG(X)
  KNN_AGG.mat[,i] = c1
}

###KNN_IN

KNN_IN.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = KNN_IN(X)
  KNN_IN.mat[,i] = c1
}

###KNN_IN

KNN_SUM.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = KNN_SUM(X)
  KNN_SUM.mat[,i] = c1
}


###LDF 

LDF.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = LDF(X)
  LDF.mat[,i] = c1$LDE
}

###LODF 

LDOF.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = LDOF(X)
  LDOF.mat[,i] = c1
}


###LOCI 

LOCI.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = LOCI(X)
  LOCI.mat[,i] = c1
}

###LOF 

LOF.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = LOF(X)
  LOF.mat[,i] = c1
}

###LOOP

LOOP.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = LOOP(X)
  LOOP.mat[,i] = c1
}

###NAN 

NAN.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = NAN(X)
  NAN.mat[,i] = c1
}

###NOF

NOF.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = NOF(X)
  NOF.mat[,i] = c1
}

###RDOS

RDOS.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = RDOS(X)
  RDOS.mat[,i] = c1
}

###RKOF

RKOF.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  X = data.frame(x1 = New.Gauge1[,i])
  c1 = RKOF(X)
  RKOF.mat[,i] = c1
}




library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)


ggplot(gapminder, aes(gdpPercap, lifeExp, size = pop, color = continent)) +
  geom_point() +
  scale_x_log10() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Year: {frame_time}', x = 'GDP per capita', y = 'life expectancy') +
  transition_time(year) +
  ease_aes('linear')


dates2 = rep(0, nrow(IDW.mat))
for(i in 1:nrow(IDW.mat)){
  m1 = IDW.mat$Month[i]
  if(nchar(m1) == 1){
    m1 = paste0("0", m1)
  }
  dates2[i] = paste0(IDW.mat$Year[i], "-", m1)
  if(i %% 100000 == 0){
    print(i)
  }
}

IDW.mat$Date2 = dates2

IDW1 = IDW.mat[which(IDW.mat$Month == 3 & IDW.mat$Year == 2021),]


# Make a ggplot, but add frame=year: one image per year
ggplot(IDW.mat, aes(x = Lon, y = Lat, colour = sLID)) +
  geom_point(alpha = 0.7, size = 0.01) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "{closest_state}", x = 'Longitude', y = 'Latitude') +
  transition_states(Date2) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) 




colnames(r.val) = c("R.Val", "s")
r.val = as.data.frame(r.val)
r.val$s = as.factor(r.val$s)

ggplot(data = r.val, aes(x = s, y = R.Val)) + geom_boxplot()  +
  theme_bw() + labs(x = "s Value", y = "R Squared Value", title = "Precipitation-sLID Cubic Spline")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")


ggplot(data = sLID.cor1, aes(x = as.factor(s), y = var1.pred)) + geom_boxplot()  +
  theme_bw() + labs(x = "s Value", y = "Correlation", title = "Precipitation-sLID Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")


sLID.cor2 = data.frame(Correlation = c(sLID.cor$s1, sLID.cor$s2, sLID.cor$s3,
                                       sLID.cor$s4, sLID.cor$s5, sLID.cor$s6),
                       s = rep(slid.vals, each = 1391))


ggplot(data = sLID.cor2, aes(x = as.factor(s), y = Correlation)) + geom_boxplot()  +
  theme_bw() + labs(x = "s Value", y = "Correlation", title = "Precipitation-sLID Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))




max.SLID = NULL
for(i in 1:length(unique(SLID.TS$Date))){
  for(j in 1:length(slid.vals)){
    max.SLID = rbind(max.SLID, c(max(SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j], 3]), slid.vals[j], unique(SLID.TS$Date)[i]))
  }
  print(i)
}

plot(max.SLID)

colnames(max.SLID) = c("sLID", "s", "Date")
max.SLID = as.data.frame(max.SLID)

max.SLID$s = factor(max.SLID$s, levels = c("25", "50", "100", "250", "500", "750"))

ggplot(data = max.SLID, aes(x = as.Date(Date), y = as.numeric(sLID), group = s)) + geom_line() + facet_wrap(~s) +
  theme_bw() + labs(x = "Date", y = "Skewness", title = "sLID Skewness Time Series") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), strip.text = element_text(size=14), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))


max.SLID[max.SLID$Date == "2017-3-15",]

max.SLID$sLID = as.numeric(max.SLID$sLID)



ggplot(data = max.SLID, aes(x = as.Date(Date), y = sLID, group = s)) + geom_line() + facet_wrap(~s) +
  theme_bw() + labs(x = "Date", y = "Maximum sLID", title = "Maximum sLID Time Series") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size = 12), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))

max.SLID$Class = (max.SLID$sLID > 8.665379)

max.SLID$Class1 = (max.SLID$sLID > rep(apply(max.SLID1,2,mean), nrow(max.SLID1)))

ggplot(data = max.SLID, aes(x = as.Date(Date), y = Class1, group = s)) + geom_point() + facet_wrap(~s) +
  theme_bw() + labs(x = "Date", y = "Maximum sLID", title = "Maximum sLID Time Series") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), strip.text = element_text(size = 12), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))


ggplot(data = max.SLID, aes(x = s, y = as.numeric(sLID))) + geom_boxplot()  +
  theme_bw() + labs(x = "s Value", y = "Max sLID", title = "Max sLID Boxplots")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))



max.S



Flood.Dates = c("2008-2-15", "2010-3-15", "2010-9-15", "2011-1-15", "2011-8-15",
                "2013-1-15", "2015-4-15", "2015-5-15", "2016-6-15", "2016-9-15",
                "2017-2-15", "2017-2-15", "2019-1-15", "2020-2-15", "2021-3-15",
                "2021-6-15", "2021-11-15")

Flood.sLID = NULL
for(i in 1:length(Flood.Dates)){
  Flood.sLID = rbind(Flood.sLID, max.SLID[max.SLID$Date == Flood.Dates[i],])
}

MarchSLID = SLID.TS[SLID.TS$Date == "2017-3-15",]



ggplot(data = MarchSLID, aes(x = Precipitation, y = sLID)) + geom_point() + facet_wrap(~s) +
  theme_bw() + labs(x = "Precipitation (mm)", y = "sLID", title = "2017 May sLID-Precipitation") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))



max.SLID1 = matrix(0, nrow = length(unique(SLID.TS$Date)), ncol = 6)
for(i in 1:length(unique(SLID.TS$Date))){
  for(j in 1:length(slid.vals)){
    max.SLID1[i, j] = max(SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j], 3])
  }
  print(i)
}

max.SLID2 = max.SLID1 


max.SLID2 = max.SLID1
for(i in 1:ncol(max.SLID1)){
  for(j in 1:12){
    m1 = seq(j, nrow(max.SLID1), 12)
    max.SLID2[m1, i] = max.SLID1[m1, i] - mean(max.SLID2[m1, i])
  }
}


max.SLID3 = data.frame(sLID = as.vector(t(max.SLID2)), Date = max.SLID$Date, s = max.SLID$s)


ggplot(data = max.SLID3, aes(x = as.Date(Date), y = as.numeric(sLID), group = s)) + geom_line() + facet_wrap(~s) +
  theme_bw() + labs(x = "Date", y = "Seasonally Adjusted Max sLID", title = "Seasonally Adjusted Max sLID Time Series") + 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=26), axis.text=element_text(size=18), legend.text=element_text(size=17),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=20), strip.text = element_text(size=18), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))

(ar1 = auto.arima(max.SLID2[,6]))



max.SLID = NULL
for(i in 1:length(unique(SLID.TS$Date))){
  for(j in 1:length(slid.vals)){
    O1 = order(SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j], 3], decreasing = TRUE)[1]
    S1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j], ]
    max.SLID = rbind(max.SLID, S1[O1, c(1:3, 5, 7, 8, 10)])
  }
  print(i)
}


head(max.SLID)

max.SLID = as.data.frame(max.SLID)


dates2 = rep(0, nrow(max.SLID))
for(i in 1:nrow(max.SLID)){
  m1 = max.SLID$Month[i]
  if(nchar(m1) == 1){
    m1 = paste0("0", m1)
  }
  dates2[i] = paste0(max.SLID$Year[i], "-", m1)
  if(i %% 100000 == 0){
    print(i)
  }
}

max.SLID$Date2 = dates2

s1 = seq(1, nrow(max.SLID), 12 * 6)
s1 = c(s1, nrow(max.SLID) + 1)
max.SLID1 = list()
for(i in 1:(length(s1) - 1)){
  max.SLID1[[i]] = max.SLID[(s1[i]):(s1[i + 1] - 1), ]
}
m2 = max.SLID1[[1]]

p1 = ggplot(m2, aes(x = Lon, y = Lat, colour = sLID)) +
  geom_point(alpha = 0.7, size = 3) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "{closest_state}", x = 'Longitude', y = 'Latitude') +
  transition_states(Date2) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

install.packages("gifski")
library(gifski)


Loc.SLID = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
for(i in 1:nrow(New.Gauge1)){
  Loc.SLID[i,] = slid(matrix(New.Gauge1[i,], ncol = 1), s = 12)[,1]
  print(i)
}

plot(Loc.SLID[2,], type = "l")




max.SLID.list = list()
for(i in 1:30){
    c1 = colSums(max.SLID1 >= i)
    c1 = ifelse(c1 > 0, c1, 1)
    a1 = apply(max.SLID1 >= i, 2, cumsum)
    for(j in 1:nrow(a1)){
      a1[j,] = a1[j,]/c1
    }
    max.SLID.list[[i]] = data.frame(
    Value = as.vector(a1),
    s = rep(slid.vals, each = nrow(max.SLID1)),
    Date = rep(unique(dates2), 6),
    Threshold = i)
}

plot(max.SLID.list[[15]][,1])

max.SLID.anim = NULL
for(i in 1:length(max.SLID.list)){
   max.SLID.anim = rbind(max.SLID.anim, max.SLID.list[[i]])
}


ggplot(max.SLID.anim, aes(x = Date, y = Value)) +
  geom_point() +
  theme_bw() +
  facet_wrap(~as.factor(s)) +
  # Here comes the gganimate specific bits
  labs(title = "{closest_state}", x = 'Date', y = 'Count') +
  transition_states(Threshold) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))



ggplot2::ggplot(data = max.SLID.anim) +
  ggplot2::aes(x = Date, y = Value) +
  ggplot2::geom_line(color = 'steelblue', size = 1) +
  ggplot2::facet_wrap(~ as.factor(s)) + 
  gganimate::transition_states(Threshold) +
  ggplot2::labs(title = "Threshold: {closest_state}", x = 'Longitude', y = 'Latitude') +
  gganimate::shadow_mark(colour = 'grey', size = 0.75) +
  ggplot2::theme_minimal()



SLID.year1 = SLID.list[[6]]
SLID.year1 = SLID.year1[,-c(1:length(4:13))]

w2 = NULL
for(i in 1:21){
  Temp1 = SLID.year1[, ((i - 1) * 12 + 1):(i * 12)]
  w1 = NULL
  for(j in 1:12){
    w1 = c(w1, which(Temp1[,j] == max(Temp1[,j])))
  }
  w2 = c(w2, max(table(w1)))
}

QScore = data.frame(QScore = w2, Year = 2001:2021)

ggplot(data = QScore, aes(x = Year, y = QScore)) + geom_line() + 
  theme_bw() + labs(x = "Year", y = "Q Score", title = " ") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13), strip.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))







SLID.loc = New.Gauge1
for(i in 1:nrow(New.Gauge1)){
  SLID.loc[i,] = slid(matrix(New.Gauge1[i,], ncol = 1), 100)[,1]
}


pca1 = prcomp(SLID.list[[1]])$x[,1:2]

f1 = fviz_nbclust(pca1, kmeans, method = "silhouette", k.max = 10)

k1 = kmeans(pca1, centers = 2)

kmeans.df = NULL
for(i in 1:length(SLID.list)){
  pca1 = prcomp(SLID.list[[i]])$x[,1:2]
  f1 = fviz_nbclust(pca1, kmeans, method = "silhouette", k.max = 10)
  k1 = kmeans(pca1, centers = as.numeric(f1$data$clusters[which(f1$data$y == max(f1$data$y))]))
  df1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Cluster = k1$cluster)
  idw1 = idw(formula = Cluster ~ 1, locations = ~Lon + Lat, data = df1, newdata = pred.grid.aus, idp = 3)
  kmeans.df = rbind(kmeans.df, cbind(idw1$Lon, idw1$Lat, round(idw1$var1.pred), rep(slid.vals[i], nrow(idw1))))
}

colnames(kmeans.df) = c("Lon", "Lat", "Cluster", "s")

kmeans.df = as.data.frame(kmeans.df)

ggplot() + 
  geom_point(kmeans.df, mapping = aes(x = Lon, y = Lat, colour = as.factor(Cluster)), size = 0.5) + facet_wrap(~s) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "sLID PCA Clustering", colour = "Cluster")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + guides(colour = guide_legend(override.aes = list(size=18))) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)






SLID.year1 = SLID.list[[6]]
SLID.year1 = SLID.year1[,-c(1:length(4:13))]

w2 = NULL
for(i in 1:21){
  Temp1 = SLID.year1[, ((i - 1) * 12 + 1):(i * 12)]
  q1 = apply(Temp1, 2, quantile, 0.95)
  m1 = matrix(q1, byrow = TRUE, nrow = nrow(Temp1), ncol = 12)
  w2 = c(w2, max(rowSums(Temp1 > m1)))
}


pb <- txtProgressBar(min = 1, max = 6 * length(seq(0,1,0.0025)) * length(1:21), style = 3)
QScore.mat = NULL
k1 = 1
for(j in 1:6){
  SLID.year1 = SLID.list[[j]]
  SLID.year1 = SLID.year1[,-c(1:length(4:13))]
  for(k in seq(0,1,0.0025)){
    w2 = NULL
    for(i in 1:21){
      Temp1 = SLID.year1[, ((i - 1) * 12 + 1):(i * 12)]
      q1 = apply(Temp1, 2, quantile, k)
      m1 = matrix(q1, byrow = TRUE, nrow = nrow(Temp1), ncol = 12)
      QScore.mat = rbind(QScore.mat, c(max(rowSums(Temp1 >= m1)), slid.vals[j], k, (2001:2021)[i]))
      setTxtProgressBar(pb, k1)
      k1 = k1 + 1
    }
  }
}




colnames(QScore.mat) = c("QScore", "s", "Quantile", "Year")
QScore.df = as.data.frame(QScore.mat)


write.csv(QScore.df, file = "QScoredf", row.names = FALSE)

install_github("thomasp85/transformr")



##################
##Top 10% Slid Locations
##################

SLID.top = NULL
k = 1
for(i in 1:ncol(New.Gauge1)){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    SLID.top = rbind(SLID.top, as.matrix(Temp1[o1[1:floor(nrow(Temp1)/20)],]))
    k = k + 1
  }
  print(i)
}

SLID.top = as.data.frame(SLID.top)
colnames(SLID.top) = colnames(SLID.TS)

datesSLIDtop = rep(0, nrow(SLID.top))
for(i in 1:nrow(SLID.top)){
  m1 = SLID.top$Month[i]
  if(nchar(m1) == 1){
    m1 = paste0("0", m1)
  }
  datesSLIDtop[i] = paste0(SLID.top$Year[i], "-", m1)
}

SLID.top1$Date = datesSLIDtop

ggplot() + 
  geom_point(SLID.top[SLID.top[,5] == "750", ], mapping = aes(x = as.numeric(Lon), y = as.numeric(Lat), colour = as.numeric(sLID)), size = 1) +
  facet_wrap(~Date, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Locations", colour = "Value")+ 
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))





SLID.TS$id = 1:1391


SLID.top = NULL
k = 1
for(i in 1:ncol(New.Gauge1)){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    SLID.top = rbind(SLID.top, as.matrix(Temp1[o1[1:floor(nrow(Temp1)/20)],c(1:9,11)]))
    k = k + 1
  }
  print(i)
}

SLID.top = as.data.frame(SLID.top)

S2 = NULL
for(i in 1:12){
  S1 = SLID.top[SLID.top$Month == i,]
  t1 = table(S1$id)
  S2 = rbind(S2, cbind(S1[as.numeric(names(t1)), c(1:2, 7, 8)], as.numeric(t1)))
}

colnames(S2) = c("Lon", "Lat", "Year", "Month", "Frequency")

S2 = as.data.frame(S2)

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))


ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = Frequency), size = 0.75) +
  facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Locations Frequency", colour = "Count")+ 
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))




SLID.top1 = NULL
k = 1
for(i in 1:132){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    SLID.top1 = rbind(SLID.top1, as.matrix(Temp1[o1[1:floor(nrow(Temp1)/20)],c(1:9,11)]))
    k = k + 1
  }
  print(i)
}

SLID.top1 = as.data.frame(SLID.top1)

S2 = NULL
for(i in 1:12){
  S1 = SLID.top1[SLID.top1$Month == i,]
  t1 = table(S1$id)
  S2 = rbind(S2, cbind(S1[as.numeric(names(t1)), c(1:2, 8)], as.numeric(t1)))
}

colnames(S2) = c("Lon", "Lat", "Month", "Frequency")

S2 = as.data.frame(S2)

S2.months = NULL
for(i in 1:nrow(S2)){
  m1 = S2$Month[i]
  if(m1 == 1){
    S2.months = c(S2.months, "January")
  }else if(m1 == 2){
    S2.months = c(S2.months, "February")
  }else if(m1 == 3){
    S2.months = c(S2.months, "March")
  }else if(m1 == 4){
    S2.months = c(S2.months, "April")
  }else if(m1 == 5){
    S2.months = c(S2.months, "May")
  }else if(m1 == 6){
    S2.months = c(S2.months, "June")
  }else if(m1 == 7){
    S2.months = c(S2.months, "July")
  }else if(m1 == 8){
    S2.months = c(S2.months, "August")
  }else if(m1 == 9){
    S2.months = c(S2.months, "September")
  }else if(m1 == 10){
    S2.months = c(S2.months, "October")
  }else if(m1 == 11){
    S2.months = c(S2.months, "November")
  }else if(m1 == 12){
    S2.months = c(S2.months, "December")
  }
}

S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))

t.1 = table(S2$Months)

ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = Frequency), size = 0.75) +
  facet_wrap(~Months, nrow = 3) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Locations Frequency Decade 1", colour = "Count")+ 
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))

t1.1 = data.frame(Count = as.numeric(t1), Month = names(t1))

ggplot(S2, aes(Months)) + geom_bar(fill = NA, colour = "black") + theme_bw() + labs(title = "Unique Locations in Set A", y = "Count") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), strip.text = element_text(size=18), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

SLID.top2 = NULL
k = 1
for(i in 133:ncol(New.Gauge1)){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    SLID.top2 = rbind(SLID.top2, as.matrix(Temp1[o1[1:floor(nrow(Temp1)/10)],c(1:9,11)]))
    k = k + 1
  }
  print(i)
}

SLID.top2 = as.data.frame(SLID.top2)

S2 = NULL
for(i in 1:12){
  S1 = SLID.top2[SLID.top2$Month == i,]
  t1 = table(S1$id)
  S2 = rbind(S2, cbind(S1[as.numeric(names(t1)), c(1:2, 8)], as.numeric(t1)))
}

colnames(S2) = c("Lon", "Lat", "Month", "Frequency")

S2 = as.data.frame(S2)

S2.months = NULL
for(i in 1:nrow(S2)){
  m1 = S2$Month[i]
  if(is.na(m1)){
    m1 = S2$Month[i + 1]
  }
  if(m1 == 1){
    S2.months = c(S2.months, "January")
  }else if(m1 == 2){
    S2.months = c(S2.months, "February")
  }else if(m1 == 3){
    S2.months = c(S2.months, "March")
  }else if(m1 == 4){
    S2.months = c(S2.months, "April")
  }else if(m1 == 5){
    S2.months = c(S2.months, "May")
  }else if(m1 == 6){
    S2.months = c(S2.months, "June")
  }else if(m1 == 7){
    S2.months = c(S2.months, "July")
  }else if(m1 == 8){
    S2.months = c(S2.months, "August")
  }else if(m1 == 9){
    S2.months = c(S2.months, "September")
  }else if(m1 == 10){
    S2.months = c(S2.months, "October")
  }else if(m1 == 11){
    S2.months = c(S2.months, "November")
  }else if(m1 == 12){
    S2.months = c(S2.months, "December")
  }
}

S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))

ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = Frequency), size = 0.75) +
  facet_wrap(~Months, nrow = 3) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Locations Frequency Decade 2", colour = "Count")+ 
  xlim(113, 155) + ylim(-45,-9) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))




t.2 = table(S2$Months)

#All months have fewer locations in decade 2, except September

sign(t.1 - t.2)

1 - pbinom(sum(sign(t.1 - t.2) > 0), p = 0.5, 12)



top.slid.dist = NULL
k = 1
for(i in 1:ncol(New.Gauge1)){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    Temp2 = Temp1[o1[1:floor(nrow(Temp1)/10)],c(1:9,11)]
    d1 = dist(Temp2[,1:2])
    top.slid.dist = rbind(top.slid.dist, cbind(mean(d1), Temp2$Month[1], Temp2$Year[1]))
    k = k + 1
  }
  print(i)
}

plot(top.slid.dist)

colnames(top.slid.dist) = c("Distance", "Month", "Year")

top.slid.dist = as.data.frame(top.slid.dist)

top.slid.dist.months = NULL
for(i in 1:nrow(top.slid.dist)){
  m1 = top.slid.dist$Month[i]
  if(is.na(m1)){
    m1 = top.slid.dist$Month[i + 1]
  }
  if(m1 == 1){
    top.slid.dist.months = c(top.slid.dist.months, "January")
  }else if(m1 == 2){
    top.slid.dist.months = c(top.slid.dist.months, "February")
  }else if(m1 == 3){
    top.slid.dist.months = c(top.slid.dist.months, "March")
  }else if(m1 == 4){
    top.slid.dist.months = c(top.slid.dist.months, "April")
  }else if(m1 == 5){
    top.slid.dist.months = c(top.slid.dist.months, "May")
  }else if(m1 == 6){
    top.slid.dist.months = c(top.slid.dist.months, "June")
  }else if(m1 == 7){
    top.slid.dist.months = c(top.slid.dist.months, "July")
  }else if(m1 == 8){
    top.slid.dist.months = c(top.slid.dist.months, "August")
  }else if(m1 == 9){
    top.slid.dist.months = c(top.slid.dist.months, "September")
  }else if(m1 == 10){
    top.slid.dist.months = c(top.slid.dist.months, "October")
  }else if(m1 == 11){
    top.slid.dist.months = c(top.slid.dist.months, "November")
  }else if(m1 == 12){
    top.slid.dist.months = c(top.slid.dist.months, "December")
  }
}

top.slid.dist$Months = factor(top.slid.dist.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))


ggplot(top.slid.dist, aes(x = Months, y = Distance)) + geom_boxplot() +
  labs(x = "Month", y = "Mean Euclidean Distance Value", title = "Set A Locations Spatial Distancing", colour = "Count")+ theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), strip.text = element_text(size=18), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

ggplot(top.slid.dist, aes(x = Year, y = Distance)) + geom_line() + facet_wrap(~Months) +
  labs(x = "Year", y = "Mean Euclidean Distance Value", title = "Set A Locations Spatial Distancing", colour = "Count")+ theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), strip.text = element_text(size=18), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))



top.slid.dist1 = NULL
k = 1
for(i in 1:(ncol(New.Gauge1)/2)){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    Temp2 = Temp1[o1[1:floor(nrow(Temp1)/10)],c(1:9,11)]
    d1 = dist(Temp2[,1:2])
    top.slid.dist1 = rbind(top.slid.dist1, cbind(mean(d1), Temp2$Month[1], Temp2$Year[1]))
    k = k + 1
  }
  print(i)
}


colnames(top.slid.dist1) = c("Distance", "Month", "Year")

top.slid.dist1 = as.data.frame(top.slid.dist1)

top.slid.dist.months = NULL
for(i in 1:nrow(top.slid.dist1)){
  m1 = top.slid.dist1$Month[i]
  if(is.na(m1)){
    m1 = top.slid.dist1$Month[i + 1]
  }
  if(m1 == 1){
    top.slid.dist.months = c(top.slid.dist.months, "January")
  }else if(m1 == 2){
    top.slid.dist.months = c(top.slid.dist.months, "February")
  }else if(m1 == 3){
    top.slid.dist.months = c(top.slid.dist.months, "March")
  }else if(m1 == 4){
    top.slid.dist.months = c(top.slid.dist.months, "April")
  }else if(m1 == 5){
    top.slid.dist.months = c(top.slid.dist.months, "May")
  }else if(m1 == 6){
    top.slid.dist.months = c(top.slid.dist.months, "June")
  }else if(m1 == 7){
    top.slid.dist.months = c(top.slid.dist.months, "July")
  }else if(m1 == 8){
    top.slid.dist.months = c(top.slid.dist.months, "August")
  }else if(m1 == 9){
    top.slid.dist.months = c(top.slid.dist.months, "September")
  }else if(m1 == 10){
    top.slid.dist.months = c(top.slid.dist.months, "October")
  }else if(m1 == 11){
    top.slid.dist.months = c(top.slid.dist.months, "November")
  }else if(m1 == 12){
    top.slid.dist.months = c(top.slid.dist.months, "December")
  }
}

top.slid.dist1$Months = factor(top.slid.dist.months, levels = c("January", "February", "March", "April",
                                                               "May", "June", "July", "August",
                                                               "September", "October", "November", "December"))


ggplot(top.slid.dist1, aes(x = Months, y = Distance)) + geom_boxplot() +
  labs(x = "Month", y = "Mean Euclidean Distance Value", title = "Decade 1 Set A Locations Spatial Distancing", colour = "Count")+ theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), strip.text = element_text(size=18), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))


top.slid.dist2 = NULL
k = 1
for(i in (ncol(New.Gauge1)/2 + 1):(ncol(New.Gauge1))){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    Temp2 = Temp1[o1[1:floor(nrow(Temp1)/10)],c(1:9,11)]
    d1 = dist(Temp2[,1:2])
    top.slid.dist2 = rbind(top.slid.dist2, cbind(mean(d1), Temp2$Month[1], Temp2$Year[1]))
    k = k + 1
  }
  print(i)
}


colnames(top.slid.dist2) = c("Distance", "Month", "Year")

top.slid.dist2 = as.data.frame(top.slid.dist2)

top.slid.dist.months = NULL
for(i in 1:nrow(top.slid.dist1)){
  m1 = top.slid.dist2$Month[i]
  if(is.na(m1)){
    m1 = top.slid.dist2$Month[i + 1]
  }
  if(m1 == 1){
    top.slid.dist.months = c(top.slid.dist.months, "January")
  }else if(m1 == 2){
    top.slid.dist.months = c(top.slid.dist.months, "February")
  }else if(m1 == 3){
    top.slid.dist.months = c(top.slid.dist.months, "March")
  }else if(m1 == 4){
    top.slid.dist.months = c(top.slid.dist.months, "April")
  }else if(m1 == 5){
    top.slid.dist.months = c(top.slid.dist.months, "May")
  }else if(m1 == 6){
    top.slid.dist.months = c(top.slid.dist.months, "June")
  }else if(m1 == 7){
    top.slid.dist.months = c(top.slid.dist.months, "July")
  }else if(m1 == 8){
    top.slid.dist.months = c(top.slid.dist.months, "August")
  }else if(m1 == 9){
    top.slid.dist.months = c(top.slid.dist.months, "September")
  }else if(m1 == 10){
    top.slid.dist.months = c(top.slid.dist.months, "October")
  }else if(m1 == 11){
    top.slid.dist.months = c(top.slid.dist.months, "November")
  }else if(m1 == 12){
    top.slid.dist.months = c(top.slid.dist.months, "December")
  }
}

top.slid.dist2$Months = factor(top.slid.dist.months, levels = c("January", "February", "March", "April",
                                                                "May", "June", "July", "August",
                                                                "September", "October", "November", "December"))


ggplot(top.slid.dist2, aes(x = Months, y = Distance)) + geom_boxplot() +
  labs(x = "Month", y = "Mean Euclidean Distance Value", title = "Decade 2 Set A Locations Spatial Distancing", colour = "Count")+ theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=16), legend.text=element_text(size=15),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), strip.text = element_text(size=18), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))
######################
#Ripley's K test
######################

SLID.ppp = NULL
k = 1
for(i in 1:263){
  for(j in 1:length(slid.vals)){
    Temp1 = SLID.TS[SLID.TS$Date == unique(SLID.TS$Date)[i] & SLID.TS$s == slid.vals[j],]
    o1 = order(Temp1$sLID, decreasing = TRUE)
    Temp2 = Temp1[o1[1:floor(nrow(Temp1)/10)],c(1:9,11)]
    ppp1 = as.ppp(Temp2[,1:2], ausmap)
    k = k + 1
  }
  print(i)
}


slid.vals = c(1300)
SLID.TS2 = NULL
k = 1
for(i in 1:ncol(New.Gauge1)){
  for(j in slid.vals){
    s1 = slid(matrix(New.Gauge1[,i], ncol = 1), j)
    SLID.TS2 = rbind(SLID.TS2, cbind(NOAA.aus[[1]]$Lon, NOAA.aus[[1]]$Lat, s1[,1], s1[,2], rep(j, nrow(New.Gauge1)), New.Gauge1[,i],
                                     NOAA.aus[[i + 255]]$Year, NOAA.aus[[i + 255]]$Month))
    
    k = k + 1
  }
  print(i)
}

colnames(SLID.TS2) = c("Lon", "Lat", "sLID", "Class", "s", "Precipitation", "Year", "Month")
SLID.TS2 = as.data.frame(SLID.TS2)





S2 = SLID.TS2

S2 = as.data.frame(S2)

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))



S2$Date = as.Date(paste(S2$Year, ifelse(nchar(S2$Month) == 1, paste0("0", S2$Month), S2$Month), "15", sep = "-"))

Class = NULL
for(i in 1:length(unique(S2$Date))){
  Temp = S2[S2$Date == unique(S2$Date)[i],]
  O1 = order(Temp$sLID, decreasing = TRUE)
  bin1 = rep(0, nrow(Temp))
  bin1[O1[1:69]] = 1
  Class = c(Class, bin1)
}

S2$Class = Class




p1 = ggplot(S2, aes(x = Precipitation, y = sLID, colour = factor(Class)), size = 0.5) +
  geom_point() +
  theme_bw() +
  facet_wrap(~as.factor(Months)) +
  # Here comes the gganimate specific bits
  labs(title = "Year : {closest_state}", x = 'Precipitation', y = '750-LID', colour = "Extreme\n750-LID") +
  transition_states(Year) +
  ease_aes('linear') + scale_colour_manual(
    values = c("#82db5b", "#f03c3b"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "TRUE", "0" = "FALSE")
  ) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm"))+
  guides(colour = guide_legend(override.aes = list(size=10)))


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/sLIDScatterVid")
animate(p1, nframes = 200)



S3 = S2[S2$Class == 1, ]

p2 = ggplot(S3, aes(x = Lon, y = Lat, colour = log(sLID))) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~as.factor(Months)) +
  labs(title = "Year : {closest_state}", x = 'Longitude', y = 'Latitude', colour = "Log\n750-LID") +
  transition_states(Year) +
  ease_aes('linear')  +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) + 
  geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  xlim(113, 155) + ylim(-45,-9)


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/sLIDMapVid")
animate(p2, nframes = 200)


SLID.TS2$id = 1:1391
S2 = SLID.TS2
S2 = as.data.frame(S2)

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))



S2$Date = as.Date(paste(S2$Year, ifelse(nchar(S2$Month) == 1, paste0("0", S2$Month), S2$Month), "15", sep = "-"))

Class = NULL
for(i in 1:length(unique(S2$Date))){
  Temp = S2[S2$Date == unique(S2$Date)[i],]
  O1 = order(Temp$sLID, decreasing = TRUE)
  bin1 = rep(0, nrow(Temp))
  bin1[O1[1:139]] = 1
  Class = c(Class, bin1)
}

S2$Class = Class

S2$C1 = 0

for(i in 1:1391){
  for(j in 1:12){
    Temp = S2[S2$id == i & S2$Month == j,]
    c1 = cumsum(Temp$Class)
    S2$C1[S2$id == i & S2$Month == j] = c1
  }
}

S3 = S2[S2$C1!= 0, ]


p2 = ggplot(S3, aes(x = Lon, y = Lat, colour = C1)) +
  geom_point(size = 0.5) +
  theme_bw() +
  facet_wrap(~as.factor(Months)) +
  # Here comes the gganimate specific bits
  labs(title = "Year : {closest_state}", x = 'Longitude', y = 'Latitude', colour = "Extreme\nCount") +
  transition_states(Year) +
  ease_aes('linear')  +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/sLIDMapVid4")
animate(p2, nframes = 200)


