years = c(rep(2000,10),sort(rep(seq(2001,2019),12)), rep(2020,1))
months = c(rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),20), "01")
days = c(rep(c("31","28","31","30","31","30","31","31","30","31","30","31"),20),"31")
months = cbind(months, days)
months = months[-(1:2),]
dates = cbind(years, months)
for(i in 1:(dim(dates)[1])){
  if(dates[i,2] == "02"){
    if(dates[i,1] == "2004" | dates[i,1] == "2008" | dates[i,1] == "2012" | dates[i,1] == "2016"){
      dates[i,3] = "29"
    }
  }
}

require(rgdal)
aus = readOGR(".", "COM20111216_ELB_region")
library(raster)
el1 <- getData('alt', country='AUS', mask=TRUE)

Austra = list()
for(i in 1:1){
  Austra[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid", dates[i,1], dates[i,2]))
  print(i)
}

A1 = Austra1
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, ausmap)

A2 = as.data.frame(A1)
A2 = A2[,-1]
A2 = A2[!is.na(a3[,1]),]
dim(A2)

for(i in 1:length(Austra)){
  Austra[[i]] = Austra[[i]][,-1]
}

for(i in 1:length(Austra)){
  Austra[[i]] = Austra[[i]][!is.na(a3[,1]),]
}

el2 <- extract(el1, Austra[[1]][,2:1])
for(i in 1:length(el2)){
  if(is.na(el2[i])){
    el2[i] = el2[i - 1]
  }
}

for(i in 1:length(Austra)){
  Austra[[i]]$Elevation = el2
}

Austra.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, Austra[[12*i + j]][,3])
    Austra.M[[j]] = S2
  }
}

plot(newmap, xlim = c(108.4,160.6), ylim = c(-43.65,-10.58), main = "Elevation")
scatter2D(Austra[[1]]$Lon, Austra[[1]]$Lat, colvar = Austra[[1]]$Elevation, cex = 0.2, xlim = c(108.4,160.6), ylim = c(-43.65,-10.58),
          xlab = "Longitude", ylab = "Latitude", main = "Elevation")
segments(x0 = 148.25, y0 = -36.45, x1 = 157, y1 = -40)
text(x = 157, y = -40.5, labels = "Mt Kosciuszko", cex = 0.75)
segments(x0 = 126.284, y0 = -29.068, x1 = 130, y1 = -37)
text(x = 130, y = -37.5, labels = "Great Victorian Desert", cex = 0.75)
segments(x0 = 121.284, y0 = -30, x1 = 113, y1 = -40)
text(x = 113, y = -40.5, labels = "Kalgoorlie", cex = 0.75)
segments(x0 = 122.2, y0 = -22.6, x1 = 117, y1 = -15)
text(x= 116, y = -14.5, labels = "Karlamilyi National Park", cex = 0.75)
segments(x0 = 151.95, y0 = -27.57, x1 = 155, y1 = -19)
text(x= 155, y = -18.3, labels = "Toowoomba", cex = 0.75)
segments(x0 = 146.6, y0 = -42.14, x1 = 135, y1 = -41)
text(x= 135, y = -40.3, labels = "Tasmania", cex = 0.75)
segments(x0 = 145.77, y0 = -16.923, x1 = 149, y1 = -13)
text(x = 149, y = -12.3, labels = "Cairns", cex = 0.75)

Austra.M2 = Austra.M

Austra.M[[1]] = Austra.M2[[11]]
Austra.M[[2]] = Austra.M2[[12]]
Austra.M[[3]] = Austra.M2[[1]]
Austra.M[[4]] = Austra.M2[[2]]
Austra.M[[5]] = Austra.M2[[3]]
Austra.M[[6]] = Austra.M2[[4]]
Austra.M[[7]] = Austra.M2[[5]]
Austra.M[[8]] = Austra.M2[[6]]
Austra.M[[9]] = Austra.M2[[7]]
Austra.M[[10]] = Austra.M2[[8]]
Austra.M[[11]] = Austra.M2[[9]]
Austra.M[[12]] = Austra.M2[[10]]



Mean1 = apply(Austra.M2[[6]], 1, mean)
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = Mean1, cex = 0.2)

samp1 = sample(1:nrow(Austra.M[[1]]), size = 50)

C1 = cov(t(Austra.M2[[1]][samp1,]), t(Austra.M2[[3]][samp1,]))
C2 = cov(t(Austra.M2[[1]][samp1[-1],]), t(Austra.M2[[3]][samp1[-1],]))
det(C1)
solve(C1)
solve(C2)


Q = NULL
for(i in 1:nrow(Austra.M[[1]])){
  Q = rbind(Q, c(cov(Austra.M[[3]][1000,], Austra.M[[1]][i,]),  distm(Austra[[1]][1,2:1], Austra[[1]][i,2:1], fun = distCosine)))
  if(i %% 1000 == 0){
    print(i)
  }
}
plot(Q, cex = 0.2)

scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = Q[,1], cex = 0.2)



#3 Month
Austra3 = list()
for(i in 1:(length(Austra) - 2)){
  Austra3[[i]] = cbind(Austra[[i]][,1:2], Austra[[i]][,3] + Austra[[i + 1]][,3] + Austra[[i + 2]][,3], Austra[[i]][,5:6], Austra[[i + 2]][,5:6])
}

for(i in 1:length(Austra3)){
  colnames(Austra3[[i]]) = c("Lat", "Lon", "Rain", "Start.Year", "Start.Month", "End.Year", "End.Month")
}

scatter2D(Austra3[[1]][,2], Austra3[[1]][,1], colvar = Austra3[[10]][,3], cex = 0.2)

samp1 = sample(1:nrow(Austra.M[[1]]), size = 18)


ONI = read.table("ONI.data.txt", header = FALSE)
colnames(ONI) = c("Year", "Jan", "Feb", "Mar", "Apr", "May", "June", "July", "Aug", "Sep", "Oct", "Nov", "Dec")

ONI.Mat = NULL
for(i in 1:nrow(ONI)){
  for(j in 2:ncol(ONI)){
    ONI.Mat = rbind(ONI.Mat, c(as.numeric(paste0(ONI[i,1], "0", j-1)), ONI[i,j]))
  }
}
SOI = read.csv("https://www.ncdc.noaa.gov/teleconnections/enso/indicators/soi/data.csv", header = FALSE)
SOI = SOI[-c(1:2),]
head(SOI)

SOI1 = SOI[,1]
SOI2 = SOI[,2]

if(class(SOI1) == "factor"){
  a.4 = as.numeric(as.character(SOI1))
  a.4[is.na(a.4)] = 0
  SOI1 = a.4
}

if(class(SOI2) == "factor"){
  a.4 = as.numeric(as.character(SOI2))
  a.4[is.na(a.4)] = 0
  SOI2 = a.4
}

SOI = cbind(SOI1, SOI2)

IOD = read.table("IODcomplete.txt", header = FALSE)
IOD[219:nrow(IOD),]
IOD.Mat = NULL
for(i in 1:nrow(IOD)){
  for(j in 2:ncol(IOD)){
    IOD.Mat = rbind(IOD.Mat, c(as.numeric(paste0(IOD[i,1], "0", j-1)), IOD[i,j]))
  }
}

SIOD = read.table("SIOD.txt", header = FALSE)



ONI1 = ONI.Mat[603:835,]
SOI1 = SOI[591:823,]
IOD1 = cbind(SOI1[,1], IOD[219:(nrow(IOD)-2),])
SIOD1 = cbind(SOI1[,1], SIOD[219:(nrow(SIOD)-2),])

T1 = cbind(IOD1[,2], SIOD1[,2], SOI1[,2], ONI1[,2])
colnames(T1) = c("IOD", "SIOD", "SOI", "ONI")
pairs(T1, cex = 0.3)


#IOD Monthly
setwd("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/IODplots")
#Month 1
IOD.cor1 = rep(0, nrow(Austra.M[[1]]))
for(j in 1:nrow(Austra.M[[1]])){
  IOD.cor1[j] = cor(Austra.M[[1]][j,], IOD1[seq(1,225,12),2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "March")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor1, cex = 0.2, pch = 19, add = TRUE)

#Month 2
IOD.cor2 = rep(0, nrow(Austra.M[[2]]))
for(j in 1:nrow(Austra.M[[2]])){
  IOD.cor2[j] = cor(Austra.M[[2]][j,], IOD1[seq(1,225,12) + 1,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "April")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor2, cex = 0.2, pch = 19, add = TRUE)

#Month 3
IOD.cor3 = rep(0, nrow(Austra.M[[3]]))
for(j in 1:nrow(Austra.M[[2]])){
  IOD.cor3[j] = cor(Austra.M[[3]][j,], IOD1[seq(1,225,12) + 2,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "May")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor3, cex = 0.2, pch = 19, add = TRUE)

#Month 4
IOD.cor4 = rep(0, nrow(Austra.M[[4]]))
for(j in 1:nrow(Austra.M[[2]])){
  IOD.cor4[j] = cor(Austra.M[[4]][j,], IOD1[seq(1,225,12) + 3,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "June")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor4, cex = 0.2, pch = 19, add = TRUE)

#Month 5
IOD.cor5 = rep(0, nrow(Austra.M[[5]]))
for(j in 1:nrow(Austra.M[[2]])){
  IOD.cor5[j] = cor(Austra.M[[5]][j,], IOD1[seq(1,225,12) + 4,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "July")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor5, cex = 0.2, pch = 19, add = TRUE)

#Month 6
IOD.cor6 = rep(0, nrow(Austra.M[[5]]))
for(j in 1:nrow(Austra.M[[2]])){
  IOD.cor6[j] = cor(Austra.M[[6]][j,], IOD1[seq(1,225,12) + 5,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "August")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor6, cex = 0.2, pch = 19, add = TRUE)

#Month 7
IOD.cor7 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  IOD.cor7[j] = cor(Austra.M[[7]][j,], IOD1[seq(1,225,12) + 6,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "September")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor7, cex = 0.2, pch = 19, add = TRUE)


which(IOD.cor7 == min(IOD.cor7))

plot(IOD1[seq(1,225,12) + 6,2], Austra.M[[7]][which(IOD.cor7 == min(IOD.cor7)),], cex = 0.2)
abline(lm( Austra.M[[7]][which(IOD.cor7 == min(IOD.cor7)),] ~ IOD1[seq(1,225,12) + 6,2]))

#Month 8
IOD.cor8 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  IOD.cor8[j] = cor(Austra.M[[8]][j,], IOD1[seq(1,225,12) + 7,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "October")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor8, cex = 0.2, pch = 19, add = TRUE)

#Month 9
IOD.cor9 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  IOD.cor9[j] = cor(Austra.M[[9]][j,], IOD1[seq(1,225,12) + 8,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "November")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor9, cex = 0.2, pch = 19, add = TRUE)

#Month 10
IOD.cor10 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  IOD.cor10[j] = cor(Austra.M[[10]][j,-19], IOD1[seq(1,213,12) + 9,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "December")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor10, cex = 0.2, pch = 19, add = TRUE)

#Month 11
IOD.cor11 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  IOD.cor11[j] = cor(Austra.M[[11]][j,-19], IOD1[seq(1,213,12) + 10,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "January")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor11, cex = 0.2, pch = 19, add = TRUE)

#Month 12
IOD.cor12 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  IOD.cor12[j] = cor(Austra.M[[12]][j,-19], IOD1[seq(1,213,12) + 11,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "February")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = IOD.cor12, cex = 0.2, pch = 19, add = TRUE)


#SOI Monthly
#Month 1
SOI.cor1 = rep(0, nrow(Austra.M[[1]]))
for(j in 1:nrow(Austra.M[[1]])){
  SOI.cor1[j] = cor(Austra.M[[1]][j,], SOI1[seq(1,225,12),2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "March")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor1, cex = 0.2, pch = 19, add = TRUE)

#Month 2

SOI.cor2 = rep(0, nrow(Austra.M[[2]]))
for(j in 1:nrow(Austra.M[[2]])){
  SOI.cor2[j] = cor(Austra.M[[2]][j,], SOI1[seq(1,225,12) + 1,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "April")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor2, cex = 0.2, pch = 19, add = TRUE)

#Month 3
SOI.cor3 = rep(0, nrow(Austra.M[[3]]))
for(j in 1:nrow(Austra.M[[2]])){
  SOI.cor3[j] = cor(Austra.M[[3]][j,], SOI1[seq(1,225,12) + 2,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "May")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor3, cex = 0.2, pch = 19, add = TRUE)

#Month 4
SOI.cor4 = rep(0, nrow(Austra.M[[4]]))
for(j in 1:nrow(Austra.M[[2]])){
  SOI.cor4[j] = cor(Austra.M[[4]][j,], SOI1[seq(1,225,12) + 3,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "June")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor4, cex = 0.2, pch = 19, add = TRUE)

#Month 5
SOI.cor5 = rep(0, nrow(Austra.M[[5]]))
for(j in 1:nrow(Austra.M[[2]])){
  SOI.cor5[j] = cor(Austra.M[[5]][j,], SOI1[seq(1,225,12) + 4,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "July")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor5, cex = 0.2, pch = 19, add = TRUE)

#Month 6
SOI.cor6 = rep(0, nrow(Austra.M[[5]]))
for(j in 1:nrow(Austra.M[[2]])){
  SOI.cor6[j] = cor(Austra.M[[6]][j,], SOI1[seq(1,225,12) + 5,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "August")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor6, cex = 0.2, pch = 19, add = TRUE)

#Month 7
SOI.cor7 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  SOI.cor7[j] = cor(Austra.M[[7]][j,], SOI1[seq(1,225,12) + 6,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "September")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor7, cex = 0.2, pch = 19, add = TRUE)


which(SOI.cor7 == min(SOI.cor7))

plot(SOI1[seq(1,225,12) + 6,2], Austra.M[[7]][which(SOI.cor7 == min(SOI.cor7)),], cex = 0.2)
abline(lm( Austra.M[[7]][which(SOI.cor7 == min(SOI.cor7)),] ~ SOI1[seq(1,225,12) + 6,2]))

#Month 8
SOI.cor8 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  SOI.cor8[j] = cor(Austra.M[[8]][j,], SOI1[seq(1,225,12) + 7,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "October")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor8, cex = 0.2, pch = 19, add = TRUE)

#Month 9
SOI.cor9 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  SOI.cor9[j] = cor(Austra.M[[9]][j,], SOI1[seq(1,225,12) + 8,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "November")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor9, cex = 0.2, pch = 19, add = TRUE)

#Month 10
SOI.cor10 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  SOI.cor10[j] = cor(Austra.M[[10]][j,-19], SOI1[seq(1,213,12) + 9,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "December")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor10, cex = 0.2, pch = 19, add = TRUE)

#Month 11
SOI.cor11 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  SOI.cor11[j] = cor(Austra.M[[11]][j,-19], SOI1[seq(1,213,12) + 10,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "January")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor11, cex = 0.2, pch = 19, add = TRUE)

#Month 12
SOI.cor12 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  SOI.cor12[j] = cor(Austra.M[[12]][j,-19], SOI1[seq(1,213,12) + 11,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "February")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = SOI.cor12, cex = 0.2, pch = 19, add = TRUE)


#ONI Monthly
#Month 1
ONI.cor1 = rep(0, nrow(Austra.M[[1]]))
for(j in 1:nrow(Austra.M[[1]])){
  ONI.cor1[j] = cor(Austra.M[[1]][j,], ONI1[seq(1,225,12),2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "March")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor1, cex = 0.2, pch = 19, add = TRUE)

#Month 2
ONI.cor2 = rep(0, nrow(Austra.M[[2]]))
for(j in 1:nrow(Austra.M[[2]])){
  ONI.cor2[j] = cor(Austra.M[[2]][j,], ONI1[seq(1,225,12) + 1,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "April")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor2, cex = 0.2, pch = 19, add = TRUE)

#Month 3
ONI.cor3 = rep(0, nrow(Austra.M[[3]]))
for(j in 1:nrow(Austra.M[[2]])){
  ONI.cor3[j] = cor(Austra.M[[3]][j,], ONI1[seq(1,225,12) + 2,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "May")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor3, cex = 0.2, pch = 19, add = TRUE)

#Month 4
ONI.cor4 = rep(0, nrow(Austra.M[[4]]))
for(j in 1:nrow(Austra.M[[2]])){
  ONI.cor4[j] = cor(Austra.M[[4]][j,], ONI1[seq(1,225,12) + 3,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "June")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor4, cex = 0.2, pch = 19, add = TRUE)

#Month 5
ONI.cor5 = rep(0, nrow(Austra.M[[5]]))
for(j in 1:nrow(Austra.M[[2]])){
  ONI.cor5[j] = cor(Austra.M[[5]][j,], ONI1[seq(1,225,12) + 4,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "July")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor5, cex = 0.2, pch = 19, add = TRUE)

#Month 6
ONI.cor6 = rep(0, nrow(Austra.M[[5]]))
for(j in 1:nrow(Austra.M[[2]])){
  ONI.cor6[j] = cor(Austra.M[[6]][j,], ONI1[seq(1,225,12) + 5,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "August")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor6, cex = 0.2, pch = 19, add = TRUE)

#Month 7
ONI.cor7 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  ONI.cor7[j] = cor(Austra.M[[7]][j,], ONI1[seq(1,225,12) + 6,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "September")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor7, cex = 0.2, pch = 19, add = TRUE)


which(abs(ONI.cor7) == min(abs(ONI.cor7)))

plot(ONI1[seq(1,225,12) + 6,2], Austra.M[[7]][which(abs(ONI.cor7) == min(abs(ONI.cor7))),], cex = 0.2)
abline(lm( Austra.M[[7]][which(abs(ONI.cor7) == min(abs(ONI.cor7))),] ~ ONI1[seq(1,225,12) + 6,2]))

#Month 8
ONI.cor8 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  ONI.cor8[j] = cor(Austra.M[[8]][j,], ONI1[seq(1,225,12) + 7,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "October")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor8, cex = 0.2, pch = 19, add = TRUE)

#Month 9
ONI.cor9 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  ONI.cor9[j] = cor(Austra.M[[9]][j,], ONI1[seq(1,225,12) + 8,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "November")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor9, cex = 0.2, pch = 19, add = TRUE)

#Month 10
ONI.cor10 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  ONI.cor10[j] = cor(Austra.M[[10]][j,-19], ONI1[seq(1,213,12) + 9,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "December")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor10, cex = 0.2, pch = 19, add = TRUE)

#Month 11
ONI.cor11 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  ONI.cor11[j] = cor(Austra.M[[11]][j,-19], ONI1[seq(1,213,12) + 10,2])
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "January")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor11, cex = 0.2, pch = 19, add = TRUE)

#Month 12
ONI.cor12 = rep(0, nrow(Austra.M[[7]]))
for(j in 1:nrow(Austra.M[[7]])){
  ONI.cor12[j] = cor(Austra.M[[12]][j,-19], ONI1[seq(1,213,12) + 11,2], method = "pearson")
}

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58), main = "February")
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ONI.cor12, cex = 0.2, pch = 19, add = TRUE)

D1 = cbind(ONI1[seq(1,213,12) + 11,2], IOD1[seq(1,213,12) + 11,2], SOI1[seq(1,213,12) + 11,2])

mod1 = lm(sqrt(Austra.M[[12]][60790,-19]) ~ D1[,1] + D1[,2] + D1[,3])
summary(mod1)


help(ts)

head(Austra[[225]])
M.all = matrix(0, nrow = nrow(Austra[[1]]), ncol = 225)
for(i in 1:225){
  M.all[,i] = Austra[[i]][,3]
  if(i %% 100 == 0){
    print(i)
  }
}


VB = 56577
VB.1 = Austra.M2[[1]][VB,]
VB.1.Austra.1.cor = rep(0, nrow(Austra.M2[[1]]))
for(i in 1:nrow(Austra.M2[[1]])){
  VB.1.Austra.1.cor[i] = cor(VB.1, Austra.M2[[1]][i,])
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M2[[1]][VB,]
VB.1.Austra.12.cor = rep(0, nrow(Austra.M2[[1]]))
for(i in 1:nrow(Austra.M2[[1]])){
  VB.1.Austra.12.cor[i] = cor(VB.1, Austra.M2[[10]][i,])
  if(i %% 10000 == 0){
    print(i)
  }
}

scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.12.cor, cex = 0.2)

samp1 = sample(1:nrow(Austra.M2[[1]]), 2000)

Cor.Mat = matrix(0, 2000, 2000)
k = 1
for(i in samp1){
  VB.1.Austra.12.cor = rep(0, 2000)
  l = 1
  for(j in samp1){
    VB.1.Austra.12.cor[l] = cor(Austra.M2[[1]][i,ONI1[seq(1,213,12) + 10,2] > 0], Austra.M2[[12]][j,ONI1[seq(1,213,12) + 10,2] > 0])
    l = l + 1
  }
  Cor.Mat[k,] = VB.1.Austra.12.cor
  if(k %% 100 == 0){
    print(k)
  }
  k = k + 1
}

plot(apply(Cor.Mat, 1, max), cex = 0.2)

scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = Cor.Mat[3318,], cex = 0.2)
points(Austra[[1]][63818,2], Austra[[1]][63818,1], cex = 1, pch = 5)

Cor.Mat2 = matrix(0, 2000, 2000)
k = 1
for(i in samp1){
  VB.1.Austra.12.cor = rep(0, 2000)
  l = 1
  for(j in samp1){
    VB.1.Austra.12.cor[l] = cor(Austra.M2[[1]][i,], Austra.M2[[11]][j,])
    l = l + 1
  }
  Cor.Mat2[k,] = VB.1.Austra.12.cor
  if(k %% 100 == 0){
    print(k)
  }
  k = k + 1
}

which(Cor.Mat == max(Cor.Mat), arr.ind = TRUE)
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = Cor.Mat[which(Cor.Mat == max(Cor.Mat), arr.ind = TRUE)[1],], cex = 0.2)
points(Austra[[1]][samp1[which(Cor.Mat == max(Cor.Mat), arr.ind = TRUE)[1]],2], Austra[[1]][samp1[which(Cor.Mat == max(Cor.Mat), arr.ind = TRUE)[1]],1], cex = 1, pch = 5)
points(Austra[[1]][samp1[which(Cor.Mat[981,] > 0.98)],2], Austra[[1]][samp1[which(Cor.Mat[981,] > 0.98)],1], cex = 1, pch = 6)


plot(apply(Cor.Mat, 1, median), cex = 0.2)

which(Cor.Mat == max(Cor.Mat), arr.ind = TRUE)



Melb = 56337

set.seed(1998)
samp1 = sample(1:nrow(Austra.M2[[1]]), 5000)

Cor.Mat.NS = NULL
k = 1
for(j in samp1){
  Cor.Mat.NS = c(Cor.Mat.NS, cor(Austra.M2[[1]][Melb,], Austra.M2[[12]][j,]))
}

plot(newmap, xlim = c(113.4,160.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = Cor.Mat.NS, cex = 0.2, add = TRUE)

Cor.Mat.OP = NULL
k = 1
for(j in samp1){
  Cor.Mat.OP = c(Cor.Mat.OP, cor(Austra.M2[[1]][Melb,ONI1[seq(1,213,12) + 10,2] > 0], Austra.M2[[12]][j,ONI1[seq(1,213,12) + 10,2] > 0]))
}

plot(newmap, xlim = c(113.4,160.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = Cor.Mat.OP, cex = 0.2, add = TRUE)


Cor.Mat.ON = NULL
k = 1
for(j in samp1){
  Cor.Mat.ON = c(Cor.Mat.ON, cor(Austra.M2[[1]][Melb,ONI1[seq(1,213,12) + 10,2] < 0], Austra.M2[[12]][j,ONI1[seq(1,213,12) + 10,2] < 0]))
}

Cor.Mat.NS = c(Cor.Mat.NS, max(Cor.Mat.OP), min(Cor.Mat.OP))
plot(newmap, xlim = c(113.4,160.6), ylim = c(-43.65,-10.58), main = "NS")
scatter2D(c(Austra[[1]][samp1,2],0,0), c(Austra[[1]][samp1,1],0,0), colvar = Cor.Mat.NS, cex = 0.2, add = TRUE)

Cor.Mat.ON = c(Cor.Mat.ON, max(Cor.Mat.OP), min(Cor.Mat.OP))
plot(newmap, xlim = c(113.4,160.6), ylim = c(-43.65,-10.58), main = "SN")
scatter2D(c(Austra[[1]][samp1,2],0,0), c(Austra[[1]][samp1,1],0,0), colvar = Cor.Mat.ON, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(113.4,160.6), ylim = c(-43.65,-10.58), main = "SP")
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = Cor.Mat.OP, cex = 0.2, add = TRUE)




Koppen = read.table("https://raw.githubusercontent.com/hinestein/Koppen/master/Fixedkpngrp.txt")
c1 = seq(112, by = 0.025, length.out = 1681)
c2 = seq(-44, by = 0.025, length.out = 1361)
cord1 = matrix(c(0), nrow = length(c1) * length(c2), ncol = 2)
k = 1
for(i in 1:length(c1)){
  for(j in 1:length(c2)){
    cord1[k,] = c(c1[i], rev(c2)[j])
    k = k + 1
  }
}
Koppen.Mat = cord1
Koppen.Mat = cbind(Koppen.Mat, rep(0, length(c1) * length(c2)))
for(i in 1:ncol(Koppen)){
  Koppen.Mat[1:nrow(Koppen) + nrow(Koppen) * (i - 1),3] = as.matrix(Koppen)[,i]
  if(i %% 100 == 0){
    print(i)
  }
}

set.seed(1998)
samp1 = sample(1:nrow(Koppen.Mat), 100000)

colnames(Koppen.Mat) = c("Lon", "Lat", "Class")
Koppen.Mat = as.data.frame(Koppen.Mat)

Koppen.Mat2 = Koppen.Mat[samp1,]
Koppen.Mat2 = Koppen.Mat2[Koppen.Mat2[,3] != -9999, ]

cord1 = Koppen.Mat2[,1:2]
cord1 = as.data.frame(cord1)
colnames(cord1) = c("Lon", "Lat")
coordinates(cord1) = ~Lon + Lat

proj4string(cord1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(cord1, ausmap)

Koppen.Mat2 = Koppen.Mat2[!is.na(a3[,1]),]

scatter2D(Koppen.Mat2[,1], Koppen.Mat2[,2], colvar = Koppen.Mat2[,3], cex = 0.1)

classes = rep(0, nrow(Koppen.Mat2))
for(i in 1:nrow(Koppen.Mat2)){
  if(Koppen.Mat2$Class[i] == 1){
    classes[i] = "Temperate, no dry season (cool summer)"
  }else if(Koppen.Mat2$Class[i] == 2){
    classes[i] = "Temperate, distinctly dry (and mild) summer"
  }else if(Koppen.Mat2$Class[i] == 3){
    classes[i] = "Temperate, no dry season (mild summer)"
  }else if(Koppen.Mat2$Class[i] == 4){
    classes[i] = "Temperate, distinctly dry (and warm) summer"
  }else if(Koppen.Mat2$Class[i] == 5){
    classes[i] = "Temperate, moderately dry winter (warm summer)"
  }else if(Koppen.Mat2$Class[i] == 6){
    classes[i] = "Temperate, no dry season (warm summer)"
  }else if(Koppen.Mat2$Class[i] == 7){
    classes[i] = "Temperate, distinctly dry (and hot) summer"
  }else if(Koppen.Mat2$Class[i] == 8){
    classes[i] = "Temperate, moderately dry winter (hot summer)"
  }else if(Koppen.Mat2$Class[i] == 9){
    classes[i] = "Temperate, no dry season (hot summer)"
  }else if(Koppen.Mat2$Class[i] == 11){
    classes[i] = "Grassland, warm (summer drought)"
  }else if(Koppen.Mat2$Class[i] == 12){
    classes[i] = "Grassland, warm (persistently dry)"
  }else if(Koppen.Mat2$Class[i] == 13){
    classes[i] = "Grassland, hot (winter drought)"
  }else if(Koppen.Mat2$Class[i] == 14){
    classes[i] = "Grassland, hot (summer drought)"
  }else if(Koppen.Mat2$Class[i] == 15){
    classes[i] = "Grassland, hot (persistently dry)"
  }else if(Koppen.Mat2$Class[i] == 21){
    classes[i] = "Desert, warm (persistently dry)"
  }else if(Koppen.Mat2$Class[i] == 22){
    classes[i] = "Desert, hot (winter drought)"
  }else if(Koppen.Mat2$Class[i] == 23){
    classes[i] = "Desert, hot (summer drought)"
  }else if(Koppen.Mat2$Class[i] == 24){
    classes[i] = "Desert, hot (persistently dry)"
  }else if(Koppen.Mat2$Class[i] == 31){
    classes[i] = "Subtropical, moderately dry winter"
  }else if(Koppen.Mat2$Class[i] == 32){
    classes[i] = "Subtropical, distinctly dry winter"
  }else if(Koppen.Mat2$Class[i] == 33){
    classes[i] = "Subtropical, distinctly dry summer"
  }else if(Koppen.Mat2$Class[i] == 34){
    classes[i] = "Subtropical, no dry season"
  }else if(Koppen.Mat2$Class[i] == 35){
    classes[i] = "Tropical, savanna"
  }else if(Koppen.Mat2$Class[i] == 36){
    classes[i] = "Tropical, rainforest (monsoonal)"
  }else if(Koppen.Mat2$Class[i] == 37){
    classes[i] = "Tropical, rainforest (persistently wet)"
  }else if(Koppen.Mat2$Class[i] == 41){
    classes[i] = "Equatorial, savanna"
  }else{
    classes[i] = "Equatorial, rainforest (monsoonal)"
  }
}

Koppen.Mat4 = cbind(Koppen.Mat2, classes)

c1 = as.factor(classes)



Koppen.Mat3 = matrix(0, nrow = nrow(Austra1), ncol = 3)
K4 = rep(0, nrow(Koppen.Mat3))

D2 = rep(0, nrow(Austra1))
for(i in 1:nrow(Austra1)){
  D2[i] = which(Koppen.Mat2[,1] < Austra1[i,2] + 0.06 & Koppen.Mat2[,1] > Austra1[i,2] - 0.06 & Koppen.Mat2[,2] < Austra1[i,1] + 0.06 & Koppen.Mat2[,2] > Austra1[i,1] - 0.06)[1]
  if(i %% 100 == 0){
    print(i)
  }
}


d1 = Koppen.Mat2[D2,3]

d1[is.na(d1)] = d1[which(is.na(d1))]

d2 = d1
d3 = d2
for(i in 1:length(d3)){
  if(!is.na(d2[i])){
    if(0 < d2[i] & d2[i] < 10){
      d3[i] = "Temperate"
    }else if(10 < d2[i] & d2[i] < 16){
      d3[i] = "Grassland"
    }else if(20 < d2[i] & d2[i] < 25){
      d3[i] = "Desert"
    }else if(30 < d2[i] & d2[i] < 35){
      d3[i] = "Subtropical"
    }else if(34 < d2[i] & d2[i] < 38){
      d3[i] = "Tropical"
    }else if(d2[i] > 39){
      d3[i] = "Equatorial"
    }
  }
}

A1 = data.frame(Lon = Austra1$Lon, Lat = Austra1$Lat, Class = d3)
A1 = A1[complete.cases(A1),]

ggplot(A1, aes(x = Lon, y = Lat, color = Class)) + geom_point(size = 0.5) +
  scale_color_brewer(palette = 5, type = "div") + guides(colour = guide_legend(override.aes = list(size=18))) + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(color = "Class", x = "Longitude", y = "Latitude", title = "Australian Köppen Climate Classification")

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = A1, 
             mapping = aes(x = Lon, y = Lat, colour = Class), size = 1) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(color = "Class", x = "Longitude", y = "Latitude", title = "Australian Köppen Climate Classification")

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(A1, mapping = aes(x = Lon, y = Lat, color = Class), size = 1) +
  scale_color_brewer(palette = 5, type = "div") + guides(colour = guide_legend(override.aes = list(size=18))) + theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  labs(color = "Class", x = "Longitude", y = "Latitude", title = "Australian Köppen Climate Classification") +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9))

m1 = seq(1,231,12)
m2 = seq(2,231,12)
m3 = seq(3,231,12)
m4 = seq(4,231,12)
m5 = seq(5,231,12)
m6 = seq(6,231,12)
m7 = seq(7,231,12)
m8 = seq(8,231,12)
m9 = seq(9,231,12)
m10 = seq(10,231,12)
m11 = seq(11,231,12)
m12 = seq(12,231,12)

Totm1 = rep(0, nrow(Austra[[1]]))
for(i in m1){
  Totm1 = Totm1 + Austra[[i]]$RainRate
}

Totm2 = rep(0, nrow(Austra[[2]]))
for(i in m2){
  Totm2 = Totm2 + Austra[[i]]$RainRate
}

Totm3 = rep(0, nrow(Austra[[3]]))
for(i in m3){
  Totm3 = Totm3 + Austra[[i]]$RainRate
}

Totm4 = rep(0, nrow(Austra[[4]]))
for(i in m4){
  Totm4 = Totm4 + Austra[[i]]$RainRate
}

Totm5 = rep(0, nrow(Austra[[5]]))
for(i in m5){
  Totm5 = Totm5 + Austra[[i]]$RainRate
}

Totm6 = rep(0, nrow(Austra[[6]]))
for(i in m6){
  Totm6 = Totm6 + Austra[[i]]$RainRate
}

Totm7 = rep(0, nrow(Austra[[7]]))
for(i in m7){
  Totm7 = Totm7 + Austra[[i]]$RainRate
}

Totm8 = rep(0, nrow(Austra[[8]]))
for(i in m8){
  Totm8 = Totm8 + Austra[[i]]$RainRate
}

Totm9 = rep(0, nrow(Austra[[9]]))
for(i in m9){
  Totm9 = Totm9 + Austra[[i]]$RainRate
}

Totm10 = rep(0, nrow(Austra[[10]]))
for(i in m10){
  Totm10 = Totm10 + Austra[[i]]$RainRate
}

Totm11 = rep(0, nrow(Austra[[11]]))
for(i in m11){
  Totm11 = Totm11 + Austra[[i]]$RainRate
}

Totm12 = rep(0, nrow(Austra[[12]]))
for(i in m12){
  Totm12 = Totm12 + Austra[[i]]$RainRate
}


plot.title = element_text(hjust = -0.5)
Koppen.Mat3$classes = d3

De = which(Austra[[1]]$KClass == "Desert")
Eq = which(Austra[[1]]$KClass == "Equatorial")
Gr = which(Austra[[1]]$KClass == "Grassland")
Su = which(Austra[[1]]$KClass == "Subtropical")
Te = which(Austra[[1]]$KClass == "Temperate")
Tr = which(Austra[[1]]$KClass == "Tropical")

plot(newmap, xlim = c(113.4,160.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][Tr,2], Austra[[1]][Tr,1], colvar = Totm12[Tr]/length(m12), cex = 0.1)


scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[1]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[5]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[6]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[7]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[8]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[10]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[11]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[12]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[13]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[14]]][Te,3], cex = 0.2)
scatter2D(Austra[[4]][Te,2], Austra[[2]][Te,1], colvar = Austra[[m7[3]]][Te,3], cex = 0.2)



which(SOI1[m7,2] > 0)

SeaIce = read.csv("SeaIceM.csv")
SeaIce = SeaIce[257:489,]
head(SeaIce)


K2 = d2
table(K2)



###################################
#Class 0, No SOI/IOD influence
###################################

w0 = which(K2 == 1)
Y01 = Austra[[231]][w0,3]

samp1 = sort(sample(1:nrow(Austra[[1]]), 5000))

p.lag = 10
W.list = list()
for(i in 1:p.lag){
  W.list[[i]] = matrix(0,ncol = nrow(Austra[[1]]), nrow = length(Y0))
}
for(i in 1:p.lag){
  for(j in 1:length(Y0)){
    rho0 = rep(0, nrow(Austra[[1]]))
    for(k in 1:nrow(Austra[[1]])){
      rho0[k] = cor(Austra.M2[[3]][w0[j],], Austra.M2[[3 - i - 1]][samp1[k],])
    }
    W.list[[i]][j,] = rho0
    print(j)
  }
}


###################################
#Class 1, No IOD influence
###################################
w1 = which(K2 == 1)

M.all = NULL
for(i in 1:length(Austra)){
  M.all = cbind(M.all, Austra[[i]][,3])
  if(i %% 100 == 0){
    print(i)
  }
}

dates1 = NULL
for(i in 1:(nrow(dates) - 1)){
  dates1 = c(dates1, paste0(dates[i,1], dates[i,2]))
}

colnames(M.all) = dates1


Y = M.all[w1,231]

Y.list = list()
for(i in 1:12){
  Y.list[[i]] = M.all[samp1,231 - i]
}



set.seed(1998)
samp1 = sort(sample(1:nrow(Austra[[1]]), 1000))
p.lag = 10
W.list = list()
for(i in 1:p.lag){
  W.list[[i]] = matrix(0,ncol = length(samp1), nrow = length(Y))
}
plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], col = "blue", cex = 0.1, add = TRUE)
#March months with ONI > 0
O1 = seq(3,231,12)[which(IOD1[seq(3,231,12),2] > 0)][-1]
for(i in 1:p.lag){
  O2 = O1 - i - 5
  M2 = M.all[samp1, O2]
  for(j in 1:length(Y)){
    rho0 = rep(0, length(samp1))
    for(k in 1:length(samp1)){
      rho0[k] = cor(M.all[w1[j],O1], M.all[samp1[k],O2])
    }
    W.list[[i]][j,] = ifelse(rho0 > sort(rho0, decreasing = TRUE)[10], rho0, 0)/sum(ifelse(rho0 > sort(rho0, decreasing = TRUE)[10], rho0, 0), na.rm = TRUE)
    if(j %% 100 == 0){
      print(j)
    }
  }
  print(i)
}

Wna.list = list()
for(i in 1:length(W.list)){
  Wna.list[[i]] = unique(which(is.na(W.list[[i]]), arr.ind = TRUE)[,2])
}

for(i in 1:length(W.list)){
  if(length(Wna.list[[i]]) == 0){
    Wna.list[[i]] = 1000000
  }
}


for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,-Wna.list[[i]]]
}

W0.list = list()
for(i in 1:p.lag){
  e0 = NULL
  for(j in 1:ncol(W.list[[i]])){
    if(sum(W.list[[i]][,j]) == 0){
      e0 = c(e0, j)
    }
  }
  W0.list[[i]] = e0
}

for(i in 1:p.lag){
  W.list[[i]] = W.list[[i]][,-W0.list[[i]]]
}

Y = M.all[w1,231]

Y.list = list()
for(i in 1:p.lag){
  Y.list[[i]] = M.all[samp1,231 - i - 5]
}

for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-W0.list[[i]]]
}

for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-Wna.list[[i]]]
}

W1.list = list()
for(i in 1:p.lag){
  W1.list[[i]] = t(Y) %*% W.list[[i]] %*% Y.list[[i]]
}

W2.list = list()
for(i in 1:p.lag){
  W2.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% Y
}

D.list = list()
for(i in 1:p.lag){
  D.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% W.list[[i]] %*% Y.list[[i]]
}

X.ti = cbind(rep(1, length(Y)), as.matrix(Austra[[1]][which(Austra[[1]]$KClass == 1),1:2]), Austra[[1]][which(Austra[[1]]$KClass == 1),9])
W3.list = list()
for(i in 1:p.lag){
  W3.list[[i]] = t(X.ti) %*% W.list[[i]] %*% Y.list[[i]]
}

W4.list = list()
for(i in 1:p.lag){
  W4.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% X.ti
}

W5.list = list()
for(i in 1:p.lag){
  W5.list[[i]] = W.list[[i]] %*% Y.list[[i]]
}

W6.list = list()
for(i in 1:p.lag){
  W6.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]])
}

Lmult = function(a.vec, A.list, B.list){
  L.out = matrix(0, nrow = nrow(A.list[[1]]), ncol = 1)
  for(i in 1:length(a.vec)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}

c1 = NULL
p = p.lag
beta.hat = rep(0, ncol(X.ti))
phi.hat = rep(0, p.lag)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:500){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:p){
    phi.hat[j] = (W1.list[[j]] + W2.list[[j]] - t(beta.hat) %*% W3.list[[j]]
                  -  W4.list[[j]] %*% beta.hat- t(Lmult(phi.hat[-j], W.list[-j], Y.list[-j])) %*% W5.list[[j]]
                  - W6.list[[j]] %*% Lmult(phi.hat[-j], W.list[-j], Y.list[-j]))/(2 * D.list[[j]])
  }
  print(phi.hat[1])
  c1 = cbind(c1, phi.hat)
} 

Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
plot(Y - Y.hat, cex = 0.2)

plot(newmap, xlim = c(140.4,156.6), ylim = c(-43.65,-30.58))
scatter2D(Austra[[1]][Austra[[1]]$KClass == 1,2], Austra[[1]][Austra[[1]]$KClass == 1,1], colvar = Y.hat,add = TRUE, cex = 0.2)
scatter2D(Austra[[1]][Austra[[1]]$KClass == 1,2], Austra[[1]][Austra[[1]]$KClass == 1,1], colvar = Y, cex = 0.4, pch = 19)

Y = M.all[w0,231]

Y.list = list()
for(i in 1:p.lag){
  Y.list[[i]] = M.all[samp1,231 - i - 5]
}

for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-Wna.list[[i]]]
  Y.list[[i]] = Y.list[[i]][-W0.list[[i]]]
}

Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
plot(Y - Y.hat, cex = 0.2)


##################################
#Class 7
##################################
w7 = which(K2 == 7)

M.all = NULL
for(i in 1:length(Austra)){
  M.all = cbind(M.all, Austra[[i]][,3])
  if(i %% 100 == 0){
    print(i)
  }
}

dates1 = NULL
for(i in 1:(nrow(dates) - 1)){
  dates1 = c(dates1, paste0(dates[i,1], dates[i,2]))
}

colnames(M.all) = dates1


Y = sqrt(M.all[w7,231])


set.seed(1998)
samp1 = sort(sample(1:nrow(Austra[[1]]), 1000))
p.lag = 10
W.list = list()
for(i in 1:p.lag){
  W.list[[i]] = matrix(0,ncol = length(samp1), nrow = length(Y))
}
plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], col = "blue", cex = 0.1, add = TRUE)
#March months with ONI > 0
O1 = seq(3,231,12)[which(ONI1[seq(3,231,12),2] > 0)][-1]
for(i in 1:p.lag){
  O2 = O1 - i - 5
  M2 = M.all[samp1, O2]
  for(j in 1:length(Y)){
    rho0 = rep(0, length(samp1))
    for(k in 1:length(samp1)){
      rho0[k] = cor(M.all[w7[j],O1], M.all[samp1[k],O2])
    }
    W.list[[i]][j,] = ifelse(rho0 > sort(rho0, decreasing = TRUE)[10], rho0, 0)/sum(ifelse(rho0 > sort(rho0, decreasing = TRUE)[10], rho0, 0), na.rm = TRUE)
    if(j %% 100 == 0){
      print(j)
    }
  }
  print(i)
}

Wna.list = list()
for(i in 1:length(W.list)){
  Wna.list[[i]] = unique(which(is.na(W.list[[i]]), arr.ind = TRUE)[,2])
}

for(i in 1:length(W.list)){
  if(length(Wna.list[[i]]) == 0){
    Wna.list[[i]] = 1000000
  }
}

Y.list = list()
for(i in 1:12){
  Y.list[[i]] = sqrt(M.all[samp1,231 - i])
}


for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,-Wna.list[[i]]]
}

for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-Wna.list[[i]]]
}

W0.list = list()
for(i in 1:p.lag){
  e0 = NULL
  for(j in 1:ncol(W.list[[i]])){
    if(sum(W.list[[i]][,j]) == 0){
      e0 = c(e0, j)
    }
  }
  W0.list[[i]] = e0
}

for(i in 1:p.lag){
  W.list[[i]] = W.list[[i]][,-W0.list[[i]]]
}


for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-W0.list[[i]]]
}

W1.list = list()
for(i in 1:p.lag){
  W1.list[[i]] = t(Y) %*% W.list[[i]] %*% Y.list[[i]]
}

W2.list = list()
for(i in 1:p.lag){
  W2.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% Y
}

D.list = list()
for(i in 1:p.lag){
  D.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% W.list[[i]] %*% Y.list[[i]]
}

X.ti = cbind(rep(1, length(Y)), as.matrix(Austra[[1]][which(Austra[[1]]$KClass == 7),1:2]), Austra[[1]][which(Austra[[1]]$KClass == 7),9])
W3.list = list()
for(i in 1:p.lag){
  W3.list[[i]] = t(X.ti) %*% W.list[[i]] %*% Y.list[[i]]
}

W4.list = list()
for(i in 1:p.lag){
  W4.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% X.ti
}

W5.list = list()
for(i in 1:p.lag){
  W5.list[[i]] = W.list[[i]] %*% Y.list[[i]]
}

W6.list = list()
for(i in 1:p.lag){
  W6.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]])
}

Lmult = function(a.vec, A.list, B.list){
  L.out = matrix(0, nrow = nrow(A.list[[1]]), ncol = 1)
  for(i in 1:length(a.vec)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}

c1 = NULL
p = p.lag
beta.hat = rep(0, ncol(X.ti))
phi.hat = rep(0, p.lag)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:500){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:p){
    phi.hat[j] = (W1.list[[j]] + W2.list[[j]] - t(beta.hat) %*% W3.list[[j]]
                  -  W4.list[[j]] %*% beta.hat- t(Lmult(phi.hat[-j], W.list[-j], Y.list[-j])) %*% W5.list[[j]]
                  - W6.list[[j]] %*% Lmult(phi.hat[-j], W.list[-j], Y.list[-j]))/(2 * D.list[[j]])
  }
  print(phi.hat[1])
  c1 = cbind(c1, phi.hat)
} 

Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
plot(Y - Y.hat, cex = 0.2)

plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][Austra[[1]]$KClass == 7,2], Austra[[1]][Austra[[1]]$KClass == 7,1], col = "blue",add = TRUE, cex = 0.2)
scatter2D(Austra[[1]][Austra[[1]]$KClass == 7,2], Austra[[1]][Austra[[1]]$KClass == 7,1], colvar = Y - Y.hat^2, cex = 0.4, pch = 19)
scatter2D(Austra[[1]][Austra[[1]]$KClass == 7,2], Austra[[1]][Austra[[1]]$KClass == 7,1], colvar = Y, cex = 0.4, pch = 19)
Y = M.all[w7,207]

Y.list = list()
for(i in 1:p.lag){
  Y.list[[i]] = sqrt(M.all[samp1,207 - i - 5])
}

for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-Wna.list[[i]]]
  Y.list[[i]] = Y.list[[i]][-W0.list[[i]]]
}

Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
plot(sqrt(Y) - Y.hat, cex = 0.2)
plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][Austra[[1]]$KClass == 2,2], Austra[[1]][Austra[[1]]$KClass == 2,1], colvar = Y - Y.hat, cex = 0.4, pch = 19, add = TRUE)
scatter2D(Austra[[1]][Austra[[1]]$KClass == 2,2], Austra[[1]][Austra[[1]]$KClass == 2,1], colvar = Y, cex = 0.4, pch = 19)
points((Austra[[1]][Austra[[1]]$KClass == 3,2]), (Austra[[1]][Austra[[1]]$KClass == 3,1]), pch = 5, cex = 1)


AA1 = Austra[[1]][Austra[[1]]$KClass == 2,]
coordinates(AA1) = ~Lon + Lat
Lon = Austra[[1]]$Lon
Lat = Austra[[1]]$Lat
RainRate = Austra[[1]]$RainRate
K1 = idw(formula = RainRate ~ 1, locations = Lon + Lat)


library("dplyr")
library("fields")
library("ggplot2")
library("gstat")
library("RColorBrewer")
library("sp")
library("spacetime")
library("STRbook")



###############################
#Class 24
###############################
w24 = which(K2 == 24)

M.all = NULL
for(i in 1:length(Austra)){
  M.all = cbind(M.all, Austra[[i]][,3])
  if(i %% 100 == 0){
    print(i)
  }
}

dates1 = NULL
for(i in 1:(nrow(dates) - 1)){
  dates1 = c(dates1, paste0(dates[i,1], dates[i,2]))
}

colnames(M.all) = dates1


Y = sqrt(M.all[w24,231])

Y.list = list()
for(i in 1:12){
  Y.list[[i]] = sqrt(M.all[samp1,231 - i])
}



set.seed(1998)
samp1 = sort(sample(1:nrow(Austra[[1]]), 1000))
p.lag = 10
W.list = list()
for(i in 1:p.lag){
  W.list[[i]] = matrix(0,ncol = length(samp1), nrow = length(Y))
}
plot(newmap, xlim = c(113.4,156.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], col = "blue", cex = 0.1, add = TRUE)
#March months with ONI > 0
O1 = seq(3,231,12)[which(IOD1[seq(3,231,12),2] > 0)][-1]
for(i in 1:p.lag){
  O2 = O1 - i - 5
  M2 = M.all[samp1, O2]
  for(j in 1:length(Y)){
    rho0 = rep(0, length(samp1))
    for(k in 1:length(samp1)){
      rho0[k] = cor(M.all[w24[j],O1], M.all[samp1[k],O2])
      if(sd(M.all[w24[j],O1]) == 0 | sd(M.all[samp1[k],O2]) == 0){
      }
    }
    W.list[[i]][j,] = ifelse(rho0 > sort(rho0, decreasing = TRUE)[10], rho0, 0)/sum(ifelse(rho0 > sort(rho0, decreasing = TRUE)[10], rho0, 0), na.rm = TRUE)
    if(j %% 100 == 0){
      print(j)
    }
  }
  print(i)
}

Wna.list = list()
for(i in 1:length(W.list)){
  Wna.list[[i]] = unique(which(is.na(W.list[[i]]), arr.ind = TRUE)[,2])
}

for(i in 1:length(W.list)){
  if(length(Wna.list[[i]]) == 0){
    Wna.list[[i]] = 1000000
  }
}


for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,-Wna.list[[i]]]
}

W0.list = list()
for(i in 1:p.lag){
  e0 = NULL
  for(j in 1:ncol(W.list[[i]])){
    if(sum(W.list[[i]][,j]) == 0){
      e0 = c(e0, j)
    }
  }
  W0.list[[i]] = e0
}

for(i in 1:p.lag){
  W.list[[i]] = W.list[[i]][,-W0.list[[i]]]
}

for(i in 1:p.lag){
  Y.list[[i]] = Y.list[[i]][-W0.list[[i]]]
}

W1.list = list()
for(i in 1:p.lag){
  W1.list[[i]] = t(Y) %*% W.list[[i]] %*% Y.list[[i]]
}

W2.list = list()
for(i in 1:p.lag){
  W2.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% Y
}

D.list = list()
for(i in 1:p.lag){
  D.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% W.list[[i]] %*% Y.list[[i]]
}

X.ti = cbind(rep(1, length(Y)), as.matrix(Austra[[1]][which(Austra[[1]]$KClass == 24),1:2]), Austra[[1]][which(Austra[[1]]$KClass == 24),9])
W3.list = list()
for(i in 1:p.lag){
  W3.list[[i]] = t(X.ti) %*% W.list[[i]] %*% Y.list[[i]]
}

W4.list = list()
for(i in 1:p.lag){
  W4.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% X.ti
}

W5.list = list()
for(i in 1:p.lag){
  W5.list[[i]] = W.list[[i]] %*% Y.list[[i]]
}

W6.list = list()
for(i in 1:p.lag){
  W6.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]])
}

Lmult = function(a.vec, A.list, B.list){
  L.out = matrix(0, nrow = nrow(A.list[[1]]), ncol = 1)
  for(i in 1:length(a.vec)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}

c1 = NULL
p = p.lag
beta.hat = rep(0, ncol(X.ti))
phi.hat = rep(0, p.lag)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:500){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:p){
    phi.hat[j] = (W1.list[[j]] + W2.list[[j]] - t(beta.hat) %*% W3.list[[j]]
                  -  W4.list[[j]] %*% beta.hat- t(Lmult(phi.hat[-j], W.list[-j], Y.list[-j])) %*% W5.list[[j]]
                  - W6.list[[j]] %*% Lmult(phi.hat[-j], W.list[-j], Y.list[-j]))/(2 * D.list[[j]])
  }
  print(phi.hat[1])
  c1 = cbind(c1, phi.hat)
} 

Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
plot(Y^2 - Y.hat^2, cex = 0.2)













