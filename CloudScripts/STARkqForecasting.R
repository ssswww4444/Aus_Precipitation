###########################
#STAR (k,q) Forecasting
###########################

library("dplyr")
library("fields")
library("gstat")
library("ggplot2")
library("RcolorBrewer")
library("STRbook")
library("sp")
library("spacetime")

lmult5 = function(X.list, a.vec){
  out = list()
  for(i in 1:length(X.list)){
    out[[i]] = matrix(0, nrow = nrow(X.list[[i]][[1]]))
    for(j in 1:length(X.list[[i]])){
      out[[i]] = out[[i]] + a.vec[j] * X.list[[i]][[j]]
    }
  }
  out
}

lmult6 = function(A.list, B.list){
  l.out = matrix(0, nrow = length(beta.hat), ncol = 1)
  for(i in 1:length(A.list)){
    l.out = l.out + t(A.list[[i]]) %*% B.list[[i]]
  }
  l.out
}

lmult7 = function(a.vec, W.list, Y.list, k, n, m){
  l.out = list()
  for(i in 1:length(Y.list)){
    l.out[[i]] = matrix(0, nrow = nrow(W.list[[1]]), ncol = 1)
    for(j in (1:length(n:m))[-k]){
      l.out[[i]] = l.out[[i]] + a.vec[j] * W.list[[j]] %*% Y.list[[i]][[j]]
    }
  }
  l.out
}



lmult8 = function(A.list, B.list){
  l.out = 0
  for(i in 1:length(A.list)){
    l.out = l.out + (A.list[[i]]) %*% B.list[[i]]
  }
  l.out
}

lmult9 = function(A.list, B.list){
  l.out = 0
  for(i in 1:length(A.list)){
    l.out = l.out + t(A.list[[i]]) %*% B.list[[i]]
  }
  l.out
}

lmult6 = function(A.list, B.list){
  l.out = matrix(0, nrow = length(beta.hat), ncol = 1)
  for(i in 1:length(A.list)){
    l.out = l.out + t(A.list[[i]]) %*% B.list[[i]]
  }
  l.out
}

ladd = function(A.list){
  if(is.null(ncol(A.list[[1]]))){
    nc = 1
  }else{
    nc = ncol(A.list[[1]])
  }
  if(is.null(nrow(A.list[[1]]))){
    nr = length(A.list[[1]])
  }else{
    nr = nrow(A.list[[1]])
  }
  out = matrix(0, nrow = nr, ncol = nc)
  for(i in 1:length(A.list)){
    out = out + A.list[[i]]
  }
  out
}


Lmult = function(a.vec, A.list, B.list){
  L.out = 0
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}


Koppen <- readOGR(dsn = ".", layer = "c1976_2000")


set.seed(1998)
m.Aus = sort(sample(1:length(Aus), 1000))
m.World = sort(sample(1:nrow(M1.all), 10000))

precip4 = list()
for(i in 1:length(precip3)){
  precip4[[i]] = precip3[[i]][m.World,]
}

load("precip4")

M2.all = M1.all[m.World,]


A1 = precip4[[1]][,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=bessel +no_defs"
a3 = over(A1, Koppen)

for(i in 1:length(precip3)){
  precip3[[i]]$Kclass = a3[,2]
  print(i)
}


for(i in 1:length(precip4)){
  precip4[[i]]$Kclass = a3[,2]
}

K1 = list()
k = 1
for(i in (unique(a3[,2])[!is.na(unique(a3))])){
  K1[[k]] = which(precip4[[1]]$Kclass == i)
  k = k + 1
}

K2 = list()
k = 1
for(i in 1:length(K1)){
  if(length(K1[[i]]) > 5){
    K2[[k]] = K1[[i]]
    k = k + 1
  }
}

which(!is.na(precip3[[1]]$Kclass[m.Aus]))
prep1 = precip3[[1]][m.Aus,]
prep2 = prep1[which(!is.na(precip3[[1]]$Kclass[m.Aus])),]

d4 = data.frame(Kclass = prep2$Kclass, Lon = prep2$Lon, Lat = prep2$Lat)
pred.grid1 = data.frame(Lon = prep1$Lon, Lat = prep1$Lat)
idw4 = idw(formula = Kclass ~ 1, locations = ~Lon + Lat, data = d4, newdata = pred.grid1, idp = 3)
K1 = round(idw4$var1.pred)

K1 = list()
k = 1
for(i in (unique(a3[,2])[!is.na(unique(a3))])){
  K1[[k]] = which(precip4[[1]]$Kclass == i)
  k = k + 1
}

K2 = list()
k = 1
for(i in 1:length(K1)){
  if(length(K1[[i]]) > 5){
    K2[[k]] = K1[[i]]
    k = k + 1
  }
}


M2 = list()
M2[[1]] = sort(c(m05, m06, m07, m08, m09, m10, m11, m12, m01, m02, m03, m04))



cor.list = list()
cor.pos.list = list()
pb <- txtProgressBar(min = 1, max = 12 * length(m.Aus), style = 3)
t1 = 1
for(l in 1:1){
  cor1.list = list()
  cor1.pos.list = list()
  for(k in 1:18){
    n = M2[[l]]
    m = n - k
    if(max(m) > max(n)){
      m = m[-length(m)]
    }
    if(length(m) < length(n)){
      n = n[-1]
    }
    k5 = sum(m < 1)
    if(k5 > 0){
      n = n[-c(1:k5)]
      m = m[-c(1:k5)]
    }
    cor1_12.Mat = matrix(0, nrow = length(Aus), ncol = length(K2) * 5)
    cor.Mat = matrix(0, nrow = length(Aus), ncol = length(K2) * 5)
    for(i in m.Aus){
      t1 = t1 + 1
      setTxtProgressBar(pb, t1)
      cor.pos = NULL
      cor.1 = NULL
      for(s in 1:length(K2)){
        cor.Gr = rep(0, length(K2[[s]]))
        p = 1
        for(j in K2[[s]]){
          cor.Gr[p] = cor(Aus.all[i, n], M2.all[j, m])
          p = p + 1
        }
        x3 = cbind(cor.Gr, K2[[s]])
        x1 = x3[order(x3[,1], decreasing = TRUE),]
        cor.1 = c(cor.1, x1[1:5,1])
        x2 = x1[1:5,2]
        cor.pos = c(cor.pos, x2)
      }
      cor1_12.Mat[i,] = cor.1
      cor.Mat[i,] = cor.pos
    }
    cor1.list[[k]] = cor1_12.Mat
    cor1.pos.list[[k]] = cor.Mat
  }
  cor.list[[l]] = cor1.list
  cor.pos.list[[l]] = cor1.pos.list
}


#save(cor.list, file="cor.listtot")
#save(cor.pos.list, file="cor.pos.listtot")

load("cor.listtot")
load("cor.pos.listtot")
#c11 = cor.pos.list

max(cor.list[[1]][[1]][m.Aus,], na.rm = TRUE)


k.start = 2
q = k.start + 8
W = list()
N2 = cor.pos.list[[5]]
N4 = cor.list[[5]]
h = 1
for(i in k.start:q){
  N1 = matrix(0, ncol = nrow(M2.all), nrow = length(m.Aus))
  N3 = N2[[i]][m.Aus,]
  N5 = N4[[i]][m.Aus,]
  k = 1
  for(j in 1:length(m.Aus)){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W[[h]] = N1
  h = h + 1
}

Austra1 = read.csv("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid202001")
A1 = Austra1[,2:1]
A1 = as.data.frame(A1)
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)
Aus1 = which(a3$ISO3 == "AUS")

SOI = read.csv("http://www.jamstec.go.jp/virtualearth/data/SINTEX/SINTEX_Nino34.csv")
IOD = read.csv("http://www.jamstec.go.jp/virtualearth/data/SINTEX/SINTEX_DMI.csv")
SOI1 = SOI[,1:2]
IOD1 = IOD[,1:2]
SOI1 = SOI1[1:(nrow(SOI1) - 12),]
IOD1 = IOD1[1:(nrow(IOD1) - 12),]

dates3 = dates2[dates2[,1] > 1981,]

m01.1 = seq(1,nrow(dates3),12)
m02.1 = seq(2,nrow(dates3),12)
m03.1 = seq(3,nrow(dates3),12)
m04.1 = seq(4,nrow(dates3),12)
m05.1 = seq(5,nrow(dates3),12)
m06.1 = seq(6,nrow(dates3),12)
m07.1 = seq(7,nrow(dates3),12)
m08.1 = seq(8,nrow(dates3),12)
m09.1 = seq(9,nrow(dates3),12)
m10.1 = seq(10,nrow(dates3),12)
m11.1 = seq(11,nrow(dates3),12)
m12.1 = seq(12,nrow(dates3),12)

M1 = list()
M1[[1]] = m01.1
M1[[2]] = m02.1
M1[[3]] = m03.1
M1[[4]] = m04.1
M1[[5]] = m05.1
M1[[6]] = m06.1
M1[[7]] = m07.1
M1[[8]] = m08.1
M1[[9]] = m09.1
M1[[10]] = m10.1
M1[[11]] = m11.1
M1[[12]] = m12.1

################
#Set up

t1 = m10.1

phi.hat = rep(0, q - k.start + 1)
mi1 = t1[t1 > q] + 12
mi1 = mi1[mi1 < 474]
T = length(mi1)

mi1.1 = t1[t1 > q]
mi1.1 = mi1.1[mi1.1 < 478 - 12]

k = 1
cor.SOI = rep(0, length(m.Aus))
cor.IOD = rep(0, length(m.Aus))
for(i in m.Aus){
  cor.SOI[k] = cor(SOI1[t1, 2], Aus.all[i, t1])
  cor.IOD[k] = cor(IOD1[t1, 2], Aus.all[i, t1])
  k = k + 1
}


G <- auto_basis(data = precip3[[1]][Aus[m.Aus],c(2,1)] %>% # Take Tmax
                  SpatialPoints(), # To sp obj
                nres = 1, # One resolution
                type = "Gaussian") 

S <- eval_basis(basis = G, # basis functions
                s = precip3[[1]][Aus[m.Aus],c(2,1)] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names

X.ti = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], rowMeans(sqrt(Aus.all[m.Aus,mi1])))

X.ti <- cbind(X.ti, S)

X.list = list()
for(i in 1:T){
  X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[mi1.1[i],2], cor.IOD * IOD1[mi1.1[i],2], cor.SOI * SOI1[mi1.1[i],2] * cor.IOD * IOD1[mi1.1[i],2])
}


Y = list()
k = 1
for(i in mi1){
  Y[[k]] = sqrt(Aus.all[m.Aus,i])
  k = k + 1
}

Y.list = list()
for(i in 1:T){
  a = list()
  k = 1
  for(j in k.start:q){
    a[[k]] = sqrt(M1.all[m.World, mi1[i] - j] )
    k = k + 1
  }
  Y.list[[i]] = a
}


######
#Beta

A.1 = list()
for(i in 1:T){
  A.1[[i]] = t(X.list[[i]]) %*% X.list[[i]]
}

B.1 = list()
for(i in 1:T){
  B.1[[i]] = t(X.list[[i]]) %*% Y[[i]]
}

C.1 = list()
for(i in 1:T){
  a = list()
  for(j in 1:length(phi.hat)){
    a[[j]] = W[[j]] %*% Y.list[[i]][[j]]
  }
  C.1[[i]] = a
}
A.1.r = solve(ladd(A.1))
B.1.r = ladd(B.1)


##############
#phi

A.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = t(Y[[j]]) %*% W[[i]] %*% Y.list[[j]][[i]]
  }
  A.2[[i]] = a
}

B.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = t(Y.list[[j]][[i]]) %*% t(W[[i]]) %*% Y[[j]]
  }
  B.2[[i]] = a
}

C.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = t(X.list[[j]]) %*% W[[i]] %*% Y.list[[j]][[i]]
  }
  C.2[[i]] = a
}

D.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = t(Y.list[[j]][[i]]) %*% t(W[[i]]) %*% X.list[[j]]
  }
  D.2[[i]] = a
}

E.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = t(Y.list[[j]][[i]]) %*% t(W[[i]])
  }
  E.2[[i]] = a
}

F.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = W[[i]] %*% Y.list[[j]][[i]]
  }
  F.2[[i]] = a
}

G.2 = list()
for(i in 1:length(phi.hat)){
  a = list()
  for(j in 1:T){
    a[[j]] = t(Y.list[[j]][[i]]) %*% t(W[[i]]) %*% W[[i]] %*% Y.list[[j]][[i]]
  }
  G.2[[i]] = a
}

A.2.r = list()
for(i in 1:length(A.2)){
  A.2.r[[i]] = ladd(A.2[[i]])
}

B.2.r = list()
for(i in 1:length(B.2)){
  B.2.r[[i]] = ladd(B.2[[i]])
}

C.2.r = list()
for(i in 1:length(C.2)){
  C.2.r[[i]] = ladd(C.2[[i]])
}

D.2.r = list()
for(i in 1:length(D.2)){
  D.2.r[[i]] = ladd(D.2[[i]])
}

G.2.r = list()
for(i in 1:length(G.2)){
  G.2.r[[i]] = ladd(G.2[[i]])
}


H.1 = list()
for(i in 1:length(Y.list)){
  H.1[[i]] = list()
  for(j in 1:length(Y.list[[i]])){
    H.1[[i]][[j]] = W[[j]] %*% Y.list[[i]][[j]]
  }
}

lmult10 = function(a.vec, H.list, k, n, m){
  l.out = list()
  for(i in 1:length(Y.list)){
    l.out[[i]] = matrix(0, nrow = nrow(H.list[[1]][[1]]), ncol = 1)
    for(j in (1:length(n:m))[-k]){
      l.out[[i]] = l.out[[i]] + a.vec[j] * H.list[[i]][[j]]
    }
  }
  l.out
}


L1 = Inf
k = 1
phi.hat = rep(0, q - k.start + 1)
beta.hat = matrix(0, nrow = ncol(X.list[[1]]), ncol = 1)
while(abs(L1 - phi.hat[1]) > 10^(-15)){ 
  L1 = phi.hat[1]
  beta.hat = A.1.r %*% (B.1.r - lmult6(X.list, lmult5(C.1, phi.hat)))
  for(j in 1:length(phi.hat)){
    phi.hat[j] = (A.2.r[[j]] + (B.2.r[[j]]) - t(beta.hat) %*% (C.2.r[[j]]) - (D.2.r[[j]]) %*% beta.hat -
                    lmult8(E.2[[j]], lmult10(phi.hat, H.1, j, k.start, q)) - lmult9(lmult10(phi.hat, H.1, j, k.start, q), F.2[[j]]))/(2*(G.2.r[[j]]))
  }
  print( c(k,phi.hat[1]))
  k = k + 1
}


Y.hat = list()
for(i in 1:length(Y)){
  Y.hat[[i]] = X.list[[i]] %*% beta.hat + Lmult(phi.hat, W, Y.list[[i]]) 
}
max(Y.hat[[1]])


X.ti1 = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], rowMeans(sqrt(Aus.all[m.Aus,mi1])))
X.ti1 <- cbind(X.ti1, S)
X.list1 = list()
l = 1
for(i in T:T){
  X.list1[[l]] = cbind(X.ti1, cor.SOI * SOI1[mi1.1[i],2], cor.IOD * IOD1[mi1.1[i],2], cor.SOI * SOI1[mi1.1[i],2] * cor.IOD * IOD1[mi1.1[i],2])
}

Y1 = list()
k = 1
for(i in (max(mi1))){
  Y1[[k]] = sqrt(Aus.all[m.Aus,i])
  k = k + 1
}

Y.list1 = list()
for(i in T:T){
  a = list()
  for(j in k.start:q){
    a[[j]] = sqrt(M1.all[m.World, max(mi1) - j] )
  }
  Y.list1[[1]] = a
}

Y.hat1 = list()
for(i in 1:1){
  Y.hat1[[i]] = X.list1[[i]] %*% beta.hat + Lmult(phi.hat, W, Y.list1[[i]]) 
}



d1 = data.frame(Est = Y.hat1[[1]], Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Est ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

d1 = data.frame(Est = Y1[[1]], Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw2 = idw(formula = Est ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

g1 = ggplot(idw1, aes(x = Lon, y = Lat)) + geom_point(aes(colour = (var1.pred)^2), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred^2, idw2$var1.pred^2)), max(idw1$var1.pred^2, idw2$var1.pred^2))) +
  theme_bw() + labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude", title = "June 2020 One Month Forecast Precipitation")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))


gactual = ggplot(idw2, aes(x = Lon, y = Lat)) + geom_point(aes(colour = (var1.pred)^2), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred^2, idw2$var1.pred^2)), max(idw1$var1.pred^2, idw2$var1.pred^2))) +
  theme_bw() + labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude", title = "June 2020 Observed Precipitation")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

library("grid")
library("gridExtra")

grid.arrange(gactual, g1, nrow = 1)

sqrt(sum((Y.hat1[[1]]^2 - Y1[[1]]^2)^2)/length(Y1[[1]]))




######################
#NR Algorithm
######################

Lmult = function(a.vec, A.list, B.list){
  L.out = 0
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}



set.seed(1998)
m.Aus = sort(sample(1:length(Aus), 1000))
m.World = sort(sample(1:nrow(M1.all), 10000))

M2.all = M1.all[m.World,]

m.now = m01[m01 > q]
k = 1
q = k + 5
Y = list()
X = list()
Y.w = list()
for(i in 1:length(m.now)){
  Y[[i]] = Aus.all[m.Aus,m.now[i]]
  X[[i]] = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2])
  Y.w[[i]] = list()
  for(j in 1:length(k:q)){
    Y.w[[i]][[j]] = M1.all[m.World, m.now[i] - (k:q)[j]]
  }
}





lmult.1 = function(W.list, Y.w.list, a.vec){
  out = 0
  l = 1
  for(i in 1:length(W.list)){
    out = out + a.vec[i] * W.list[[i]] %*% Y.w.list[[i]]
    l = l + 1
  }
  out
}

lmult.2 = function(Y.w.list, a.vec){
  out = 0
  for(i in 1:length(Y.w.list)){
    out = out + a.vec[i] * Y.w.list[[i]]
  }
  out
}

lmult.1(W, Y.w[[1]], rep(0,6))

l.h = function(beta, phi, sigma, X.list, Y.list, W.list, Y.w.list, k, q){
  out.1 = list()
  out.2 = list()
  for(i in 1:length(Y.list)){
    out.1[[i]] = -(1/sigma^2) * (-t(X.list[[i]]) %*% Y.list[[i]] + t(X.list[[i]]) %*% X.list[[i]] %*% beta + t(X.list[[i]]) %*% lmult.1(W.list, Y.w.list[[i]], phi))
  }
  out.1.1 = ladd(out.1)
  out.2.1 = NULL
  for(i in 1:length(k:q)){
    out.2[[i]] = list()
    for(j in 1:length(Y.list)){
      out.2[[i]][[j]] = -(1/(2*sigma^2)) * (-t(Y.list[[j]]) %*% W.list[[i]] %*% Y.w.list[[j]][[i]] + t(beta) %*% t(X.list[[j]]) %*% W.list[[i]] %*% Y.w.list[[j]][[i]] - 
                                              t(Y.w.list[[j]][[i]]) %*% t(W.list[[i]]) %*% Y.list[[j]] + t(Y.w.list[[j]][[i]]) %*% t(W.list[[i]]) %*% X.list[[j]] %*% beta + 
                                              t(Y.w.list[[j]][[i]]) %*% t(W.list[[i]]) %*% lmult.1(W.list, Y.w.list[[j]], phi) +
                                              t(lmult.1(W.list, Y.w.list[[j]], phi)) %*% W.list[[i]] %*% Y.w.list[[j]][[i]])
    }
    out.2.1 = c(out.2.1, ladd(out.2[[i]]))
  }
  out.3 = list()
  for(i in 1:length(Y.list)){
    out.3[[i]] = (-length(Y.list[[i]])/sigma) +(1/sigma^3) * t(Y.list[[i]] - X.list[[i]] %*% beta - lmult.1(W.list, Y.w.list[[i]], phi)) %*% (Y.list[[i]] -
                                                                                                                                                X.list[[i]] %*% beta - lmult.1(W.list, Y.w.list[[i]], phi))
    
  }
  out.3.1 = ladd(out.3)
  out = c(out.1.1, out.2.1, out.3.1)
  out
}

l.h(c(0,0,0), rep(0,6), 1, X, Y, W, Y.w,1,6)

XX.1 = list()
for(i in 1:length(X)){
  XX.1[[i]] = t(X[[i]]) %*% X[[i]]
}

XWY.1 = list()
for(i in 1:length(W)){
  XWY.1[[i]] = list()
  for(j in 1:length(X)){
    XWY.1[[i]][[j]] = t(X[[j]]) %*% W[[i]] %*% Y.w[[j]][[i]]
  }
}

XY.1 = list()
for(i in 1:length(X)){
  XY.1[[i]] = t(X[[i]]) %*% Y[[i]]
}

WY.1 = list()
for(i in 1:length(W)){
  WY.1[[i]] = list()
  for(j in 1:length(Y)){
    WY.1[[i]][[j]]= W[[i]] %*% Y.w[[j]][[i]]
  }
}

YWWY.1 = list()
for(i in 1:length(W)){
  YWWY.1[[i]]= list()
  for(j in 1:length(W)){
    YWWY.1[[i]][[j]] = list() 
    for(l in 1:length(Y)){
      YWWY.1[[i]][[j]][[l]] = t(Y.w[[l]][[i]]) %*% t(W[[i]]) %*% W[[j]] %*% Y.w[[l]][[j]]
    }
  }
}

YWY.1 = list()
for(i in 1:length(W)){
  YWY.1[[i]] = list()
  for(j in 1:length(Y)){
    YWY.1[[i]][[j]] = t(Y[[j]]) %*% W[[i]] %*% Y.w[[j]][[i]]
  }
}

YWY.2 = list()
for(i in 1:length(W)){
  YWY.2[[i]] = list()
  for(j in 1:length(Y)){
    YWY.2[[i]][[j]] = t(Y.w[[j]][[i]]) %*% t(W[[i]]) %*% Y[[j]]
  }
}

YWX.1 = list()
for(i in 1:length(W)){
  YWX.1[[i]] = list()
  for(j in 1:length(Y)){
    YWX.1[[i]][[j]] = t(Y.w[[j]][[i]]) %*% t(W[[i]]) %*% X[[j]]
  }
}

YW.1 = list()
for(i in 1:length(W)){
  YW.1[[i]] = list()
  for(j in 1:length(Y)){
    YW.1[[i]][[j]] = t(Y.w[[j]][[i]]) %*% t(W[[i]])
  }
}

l.h = function(beta, phi, sigma, X.list, Y.list, W.list, Y.w.list, k, q, XX.1, XWY.1, XY.1, WY.1, YWWY.1, YWY.1, YWY.2, YWX.1, YW.1){
  out.1 = list()
  out.2 = list()
  for(i in 1:length(Y.list)){
    out.1[[i]] = -(1/sigma^2) * (-XY.1[[i]] + XX.1[[i]] %*% beta + t(X.list[[i]]) %*% lmult.1(W.list, Y.w.list[[i]], phi))
  }
  out.1.1 = ladd(out.1)
  out.2.1 = NULL
  for(i in 1:length(k:q)){
    out.2[[i]] = list()
    for(j in 1:length(Y.list)){
      out.2[[i]][[j]] = -(1/(2*sigma^2)) * (-YWY.1[[i]][[j]] + t(beta) %*% XWY.1[[i]][[j]] - YWY.2[[i]][[j]] + YWX.1[[i]][[j]] %*% beta + 
                                              YW.1[[i]][[j]] %*% lmult.1(W.list, Y.w.list[[j]], phi) +
                                              t(lmult.1(W.list, Y.w.list[[j]], phi)) %*% WY.1[[i]][[j]])
    }
    out.2.1 = c(out.2.1, ladd(out.2[[i]]))
  }
  out.3 = list()
  for(i in 1:length(Y.list)){
    out.3[[i]] = (-length(Y.list[[i]])/sigma) +(1/sigma^3) * t(Y.list[[i]] - X.list[[i]] %*% beta - lmult.1(W.list, Y.w.list[[i]], phi)) %*% (Y.list[[i]] -
                                                                                                                                                X.list[[i]] %*% beta - lmult.1(W.list, Y.w.list[[i]], phi))
    
  }
  out.3.1 = ladd(out.3)
  out = c(out.1.1, out.2.1, out.3.1)
  out
}

L.F = function(beta, phi, sigma, X.list, Y.list, W.list, Y.w.list, k, q, XX.1, XWY.1, XY.1, WY.1, YWWY.1, YWY.1, YWY.2, YWX.1, YW.1){
  out.1.1 = list()
  for(i in 1:length(Y.list)){
    out.1.1[[i]] = -(1/sigma^2) * XX.1[[i]]
  }
  out.1.1.1 = ladd(out.1.1)
  out.2.1.1 = NULL
  out.2.1 = list()
  for(i in 1:length(k:q)){
    out.2.1[[i]] = list()
    for(j in 1:length(Y.list)){
      out.2.1[[i]][[j]] = -(1/sigma^2) * XWY.1[[i]][[j]]
    }
    out.2.1.1 = cbind(out.2.1.1, ladd(out.2.1[[i]]))
  }
  out.3.1 = list()
  for(i in 1:length(Y.list)){
    out.3.1[[i]] = (2/sigma^3) * (-XY.1[[i]] + XX.1[[i]] %*% beta + t(X.list[[i]]) %*% lmult.1(W.list, Y.w.list[[i]], phi))
  }
  out.3.1.1 = ladd(out.3.1)
  out.2.2 = list()
  out.2.2.1 = NULL
  for(i in 1:length(k:q)){
    out.2.2[[i]] = list()
    out.2.2.2 = NULL
    for(j in 1:length(k:q)){
      out.2.2[[i]][[j]] = list()
      for(l in 1:length(Y.list)){
        out.2.2[[i]][[j]][[l]] = -(1/(2*sigma^2)) * (YWWY.1[[i]][[j]][[l]] + YWWY.1[[j]][[i]][[l]])
      }
      out.2.2.2 = c(out.2.2.2, ladd(out.2.2[[i]][[j]]))
    }
    out.2.2.1 = cbind(out.2.2.1, out.2.2.2)
  }
  out.2.3 = list()
  out.2.3.1 = NULL
  for(i in 1:length(k:q)){
    out.2.3[[i]] = list()
    for(j in 1:length(Y.list)){
      out.2.3[[i]][[j]] = (1/sigma^3) * (-YWY.1[[i]][[j]] + t(beta) %*% XWY.1[[i]][[j]] - YWY.2[[i]][[j]] + YWX.1[[i]][[j]] %*% beta +
                                           YW.1[[i]][[j]] %*% lmult.1(W.list, Y.w.list[[j]], phi) + 
                                           t(lmult.1(W.list, Y.w.list[[j]], phi)) %*% WY.1[[i]][[j]])
    }
    out.2.3.1 = c(out.2.3.1, ladd(out.2.3[[i]]))
  }
  out.3.3 = list()
  for(i in 1:length(Y.list)){
    out.3.3[[i]] = (length(Y.list[[i]])/sigma^2) - (3/sigma^4)*t(Y.list[[i]] - X.list[[i]] %*% beta - lmult.1(W.list, Y.w.list[[i]], phi)) %*% (Y.list[[i]] -
                                                                                                                                                  X.list[[i]] %*% beta - lmult.1(W.list, Y.w.list[[i]], phi))
  }
  out.3.3.1 = ladd(out.3.3)
  out = cbind(out.1.1.1, out.2.1.1, out.3.1.1)
  out = rbind(out, cbind(t(out.2.1.1), out.2.2.1, out.2.3.1))
  out = rbind(out, cbind(t(out.3.1.1), t(out.2.3.1), out.3.3.1))
  out
}


LH = l.h(c(0,0,0), rep(0,6), 1, X.list = X, Y.list = Y, W.list = W, Y.w,1,6,XX.1, XWY.1, XY.1, WY.1, YWWY.1, YWY.1, YWY.2, YWX.1, YW.1)
LF = L.F(c(0,0,0), rep(0,6), 1, X.list = X, Y.list = Y, W.list = W, Y.w,1,6,XX.1, XWY.1, XY.1, WY.1, YWWY.1, YWY.1, YWY.2, YWX.1, YW.1)
solve(LF) %*% LH

beta.hat = rep(0, ncol(X[[1]]))
phi.hat = rep(0, length(W))
sigma.hat = 1
theta.hat = c(beta.hat, phi.hat, sigma.hat)
l = Inf
k1 = 0
while(abs(l - theta.hat[1]) > 10^(-3)){
  l = theta.hat[1]
  LH = l.h(beta.hat, phi.hat, sigma.hat, X.list = X, Y.list = Y, W.list = W, Y.w, k, q,XX.1, XWY.1, XY.1, WY.1, YWWY.1, YWY.1, YWY.2, YWX.1, YW.1)
  LF = L.F(beta.hat, phi.hat, sigma.hat, X.list = X, Y.list = Y, W.list = W, Y.w, k, q,XX.1, XWY.1, XY.1, WY.1, YWWY.1, YWY.1, YWY.2, YWX.1, YW.1)
  theta.hat = theta.hat - solve(LF) %*% LH
  beta.hat = theta.hat[1:length(beta.hat)]
  phi.hat = theta.hat[(length(beta.hat) + 1):(length(phi.hat) + length(beta.hat))]
  sigma.hat = theta.hat[length(theta.hat)]
  k1 = k1 + 1
  print(k1)
  print(theta.hat)
}


###############
##CV
###############
set.seed(1998)
cv = split(1:length(Y[[1]]), paste0("G", 1:10))
Y.CV = list()
X.list.CV = list()
W.CV = list()
for(i in 1:length(Y)){
  Y.CV[[i]] = list()
  X.list.CV[[i]] = list()
  for(j in 1:length(cv)){
    Y.CV[[i]][[j]] = Y[[i]][cv[[j]]]
    X.list.CV[[i]][[j]] = X.list[[i]][cv[[j]],]
  }
}
for(i in 1:length(W)){
  W.CV[[i]] = list()
  for(j in 1:length(cv)){
    W.CV[[i]][[j]] = W[[i]][cv[[j]],]
  }
}

Y.new = list()
X.CV = list()
for(i in 1:length(cv)){
  Y.new[[i]] = list()
  X.CV[[i]] = list()
  for(j in 1:length(Y)){
    Y.new[[i]][[j]] = Y[[j]][-cv[[i]]]
    X.CV[[i]][[j]] = X.list[[j]][-cv[[i]], ]
  }
}

W.CV = list()
for(i in 1:length(cv)){
  W.CV[[i]] = list()
  for(j in 1:length(W)){
    W.CV[[i]][[j]] = W[[j]][-cv[[i]], ]
  }
}


RSS.CV = list()
for(p in 0:10){
  k.start = 3
  q = k.start + p
  W = list()
  N2 = cor.pos.list[[6]]
  N4 = cor.list[[6]]
  h = 1
  for(i in k.start:q){
    N1 = matrix(0, ncol = nrow(M2.all), nrow = length(m.Aus))
    N3 = N2[[i]][m.Aus,]
    N5 = N4[[i]][m.Aus,]
    k = 1
    for(j in 1:length(m.Aus)){
      N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
      k = k + 1
    }
    W[[h]] = N1
    h = h + 1
  }
  
  t1 = m11.1
  phi.hat = rep(0, q - k.start + 1)
  mi1 = t1[t1 > q] + 12
  mi1 = mi1[mi1 < 475]
  T = length(mi1)
  
  mi1.1 = t1[t1 > q]
  mi1.1 = mi1.1[mi1.1 < 476 - 12]
  
  k = 1
  cor.SOI = rep(0, length(m.Aus))
  cor.IOD = rep(0, length(m.Aus))
  for(i in m.Aus){
    cor.SOI[k] = cor(SOI1[t1, 2], Aus.all[i, t1])
    cor.IOD[k] = cor(IOD1[t1, 2], Aus.all[i, t1])
    k = k + 1
  }
  
  X.ti = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], rowMeans(sqrt(Aus.all[m.Aus,mi1])))
  
  X.ti <- cbind(X.ti, S)
  
  X.list = list()
  for(i in 1:T){
    X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[mi1.1[i],2], cor.IOD * IOD1[mi1.1[i],2], cor.SOI * SOI1[mi1.1[i],2] * cor.IOD * IOD1[mi1.1[i],2])
  }
  
  
  
  Y = list()
  k = 1
  for(i in mi1){
    Y[[k]] = sqrt(Aus.all[m.Aus,i])
    k = k + 1
  }
  
  Y.list = list()
  for(i in 1:T){
    a = list()
    l = 1
    for(j in k.start:q){
      a[[l]] = sqrt(M1.all[m.World, mi1[i] - j] )
      l = l + 1
    }
    Y.list[[i]] = a
  }
  
  
  
  set.seed(1998)
  cv = split(1:length(Y[[1]]), paste0("G", 1:10))
  Y.CV = list()
  X.list.CV = list()
  W.CV = list()
  for(i in 1:length(Y)){
    Y.CV[[i]] = list()
    X.list.CV[[i]] = list()
    for(j in 1:length(cv)){
      Y.CV[[i]][[j]] = Y[[i]][cv[[j]]]
      X.list.CV[[i]][[j]] = X.list[[i]][cv[[j]],]
    }
  }
  for(i in 1:length(W)){
    W.CV[[i]] = list()
    for(j in 1:length(cv)){
      W.CV[[i]][[j]] = W[[i]][cv[[j]],]
    }
  }
  
  Y.new = list()
  X.CV = list()
  for(i in 1:length(cv)){
    Y.new[[i]] = list()
    X.CV[[i]] = list()
    for(j in 1:length(Y)){
      Y.new[[i]][[j]] = Y[[j]][-cv[[i]]]
      X.CV[[i]][[j]] = X.list[[j]][-cv[[i]], ]
    }
  }
  
  W.CV = list()
  for(i in 1:length(cv)){
    W.CV[[i]] = list()
    for(j in 1:length(W)){
      W.CV[[i]][[j]] = W[[j]][-cv[[i]], ]
    }
  }
  
  
  RSS = NULL
  for(m in 1:length(cv)){
    print(m)
    A.1 = list()
    for(i in 1:T){
      A.1[[i]] = t(X.CV[[m]][[i]]) %*% X.CV[[m]][[i]]
    }
    
    B.1 = list()
    for(i in 1:T){
      B.1[[i]] = t(X.CV[[m]][[i]]) %*% Y.new[[m]][[i]]
    }
    
    C.1 = list()
    for(i in 1:T){
      a = list()
      for(j in 1:length(phi.hat)){
        a[[j]] = W.CV[[m]][[j]] %*% Y.list[[i]][[j]]
      }
      C.1[[i]] = a
    }
    A.1.r = solve(ladd(A.1))
    B.1.r = ladd(B.1)
    
    
    ##############
    #phi
    
    A.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = t(Y.new[[m]][[j]]) %*% W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
      }
      A.2[[i]] = a
    }
    
    B.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]]) %*% Y.new[[m]][[j]]
      }
      B.2[[i]] = a
    }
    
    C.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = t(X.CV[[m]][[j]]) %*% W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
      }
      C.2[[i]] = a
    }
    
    D.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]]) %*% X.CV[[m]][[j]]
      }
      D.2[[i]] = a
    }
    
    E.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]])
      }
      E.2[[i]] = a
    }
    
    F.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
      }
      F.2[[i]] = a
    }
    
    G.2 = list()
    for(i in 1:length(phi.hat)){
      a = list()
      for(j in 1:T){
        a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]]) %*% W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
      }
      G.2[[i]] = a
    }
    
    A.2.r = list()
    for(i in 1:length(A.2)){
      A.2.r[[i]] = ladd(A.2[[i]])
    }
    
    B.2.r = list()
    for(i in 1:length(B.2)){
      B.2.r[[i]] = ladd(B.2[[i]])
    }
    
    C.2.r = list()
    for(i in 1:length(C.2)){
      C.2.r[[i]] = ladd(C.2[[i]])
    }
    
    D.2.r = list()
    for(i in 1:length(D.2)){
      D.2.r[[i]] = ladd(D.2[[i]])
    }
    
    G.2.r = list()
    for(i in 1:length(G.2)){
      G.2.r[[i]] = ladd(G.2[[i]])
    }
    
    H.1 = list()
    for(i in 1:length(Y.list)){
      H.1[[i]] = list()
      for(j in 1:length(Y.list[[i]])){
        H.1[[i]][[j]] = W.CV[[m]][[j]] %*% Y.list[[i]][[j]]
      }
    }
    
    lmult10 = function(a.vec, H.list, k, n, m){
      l.out = list()
      for(i in 1:length(Y.list)){
        l.out[[i]] = matrix(0, nrow = nrow(H.list[[1]][[1]]), ncol = 1)
        for(j in (1:length(n:m))[-k]){
          l.out[[i]] = l.out[[i]] + a.vec[j] * H.list[[i]][[j]]
        }
      }
      l.out
    }
    
    
    L1 = Inf
    k = 1
    phi.hat = rep(0, q - k.start + 1)
    beta.hat = matrix(0, nrow = ncol(X.list[[1]]), ncol = 1)
    while(abs(L1 - phi.hat[1]) > 10^(-5)){ 
      L1 = phi.hat[1]
      beta.hat = A.1.r %*% (B.1.r - lmult6(X.CV[[m]], lmult5(C.1, phi.hat)))
      for(j in 1:length(phi.hat)){
        phi.hat[j] = (A.2.r[[j]] + (B.2.r[[j]]) - t(beta.hat) %*% (C.2.r[[j]]) - (D.2.r[[j]]) %*% beta.hat -
                        lmult8(E.2[[j]], lmult10(phi.hat, H.1, j, k.start, q)) - lmult9(lmult10(phi.hat, H.1, j, k.start, q), F.2[[j]]))/(2*(G.2.r[[j]]))
      }
      k = k + 1
    }
    
    X.list.1 = list()
    for(i in 1:length(X.list)){
      X.list.1[[i]] = X.list[[i]][cv[[m]],]
    }
    
    W1 = list()
    for(i in 1:length(W)){
      W1[[i]] = W[[i]][cv[[m]], ]
    }
    
    Y.hat1 = list()
    for(i in 1:T){
      Y.hat1[[i]] = X.list.1[[i]] %*% beta.hat + Lmult(phi.hat, W1, Y.list[[i]]) 
    }
    
    Y.Check = list()
    for(i in 1:length(Y)){
      Y.Check[[i]] = Y[[i]][cv[[m]]]
    }
    
    for(i in 1:length(Y)){
      RSS = c(RSS, Y.Check[[i]] - Y.hat1[[i]])
    }
  }
  RSS.CV[[p + 1]] = RSS
  print(p)
}


for(i in 1:length(RSS.CV)){
  print(sum(RSS.CV[[i]]^2)/length(RSS.CV[[i]]))
}





####################
#CV dates and leads
####################

w1 = c(1)

SOI = as.numeric(SOI3[as.numeric(substr(SOI3[,2],1,4)) > 1980, 1])
IOD = as.numeric(IOD3[as.numeric(substr(IOD3[,2],1,4)) > 1980, 1])

pb <- txtProgressBar(min = 1, max = length(M) * 6 * 10, style = 3)
Q1 = list()
k.count = 1
for(g in 1:length(M)){
  Q2 = list()
  t1 = M[[g]]
  N2 = cor.pos.list[[1]]
  N4 = cor.list[[1]]
  for(k1 in 1:6){

    RSS.CV = list()
    k.start = k1
    for(p in 0:12){
      setTxtProgressBar(pb, k.count)
      k.count = k.count + 1

      q = k.start + p
      h = 1
      W = list()
      for(i in k.start:q){

        N1 = matrix(0, ncol = nrow(M2.all), nrow = length(m.Aus))
        N3 = N2[[i]][m.Aus,]
        N5 = N4[[i]][m.Aus,]
        k = 1
        for(j in 1:length(m.Aus)){
          N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
          k = k + 1
        }
        W[[h]] = N1
        h = h + 1
      }
      
      phi.hat = rep(0, q - k.start + 1)
      mi1 = t1[t1 > q] + 12
      mi1 = mi1[mi1 < nrow(dates2) + 1]
      T = length(mi1)
      
      mi1.1 = t1[t1 > q]
      mi1.1 = mi1.1[mi1.1 < nrow(dates2) + 1 - 12]
      
      k = 1
      cor.SOI = rep(0, length(m.Aus))
      cor.IOD = rep(0, length(m.Aus))
      for(i in m.Aus){
        cor.SOI[k] = cor(SOI1[t1], Aus.all[i, t1])
        cor.IOD[k] = cor(IOD1[t1], Aus.all[i, t1])
        k = k + 1
      }
      X.ti = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], rowMeans(sqrt(Aus.all[m.Aus,mi1])))
      
      X.ti <- cbind(X.ti, S)
      
      X.list = list()
      for(i in 1:T){
        X.list[[i]] = cbind(X.ti, cor.SOI * SOI1[mi1.1[i]], cor.IOD * IOD1[mi1.1[i]], cor.SOI * SOI1[mi1.1[i]] * cor.IOD * IOD1[mi1.1[i]])
      }
      
      Y = list()
      k = 1
      for(i in mi1){
        Y[[k]] = sqrt(Aus.all[m.Aus,i])
        k = k + 1
      }
      
      Y.list = list()
      for(i in 1:T){
        a = list()
        l = 1
        for(j in k.start:q){
          a[[l]] = sqrt(M1.all[m.World, mi1[i] - j] )
          l = l + 1
        }
        Y.list[[i]] = a
      }
      
      set.seed(1998)
      cv = split(1:length(Y[[1]]), paste0("G", 1:10))
      Y.CV = list()
      X.list.CV = list()
      W.CV = list()
      for(i in 1:length(Y)){
        Y.CV[[i]] = list()
        X.list.CV[[i]] = list()
        for(j in 1:length(cv)){
          Y.CV[[i]][[j]] = Y[[i]][cv[[j]]]
          X.list.CV[[i]][[j]] = X.list[[i]][cv[[j]],]
        }
      }
      
      for(i in 1:length(W)){
        W.CV[[i]] = list()
        for(j in 1:length(cv)){
          W.CV[[i]][[j]] = W[[i]][cv[[j]],]
        }
      }
      Y.new = list()
      X.CV = list()
      for(i in 1:length(cv)){
        Y.new[[i]] = list()
        X.CV[[i]] = list()
        for(j in 1:length(Y)){
          Y.new[[i]][[j]] = Y[[j]][-cv[[i]]]
          X.CV[[i]][[j]] = X.list[[j]][-cv[[i]], ]
        }
      }

      W.CV = list()
      for(i in 1:length(cv)){
        W.CV[[i]] = list()
        for(j in 1:length(W)){
          W.CV[[i]][[j]] = W[[j]][-cv[[i]], ]
        }
      }

      
      RSS = NULL
      for(m in 1:length(cv)){
        A.1 = list()
        for(i in 1:T){
          A.1[[i]] = t(X.CV[[m]][[i]]) %*% X.CV[[m]][[i]]
        }
        
        B.1 = list()
        for(i in 1:T){
          B.1[[i]] = t(X.CV[[m]][[i]]) %*% Y.new[[m]][[i]]
        }
        
        C.1 = list()
        for(i in 1:T){
          a = list()
          for(j in 1:length(phi.hat)){
            a[[j]] = W.CV[[m]][[j]] %*% Y.list[[i]][[j]]
          }
          C.1[[i]] = a
        }
        
        A.1.r = solve(ladd(A.1))
        B.1.r = ladd(B.1)
        
        
        ##############
        #phi
        
        A.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = t(Y.new[[m]][[j]]) %*% W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
          }
          A.2[[i]] = a
        }
        
        B.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]]) %*% Y.new[[m]][[j]]
          }
          B.2[[i]] = a
        }
        
        C.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = t(X.CV[[m]][[j]]) %*% W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
          }
          C.2[[i]] = a
        }
        
        D.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]]) %*% X.CV[[m]][[j]]
          }
          D.2[[i]] = a
        }
        
        E.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]])
          }
          E.2[[i]] = a
        }
        
        F.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
          }
          F.2[[i]] = a
        }
        
        G.2 = list()
        for(i in 1:length(phi.hat)){
          a = list()
          for(j in 1:T){
            a[[j]] = t(Y.list[[j]][[i]]) %*% t(W.CV[[m]][[i]]) %*% W.CV[[m]][[i]] %*% Y.list[[j]][[i]]
          }
          G.2[[i]] = a
        }
        
        A.2.r = list()
        for(i in 1:length(A.2)){
          A.2.r[[i]] = ladd(A.2[[i]])
        }
        B.2.r = list()
        for(i in 1:length(B.2)){
          B.2.r[[i]] = ladd(B.2[[i]])
        }
        C.2.r = list()
        for(i in 1:length(C.2)){
          C.2.r[[i]] = ladd(C.2[[i]])
        }
        D.2.r = list()
        for(i in 1:length(D.2)){
          D.2.r[[i]] = ladd(D.2[[i]])
        }

        G.2.r = list()
        for(i in 1:length(G.2)){
          G.2.r[[i]] = ladd(G.2[[i]])
        }
        
        H.1 = list()
        for(i in 1:length(Y.list)){
          H.1[[i]] = list()
          for(j in 1:length(Y.list[[i]])){
            H.1[[i]][[j]] = W.CV[[m]][[j]] %*% Y.list[[i]][[j]]
          }
        }
        
        lmult10 = function(a.vec, H.list, k, n, m){
          l.out = list()
          for(i in 1:length(H.list)){
            l.out[[i]] = matrix(0, nrow = nrow(H.list[[1]][[1]]), ncol = 1)
            for(j in (1:length(n:m))[-k]){
              l.out[[i]] = l.out[[i]] + a.vec[j] * H.list[[i]][[j]]
            }
          }
          l.out
        }
        
        L1 = Inf
        k = 1
        phi.hat = rep(0, q - k.start + 1)
        beta.hat = matrix(0, nrow = ncol(X.list[[1]]), ncol = 1)
        while(abs(L1 - phi.hat[1]) > 10^(-5)){ 
          L1 = phi.hat[1]
          beta.hat = A.1.r %*% (B.1.r - lmult6(X.CV[[m]], lmult5(C.1, phi.hat)))
          for(j in 1:length(phi.hat)){
            phi.hat[j] = (A.2.r[[j]] + (B.2.r[[j]]) - t(beta.hat) %*% (C.2.r[[j]]) - (D.2.r[[j]]) %*% beta.hat -
                            lmult8(E.2[[j]], lmult10(phi.hat, H.1, j, k.start, q)) - lmult9(lmult10(phi.hat, H.1, j, k.start, q), F.2[[j]]))/(2*(G.2.r[[j]]))
          }
          k = k + 1
        }
        
        X.list.1 = list()
        for(i in 1:length(X.list)){
          X.list.1[[i]] = X.list[[i]][cv[[m]],]
        }
        
        W1 = list()
        for(i in 1:length(W)){
          W1[[i]] = W[[i]][cv[[m]], ]
        }
        
        Y.hat1 = list()
        for(i in 1:T){
          Y.hat1[[i]] = X.list.1[[i]] %*% beta.hat + Lmult(phi.hat, W1, Y.list[[i]]) 
        }
        
        Y.Check = list()
        for(i in 1:length(Y)){
          Y.Check[[i]] = Y[[i]][cv[[m]]]
        }
        
        for(i in 1:length(Y)){
          RSS = c(RSS, Y.Check[[i]] - Y.hat1[[i]])
        }
      }
      RSS.CV[[p + 1]] = RSS
    }
    CVE1 = NULL
    for(i in 1:length(RSS.CV)){
      CVE1 = c(CVE1, sum(RSS.CV[[i]]^2)/length(RSS.CV[[i]]))
    }
    Q2[[k1]] = CVE1
  }
  Q1[[g]] = Q2
  save(Q1, file = "CV.list")
}







