best.SOI = NULL
for(lag in 1:6){
  opt.phi = NULL
  for(k in 1:10){
    lphi = k
    ar.mat = NULL
    for(i in (lphi + lag):nrow(SOI1)){
      ar.mat = rbind(ar.mat, c(SOI1[i,2], SOI1[(i - lag):(i - lag - lphi + 1),2]))
    }
    set.seed(1998)
    cv.error = NULL
    for(i in 1:10){
      samp1 = sample(1:nrow(ar.mat), size = 40)
      ar.mat1 = ar.mat[-samp1,]
      yc = ar.mat[samp1,1]
      Xc = cbind(1, ar.mat[samp1,-1])
      yphi = ar.mat1[,1]
      Xphi = cbind(1, ar.mat1[,-1])
      phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
      yc.hat = Xc %*% phi.hat
      cv.error = c(cv.error, sum((yc.hat - yc)^2)/nrow(ar.mat))
    }
    opt.phi = c(opt.phi, mean(cv.error))
  }
  best.SOI = rbind(best.SOI, c(min(opt.phi), which(opt.phi == min(opt.phi))))
}

best.IOD = NULL
for(lag in 1:6){
  opt.phi = NULL
  for(k in 1:10){
    lphi = k
    ar.mat = NULL
    for(i in (lphi + lag):nrow(IOD1)){
      ar.mat = rbind(ar.mat, c(IOD1[i,2], IOD1[(i - lag):(i - lag - lphi + 1),2]))
    }
    set.seed(1998)
    cv.error = NULL
    for(i in 1:10){
      samp1 = sample(1:nrow(ar.mat), size = 40)
      ar.mat1 = ar.mat[-samp1,]
      yc = ar.mat[samp1,1]
      Xc = cbind(1, ar.mat[samp1,-1])
      yphi = ar.mat1[,1]
      Xphi = cbind(1, ar.mat1[,-1])
      phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
      yc.hat = Xc %*% phi.hat
      cv.error = c(cv.error, sum((yc.hat - yc)^2)/(nrow(ar.mat) - lphi))
    }
    opt.phi = c(opt.phi, mean(cv.error))
  }
  best.IOD = rbind(best.IOD, c(min(opt.phi), which(opt.phi == min(opt.phi))))
}

SOI.list = list()
for(k in 1:nrow(best.SOI)){
  lag = k
  lphi = best.SOI[k,2]
  ar.mat = NULL
  for(i in (lphi + lag):nrow(SOI1)){
    ar.mat = rbind(ar.mat, c(SOI1[i,2], SOI1[(i - lag):(i - lag - lphi + 1),2]))
  }
  yphi = ar.mat[,1]
  Xphi = cbind(1, ar.mat[,-1])
  phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
  y.hat = Xphi %*% phi.hat
  SOI.list[[k]] = y.hat
}

SOI.list = list()
for(k in 3:3){
  lag = k
  lphi = best.SOI[k,2]
  ar.mat = NULL
  for(i in (lphi + lag):nrow(SOI1)){
    ar.mat = rbind(ar.mat, c(SOI1[i,2], SOI1[(i - lag):(i - lag - lphi + 1),2]))
  }
  yphi = ar.mat[,1]
  Xphi = cbind(1, ar.mat[,-1])
  phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
  y.hat = Xphi %*% phi.hat
  SOI.list[[k]] = y.hat
}

f = 0
Sep.SOI.hat = t(c(1, SOI1[(nrow(SOI1) - f):(nrow(SOI1) - 4 - f), 2])) %*% phi.hat


IOD.list = list()
for(k in 3:3){
  lag = k
  lphi = best.IOD[k,2]
  ar.mat = NULL
  for(i in (lphi + lag):nrow(IOD1)){
    ar.mat = rbind(ar.mat, c(IOD1[i,2], IOD1[(i - lag):(i - lag - lphi + 1),2]))
  }
  yphi = ar.mat[,1]
  Xphi = cbind(1, ar.mat[,-1])
  phi.hat = solve(t(Xphi) %*% Xphi) %*% t(Xphi) %*% yphi
  y.hat = Xphi %*% phi.hat
  IOD.list[[k]] = y.hat
}

f = 1
Sep.IOD.hat = t(c(1, IOD1[(nrow(IOD1) - f):(nrow(IOD1) - 8 - f), 2])) %*% phi.hat
Sep.IOD.hat
tail(y.hat)

######## New Forecast
G <- auto_basis(data = precip3[[1]][Aus[m.Aus],c(2,1)] %>% # Take Tmax
                  SpatialPoints(), # To sp obj
                nres = 1, # One resolution
                type = "Gaussian") 

S <- eval_basis(basis = G, # basis functions
                s = precip3[[1]][Aus[m.Aus],c(2,1)] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names

X.ti1 = cbind(rep(1, length(m.Aus)), precip3[[1]][Aus[m.Aus],1], precip3[[1]][Aus[m.Aus],2], rowMeans(sqrt(Aus.all[m.Aus,mi1])))
X.ti1 = cbind(X.ti1, S)
X.list1 = list()
l = 1
for(i in T:T){
  X.list1[[l]] = cbind(X.ti1, cor.SOI * Sep.SOI.hat[1,1], cor.IOD * Sep.IOD.hat[1,1], cor.SOI * Sep.SOI.hat[1,1] * cor.IOD * Sep.IOD.hat[1,1])
}

k.start = 2
q = k.start + 8

Y.list1 = list()
for(i in T:T){
  a = list()
  l = 1
  for(j in k.start:q){
    a[[l]] = sqrt(M1.all[m.World, 478 - j] )
    l = l + 1
  }
  Y.list1[[1]] = a
}

Y.hat1 = list()
for(i in 1:1){
  Y.hat1[[i]] = X.list1[[i]] %*% beta.hat + Lmult(phi.hat, W, Y.list1[[i]]) 
}
max(Y.hat1[[1]])

d1 = data.frame(Est = Y.hat1[[1]] , Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw1 = idw(formula = Est ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred^2), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(min(c(idw1$var1.pred^2)), max(idw1$var1.pred^2))) +
  theme_bw() + labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude", title = "October 2020 Two Month Lead Precipitation Forecast")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) + xlim(110,155) + ylim(-45,-9)
g1


Y.error = matrix(0, nrow = length(Y.hat), ncol = length(Y.hat[[1]]))
for(i in 1:length(Y.hat)){
  Y.error[i,] = (Y.hat[[i]] - Y[[i]])^2
}

Sigma = colSums(Y.error)/(nrow(Y.error) - 1)

rowMedian = function(X){
  apply(X, 1, median)
}

rm = rowMedian(sqrt(Aus.all[m.Aus,mi1]))
rm1 = rowMeans(sqrt(Aus.all[m.Aus,mi1]))

p1 = rep(0, length(Y.hat1[[1]]))
for(i in 1:length(Y.hat1[[1]])){
  p1[i] = 1 - pnorm(rm1[i] , mean = Y.hat1[[1]][i], sd = sqrt(Sigma[i]))
}


d1 = data.frame(Est = p1 , Lon = X.ti[,3], Lat = X.ti[,2])
pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
idw2 = idw(formula = Est ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 5)

g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(d1, mapping = aes(x = Lon, y = Lat, colour = p1), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9),
                         limits = c(0, 1)) +
  theme_bw() + labs(color = "Probaility             ", x = "Longitude", y = "Latitude", title = "Chance of Exceeding September Mean")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))

g2

grid.arrange(g1, g2 + labs(y = " "), nrow = 1)


Sigma = matrix(0, nrow = length(Y[[1]]), ncol = length(Y[[1]]))
for(i in 1:nrow(sqrt(Aus.all[m.Aus,mi1]))){
  for(j in 1:nrow(sqrt(Aus.all[m.Aus,mi1]))){
    Sigma[i,j] = cov(sqrt(Aus.all[m.Aus,mi1])[i,], sqrt(Aus.all[m.Aus,mi1])[j,])
  }
}

det(Sigma) == 0
eigen(Sigma)


x1 = c(33.8, 32.2, 30.7, 35.4, 31, 30.3, 26.8, 33.2, 27.8, 27.2)
sqrt((1/(length(x1) - 1)) * sum((x1 - mean(x1))^2))
sd(x1)

install.packages("binom")
library(binom) 
p<- seq(0, 1, 0.001)
coverage.wald    <- binom.coverage(p, 250, method = "asymptotic")$coverage
coverage.agresti <- binom.coverage(p, 250, method = "agresti-coull")$coverage
plot( p, coverage.wald, type = "l", ylab = "coverage")
lines(p, coverage.agresti, col = "blue", lwd = 2)
abline(h = 0.95, col = "red")# desired coverage
