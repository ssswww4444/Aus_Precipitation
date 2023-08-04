#Function that calculates the pth multivariate empirical dynamic quantile
#Inputs are X.list, p and method
#X.list is a list of nxT matrices for n time-series at T time intervals where each list entry is represents another dimension
#p is a point or a vector of quantiles to calculate, default is p = 0.5
#Method is determines which method of depth used. Can be Mahalanobis, Tukey, Liu or Oja.
#For dimensions greater than 2, Mahalanobis is by far the fastest
#Output is a point or vector row numbers for the pth quantile
#Can work for one dimensional time-series data
#Data set for each variable can be scaled in the function

X.list = X

MEDQ = function(X.list,p = 0.5, method = "Mahalanobis", scale = FALSE, weight = FALSE){
  d = length(X.list)
  if(!inherits(X.list,"list")){
    stop("X.list must be in the form of a list")
  }
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
    vec = vec * sign(vec[1,1])
    for(j in 1:nrow(depth3)){
      z1 = x1[zt,d]
      if(d > 1){
        for(k in 1:(d-1)){
          z1 = z1 - (vec[k,1]/(vec[d,1])) * (x1[j,k] - x1[zt,k])
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
  pb <- txtProgressBar(min = 1, max = nr * nc, style = 3)
  t1 = 1
  n.2 = matrix(0, nrow = nr, ncol = length(p))
  for(i in 1:nr){
    n.1 = 0
    for(j in 1:nc){
      setTxtProgressBar(pb, t1)
      t1 = t1 + 1
      n.1 = n.1 + p * (dist.1[[j]][,i] %*% (depth.dash[,j] > depth.dash[i,j]))[1,1] + (1 - p) * (dist.1[[j]][,i] %*% (1 - (depth.dash[,j] > depth.dash[i,j])))[1,1]
    }
    n.2[i,] = n.1
  }
  
  n.3 = apply(n.2, 2, order)[1,]
  n.3
  #n.2
}

#Example for 2 dimensional positively correlated data
A.r1 = NULL
A.r2 = NULL
for(i in 1:500){
  rn = rnorm(1)
  A.r1 = rbind(A.r1, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) + rn)
  A.r2 = rbind(A.r2, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) - 2 * rn + 10)
}

X.list = list()
X.list[[1]] = A.r1
X.list[[2]] = A.r2

p = c(0.05,0.5,0.95)
M = MEDQ(X.list = X.list, p = p, method = "Mahalanobis")
M1 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", weight = FALSE, scale = "TRUE")

par(mfrow = c(1,2))
ts.plot(t(X.list[[1]]))
lines(X.list[[1]][M1[1],], col = "green")
lines(X.list[[1]][M1[2],], col = "red")
lines(X.list[[1]][M1[3],], col = "blue")

ts.plot(t(X.list[[2]]))
lines(X.list[[2]][M1[1],], col = "green")
lines(X.list[[2]][M1[2],], col = "red")
lines(X.list[[2]][M1[3],], col = "blue")

D.depth = function(X.list){
  nr = nrow(X.list[[1]])
  nc = ncol(X.list[[1]])
  d = length(X.list)
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
          z1 = z1 - (vec[k,1]/abs(vec[d,1])) * (x1[j,k] - x1[zt,k])
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
  depth.dash
}

d1 = D.depth(X.list)



TS.1 = as.vector(t(A.r1))
TS.2 = as.vector(t(A.r2))
time1 = rep(1:ncol(A.r1), nrow(A.r1))
group1 = rep(1:nrow(A.r1), each = ncol(A.r1))

TS.df = data.frame(Time = time1, Ar1 = TS.1, Ar2 = TS.2, Group = group1)

TS.1.1 = as.vector(t(A.r1[M1,]))
TS.2.1 = as.vector(t(A.r2[M1,]))
time1.1 = rep(1:ncol(A.r1[M1,]), nrow(A.r1[M1,]))
group1.1 = rep(1:nrow(A.r1[M1,]), each = ncol(A.r1[M1,]))

TS.df.1 = data.frame(Time = time1.1, Ar1 = TS.1.1, Ar2 = TS.2.1, Group = group1.1, Value = rep(p, each = ncol(A.r1)))


g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 1") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value * 100), size = 1.5) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(1,0, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g2 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar2, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 2") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar2, colour = Value * 100), size = 1.5) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(1,0, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

grid.arrange(g1 + theme(legend.position = "none") + labs(x = " ", y = " "), g2 + theme(legend.position = "none") + labs(x = " ", y = " "), nrow = 1,
             bottom = textGrob("Time", gp=gpar(fontsize=16,font=8), vjust = 0, hjust = 0), 
             left = textGrob("Value", vjust = 1.5, gp=gpar(fontsize=16,font=8), hjust = 0.2, rot = 90))









d = length(X.list)
if(!inherits(X.list,"list")){
  stop("X.list must be in the form of a list")
}
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
nc = 1

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

sigma0 <- cov(my_sample)
sigma <- cov(my_sample %*% A)

EVV0=eigen(sigma0)
vec0=EVV0$vectors

EVV=eigen(sigma)
vec=EVV$vectors


plot(X.list[[1]][,1], X.list[[1]][,2], colour = depth.dash, cex = 1, pch = 19)
scatter2D(X.list[[1]][,1], X.list[[2]][,1], colvar = depth.dash, cex = 1, pch = 19)
scatter2D(X.list[[1]][,1], X.list[[2]][,1], colvar = depth3, cex = 1, pch = 19)

ggplot() + geom_point()


MEDQ = function(X.list,p = 0.5, method = "Mahalanobis", scale = TRUE){
  d = length(X.list)
  if(!inherits(X.list,"list")){
    stop("X.list must be in the form of a list")
  }
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
      a1 = angle(x1[j,] - x1[zt,], vec[1,])
        depth.dash[j,i] = (1 - a1/180) * (2 * depth3[zt,i] - depth3[j,i] ) + (a1/180) * depth3[j,i]
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
    print(k)
    n.2 = NULL
    for(i in 1:nr){
      n.1 = 0
      for(j in 1:nc){
        n.1 = n.1 + k * dist.1[[j]][,i] %*% (depth.dash[,j] > depth.dash[i,j]) + (1 - k) * dist.1[[j]][,i] %*% (1 - (depth.dash[,j] > depth.dash[i,j]))
      }
      n.2 = c(n.2, n.1)
    }
    n.3 = c(n.3, which(n.2 == min(n.2))[1])
  }
  n.3
}


dim(P1)
S.Ent.N = matrix(0, nrow = nrow(NOAA.all.aus), ncol = length(1:(ncol(NOAA.all.aus) - 12)))
for(i in 1:(ncol(NOAA.all.aus) - 12)){
  S.Ent.N[,i] = apply(NOAA.all.aus[,i:(i + 11)], 1, entropy1)
}


set.seed(1998)
samp1 = sort(sample(1:nrow(WORLD), 10^5))

A1 = NOAA.aus[[1]][,1:2]
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
a3 = over(A1, shape)

Aus1 = which(a3$ISO3 == "AUS")

NJ.loc = rep(0, length(Aus1))
for(i in 1:length(Aus1)){
  d1 = abs(JAXA[[1]]$Longitude - NOAA.aus[[1]]$Lon[Aus1[i]])
  d2 = abs(JAXA[[1]]$Latitude - NOAA.aus[[1]]$Lat[Aus1[i]])
  d3 = d1 + d2
  O1 = order(d3)[1]
  NJ.loc[i] = O1
  print(i)
}


JAXA.all = matrix(0, nrow = length(NJ.loc), ncol = length(JAXA))
for(i in 1:length(JAXA)){
  JAXA.all[, i] = JAXA[[i]]$Precip[NJ.loc] * dates4[i,3]
}

N2 = NOAA.all.aus[Aus1,]

NJ.list = list()

NJ.list[[1]] = N2[,(which(datesNOAA[,1] == 2000 & datesNOAA[,2] == 4)):(which(datesNOAA[,1] == 2021 & datesNOAA[,2] == 7))]
NJ.list[[2]] = JAXA.all[,1:(which(dates4[,1] == 2021 & dates4[,2] == 7))]

M.NJ = MEDQ(NJ.list, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

NJ.list2 = list()

NJ.list2[[1]] = t(apply(N2[,(which(datesNOAA[,1] == 2000 & datesNOAA[,2] == 4)):(which(datesNOAA[,1] == 2021 & datesNOAA[,2] == 7))], 1, cumsum))
NJ.list2[[2]] = t(apply(JAXA.all[,1:(which(dates4[,1] == 2021 & dates4[,2] == 7))], 1, cumsum))

M.NJ2 = MEDQ(NJ.list2, p = seq(0,1,0.001), weight = FALSE, scale = TRUE)


dd1 = cbind(M.NJ2, seq(0,1,0.001))
v1 = NULL
for(i in unique(M.NJ2)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df = data.frame(Lon = (JAXA[[1]]$Longitude[NJ.loc])[unique(M.NJ2)], Lat = (JAXA[[1]]$Latitude[NJ.loc])[unique(M.NJ2)], Quantile = v1)

dist1 = NULL
for(i in 1:nrow(MEDQ.df)){
  d3 = distm(MEDQ.df[i,1:2], G.df[,2:1], fun = distCosine)
  dist1 = c(dist1, sort(d3)[1])
}

MEDQ.df$Cur = ifelse(dist1 > 25000, "No Gauge", "Gauge")

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df, mapping = aes(x = Lon, y = Lat, colour = Quantile, shape = Cur), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "MEDQ Locations", shape = "Status")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


A1 = cbind(N1[unique(M1)[1],], J1[unique(M1)[1],])

colnames(A1) = c("NOAA", "JAXA")
A1 = as.data.frame(A1)

p.opt = VARselect(A1, lag.max = 10, type = "const", season = 12)

v1 = VAR(A1, p = 4)

matplot(fitted(v1) - A1, type = "l")

dd1 = cbind(M.NJ2, seq(0,1,0.001))
v1 = NULL
for(i in unique(M.NJ2)){
  v1 = c(v1, mean(dd1[which(dd1[,1] == i), 2]))
}


MEDQ.df = data.frame(Lon = (JAXA[[1]]$Longitude[NJ.loc])[unique(M.NJ2)], Lat = (JAXA[[1]]$Latitude[NJ.loc])[unique(M.NJ2)], Quantile = v1)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Entropy MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9))


VAR(N1[unique(M1),])

data(Canada)
VAR(Canada, p = 2, type = "none")
VAR(Canada, p = 2, type = "const")
VAR(Canada, p = 2, type = "trend")
VAR(Canada, p = 2, type = "both")


N2 = t(N1[unique(M1),])

colnames(N2) = paste0("Loc",1:ncol(N2))
N2 = as.data.frame(N2)

T1 = as.ts(N2, class = "mts")

V1 = VAR(T1, p = 1, type = "none")


library(gstat)
library(RColorBrewer)
load(system.file("data", "meuse.rda", package = "sp"))

# Create a SpatialPointsDataFrame Object from the data.frame
meuse.sp <- meuse  #Copy the data.  It's still a data.frame
coordinates(meuse.sp) <- ~x + y  # Now it's SpatialPointsDataFrame, with coordinates x and y
# Create a categorical variable and plot it
q <- quantile(meuse$zinc, seq(0.1, 0.9, 0.1))
# These are the actual values of the quantiles
q

meuse.i <- gstat(id = "zinc1", formula = I(zinc < q[1]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.1, set = list(order = 4, zero = 1e-05))
# The order=4 varible is set as per the instructions in the gstat manual.
# this tells gstat that each indicator is cumulative.  You can't be in the
# second category without also being in the first category.
meuse.i <- gstat(meuse.i, "zinc2", formula = I(zinc < q[2]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.2)
meuse.i <- gstat(meuse.i, "zinc3", formula = I(zinc < q[3]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.3)
meuse.i <- gstat(meuse.i, "zinc4", formula = I(zinc < q[4]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.4)
meuse.i <- gstat(meuse.i, "zinc5", formula = I(zinc < q[5]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.5)
meuse.i <- gstat(meuse.i, "zinc6", formula = I(zinc < q[6]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.6)
meuse.i <- gstat(meuse.i, "zinc7", formula = I(zinc < q[7]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.7)
meuse.i <- gstat(meuse.i, "zinc8", formula = I(zinc < q[8]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.8)
meuse.i <- gstat(meuse.i, "zinc9", formula = I(zinc < q[9]) ~ 1, data = meuse.sp, 
                 nmax = 7, beta = 0.9)

# Create a semivariogram model with range equal 1200, and 'dummy' partial
# sill and nugget of 1.  We will fit these later.  'One size fits all'
meuse.i <- gstat(meuse.i, model = vgm(1, "Sph", 1000, 1), fill.all = T)

# Estimate the empiricalvariogram of each indicator
x <- variogram(meuse.i)
plot(x)



NJ.df = data.frame(JAXA = as.vector(t(J1[unique(M1),])), NOAA = as.vector(t(N1[unique(M1),])))

NJ.df$Period = rep(1:ncol(J1), length(unique(M1)))

NJ.sp = data.frame(Lon = rep(MEDQ.df$Lon, each = ncol(J1)), Lat = rep(MEDQ.df$Lat, each = ncol(J1)))

coordinates(NJ.sp) = ~Lon + Lat

NJ.sp$JAXA = NJ.df$JAXA
NJ.sp$NOAA = NJ.df$NOAA
NJ.sp$Period = NJ.df$Period

NJ.i = gstat(id = "JAXA", formula = JAXA ~ Period, data = NJ.sp, nmax = 10)
NJ.i = gstat(NJ.i, "NOAA", formula = NOAA ~ Period, data = NJ.sp, nmax = 10)

NJ.i <- gstat(NJ.i, model = vgm(1, "Sph", 1000, 1), fill.all = T)

x = variogram(NJ.i, width = 0.75)
plot(x)

NJ.fit = fit.lmc(x, NJ.i)

plot(x, model = NJ.fit)


NJ.spgrid <- meuse.grid
coordinates(meuse.spgrid) <- ~x + y

zk <- predict(meuse.fit, newdata = meuse.spgrid, indicators = TRUE)




























