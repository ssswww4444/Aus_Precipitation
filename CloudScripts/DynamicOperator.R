library("ggplot2")
library("STRbook")

library("expm")
library("Matrix")

data("SSTlandmask", package = "STRbook")
data("SSTlonlat", package = "STRbook")
data("SSTdata", package = "STRbook")
delete_rows <- which(SSTlandmask == 1) # remove land values
SST_Oct97 <- SSTdata[-delete_rows, 334] # save Oct 1997 SSTs
SSTdata <- SSTdata[-delete_rows, 1:328] # until April 1997
SSTlonlat$mask <- SSTlandmask # assign mask to df
Z <- t(SSTdata) # data matrix
spat_mean <- apply(SSTdata, 1, mean) # spatial mean
nT <- ncol(SSTdata) # no. of time points
Zspat_detrend <- Z - outer(rep(1, nT), # detrend data
                           spat_mean)
Zt <- 1/sqrt(nT-1)*Zspat_detrend # normalize
E <- svd(Zt) # SVD
n <- 10
TS <- Zspat_detrend %*% E$v[, 1:n]
summary(colMeans(TS))
tau <- 6
nT <- nrow(TS)
TStplustau <- TS[-(1:tau), ] # TS with first tau time pts removed
TSt <- TS[-((nT-(tau - 1)):nT), ] # TS with last tau time pts removed

Cov0 <- crossprod(TS)/nT
Covtau <- crossprod(TStplustau,TSt)/(nT - tau)

C0inv <- solve(Cov0)
Mest <- Covtau %*% C0inv
Ceta <- Cov0 - Covtau %*% C0inv %*% t(Covtau)

par(mfrow = c(1,1))
image(Mest)
image(Ceta)

SSTlonlat$pred <- NA
alpha_forecast <- Mest %*% TS[328, ]

idx <- which(SSTlonlat$mask == 0)
SSTlonlat$curr[idx] <- as.numeric(E$v[, 1:n] %*% TS[328, ] +
                                    spat_mean)
SSTlonlat$pred[idx] <- as.numeric(E$v[, 1:n] %*% alpha_forecast +
                                    spat_mean)

SSTlonlat$obs1[idx] <- SSTdata[, 328]
SSTlonlat$obs2[idx] <- SST_Oct97

C <- Mest %*% Cov0 %*% t(Mest) + Ceta

SSTlonlat$predse[idx] <-
  sqrt(diag(E$v[, 1:n] %*% C %*% t(E$v[, 1:n])))

DSTM_Results <- DSTM_EM(Z = SSTdata,
                        Cov0 = Cov0,
                        muinit = matrix(0, n, 1),
                        M = Mest,
                        Ceta = Ceta,
                        sigma2_eps = 0.1,
                        H = H <- E$v[, 1:n],
                        itermax = 10,
                        tol = 1)

par(mfrow = c(1,3))
for(i in 1:3) {
  plot(DSTM_Results$alpha_smooth[i, ], type = 'l',
       xlab = "t", ylab = bquote(alpha[.(i)]))
  lines(TS[, i], lty = 'dashed', col = 'red')
}

image(as(DSTM_Results$Mest, "Matrix"))
image(as(DSTM_Results$Mest %^% 6, "Matrix"))
image(as(Mest, "Matrix"))

alpha <- DSTM_Results$alpha_smooth[, nT]
P <- DSTM_Results$Cov0
for(t in 1:tau) {
  alpha <- DSTM_Results$Mest %*% alpha
  P <- DSTM_Results$Mest %*% P %*% t(DSTM_Results$Mest) + DSTM_Results$Ceta
}
par(mfrow = c(1,2))
image(P)  
image(C)   

data("SSTlandmask", package = "STRbook")
data("SSTlonlat", package = "STRbook")
data("SSTdata", package = "STRbook")

delete_rows <- which(SSTlandmask == 1)
SSTdata <- SSTdata[-delete_rows, 1:396]

Z <- t(SSTdata)
dim(Z)

spat_mean <- apply(SSTdata, 1, mean)
nT <- ncol(SSTdata)
## Then subtract and standardize:
Zspat_detrend <- Z - outer(rep(1, nT), spat_mean)
Zt <- 1/sqrt(nT - 1)*Zspat_detrend

E <- svd(Zt)

V <- E$v
colnames(E$v) <- paste0("EOF", 1:ncol(SSTdata)) # label columns
EOFs <- cbind(SSTlonlat[-delete_rows, ], E$v)
head(EOFs[, 1:6])

TS <- data.frame(E$u) %>% # convert U to data frame
  mutate(t = 1:nrow(E$u)) %>% # add a time field
  gather(EOF, PC, -t)

TS$nPC <- TS$PC * sqrt(nT-1)

ggplot(EOFs) + geom_tile(aes(x = lon, y = lat, fill = EOF1)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")

ggplot(SSTlonlat) + geom_tile(aes(x = lon, y = lat, fill = pred)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")


cumsum(apply(E$v, 2, var))/sum(apply(E$v, 2, var))

TS = Zspat_detrend %*% E$v
(cumsum(apply(TS, 2, var))/sum(apply(TS, 2, var)))[15]



library("ggplot2")
library("STRbook")

library("expm")
library("Matrix")

data("SSTlandmask", package = "STRbook")
data("SSTlonlat", package = "STRbook")
data("SSTdata", package = "STRbook")
delete_rows <- which(SSTlandmask == 1) # remove land values
SST_Oct97 <- SSTdata[-delete_rows, 329] # save Oct 1997 SSTs
SSTdata <- SSTdata[-delete_rows, 1:328] # until April 1997
SSTlonlat$mask <- SSTlandmask # assign mask to df
Z <- t(SSTdata) # data matrix
spat_mean <- apply(SSTdata, 1, mean) # spatial mean
nT <- ncol(SSTdata) # no. of time points
Zspat_detrend <- Z - outer(rep(1, nT), # detrend data
                           spat_mean)
Zt <- 1/sqrt(nT-1)*Zspat_detrend # normalize
E <- svd(Zt) # SVD
plot(cumsum(apply(Zspat_detrend %*% E$v, 2, var))/sum(apply(Zspat_detrend %*% E$v, 2, var)), type = "l")
n <- 15
TS <- Zspat_detrend %*% E$v[, 1:n]
summary(colMeans(TS))
tau <- 1
nT <- nrow(TS)
TStplustau <- TS[-(1:tau), ] # TS with first tau time pts removed
TSt <- TS[-((nT-(tau - 1)):nT), ] # TS with last tau time pts removed

Cov0 <- crossprod(TS)/nT
Covtau <- crossprod(TStplustau,TSt)/(nT - tau)

C0inv <- solve(Cov0)
Mest <- Covtau %*% C0inv
Ceta <- Cov0 - Covtau %*% C0inv %*% t(Covtau)

par(mfrow = c(1,1))
image(Mest)
image(Ceta)

SSTlonlat$pred <- NA
alpha_forecast <- Mest %*% TS[328, ]

idx <- which(SSTlonlat$mask == 0)
SSTlonlat$curr[idx] <- as.numeric(E$v[, 1:n] %*% TS[328, ] +
                                    spat_mean)
SSTlonlat$pred[idx] <- as.numeric(E$v[, 1:n] %*% alpha_forecast +
                                    spat_mean)

SSTlonlat$obs1[idx] <- SSTdata[, 328]
SSTlonlat$obs2[idx] <- SST_Oct97

C <- Mest %*% Cov0 %*% t(Mest) + Ceta

SSTlonlat$predse[idx] <-
  sqrt(diag(E$v[, 1:n] %*% C %*% t(E$v[, 1:n])))

ggplot(SSTlonlat) + geom_tile(aes(x = lon, y = lat, fill = obs2)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")


DSTM_Results <- DSTM_EM(Z = SSTdata,
                        Cov0 = Cov0,
                        muinit = matrix(0, n, 1),
                        M = Mest,
                        Ceta = Ceta,
                        sigma2_eps = 0.1,
                        H = H <- E$v[, 1:n],
                        itermax = 10,
                        tol = 1)

par(mfrow = c(1,3))
for(i in 1:3) {
  plot(DSTM_Results$alpha_smooth[i, ], type = 'l',
       xlab = "t", ylab = bquote(alpha[.(i)]))
  lines(TS[, i], lty = 'dashed', col = 'red')
}

image(as(DSTM_Results$Mest, "Matrix"))
image(as(DSTM_Results$Mest %^% 6, "Matrix"))
image(as(Mest, "Matrix"))

alpha <- DSTM_Results$alpha_smooth[, nT]
P <- DSTM_Results$Cov0
for(t in 1:10000) {
  alpha <- DSTM_Results$Mest %*% alpha
  P <- DSTM_Results$Mest %*% P %*% t(DSTM_Results$Mest) + DSTM_Results$Ceta
}


ST1 = cbind(SSTlonlat[SSTlonlat$mask == 0,1:2], E$v[,1:n] %*% alpha + spat_mean)
colnames(ST1) = c("Lon", "Lat", "pred")
ST1 = as.data.frame(ST1)

ggplot(ST1) + geom_tile(aes(x = Lon, y = Lat, fill = pred)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")

ggplot(SSTlonlat) + geom_tile(aes(x = lon, y = lat, fill = obs2)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")



library("dplyr")
library("FRK")
library("ggplot2")
library("gstat")
library("RColorBrewer")
library("sp")
library("spacetime")
library("STRbook")
library("tidyr")

library("FRK")

data("STObj3", package = "STRbook") # load STObj3
STObj4 <- STObj3[, "1993-07-01::1993-07-31"] # subset time
STObj5 <- as(STObj4[, -14], "STIDF") # omit t = 14
STObj5 <- subset(STObj5, !is.na(STObj5$z)) # remove NAs
STObj6 = as.data.frame(STObj5)

BAUs <- auto_BAUs(manifold = STplane(), # ST field on the plane
                  type = "grid", # gridded (not "hex")
                  data = STObj5, # data
                  cellsize = c(1, 0.75, 1), # BAU cell size
                  convex = -0.12, # hull extension
                  tunit = "days") # time unit is "days"
par(mfrow = c(1,1))
plot(as(BAUs[, 1], "SpatialPixels")) # plot pixel BAUs
plot(SpatialPoints(STObj5),
     add = TRUE, col = "red") # plot data points
BAUs_hex <- auto_BAUs(manifold = STplane(), # model on the plane
                      type = "hex", # hex (not "grid")
                      data = STObj5, # data
                      cellsize = c(1, 0.75, 1), # BAU cell size
                      nonconvex_hull = FALSE, # convex hull
                      tunit = "days") # time unit is "days"
plot(as(BAUs_hex[, 1], "SpatialPolygons"))

G_spatial <- auto_basis(manifold = plane(), # fns on plane
                        data = as(STObj5, "Spatial"), # project
                        nres = 2, # 2 res.
                        type = "bisquare", # bisquare.
                        regular = 0) # irregular
t_grid <- matrix(seq(1, 31, length = 20))

G_temporal <- local_basis(manifold = real_line(), # fns on R1
                          type = "bisquare", # bisquare
                          loc = t_grid, # centroids
                          scale = rep(2, 20)) # aperture par.
G <- TensorP(G_spatial, G_temporal) #

S5 = eval_basis(basis = G, s = STObj6[, c("lon", "lat", "z")] %>% as.matrix())

class(STObj5)
library("leaps")
library("lmtest")
library("nlme")

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
                type = "Gaussian") # Gaussian BFs

S <- eval_basis(basis = G, # basis functions
                s = Tmax[,c("lon","lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names



data("SSTlandmask", package = "STRbook")
data("SSTlonlat", package = "STRbook")
data("SSTdata", package = "STRbook")
delete_rows <- which(SSTlandmask == 1) # remove land values
SST_Oct97 <- SSTdata[-delete_rows, 329] # save Oct 1997 SSTs
SSTdata <- SSTdata[-delete_rows, 1:328] # until April 1997
SSTlonlat$mask <- SSTlandmask # assign mask to df

SSTvec = NULL
for(i in 1:ncol(SSTdata)){
  SSTvec = c(SSTvec, SSTdata[,i])
  if(i %% 100 == 0){
    print(i)
  }
}

SSTdata1 = cbind(rep(SSTlonlat$lon[-delete_rows], ncol(SSTdata)), 
                 rep(SSTlonlat$lat[-delete_rows], ncol(SSTdata)), SSTvec,
                 c(rep(rep(1:12, each = nrow(SSTdata)), 27), rep(1:4, each = nrow(SSTdata))),
                 rep(1:ncol(SSTdata), each = nrow(SSTdata)),
                 rep(rev(SOI[580:(580 - ncol(SSTdata) + 1),2]), each = nrow(SSTdata)),
                 rep(rev(IOD.Mat[554:(554 - ncol(SSTdata) + 1),2]), each = nrow(SSTdata)),
                 rep(rev(ONI.Mat[592:(592 - ncol(SSTdata) + 1),2]), each = nrow(SSTdata)))
colnames(SSTdata1) = c("lon", "lat", "SST", "z", "z1", "SOI", "IOD", "ONI")
y = rep(0, nrow(SSTdata1))
for(i in 1:nrow(SSTdata1)){
  if(SSTdata1[i,4] == 1){
    y[i] = "Jan"
  }else if(SSTdata1[i,4] == 2){
    y[i] = "Feb"
  }else if(SSTdata1[i,4] == 3){
    y[i] = "Mar"
  }else if(SSTdata1[i,4] == 4){
    y[i] = "Apr"
  }else if(SSTdata1[i,4] == 5){
    y[i] = "May"
  }else if(SSTdata1[i,4] == 6){
    y[i] = "June"
  }else if(SSTdata1[i,4] == 7){
    y[i] = "July"
  }else if(SSTdata1[i,4] == 8){
    y[i] = "Aug"
  }else if(SSTdata1[i,4] == 9){
    y[i] = "Sept"
  }else if(SSTdata1[i,4] == 10){
    y[i] = "Oct"
  }else if(SSTdata1[i,4] == 11){
    y[i] = "Nov"
  }else if(SSTdata1[i,4] == 12){
    y[i] = "Dec"
  }
}

SSTdata2 = data.frame(lon = SSTdata1[,1], lat = SSTdata1[,2], SST = SSTdata1[,3], month = y, z1 = SSTdata1[,5], SOI = SSTdata[,6], IOD = SSTdata[,7], ONI = SSTdata[,8])


G_spatial <- auto_basis(manifold = plane(), # fns on plane
                        data = SSTdata1[, c("lon", "lat")] %>% SpatialPoints(), # project
                        nres = 1, # 2 res.
                        type = "bisquare", # bisquare.
                        regular = 0) # irregular
t_grid <- matrix(seq(1, 328, length = 50))

G_temporal <- local_basis(manifold = real_line(), # fns on R1
                          type = "bisquare", # bisquare
                          loc = t_grid, # centroids
                          scale = rep(2, 50)) # aperture par.
G <- TensorP(G_spatial, G_temporal) #
S1 = eval_basis(basis = G_temporal, s = SSTdata1[,c("z")] %>% as.matrix()) %>% as.matrix()

S <- eval_basis(basis = G_spatial, # basis functions
                s = SSTdata1[,c("lon","lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names

SSTdata2 = as.data.frame(cbind(SSTdata2, S))
SST_lm = lm(SST ~ (lon + lat + month + z1 + SOI + ONI + IOD)^3 + ., data = SSTdata2)
summary(SST_lm)

Z <- t(SSTdata) # data matrix
spat_mean <- apply(SSTdata, 1, mean) # spatial mean
nT <- ncol(SSTdata) # no. of time points
Zspat_detrend <- Z - outer(rep(1, nT), # detrend data
                           spat_mean)
Zt <- 1/sqrt(nT-1)*Zspat_detrend # normalize
E <- svd(Zt) # SVD
n <- 10

TS <- Zspat_detrend %*% E$v[, 1:n]
summary(colMeans(TS))
tau <- 1
nT <- nrow(TS)
TStplustau <- TS[-(1:tau), ] # TS with first tau time pts removed
TSt <- TS[-((nT-(tau - 1)):nT), ] # TS with last tau time pts removed

Cov0 <- crossprod(TS)/nT
Covtau <- crossprod(TStplustau,TSt)/(nT - tau)

C0inv <- solve(Cov0)
Mest <- Covtau %*% C0inv
Ceta <- Cov0 - Covtau %*% C0inv %*% t(Covtau)

par(mfrow = c(1,1))
image(Mest)
image(Ceta)
SSTlonlat1 = SSTlonlat[-delete_rows,]
SSTlonlat1$pred <- NA
alpha_forecast <- Mest %*% TS[328, ]

idx <- which(SSTlonlat$mask == 0)
SSTlonlat1$curr <- as.numeric(E$v[, 1:n] %*% TS[328, ] +
                                    spat_mean)
SSTlonlat1$pred <- as.numeric(E$v[, 1:n] %*% alpha_forecast +
                                    spat_mean)

SSTlonlat1$obs1 <- SSTdata[, 328]
SSTlonlat1$obs2 <- SST_Oct97

C <- Mest %*% Cov0 %*% t(Mest) + Ceta

SSTlonlat1$predse <- sqrt(diag(E$v[, 1:n] %*% C %*% t(E$v[, 1:n])))

ggplot(SSTlonlat1) + geom_tile(aes(x = lon, y = lat, fill = obs2)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")


DSTM_Results <- DSTM_EM(Z = SSTdata,
                        Cov0 = Cov0,
                        muinit = matrix(0, n, 1),
                        M = Mest,
                        Ceta = Ceta,
                        sigma2_eps = 0.1,
                        H = H <- E$v[, 1:n],
                        itermax = 10,
                        tol = 1)

par(mfrow = c(1,3))
for(i in 1:3) {
  plot(DSTM_Results$alpha_smooth[i, ], type = 'l',
       xlab = "t", ylab = bquote(alpha[.(i)]))
  lines(TS[, i], lty = 'dashed', col = 'red')
}

image(as(DSTM_Results$Mest, "Matrix"))
image(as(DSTM_Results$Mest %^% 6, "Matrix"))
image(as(Mest, "Matrix"))

alpha <- DSTM_Results$alpha_smooth[, nT]
P <- DSTM_Results$Cov0
for(t in 1:tau) {
  alpha <- DSTM_Results$Mest %*% alpha
  P <- DSTM_Results$Mest %*% P %*% t(DSTM_Results$Mest) + DSTM_Results$Ceta
}


ndata = data.frame(lon = SSTlonlat[-delete_rows,1], lat = SSTlonlat[-delete_rows,2],
                   month = rep("Oct", nrow(SSTdata)), z1 = rep(229, nrow(SSTdata)),
                   SOI = rep(SOI[581,2], nrow(SSTdata)), IOD = rep(IOD.Mat[551,2], nrow(SSTdata)), ONI = rep(ONI.Mat[593,2], nrow(SSTdata)))
ndata = cbind(ndata, S[1:nrow(SSTdata),])
p1 = predict(SST_lm, newdata = ndata)
ST1 = cbind(SSTlonlat1[,c("lon", "lat")], E$v[,1:n] %*% alpha + p1)
ST1 = cbind(ST1, SST_Oct97)
colnames(ST1) = c("Lon", "Lat", "pred", "obs")
ST1 = as.data.frame(ST1)

ggplot(ST1) + geom_tile(aes(x = Lon, y = Lat, fill = pred)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")

ggplot(ST1) + geom_tile(aes(x = Lon, y = Lat, fill = obs)) +
  fill_scale(name = "degC") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")

data("SSTdata", package = "STRbook")
SSTdata[-delete_rows,329]



#Rain data
M1 = matrix(0, nrow = nrow(Austra[[1]]), ncol = length(Austra))
for(i in 1:length(Austra)){
  M1[,i] = Austra[[i]][,3]
}
M2 = M1[, length(Austra)]
M1 = (M1[, - length(Austra)])^(1/2)

Rainlonlat = Austra[[1]][,2:1]
Z = t(M1)
spat_mean = apply(M1, 1, mean)
nT = ncol(M1)
Zspat_detrend = Z - outer(rep(1, nT), spat_mean)
Zt = 1/sqrt(nT - 1)*Zspat_detrend
E = svd(Zt)

n = 10

TS = Zspat_detrend %*% E$v[, 1:n]
tau = 1
nT = nrow(TS)
TStplustau = TS[-(1:tau),]
TSt = TS[-((nT - 0):nT),]

Cov0 = crossprod(TS)/nT
Covtau = crossprod(TStplustau, TSt)/(nT - tau)

C0inv = solve(Cov0)
Mest = Covtau %*% C0inv
Ceta = Cov0 - Covtau %*% C0inv %*% t(Covtau)

par(mfrow = c(1,1))
image(Mest)
image(Ceta)


Rainlonlat$pred = NA
alpha_forecast = Mest %*% TS[230,]

Rainlonlat$curr = as.numeric(E$v[, 1:n] %*% TS[230,] + spat_mean)
Rainlonlat$pred = as.numeric(E$v[, 1:n] %*% alpha_forecast + spat_mean)

Rainlonlat$obs1 = M1[,230]
Rainlonlat$obs2 = M2  

C = Mest %*% Cov0 %*% t(Mest) + Ceta  

Rainlonlat$predse = sqrt(diag(E$v[,1:n] %*% C %*% t(E$v[, 1:n])))

ggplot(Rainlonlat) + geom_tile(aes(x = Lon, y = Lat, fill = pred - obs2)) +
  fill_scale(name = "mm") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")

Rainvec = NULL
for(i in seq(3, 231 - 12, 12)){
  Rainvec = c(Rainvec, M1[,i])
  if(i %% 100 == 0){
    print(i)
  }
}

Raindata2 = data.frame(Rain = Rainvec, SOI = rep(SOI[seq(593, 820, 12),2], each = nrow(M1)), 
                       ONI = rep(ONI.Mat[seq(605, 830, 12),2], each = nrow(M1)), IOD = rep(IOD.Mat[seq(557, 780, 12),2], each = nrow(M1)),
                       Lon = rep(Rainlonlat[,1], length(seq(605, 830, 12))), Lat = rep(Rainlonlat[,2], length(seq(605, 830, 12))))

G_spatial <- auto_basis(manifold = plane(), # fns on plane
                        data = Rainlonlat[, c("Lon", "Lat")] %>% SpatialPoints(), # project
                        nres = 1, # 2 res.
                        type = "bisquare", # bisquare.
                        regular = 0) # irregular

S <- eval_basis(basis = G_spatial, # basis functions
                s = Rainlonlat[,c("Lon","Lat")] %>% # spat locations
                  as.matrix()) %>% # conv. to matrix
  as.matrix() # results as matrix
colnames(S) <- paste0("B", 1:ncol(S)) # assign column names

Raindata2 = as.data.frame(cbind(Raindata2, S))

Rain_lm = lm(Rain ~ (Lon + Lat)^2 + (SOI + ONI + IOD)^3 + ., data = Raindata2)
summary(Rain_lm)

ndata = data.frame(Lon = Rainlonlat[,1], Lat = Rainlonlat[,2],
                   SOI = rep(SOI[821,2], nrow(M1)), IOD = rep(0.47, nrow(M1)), ONI = rep(ONI.Mat[833,2], nrow(M1)))
ndata = cbind(ndata, S[1:nrow(M1),])
p1 = predict(Rain_lm, newdata = ndata)
Rainlonlat$pred = as.numeric(E$v[, 1:n] %*% alpha_forecast + spat_mean)^2

ggplot(Rainlonlat) + geom_tile(aes(x = Lon, y = Lat, fill = predse)) +
  fill_scale(name = "mm") + theme_bw() +
  xlab("Longitude (deg)") + ylab("Latitude (deg)")


DSTM_Results = DSTM_EM(Z = M1, Cov0 = Cov0, muinit = matrix(0,n,1), M = Mest, Ceta = Ceta, sigma2_eps = 0.1, H = H <- E$v[, 1:n], itermax = 10, tol = 1)
 