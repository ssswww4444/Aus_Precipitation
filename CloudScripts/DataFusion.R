NewLoc = NULL
for(i in 1:nrow(NOAA.aus[[1]])){
  d1 = distm(NOAA.aus[[1]][i,2:1], BOMFC[,2:1])
  if(min(d1) > 78000){
    NewLoc = c(NewLoc, i)
  }
}

nrow(BOMFC) + length(NewLoc)


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

G1 = list()
G1[[1]] = X[[1]]

EDQ.BOM = MEDQ(G1, p = seq(0, 1, length.out = 10), scale = FALSE, weight = FALSE)

dd1 = cbind(EDQ.BOM, seq(0, 1, length.out = 10))
v1 = NULL
for(i in unique(EDQ.BOM)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

All.locations = matrix(0, ncol = 2, nrow = length(used))
k = 1
for(i in used){
  All.locations[k,] = c(All.Gauge[[i]]$Lat[1], All.Gauge[[i]]$Lon[1])
  k = k + 1
}


BOM.MEDQdf = data.frame(longitude = All.locations[unique(EDQ.BOM),2], latitude = All.locations[unique(EDQ.BOM),1], Quantile = v1)

g.BOM = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(BOM.MEDQdf, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "BOM EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.BOM


write.csv(BOM.MEDQdf, file = "BOMEDQdf", row.names = FALSE)


NB.loc = rep(0, nrow(NOAA.aus[[1]]))
JB.loc = rep(0, nrow(NOAA.aus[[1]]))
for(i in 1:nrow(NOAA.aus[[1]])){
  d1 = abs(JAXA[[1]]$Longitude - NOAA.aus[[1]][i,2])
  d2 = abs(JAXA[[1]]$Latitude - NOAA.aus[[1]][i,1])
  d3 = d1 + d2
  O1 = order(d3)[1]
  JB.loc[i] = O1
}

NB.loc

X = list()
X[[1]] = matrix(0, nrow = length(JB.loc), ncol = 256:length(NOAA.aus))
X[[2]] = matrix(0, nrow = length(NB.loc), ncol = 256:length(NOAA.aus))
for(i in 1:length(256:length(NOAA.aus))){
  X[[1]][,i] = JAXA[[i]]$Precip[JB.loc]
  X[[2]][,i] = NOAA.aus[[255 + i]]$Precipitation
}

X1 = list()
X1[[1]] = t(apply(X[[1]], 1, cumsum))
X1[[2]] = t(apply(X[[2]], 1, cumsum))

NJ.location = NOAA.aus[[1]][,2:1]

NJ.MEDQ = MEDQ(X1, p = seq(0, 1, length.out = 1000), weight = FALSE, scale = TRUE)
length(unique(NJ.MEDQ))

w = 20
m2 = apply(NJ.MEDQ, 2, order)
p1 = seq(0, 1, length.out = 1000)
p2 = seq(0, 1, length.out = w)
ord1 = rep(0, length(p2))
for(j in 1:length(p2)){
  ord1[j] = order(abs(p2[j] - p1))[1]
}
if(length(unique(m2[1,])) > w){
  out = m2[1, ord1]
}else if(length(unique(m2[1,])) == w){
  out = m2[1, ]
}else{
  out = m2[1, ord1]
  k = 2
  while(sum(duplicated(out)) > 0){
    dup = which(duplicated(out))
    out[dup] = m2[k, ord1[dup]]
    k = k + 1
  }
}


Sat.locations = NJ.location

NJ.df = data.frame(Lon = Sat.locations[out, 1], Lat = Sat.locations[out, 2], Quantile = p2)

g.Sat = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(NJ.df, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Satellite MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.Sat


write.csv(NJ.df, file = "Satdf", row.names = FALSE)



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

All.MEDQ = MEDQ(X, p = seq(0, 1, length.out = 1000), weight = FALSE, scale = TRUE)

w = 20
m2 = apply(All.MEDQ, 2, order)
p1 = seq(0, 1, length.out = 1000)
p2 = seq(0, 1, length.out = w)
ord1 = rep(0, length(p2))
for(j in 1:length(p2)){
  ord1[j] = order(abs(p2[j] - p1))[1]
}
if(length(unique(m2[1,])) > w){
  out = m2[1, ord1]
}else if(length(unique(m2[1,])) == w){
  out = m2[1, ]
}else{
  out = m2[1, ord1]
  k = 2
  while(sum(duplicated(out)) > 0){
    dup = which(duplicated(out))
    out[dup] = m2[k, ord1[dup]]
    k = k + 1
  }
}

All.locations = matrix(0, ncol = 2, nrow = length(used))
k = 1
for(i in used){
  All.locations[k,] = c(All.Gauge[[i]]$Lat[1], All.Gauge[[i]]$Lon[1])
  k = k + 1
}


All.MEDQdf = data.frame(longitude = All.locations[out,2], latitude = All.locations[out,1], Quantile = p2)

g.All = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(All.MEDQdf, mapping = aes(x = longitude, y = latitude, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "All MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.All


write.csv(All.MEDQdf, file = "AllMEDQdf", row.names = FALSE)





##############
#Merge Data
##############



X.mat.2 = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    if(!is.null(All.Gauge[[i]]) & nrow(All.Gauge[[i]]) > 12){
      y1 = (All.Gauge[[i]]$Precipitation)^(1/2)
      x1 = (All.Gauge[[i]]$Jaxa)^(1/2)
      x2 = (All.Gauge[[i]]$Chirp)^(1/2)
      mod1 = rlm(y1 ~ x1 + x2, method = "M", maxit = 200)
      X.mat.2 = rbind(X.mat.2, c(mod1$coefficients[1], mod1$coefficients[2], mod1$coefficients[3], summary(mod1)$sigma, All.Gauge[[i]]$Lat[1],
                                 All.Gauge[[i]]$Lon[1], nrow(All.Gauge[[i]])))
    }
  }, error = function(e){})
}
colnames(X.mat.2) = c("Intercept", "Jaxa", "Chirp", "Sigma", "Lat", "Lon", "n")
X.mat.2 = as.data.frame(X.mat.2)
hist(X.mat.2[,1])
hist(X.mat.2[,2])
hist(X.mat.2[,3])
X.mat.2 = X.mat.2[X.mat.2$Intercept < 2 & X.mat.2$Intercept > -3 & X.mat.2$Jaxa > -0.25 & X.mat.2$Jaxa < 1 & X.mat.2$Chirp > 0 & X.mat.2$Chirp < 1.75 &
                    X.mat.2$Lon > 100 & X.mat.2$Lat < -5 & X.mat.2$n > 11,]



O0 = Opt.Weight(Intercept ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w0 = Weight.Matrix(locations = X.mat.2[,5:4], k = O0[1], alpha = O0[2], method = "IDW")


x.i = matrix(1, nrow = nrow(X.mat.2), ncol = 1)

SLM0 = SPE1(X.mat.2[,1], w0, x.i)
beta0.hat = x.i %*% SLM0$beta.hat + SLM0$lambda.hat * w0 %*% X.mat.2[,1]
plot(beta0.hat, X.mat.2[,1])
abline(0,1)

O1 = Opt.Weight(Jaxa ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w1 = Weight.Matrix(locations = X.mat.2[,5:4], k = O1[1], alpha = O1[2], method = "IDW")

SLM1 = SPE1(X.mat.2[,2], w1, x.i)
beta1.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w1 %*% X.mat.2[,2]
plot(beta1.hat, X.mat.2[,2])
abline(0,1)

O2 = Opt.Weight(Chirp ~ 1, data = X.mat.2, locations = X.mat.2[,5:4], method = "IDW")

w2 = Weight.Matrix(locations = X.mat.2[,5:4], k = O2[1], alpha = O2[2], method = "IDW")

SLM2 = SPE1(X.mat.2[,3], w2, x.i)
beta2.hat = x.i %*% SLM1$beta.hat + SLM1$lambda.hat * w2 %*% X.mat.2[,3]
plot(beta1.hat, X.mat.2[,3])
abline(0,1)

Sep0 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,6:5], Newlocations = NOAA.aus[[1]][,2:1], k = O0[1], alpha = O0[2], method = "IDW")
Sep1 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,6:5], Newlocations = NOAA.aus[[1]][,2:1], k = 9, alpha = 1.32, method = "IDW")
Sep2 = Sep.Weight.Matrix(Oldlocations = X.mat.2[,6:5], Newlocations = NOAA.aus[[1]][,2:1], k = O2[1], alpha = O2[2], method = "IDW")

beta0.new =  SLM0$beta.hat[1,1] + SLM0$lambda.hat * Sep0 %*% X.mat.2[,1]
beta1.new =  SLM1$beta.hat[1,1] + SLM1$lambda.hat * Sep1 %*% X.mat.2[,2]
beta2.new =  SLM2$beta.hat[1,1] + SLM2$lambda.hat * Sep2 %*% X.mat.2[,3]

X.tilde1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Intercept = beta0.new, JAXA = beta1.new,  NOAA = beta2.new)

y.fitted = beta0.new + beta1.new * Jan2017J$Precip[NJ.loc] + beta2.new * NOAA.aus[[457]]$Precipitation



New.Gauge = matrix(0, nrow = nrow(X.tilde1), ncol = length(JAXA.Monthly))

for(i in 1:ncol(New.Gauge)){
  New.Gauge[,i] = (X.tilde1$Intercept + X.tilde1$JAXA * sqrt(JAXA.Monthly[[i]]$Precip[NJ.loc]) + X.tilde1$NOAA * sqrt(NOAA.aus[[255 + i]]$Precipitation))^2
}


X.new = list()

X.new[[1]] = t(apply(New.Gauge, 1, cumsum))
X.new[[2]] = X1[[1]]
X.new[[3]] = X1[[2]]

NewMEDQ = MEDQ(X.new, p = seq(0, 1, length.out = 1000), scale = TRUE)

w = 20
m2 = apply(NewMEDQ, 2, order)
p1 = seq(0, 1, length.out = 1000)
p2 = seq(0, 1, length.out = w)
ord1 = rep(0, length(p2))
for(j in 1:length(p2)){
  ord1[j] = order(abs(p2[j] - p1))[1]
}
if(length(unique(m2[1,])) > w){
  out = m2[1, ord1]
}else if(length(unique(m2[1,])) == w){
  out = m2[1, ]
}else{
  out = m2[1, ord1]
  k = 2
  while(sum(duplicated(out)) > 0){
    dup = which(duplicated(out))
    out[dup] = m2[k, ord1[dup]]
    k = k + 1
  }
}



NewMEDQdf = data.frame(Lon = NOAA.aus[[1]]$Lon[out], Lat = NOAA.aus[[1]]$Lat[out], Quantile = p2)

g.New = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(NewMEDQdf, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Fused data + Satellite MEDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.New


MEDQBomFuseddf = data.frame(Lon = c(All.MEDQdf$longitude, NewMEDQdf$Lon), Lat = c(All.MEDQdf$latitude, NewMEDQdf$Lat),
                            Quantile = c(All.MEDQdf$Quantile, NewMEDQdf$Quantile), Type = rep(c("BOM + Satellite MEDQ", "Fused + Satellite MEDQ"), each = 20))

write.csv(MEDQBomFuseddf, file = "NewMEDQdf", row.names = FALSE)


grid.arrange(g.New, g.All, g.BOM, g.Sat, nrow = 2)


New.Gauge = matrix(0, nrow = nrow(X.tilde1), ncol = length(JAXA) - 4)

for(i in 1:ncol(New.Gauge)){
  New.Gauge[,i] = (X.tilde1$Intercept + X.tilde1$JAXA * sqrt(JAXA[[i]]$Precip[NJ.loc]) + X.tilde1$NOAA * sqrt(NOAA.aus[[255 + i]]$Precipitation))^2
}


X.merge = list()

X.merge[[1]] = t(apply(New.Gauge, 1, cumsum))


MergeMEDQ = MEDQ(X.merge, p = seq(0, 1, length.out = 300), scale = TRUE)
length(unique(MergeMEDQ))

dd1 = cbind(MergeMEDQ, seq(0, 1, length.out = 10))
v1 = NULL
for(i in unique(MergeMEDQ)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}



MergeMEDQdf = data.frame(Lon = NOAA.aus[[1]]$Lon[unique(MergeMEDQ)], Lat = NOAA.aus[[1]]$Lat[unique(MergeMEDQ)], Quantile = v1)

g.Merge = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MergeMEDQdf, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Merge EDQ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))
g.Merge


grid.arrange(g.Merge, g.BOM, nrow = 2)




Alldf = data.frame(Lon = c(BOM.MEDQdf$longitude, NJ.df$Lon, All.MEDQdf$longitude, MergeMEDQdf$Lon), 
                   Lat = c(BOM.MEDQdf$latitude, NJ.df$Lat, All.MEDQdf$latitude, MergeMEDQdf$Lat),
                   Quantile = c(BOM.MEDQdf$Quantile, NJ.df$Quantile, All.MEDQdf$Quantile, MergeMEDQdf$Quantile),
                   Type = rep(factor(c("BOM EDQ", "Satellite MEDQ", "BOM + Satellite MEDQ", "Fused EDQ"),
                                 levels = c("BOM EDQ", "Satellite MEDQ", "BOM + Satellite MEDQ", "Fused EDQ")), each = 10))


ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_wrap(~Type) +
  geom_point(Alldf, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13), strip.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))



w = c(10, 20, 32, 55, 90, 200)
MergeMEDQdf = NULL
for(i in 1:length(w)){
  MergeMEDQ = MEDQ(X.merge, p = seq(0, 1, length.out = w[i]), scale = TRUE)
  
  
  dd1 = cbind(MergeMEDQ, seq(0, 1, length.out = w[i]))
  v1 = NULL
  for(i in unique(MergeMEDQ)){
    v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
  }
  
  
  
  MergeMEDQdf = rbind(MergeMEDQdf, cbind(NOAA.aus[[1]]$Lon[unique(MergeMEDQ)], NOAA.aus[[1]]$Lat[unique(MergeMEDQ)], v1, length(unique(MergeMEDQ))))
  print(i)
}

colnames(MergeMEDQdf) = c("Lon", "Lat", "Quantile", "W")
MergeMEDQdf = as.data.frame(MergeMEDQdf)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + facet_wrap(~W) +
  geom_point(MergeMEDQdf, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13), strip.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))



write.csv(MergeMEDQdf, file = "MergeDF", row.names = FALSE)



m1 = MEDQ(X.new, p = seq(0, 1,length.out = 1000), scale = TRUE)


m2 = apply(m1, 2, order)

table(m2[1,])

w = 60
p1 = seq(0, 1, length.out = 1000)
p2 = seq(0, 1, length.out = w)
ord1 = rep(0, length(p2))
for(j in 1:length(p2)){
  ord1[j] = order(abs(p2[j] - p1))[1]
}
if(length(unique(m2[1,])) > w){
  out = m2[1, ord1]
}else if(length(unique(m2[1,])) == w){
  out = m2[1, ]
}else{
  out = m2[1, ord1]
  k = 2
  while(sum(duplicated(out)) > 0){
    dup = which(duplicated(out))
    out[dup] = m2[k, ord1[dup]]
    k = k + 1
  }
}


w1 = c(10)
MEDQdf = NULL
for(i in 1:length(w1)){
  w = w1[i]
  p1 = seq(0, 1, length.out = 1000)
  p2 = seq(0, 1, length.out = w)
  ord1 = rep(0, length(p2))
  for(j in 1:length(p2)){
    ord1[j] = order(abs(p2[j] - p1))[1]
  }
  if(length(unique(m2[1,])) > w){
    out = m2[1, ord1]
  }else if(length(unique(m2[1,])) == w){
    out = m2[1, ]
  }else{
    out = m2[1, ord1]
    k = 2
    while(sum(duplicated(out)) > 0){
      dup = which(duplicated(out))
      out[dup] = m2[k, ord1[dup]]
      k = k + 1
    }
  }
  MEDQdf = rbind(MEDQdf, cbind(NOAA.aus[[1]]$Lon[out], NOAA.aus[[1]]$Lat[out], p2, length(out)))
}

colnames(MEDQdf) = c("Lon", "Lat", "Quantile", "Type")
MEDQdf = as.data.frame(MEDQdf)

ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(MEDQdf, mapping = aes(x = Lon, y = Lat, colour = Quantile), size = 4) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13), strip.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))



which(MEDQdf$Lon < 135 & MEDQdf$Lon > 133 & MEDQdf$Lat > -35 & MEDQdf$Lat < -33)

MEDQdf[which(MEDQdf$Lon < 135 & MEDQdf$Lon > 133 & MEDQdf$Lat > -35 & MEDQdf$Lat < -33),3] = 1 - MEDQdf[which(MEDQdf$Lon < 135 & MEDQdf$Lon > 133 & MEDQdf$Lat > -35 & MEDQdf$Lat < -33),3]


write.csv(MEDQdf, file = "MEDQdf1", row.names = FALSE)




a1 = apply(d1, 1, min, na.rm = TRUE)





dim(New.Gauge)

NOAA.aus[[255 + 1]]

New.Gauge1 = New.Gauge


FusedDates = NULL
for(i in 1:length(JAXA.Monthly)){
  m1 = JAXA.Monthly[[i]]$Month[1]
  if(nchar(m1) == 1){
    m1 = paste0(0, m1)
  }
  FusedDates = c(FusedDates, paste0("D", JAXA.Monthly[[i]]$Year[1], ".", m1))
}


colnames(New.Gauge1) = FusedDates
New.Gauge1 = as.data.frame(New.Gauge1)

FusedLoc = NOAA.aus[[1]][,1:2]

write.csv(New.Gauge1, file = "FusedData", row.names = FALSE)
write.csv(FusedLoc, file = "Fused Locations", row.names = FALSE)






