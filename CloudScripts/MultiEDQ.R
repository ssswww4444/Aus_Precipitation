A.r1 = NULL
A.r2 = NULL
A.r3 = NULL
A.r4 = NULL
for(i in 1:500){
  rn = rnorm(1)
  rw = rnorm(2)
  A.r1 = rbind(A.r1, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) + rn)
  A.r2 = rbind(A.r2, arima.sim(n = 200, list(ar = c(0.5, -0.3)),
                               sd = sqrt(sqrt(0.1796))) -  rn + 10)
}

X.list = list()
X.list[[1]] = A.r1
X.list[[2]] = A.r2

depth3 = NULL
for(i in 1:ncol(A.r1)){
  mu <- apply(cbind(A.r1[,i],A.r2[,i]),2,mean)
  sigma <- cov(cbind(A.r1[,i],A.r2[,i]))
  my_sample <- cbind(A.r1[,i], A.r2[,i])
  m_distance <- mahalanobis(my_sample, mu, sigma)
  m_depth <- 1/(1 + m_distance)
  depth3 = cbind(depth3, m_depth)
}
d = length(X.list)

D1 = data.frame(x = A.r1[,200], y = A.r2[,200], Depth = depth3[,200])

ggplot() + geom_point(data = D1, 
                      mapping = aes(x = x, y = y, colour = Depth), size = 3)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  labs(color = " ", x = "x", y = "y",
       title = " ") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=10), axis.text=element_text(size=12), legend.text=element_text(size=8),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] + 1.5, yend = D1[which(depth3[,200] == max(depth3[,200])),2] + 1),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] + 2, yend = D1[which(depth3[,200] == max(depth3[,200])),2] - 1.5),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] - 1.5, yend = D1[which(depth3[,200] == max(depth3[,200])),2] - 1),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] - 2, yend = D1[which(depth3[,200] == max(depth3[,200])),2] + 1.5),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] + 0, yend = D1[which(depth3[,200] == max(depth3[,200])),2] + 1.5),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] + 0, yend = D1[which(depth3[,200] == max(depth3[,200])),2] - 1.5),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] + 2, yend = D1[which(depth3[,200] == max(depth3[,200])),2] + 0),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5) +
  geom_segment(aes(x = D1[which(depth3[,200] == max(depth3[,200])),1], y = D1[which(depth3[,200] == max(depth3[,200])),2],
                   xend = D1[which(depth3[,200] == max(depth3[,200])),1] - 2, yend = D1[which(depth3[,200] == max(depth3[,200])),2] + 0),
               arrow = arrow(length = unit(0.5, "cm")), size = 1.5)


depth.dash = matrix(0, nrow = nrow(depth3), ncol = ncol(depth3))
for(i in 1:ncol(depth3)){
  x1 <- NULL
  for(j in 1:d){
    x1 = cbind(x1, X.list[[j]][,i])
  }
  zt = which(depth3[,i] == max(depth3[,i]))[1]
  sigma <- cov(x1)
  EVV=eigen(sigma)
  vec=EVV$vectors
  ag1 = NULL
  for(j in 1:nrow(depth3)){
    a1 = angle(x1[j,] - x1[zt,], vec[,1])
    if(is.nan(a1)){
      depth.dash[j,i] = depth3[zt,i]
    }else{
      depth.dash[j,i] = (1 - abs(a1)/180) * (2 * depth3[zt,i] - depth3[j,i] ) + (a1/180) * depth3[j,i]
    }
  }
}
x2 = x1[zt,]
X1 = data.frame(x = A.r1[,200], y = A.r2[,200], depth = depth.dash[,200])

nc = 200
nr = 500
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

depth.dash1 = matrix(0, nrow = nrow(depth3), ncol = ncol(depth3))
p1 = NULL
for(i in 1:ncol(depth3)){
  x1 <- NULL
  for(j in 1:d){
    x1 = cbind(x1, X.list[[j]][,i])
  }
  zt = which(depth3[,i] == max(depth3[,i]))[1]
  sigma <- cov(x1)
  EVV=eigen(sigma)
  vec=EVV$vectors
  ag1 = NULL
  for(j in 1:nrow(depth3)){
    a1 = angle(x1[j,] - x1[zt,], vec[,1])
    if(is.nan(a1)){
      depth.dash1[j,i] = depth3[zt,i]
    }else{
        depth.dash1[j,i] = (1- a1/pi) * (2 * depth3[zt,i] - depth3[j,i] ) + a1/pi * depth3[j,i]
    }
  }
}

X1$d2 = depth.dash[,200]
X1$d1 = depth.dash1[,200]

X2 = data.frame(x = rep(X1$x, 2), y = rep(X1$y, 2), Depth = c(X1$d2, X1$d1), Type = rep(c("Piecewise", "Weighted"), each = nrow(X1)))

g1 = ggplot() + geom_point(data = X1, 
             mapping = aes(x = x, y = y, colour = d1), size = 2)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  labs(color = "Directional\nDepth", x = "x", y = "y",
       title = " ") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  geom_segment(aes(x = x2[1], y = x2[2], xend = vec[1,1]*4 + x2[1], yend = vec[2,1]*4 + x2[2]), arrow = arrow(length = unit(0.5, "cm")), size = 1.5)
g1

g2 = ggplot() + geom_point(data = X1, 
                           mapping = aes(x = x, y = y, colour = d2), size = 3)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  labs(color = "Directional Depth", x = "x", y = "y",
       title = "Piecewise") +  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=10), axis.text=element_text(size=12), legend.text=element_text(size=8),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  geom_segment(aes(x = x2[1], y = x2[2], xend = vec[1,1]*4 + x2[1], yend = vec[2,1]*4 + x2[2]), arrow = arrow(length = unit(0.5, "cm")), size = 1.5)

grid.arrange(g1 + labs(x = "") + theme(legend.position = "none"), g2 + labs(y = " ", x = " ")+ theme(legend.position = "none"), nrow = 1,
             bottom = textGrob("x", gp=gpar(fontsize=16,font=8), vjust = -1.3, hjust = -1.5))


ggplot() + geom_point(data = X2, 
                      mapping = aes(x = x, y = y, colour = Depth), size = 3)+ facet_wrap(~Type, nrow = 1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  labs(color = "Directional\nDepth", x = "x", y = "y",
       title = " ") +  theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=12),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right", strip.text = element_text(size = 14))



points(A.r1[,200], A.r2[,200], cex = 0.2, col = ifelse(depth3[,200] < depth.dash[,200],3,2))
arrows(x0 = A.r1[zt,200], y0 = A.r2[zt,200], x1 = vec[1,1]*(lambda[1]) + A.r1[zt,200], y1 = vec[2,1]*lambda[1] + A.r2[zt,200])
arrows(x0 = A.r1[zt,200], y0 = A.r2[zt,200], x1 = vec[1,2]*(lambda[2]) + A.r1[zt,200], y1 = vec[2,2]*lambda[2] + A.r2[zt,200])






dist.1 = list()
for(i in 1:ncol(A.r1)){
  dist.1[[i]] = as.matrix(dist(cbind(A.r1[,i], A.r2[,i], A.r3[,i], A.r4[,i]), upper = TRUE, diag = TRUE))
}

p = c(0.05, 0.5, 0.95)
M1 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", scale = FALSE)
M2 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", scale = TRUE)

TS.1 = as.vector(t(A.r1))
TS.2 = as.vector(t(A.r2))
TS.3 = as.vector(t(A.r1))
TS.4 = as.vector(t(A.r2))
TS.time = rep(1:ncol(A.r1), nrow(A.r1))
TS.group = rep(1:nrow(A.r1), each = ncol(A.r1))

TS.df = data.frame(Time = TS.time, Ar1 = TS.1, Ar2 = TS.2, Ar3 = TS.3, Ar4 = TS.4, Group = TS.group)

TS.1.1 = as.vector(t(A.r1[M1,]))
TS.2.1 = as.vector(t(A.r2[M1,]))
TS.3.1 = as.vector(t(A.r1[M2,]))
TS.4.1 = as.vector(t(A.r2[M2,]))
TS.time.1 = rep(1:ncol(A.r1[M1,]), nrow(A.r1[M1,]))
TS.group.1 = rep(1:nrow(A.r1[M1,]), each = ncol(A.r1[M1,]))

TS.df.1 = data.frame(Time = TS.time.1, Ar1 = TS.1.1, Ar2 = TS.2.1, Ar3 = TS.3.1, Ar4 = TS.4.1, Group = TS.group.1, Value = rep(p, each = ncol(A.r1)))

g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 1") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g2 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar2, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 2") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar2, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g3 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar3, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 1") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar3, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(1,0, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g4 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar4, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 2") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar4, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(1,0, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

grid.arrange(g1 + theme(legend.position = "none") + labs(x = " ", y = "Unstandardised"), g2 + theme(legend.position = "none") + labs(x = " ", y = " "),
             g3 + theme(legend.position = "none") + labs(x = " ", y = "Standardised"), g4 + theme(legend.position = "none") + labs(x = " ", y = " "), 
             nrow = 2, bottom = textGrob("Time", gp=gpar(fontsize=16,font=8), vjust = 0, hjust = 0), 
             left = textGrob("Value", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.2, rot = 90))

grid.arrange(g1 + theme(legend.position = "none") + labs(x = " ", y = " "), g2 + theme(legend.position = "none") + labs(x = " ", y = " "),
             nrow = 1, bottom = textGrob("Time", gp=gpar(fontsize=16,font=8), vjust = 0, hjust = 0), 
             left = textGrob("Value", vjust = 1.5, gp=gpar(fontsize=16,font=8), hjust = 0.2, rot = 90))


d1 = NULL
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
    d1 = rbind(d1, c(i, nrow(Jaxa.Gauge[[i]])))
  }
}

All.Gauge = list()
for(i in 1:length(Jaxa.Gauge)){
  if(!is.null(Jaxa.Gauge[[i]])){
    All.Gauge[[i]] = Jaxa.Gauge[[i]]
    All.Gauge[[i]]$Chirp = Gauge.Sat.2[[i]]$Estimate
    if(Jaxa.Gauge[[i]]$Latitude[1] != Gauge.Sat.2[[i]]$Lat[1]){
      print(i)
    }  
  }
}

J1 = NULL
for(i in 1:length(All.Gauge)){
  J1 = c(J1, All.Gauge[[i]]$Year[(which(All.Gauge[[i]]$Month == 1))])
}

((2003:2018) == 2004) + ((2003:2018) == 2006)

g.max = NULL
for(i in 1:length(All.Gauge)){
  g.max = c(g.max, nrow(All.Gauge[[i]]))
}

d1 = All.Gauge[[4]][,1:2]
d2 = NULL
for(i in 1:length(All.Gauge)){
  d3 = rep(0, max(g.max))
  tryCatch({
  for(j in 1:nrow(All.Gauge[[i]])){
    d3 = d3 + (d1[,1] == All.Gauge[[i]][j,1] & d1[,2] == All.Gauge[[i]][j,2])
  }
  }, error = function(e){})
  d2 = rbind(d2, d3)
}

used1 = (which(rowSums(d2) >= ncol(d2) - 20))

colSums(d2[used1,])




ar.1 = NULL
ar.2 = NULL
ar.3 = NULL
ar.loc = NULL
used2 = NULL
for(i in used1){
    ar.1 = rbind(ar.1, cumsum(All.Gauge[[i]]$Gauge[1:(187 - 20)]))
    ar.2 = rbind(ar.2, cumsum(All.Gauge[[i]]$Estimate[1:(187 - 20)]))
    ar.3 = rbind(ar.3, cumsum(All.Gauge[[i]]$Chirp[1:(187 - 20)]))
    ar.loc = rbind(ar.loc, c((All.Gauge[[i]]$Latitude)[1], 
                             (All.Gauge[[i]]$Longitude)[1]))
    used2 = c(used2, i)
}

used3 = used2[which(complete.cases(ar.1))]


ar(ar.1[1,])

ar.loc = ar.loc[complete.cases(ar.1),]
ar.1 = ar.1[complete.cases(ar.1),]
ar.2 = ar.2[complete.cases(ar.2),]
ar.3 = ar.3[complete.cases(ar.3),]

pred.loc = NULL
for(i in 1:length(All.Gauge)){
  tryCatch({
    pred.loc = rbind(pred.loc, c(All.Gauge[[i]][1,5], All.Gauge[[i]][1,6]))
  }, error = function(e){})
}

idw1 = idw(formula = E1 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)

pred.grid.MEDQ = data.frame(Latitude = pred.loc[,1], Longitude = pred.loc[,2])
ar.11 = NULL
ar.21 = NULL
ar.31 = NULL
for(i in 1:ncol(ar.1)){
  r.df = data.frame(Ar1 = ar.1[,i], Ar2 = ar.2[,i], Ar3 = ar.3[,i], Latitude = ar.loc[,1], Longitude = ar.loc[,2])
  idw1 = idw(formula = Ar1 ~ 1, locations = ~Longitude + Latitude, data = r.df, newdata = pred.grid.MEDQ, idp = 3)
  idw2 = idw(formula = Ar2 ~ 1, locations = ~Longitude + Latitude, data = r.df, newdata = pred.grid.MEDQ, idp = 3)
  idw3 = idw(formula = Ar3 ~ 1, locations = ~Longitude + Latitude, data = r.df, newdata = pred.grid.MEDQ, idp = 3)
  ar.11 = cbind(ar.11, idw1$var1.pred)
  ar.21 = cbind(ar.21, idw2$var1.pred)
  ar.31 = cbind(ar.31, idw3$var1.pred)
  print(i)
}


 
X.list = list()
X.list[[1]] = ar.1
X.list[[2]] = ar.2
X.list[[3]] = ar.3

p = seq(0,1,0.01)
M1 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", scale = FALSE)






m1 = (All.Gauge[[1]]$Month[1:(187 - 20)])
m2 = (All.Gauge[[1]]$Year[1:(187 - 20)])

mdates = paste(m2,m1, "15", sep = "-")

TS.dates = rep(as.Date(mdates), nrow(ar.1))
TS.Gauge = as.vector(t(ar.1))
TS.Est = as.vector(t(ar.2))
TS.chirp = as.vector(t(ar.3))
TS.group = rep(1:nrow(ar.1), each = length(m1))

df.Gauge = data.frame(Dates = TS.dates, Gauge = TS.Gauge, Jaxa = TS.Est, Chirp = TS.chirp, Group = TS.group)

dd1 = cbind(M1, p)
v1 = NULL
for(i in unique(M1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

TS.dates1 = rep(as.Date(mdates), length(unique(M1)))
TS.Gauge1 = as.vector(t(ar.1[unique(M1),]))
TS.Est1 = as.vector(t(ar.2[unique(M1),]))
TS.chirp1 = as.vector(t(ar.3[unique(M1),]))
TS.group1 = rep(1:nrow(ar.1[unique(M1),]), each = length(m1))
TS.value1 = rep(v1, each = length(m1))

df.1 = data.frame(Dates = TS.dates1, Gauge = TS.Gauge1, Jaxa = TS.Est1, Chirp = TS.chirp1, Group = TS.group1, Value = TS.value1)


g.Gauge = ggplot(data = df.Gauge, mapping = aes(x = Dates, y = Gauge, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Precipitation (mm)",
       title = "Rain Gauge") +
  geom_line(data = df.1, mapping = aes(x = Dates, y = Gauge, colour = Value * 100), size = 1.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12), legend.position = "none")

g.Jaxa = ggplot(data = df.Gauge, mapping = aes(x = Dates, y = Jaxa, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Precipitation (mm)",
       title = "JAXA") +
  geom_line(data = df.1, mapping = aes(x = Dates, y = Jaxa, colour = Value), size = 1.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12), legend.position = "none")

g.Chirp = ggplot(data = df.Gauge, mapping = aes(x = Dates, y = Chirp, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Precipitation (mm)",
       title = "CHIRP") +
  geom_line(data = df.1, mapping = aes(x = Dates, y = Chirp, colour = Value ), size = 1.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12), legend.position = "none")

ausmedq = data.frame(Lat = ar.loc[unique(M1),1], Lon = ar.loc[unique(M1),2], Value = v1)

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
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") 

grid.arrange(g.Gauge + theme(legend.position = "none"), medqm01 + theme(legend.position = "none"), g.Jaxa + theme(legend.position = "none"),
             g.Chirp +theme(legend.position = "none"))


g.Gauge + medqm01 + g.Jaxa + g.Chirp

X1 = list()
X1[[1]] = A.r1

M = MEDQ(X1, p = c(0.05, 0.5, 0.95), method = "Mahalanobis", scale = FALSE)

TS.1 = as.vector(t(A.r1))
TS.time = rep(1:ncol(A.r1), nrow(A.r1))
TS.group = rep(1:nrow(A.r1), each = ncol(A.r1))

TS.df = data.frame(Time = TS.time, Ar1 = TS.1, Group = TS.group)

TS.1.1 = as.vector(t(A.r1[M,]))
TS.time.1 = rep(1:ncol(A.r1[M,]), nrow(A.r1[M,]))
TS.group.1 = rep(1:nrow(A.r1[M,]), each = ncol(A.r1[M,]))

TS.df.1 = data.frame(Time = TS.time.1, Ar1 = TS.1.1, Group = TS.group.1, Value = rep(p, each = ncol(A.r1)))

g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Univariate EDQ") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value * 100), size = 1.5) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))


g1 + theme(legend.position = "none")


A.r1 = NULL
A.r2 = NULL
for(i in 1:500){
  rn = rnorm(1)
  rw = rnorm(2)
  A.r1 = rbind(A.r1, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) + rn)
  A.r2 = rbind(A.r2, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) +  rn + 10)
}

X.list = list()
X.list[[1]] = A.r1
X.list[[2]] = A.r2


M2 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", scale = FALSE)

TS.1 = as.vector(t(A.r1))
TS.2 = as.vector(t(A.r2))
TS.time = rep(1:ncol(A.r1), nrow(A.r1))
TS.group = rep(1:nrow(A.r1), each = ncol(A.r1))

TS.df = data.frame(Time = TS.time, Ar1 = TS.1, Ar2 = TS.2, Group = TS.group)

TS.1.1 = as.vector(t(A.r1[M2,]))
TS.2.1 = as.vector(t(A.r2[M2,]))
TS.time.1 = rep(1:ncol(A.r1[M2,]), nrow(A.r1[M2,]))
TS.group.1 = rep(1:nrow(A.r1[M2,]), each = ncol(A.r1[M2,]))

TS.df.1 = data.frame(Time = TS.time.1, Ar1 = TS.1.1, Ar2 = TS.2.1, Group = TS.group.1, Value = rep(p, each = ncol(A.r1)))

g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 1") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g2 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar2, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 2") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar2, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))


grid.arrange(g1 + theme(legend.position = "none") + labs(x = " "), g2 + theme(legend.position = "none")  + labs(x = " ", y = " "), nrow = 1,
             bottom = textGrob("Time", gp=gpar(fontsize=14,font=8), vjust = -1, hjust = -0.2))




A.r1 = NULL
A.r2 = NULL
A.r3 = NULL
A.r4 = NULL
for(i in 1:500){
  rn = rnorm(1)
  rw = rnorm(2)
  A.r1 = rbind(A.r1, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) + rn)
  A.r2 = rbind(A.r2, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) - 2 * rn + 10)
  A.r3 = rbind(A.r3, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) + rn - 5)
  A.r4 = rbind(A.r4, arima.sim(n = 200, list(ar = c(0.8897)),
                               sd = sqrt(sqrt(0.1796))) - rn - 10)
}

X.list = list()
X.list[[1]] = A.r1
X.list[[2]] = A.r2
X.list[[3]] = A.r3
X.list[[4]] = A.r4


M2 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", scale = FALSE)

TS.1 = as.vector(t(A.r1))
TS.2 = as.vector(t(A.r2))
TS.3 = as.vector(t(A.r3))
TS.4 = as.vector(t(A.r4))
TS.time = rep(1:ncol(A.r1), nrow(A.r1))
TS.group = rep(1:nrow(A.r1), each = ncol(A.r1))

TS.df = data.frame(Time = TS.time, Ar1 = TS.1, Ar2 = TS.2, Ar3 = TS.3, Ar4 = TS.4, Group = TS.group)

TS.1.1 = as.vector(t(A.r1[M2,]))
TS.2.1 = as.vector(t(A.r2[M2,]))
TS.3.1 = as.vector(t(A.r3[M2,]))
TS.4.1 = as.vector(t(A.r4[M2,]))
TS.time.1 = rep(1:ncol(A.r1[M2,]), nrow(A.r1[M2,]))
TS.group.1 = rep(1:nrow(A.r1[M2,]), each = ncol(A.r1[M2,]))

TS.df.1 = data.frame(Time = TS.time.1, Ar1 = TS.1.1, Ar2 = TS.2.1, Ar3 = TS.3.1, Ar4 = TS.4.1, Group = TS.group.1, Value = rep(p, each = ncol(A.r1)))

g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 1") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g2 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar2, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 2") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar2, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g3 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar3, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 3") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar3, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g4 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar4, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 4") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar4, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))


grid.arrange(g1 + theme(legend.position = "none") + labs(x = " ", y = " "), g2 + theme(legend.position = "none")  + labs(x = " ", y = " "),
             g3 + theme(legend.position = "none") + labs(x = " ", y = " "), g4 + theme(legend.position = "none")  + labs(x = " ", y = " "), nrow = 2,
             bottom = textGrob("Time", gp=gpar(fontsize=14,font=8), vjust = -1, hjust = -0.2),
             left = textGrob("Value", gp=gpar(fontsize=14,font=8), rot = 90, hjust = 0, vjust = 1.5))


X.list = list()
X.list[[1]] = A.r1
X.list[[2]] = A.r2
X.list[[3]] = A.r3
X.list[[4]] = A.r4


M2 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis", scale = FALSE)

TS.1 = as.vector(t(A.r1))
TS.2 = as.vector(t(A.r2))
TS.3 = as.vector(t(A.r3))
TS.4 = as.vector(t(A.r4))
TS.time = rep(1:ncol(A.r1), nrow(A.r1))
TS.group = rep(1:nrow(A.r1), each = ncol(A.r1))

TS.df = data.frame(Time = TS.time, Ar1 = TS.1, Ar2 = TS.2, Ar3 = TS.3, Ar4 = TS.4, Group = TS.group)

TS.1.1 = as.vector(t(A.r1[M2,]))
TS.2.1 = as.vector(t(A.r2[M2,]))
TS.3.1 = as.vector(t(A.r3[M2,]))
TS.4.1 = as.vector(t(A.r4[M2,]))
TS.time.1 = rep(1:ncol(A.r1[M2,]), nrow(A.r1[M2,]))
TS.group.1 = rep(1:nrow(A.r1[M2,]), each = ncol(A.r1[M2,]))

TS.df.1 = data.frame(Time = TS.time.1, Ar1 = TS.1.1, Ar2 = TS.2.1, Ar3 = TS.3.1, Ar4 = TS.4.1, Group = TS.group.1, Value = rep(p, each = ncol(A.r1)))

g1 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar1, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 1") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar1, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g2 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar2, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 2") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar2, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g3 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar3, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 3") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar3, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g4 = ggplot(data = TS.df, mapping = aes(x = Time, y = Ar4, group = Group)) + geom_line() +
  labs(colour = "Quantile", x = "Time", y = "Value",
       title = "Time Series 4") +
  geom_line(data = TS.df.1, mapping = aes(x = Time, y = Ar4, colour = Value * 100), size = 1) +
  scale_colour_gradientn(colours=c("blue", "Green", "red"), values = seq(0,1, length.out = 3)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))


grid.arrange(g1 + theme(legend.position = "none") + labs(x = " ", y = " "), g2 + theme(legend.position = "none")  + labs(x = " ", y = " "),
             g3 + theme(legend.position = "none") + labs(x = " ", y = " "), g4 + theme(legend.position = "none")  + labs(x = " ", y = " "), nrow = 2,
             bottom = textGrob("Time", gp=gpar(fontsize=14,font=8), vjust = -1, hjust = -0.2),
             left = textGrob("Value", gp=gpar(fontsize=14,font=8), rot = 90, hjust = 0, vjust = 1.5))





# DynamicTSQ
###############################################


WDQTS=function(x,p,h) {
  #This function computes the empirical dynamic p quantile of a set of time series  using an iterative algorithm based on weights 
  ## First the program computes the nearest h series to the series of pointwise quantiles with the L1 metric.
  # Second, it uses these h series as initial values for an iterative algorithm based on weights. 
  # Third, the solution that minimizes the objective function  is reported. 
  # Dependency 
  #  This program uses the subrutines RO and WEPT
  # Input:
  # x : The series must be in the rows of a matrix x of data, in columns the periods 1:T
  # p: the quantile to be computed 
  # h:  the number of series used as starting points in the algorithm
  #Output
  ## ifinal: The index of the series for the q quantile
  
  dd=dim(x)
  N=dd[1]
  T=dd[2]
  q=seq(1,T,1)
  for (tt in 1:T) {
    q[tt]=quantile(x[,tt],p)     }
  
  #q is the pointwise pth quantile of the series  
  #We compute the index of the  closest series to q, i1, and also the indeces of the h closest series to q, that are in orden.
  MA=x-x
  Mq=matrix(rep(q,N),N,T,byrow=TRUE)
  MA=abs(x-Mq)
  vdq=apply(MA,1,sum)
  i1=which.min(vdq)
  orden=sort(vdq,index.return=TRUE)$ix[1:h]
  # Now finds the best series among the h iterative solutions and gives the index of the best found series  in ifinal
  Msol=matrix(0,h,2)
  
  for (k in 1:h)  {
    
    ic=orden[k]
    yy=x[ic,]
    pesos=RO(x,yy,q, p)
    fin= WOPT(x,ic,pesos,q,p)
    
    Msol[k,]=c(fin[[1]],fin[[2]])
  }
  ifinal2=which.min(Msol[,2])[1]
  ifinal=Msol[ifinal2,1]
  
  out=ifinal
  return(out)
}
################################################################

RO=function(x,yy,q,p)
{
  #This function computes the corresponding T weights for the L1 distance between the series  yy and a given timewise quantile
  # Input:
  # x : The series must be in the rows of a matrix x of data, in columns the periods 1:T
  # yy: a given time series
  # q:  the timewise quantile of order p for the given data 
  # p: the quantile to be computed 
  
  #Output
  ## pesos: The weights for the given time series
  
  dd=dim(x)
  N=dd[1]
  T=dd[2]
  Mb=matrix(0,N,T)
  for (i in 1:N){
    ss1=sign(x[i,]-yy) 
    ss2=sign(x[i,]-q)
    a1=abs(x[i,]-yy)
    a2=abs(x[i,]-q)
    vd1=ifelse (ss1>0,p*a1,(1-p)*a1)
    vd2=ifelse (ss2>0,p*a2,(1-p)*a2)
    vd=vd1-vd2
    
    Mb[i,]=ifelse(abs(yy-q)>0, vd/abs(yy-q),0)
    
  }
  pesos=apply(Mb,2,mean)
  out=pesos
  return(out)
}
################################################################


WOPT=function(x,i1,pesos,q,p) {
  
  # This function iterates from an initial time series and weights and try to find another series that improves the objective function
  #This function computes the corresponding T weights for the L1 distance between the series  yy and a given timewise quantile
  # Input:
  # x : The series must be in the rows of a matrix x of data, in columns the periods 1:T
  # i1: the index of the time series to be used as starting value
  # pesos: The weights for the given time series
  # q:  the timewise quantile of order p for the given data 
  # p: the quantile to be computed 
  
  #Output
  # A list containing:
  # i1: the index of the final time series
  # Fob: The value of the objective funcion for this series
  
  dd=dim(x)
  N=dd[1]
  T=dd[2]
  pesos1=pesos
  dpes1=sum(abs(x[i1,]-q)*pesos1)
  #print(c(i1,dpes1))
  dpes=rep(0,N)
  for (i in 1:N) {
    dpes[i]=sum(abs(x[i,]-q)*pesos1)
  }
  i2=which.min(dpes)
  pesos2=RO(x,x[i2,],q, p)
  dpes2=sum(abs(x[i2,]-q)*pesos2)
  
  while (dpes2<dpes1) {
    # print(c(i2,dpes2))
    i1=i2
    dpes1=dpes2
    pesos=RO(x,x[i1,],q, p)
    for (i in 1:N) {
      dpes[i]=sum(abs(x[i,]-q)*pesos)
    }
    i2=which.min(dpes)
    pesos2=RO(x,x[i2,],q, p)
    dpes2=sum(abs(x[i2,]-q)*pesos2)
    
    
  }
  VV=rep(0,N)
  for (j in 1:N) {
    ss1=sign(x[j,]-q) 
    a1=abs(x[j,]-q)
    vd1=ifelse (ss1>0,p*a1,(1-p)*a1)
    
    VV[j]=sum(vd1)  
    
  }
  VVT=sum(VV)
  FOb=dpes1*N+VVT
  #print(c(i1,dpes1))
  #the output is the index and the value of the objective funcion 
  out=list(i1=i1,FOb=FOb)
  return(out)
}


p = seq(0,1,0.01)
k = 1
for(i in seq(0,1,0.01)){
  p[k] = WDQTS(NOAA.all.aus, i, h = 10)
  k = k + 1
  print(k)
}


N1 = list()
N1[[1]] = t(apply(NOAA.all.aus, 1, cumsum))
Me1 = MEDQ(N1, p = seq(0,1,0.001), weight = FALSE, scale = FALSE)

dd1 = cbind(Me1, seq(0,1,0.001))
v1 = NULL
for(i in unique(Me1)){
  v1 = c(v1, mean(dd1[which(dd1[,1] == i), 2]))
}

Locations = data.frame(Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon)
MEDQ.df = data.frame(Lat = Locations[unique(Me1),1], Lon = Locations[unique(Me1),2], Value = v1)


g.Map = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(MEDQ.df, mapping = aes(x = Lon, y = Lat, colour = Value), size = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Cumulative MEDQ Locations")+ 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=13), legend.text=element_text(size=10), legend.key.size = unit(0.75, "cm"),
        axis.title=element_text(size=16),  axis.text=element_text(size=12)) +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))







