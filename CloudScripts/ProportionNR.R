l.hood = function(x,b,mu,sigma){
  n = length(x)
  out1.1 = (x - mu)/sigma^2
  out1.2 = (dnorm((b - mu)/sigma) - dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out1 = sum(out1.1) + sum(out1.2)/sigma
  
  out2.0 = - n/sigma
  out2.1 = (x - mu)^2/sigma^3
  out2.2 = ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out2 = out2.0 + sum(out2.1) + sum(out2.2)/sigma^2
  
  out = c(out1, out2)
  out
}

Fisher.I = function(x, b, mu, sigma){
  out11.0 = -n/sigma^2
  out11.1 = ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out11.2 = ((dnorm((b - mu)/sigma) - dnorm(-mu/sigma))^2)/((pnorm(b,mu,sigma) - pnorm(0,mu,sigma))^2)
  out11 = out11.0 + sum(out11.1)/sigma^3 + sum(out11.2)/sigma^2
  
  out12.1 = -2 * (x - mu)/sigma^3
  out12.2 = (dnorm((b - mu)/sigma) - dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out12.3 = ((b - mu)^2 * dnorm((b - mu)/sigma) - mu^2 * dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out12.4 = ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm( - mu/sigma)) * (dnorm((b - mu)/sigma) - dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))^2
  out12 = sum(out12.1) - sum(out12.2)/sigma^2 + sum(out12.3)/sigma^4 + sum(out12.4)/sigma^3
  
  out22.0 = 2 * n/sigma^2
  out22.1 = -3 * (x - mu)^2/sigma^4
  out22.2 = 2 * ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out22.3 = ((b - mu)^3 * dnorm((b - mu)/sigma) + mu^3 * dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out22.4 = (((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm( - mu/sigma))^2)/((pnorm(b,mu,sigma) - pnorm(0,mu,sigma))^2)
  out22 = out22.0 + sum(out22.1) - sum(out22.2)/sigma^3 + sum(out22.3)/sigma^5 + sum(out22.4)/sigma^4
  
  out = rbind(c(out11, out12), c(out12, out22))
  out
}

library("entropy")

div1 = function(x){
  if(sum(x) > 0){
    a = x/sum(x)
  }else{
    a = 0
  }
  a
}

P1 = matrix(0, nrow = nrow(Aus.all), ncol = floor(ncol(Aus.all)/12) * 12)
for(i in 1:40){
  P2 = t(apply(Aus.all[,((i - 1) * 12 + 1):(i * 12)], 1, div1))
  P1[,((i - 1) * 12 + 1):(i * 12)] = P2
}

del = 0.25
Melb = which(NOAA.aus[[1]]$Lat < -37.8 + del & precip3[[1]][Aus,1] > -37.8 - del & precip3[[1]][Aus,2] < 144.96 + del & precip3[[1]][Aus,2] > 144.96 - del)[1]
Dar =  which(NOAA.aus[[1]]$Lat < -12.397 + del & precip3[[1]][Aus,1] > -12.397 - del & precip3[[1]][Aus,2] < 130.94 + del & precip3[[1]][Aus,2] > 130.94 - del)[1]


Melb1 = Aus.all[Melb,]
Dar1 = Aus.all[Dar,]
m2= dates2[,1]
m1 = dates2[,2]


P2 = matrix(P1[Dar,], nrow = 39, byrow = TRUE)

x.1 = P2[,1]
b.1 = rep(1,length(m01))

m1 = NULL
s1 = NULL
for(i in 1:length(x.1)){
  m1 = mean(x.1) + (dnorm(0) - dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0))
  s1 = var(x.1) * (1 + (0 * dnorm(0) - b.1[i] * dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)) -
                     ((dnorm(0) - dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)))^2)
}


theta.hat = c(m1, sqrt(s1))
L1 = Inf
L2 = Inf
k = 1
e1 = NULL
while((abs(theta.hat[1] - L1) > 10 ^(-6) | abs(theta.hat[2] - L2) > 10 ^(-6)) & k < 10000){
  L1 = theta.hat[1]
  L2 = theta.hat[2]
  l.1 = l.hood(x.1, b.1, theta.hat[1], theta.hat[2])
  LF = Fisher.I(x.1, b.1, theta.hat[1], theta.hat[2])
  theta.hat = theta.hat - solve(LF) %*% l.1
  k = k + 1
  theta.hat = c(theta.hat[1], abs(theta.hat[2]))
  e1 = rbind(e1, t(theta.hat))
}
theta.hat


t.h.m01 = matrix(0, ncol = 2, nrow = nrow(P1))
for(i in 1:nrow(P1)){
  x.1 = P1[i,m11]
  b.1 = rep(1, length(m11))
  m1 = NULL
  s1 = NULL
  for(i in 1:length(x.1)){
    m1 = mean(x.1) - (dnorm(0) - dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0))
    s1 = var(x.1) / (1 + (0 * dnorm(0) - b.1[i] * dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)) -
                       ((dnorm(0) - dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)))^2)
  }
  
  theta.hat = c(mean(m1), sqrt(mean(s1)))
  k = 1
  L = Inf
  a = 1
  T1 = t(theta.hat)
  while(abs(theta.hat[1] - L) > 10 ^(-5)){
    L = theta.hat[1]
    l.1 = l.hood(x.1, b.1, theta.hat[1], theta.hat[2])
    LF = Fisher.I(x.1, b.1, theta.hat[1], theta.hat[2])
    theta.hat = theta.hat - solve(LF) %*% l.1
    T1 = rbind(T1, t(theta.hat))
    k = k + 1
    if(k > 1000){
      a = a + 1
      print(a)
      k = 1
      theta.hat = c(mean(x.1) + mean(x.1) * a, sd(x.1) + sd(x.1) * a)
    }
  }
  t.h.m01[i,] = theta.hat
  print(i)
}

dim(P1)




########################
#Multivariate
########################

P1 = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12) * 12)
for(i in 1:floor(ncol(NOAA.all.aus)/12)){
  P2 = t(apply(NOAA.all.aus[,((i - 1) * 12 + 1):(i * 12)], 1, div1))
  P1[,((i - 1) * 12 + 1):(i * 12)] = P2
}

P2 = matrix(P1[Dar,], nrow = 39, byrow = TRUE)

l.hood = function(x.mat, mu.vec, sigma.vec){
  n = length(mu.vec)
  m = nrow(x.mat)
  out1 = rep(0, n)
  out2 = rep(0, n)
  for(i in 1:n){
    b = rep(1,m)
    if(i>1){
      b = 1 - rowSums(matrix(x.mat[,1:(i-1)], nrow = m))
    }
    out1.1 = (x.mat[,i] - mu.vec[i])/sigma.vec[i]^2
    out1.2 = (dnorm((b - mu.vec[i])/sigma.vec[i]) - dnorm(-mu.vec[i]/sigma.vec[i]))/(pnorm((b - mu.vec[i])/sigma.vec[i]) - pnorm(( - mu.vec[i])/sigma.vec[i]))
    out1[i] = sum(out1.1) + sum(out1.2)/sigma.vec[i]
    
    out2.0 = -m/sigma.vec[i]
    out2.1 = ((x.mat[,i] - mu.vec[i])^2)/sigma.vec[i]
    out2.2 = ((b - mu.vec[i]) * dnorm((b - mu.vec[i])/sigma.vec[i]) + mu.vec[i] * dnorm(-mu.vec[i]/sigma.vec[i]))/(pnorm((b - mu.vec[i])/sigma.vec[i]) - pnorm(( - mu.vec[i])/sigma.vec[i]))
    out2[i] = out2.0 + sum(out2.1) + sum(out2.2)/(sigma.vec[i]^2)
  }
  out = c(out1, out2)
  out
}

l.hood(x.mat = P2, mu.vec = rep(0, 11), sigma.vec = rep(1, 11))


Fisher.I(x.mat = P1, mu.vec = rep(0, 11), sigma.vec = rep(1, 11))


P1 = matrix(0, nrow = nrow(Aus.all), ncol = floor(ncol(Aus.all)/12) * 12)
for(i in 1:floor(ncol(Aus.all)/12)){
  P2 = t(apply(Aus.all[,((i - 1) * 12 + 1):(i * 12)], 1, div1))
  P1[,((i - 1) * 12 + 1):(i * 12)] = P2
}

P2 = matrix(P1[5,], nrow = 39, byrow = TRUE)
mu.hat = colMeans(P2)
mu.hat = mu.hat[-length(mu.hat)]
sigma.hat = apply(P2, 2, sd)
sigma.hat = sigma.hat[-length(sigma.hat)]
theta.hat = c(mu.hat, sigma.hat)

TH = NULL
l = Inf
k = 1
while(abs(l - theta.hat[1]) > 10^(-5)){
  l = theta.hat[1]
  LH = l.hood(x.mat = P2, mu.hat, sigma.vec = sigma.hat)
  LF = Fisher.I(x.mat = P2, mu.vec = mu.hat, sigma.vec = sigma.hat)
  theta.hat = theta.hat - solve(LF) %*% LH
  mu.hat = theta.hat[1:length(mu.hat)]
  sigma.hat = abs(theta.hat[(length(mu.hat) + 1):length(theta.hat)])
  theta.hat = c(mu.hat, abs(sigma.hat))
  k = k + 1
  print(k)
  TH = rbind(TH, t(theta.hat))
}




#############
##Mutlivariate Optim
##############

gn = function(theta){
  mu = theta[1:11]
  sigma = theta[12:22]
  d2 = 0
  for(i in 1:nrow(P2)){
    b = c(1,1-cumsum(P2[i,1:10]))
    d2 = d2 +  sum(log((1/sqrt(2*pi*sigma^2) * exp(-(P2[i, -12] - mu)^2/(2 * sigma^2))/(pnorm(b ,mu ,sigma) - pnorm(0, mu, sigma)))))
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

g3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw7, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(mu), x = "Longitude", y = "Latitude", title = expression(paste("January Proportion ", mu)))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g3


g4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw18, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = expression(sigma), x = "Longitude", y = "Latitude", title = expression(paste("January Proportion ", sigma)))+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g4

grid.arrange(g1 + labs(x = " ", y = " "), g2 + labs(x = " ", y = " "), nrow = 1, left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8)))


######
#Simulation
######

library(truncnorm)

prop.dist = function(n, mu.vec, sd.vec){
  Out = matrix(0, nrow = n, ncol = length(mu.vec) + 1)
  for(j in 1:n){
    m = length(mu.vec)
    out = rep(0, m + 1)
    for(i in 1:(m)){
      out[i] = rtruncnorm(1, a = 0, b = 1 - sum(out), mean = mu.vec[i], sd = sd.vec[i])
    }
    out[m + 1] = 1 - sum(out)
    Out[j,] = out
  }
  Out
}

del = 0.25
Melb = which(precip3[[1]][Aus[m.Aus],1] < -37.8 + del & precip3[[1]][Aus[m.Aus],1] > -37.8 - del &
               precip3[[1]][Aus[m.Aus],2] < 144.96 + del & precip3[[1]][Aus[m.Aus],2] > 144.96 - del)[1]
Dar =  which(precip3[[1]][Aus[m.Aus],1] < -12.397 + del & precip3[[1]][Aus[m.Aus],1] > -12.397 - del &
               precip3[[1]][Aus[m.Aus],2] < 130.94 + del & precip3[[1]][Aus[m.Aus],2] > 130.94 - del)[1]


O1 = prop.dist(100000, mu.vec = Opt1[Dar,1:11], sd.vec = Opt1[Dar,12:22])
O2 = prop.dist(100000, mu.vec = Opt1[Melb,1:11], sd.vec = Opt1[Melb,12:22])

E1 = apply(O1, 1, entropy)
E2 = apply(O2, 1, entropy)

dens.df = data.frame(Entropy = c(E1, E2), Group = c(rep("Darwin", length(E1)), rep("Melbourne", length(E2))))

ggplot(dens.df, aes(Entropy)) +
  geom_density(data = dens.df, colour = "blue") + facet_wrap(~Group) + theme_bw() + labs(y = "Density", title = "Simulated Monthly Entropy")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12),
        strip.text = element_text(size=12)) + geom_ribbon()




#################
##Adjusted NR
#################

l.mu = function(x, b, mu, sigma){
  out.1 = (x - mu)/sigma^2
  out.2 = ((dnorm((b - mu)/sigma) - dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma)))/sigma
  out = sum(out.1) + sum(out.2)
  out
}

l.sigma = function(x, b, mu, sigma){
  out.1 = -length(x)/sigma
  out.2 = (x - mu)^2/sigma^3
  out.3 = (((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma)))/sigma^2
  out = out.1 + sum(out.2) + sum(out.3)
  out
}

l.mu.2 = function(x, b, mu, sigma){
  n = length(x)
  out.1 = -n/sigma^2
  out.2 = ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out.3 = ((dnorm((b - mu)/sigma) - dnorm(-mu/sigma))^2)/((pnorm(b,mu,sigma) - pnorm(0,mu,sigma))^2)
  out = out.1 + sum(out.2)/sigma^3 + sum(out.3)/sigma^2
  out
}

l.sigma.2 = function(x, b, mu, sigma){
  n = length(x)
  out22.0 = 2 * n/sigma^2
  out22.1 = -3 * (x - mu)^2/sigma^4
  out22.2 = 2 * ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out22.3 = ((b - mu)^3 * dnorm((b - mu)/sigma) + mu^3 * dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out22.4 = (((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm( - mu/sigma))^2)/((pnorm(b,mu,sigma) - pnorm(0,mu,sigma))^2)
  out22 = out22.0 + sum(out22.1) - sum(out22.2)/sigma^3 + sum(out22.3)/sigma^5 + sum(out22.4)/sigma^4
  out22
}

l.mu.sigma = function(x, b, mu, sigma){
  out12.1 = -2 * (x - mu)/sigma^3
  out12.2 = (dnorm((b - mu)/sigma) - dnorm(-mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out12.3 = ((b - mu)^2 * dnorm((b - mu)/sigma) - mu^2 * dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))
  out12.4 = ((b - mu) * dnorm((b - mu)/sigma) + mu * dnorm( - mu/sigma)) * (dnorm((b - mu)/sigma) - dnorm( - mu/sigma))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))^2
  out12 = sum(out12.1) - sum(out12.2)/sigma^2 + sum(out12.3)/sigma^4 + sum(out12.4)/sigma^3
  out12
}

m1 = NULL
s1 = NULL
for(i in 1:length(x.1)){
  m1 = mean(x.1) - (dnorm(0) - dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)) * s1
  s1 = var(x.1) / (1 + (0 * dnorm(0) - b.1[i] * dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)) -
                     ((dnorm(0) - dnorm(b.1[i]))/(pnorm(b.1[i]) - pnorm(0)))^2)
}



mu.hat = mean(0)
sigma.hat = 1
sigma.hat_1 = 0
x = x.1
b = b.1
L1 = Inf
k  = 1
theta.hat = NULL
while(abs(L1 - mu.hat)>10^(-5)){
  L1 = mu.hat
  mu.hat.1 = mu.hat - l.mu(x, b, mu.hat, sigma.hat)/l.mu.2(x, b, mu.hat, sigma.hat) -
    (sigma.hat - sigma.hat_1) * l.mu.sigma(x, b, mu.hat, sigma.hat)/l.mu.2(x, b, mu.hat, sigma.hat)
  
  sigma.hat.1 = sigma.hat - l.sigma(x, b, mu.hat.1, sigma.hat)/l.sigma.2(x, b, mu.hat.1, sigma.hat) - 
    (mu.hat.1 - mu.hat) * l.mu.sigma(x, b, mu.hat.1, sigma.hat)/l.sigma.2(x, b, mu.hat.1, sigma.hat)
  
  mu.hat = mu.hat.1
  sigma.hat_1 = sigma.hat
  sigma.hat = sigma.hat.1
  k = k + 1
  theta.hat = rbind(theta.hat, c(mu.hat, sigma.hat))
}

mu.1 = sort(rnorm(100, mean = mu.hat))
s.1 = sort(rchisq(100, df = sigma.hat))
m1 = NULL
for(i in 1:length(mu.1)){
  for(j in 1:length(s.1)){
    m1 = rbind(m1, c(l.mu(x, b, mu.1[i], s.1[j]), l.sigma(x, b, mu.1[i], s.1[j])))
  }
}


m1 = m1[!is.nan(m1)[,1],]

which(abs(m1) == min(abs(m1)), arr.ind = TRUE)

which(rowSums(abs(m1))==min(rowSums(abs(m1))))




fn = function(t1){
  mu = t1[1]
  sigma = t1[2]
  (1/(sqrt(2*pi))-length(x)*log(sigma) - sum((x - mu)^2/(2*sigma^2)) -
      sum(log(pnorm(b,mu,sigma) - pnorm(0,mu,sigma))))
}

mu = 0
sigma = 1
b = rep(1, length(x))

gn = function(t1){
  mu = t1[1]
  sigma = t1[2]
  -prod(1/sqrt(2*pi*sigma^2) * exp(-(x - mu)^2/(2 * sigma^2))/(pnorm(b,mu,sigma) - pnorm(0,mu,sigma)))
}

optim(c(mean(x),sd(x)), gn)
x
b
sigma





######
#Simulation
######

library(truncnorm)

prop.dist = function(n, mu.vec, sd.vec){
  Out = matrix(0, nrow = n, ncol = length(mu.vec) + 1)
  for(j in 1:n){
    m = length(mu.vec)
    out = rep(0, m + 1)
    for(i in 1:(m)){
      out[i] = rtruncnorm(1, a = 0, b = 1 - sum(out), mean = mu.vec[i], sd = sd.vec[i])
    }
    out[m + 1] = 1 - sum(out)
    Out[j,] = out
  }
  Out
}

del = 0.25
Melb = which(precip3[[1]][Aus[m.Aus],1] < -37.8 + del & precip3[[1]][Aus[m.Aus],1] > -37.8 - del &
               precip3[[1]][Aus[m.Aus],2] < 144.96 + del & precip3[[1]][Aus[m.Aus],2] > 144.96 - del)[1]
Dar =  which(precip3[[1]][Aus[m.Aus],1] < -12.397 + del & precip3[[1]][Aus[m.Aus],1] > -12.397 - del &
               precip3[[1]][Aus[m.Aus],2] < 130.94 + del & precip3[[1]][Aus[m.Aus],2] > 130.94 - del)[1]

n = 100000
E2 = matrix(0, ncol = n, nrow = nrow(Opt1))
for(i in 1:nrow(Opt1)){
  O1 = prop.dist(n, mu.vec = Opt1[i,1:11], sd.vec = Opt1[i,12:22])
  E1 = apply(O1, 1, entropy)
  E2[i,] = E1
  print(i)
}


O1 = prop.dist(100000, mu.vec = Opt1[Dar,1:11], sd.vec = Opt1[Dar,12:22])
O2 = prop.dist(100000, mu.vec = Opt1[Melb,1:11], sd.vec = Opt1[Melb,12:22])

E1 = apply(O1, 1, entropy)
E2 = apply(O2, 1, entropy)

dens.df = data.frame(Entropy = c(E1, E2), Group = c(rep("Darwin", length(E1)), rep("Melbourne", length(E2))))

ggplot(dens.df, aes(Entropy)) +
  geom_density(data = dens.df, colour = "blue") + facet_wrap(~Group) + theme_bw() + labs(y = "Density", title = "Simulated Monthly Entropy")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12),
        strip.text = element_text(size=12)) + geom_ribbon()



q1 = t(apply(E2,1,quantile, p = c(0.025, 0.975)))

Ext.1 = matrix(0, nrow = nrow(E1[m.Aus,]), ncol = ncol(E1[m.Aus,]))
Ext.2 = matrix(0, nrow = nrow(E1[m.Aus,]), ncol = ncol(E1[m.Aus,]))
E.1 = E1[m.Aus,]
for(i in 1:nrow(E.1)){
  Ext.1[i,] = E.1[i,] < q1[i,1]
  Ext.2[i,] = E.1[i,] > q1[i,2]
}

Ext.3 = Ext.1 + Ext.2

boot(t(matrix(P1[1,], nrow = ncol(P1)/12, ncol = 12, byrow = TRUE)), statistic = entropy, R = 1000)

# dummy data
set.seed(1)
n <- 1e2
dt <- tibble(value = rnorm(n)^2)

# function that approximates the density at the provided values
approxdens <- function(x) {
  dens <- density(x)
  f <- with(dens, approxfun(x, y))
  f(x)
}

probs <- c(0.75, 0.95)

dt <- dt %>%
  mutate(dy = approxdens(value),                         # calculate density
         p = percent_rank(value),                        # percentile rank 
         pcat = as.factor(cut(p, breaks = probs,         # percentile category based on probs
                              include.lowest = TRUE)))

ggplot(dt, aes(value, dy)) +
  geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcat)) +
  geom_line() +
  scale_fill_brewer(guide = "none") +
  theme_bw()



# dummy data with 2 groups
dt2 <- tibble(category = c(rep("A", n), rep("B", n)),
              value = c(rnorm(n)^2, rnorm(n, mean = 2)))

dt2 <- dt2 %>%
  group_by(category) %>% 
  mutate(dy = approxdens(value),    
         p = percent_rank(value),
         pcat = as.factor(cut(p, breaks = probs,
                              include.lowest = TRUE)))

# faceted plot
ggplot(dt2, aes(value, dy)) +
  geom_ribbon(aes(ymin = 0, ymax = dy, fill = pcat)) +
  geom_line() +
  facet_wrap(~ category, nrow = 2, scales = "fixed") +
  scale_fill_brewer(guide = "none") +
  theme_bw()





###############
#Dirichlet proportion distribution
###############

sim1 = matrix(0, nrow = 100000, ncol = 4)
for(i in 1:100000){
  r1 = rdirichlet(1, c(0.5, 0.7, 0.3, 0.6))
  sim1[i,1] = r1[1]^2
  sim1[i,2] = sum(r1[1:2]^2)
  sim1[i,3] = sum(r1[1:3]^2)
  sim1[i,4] = sum(r1[1:4]^2)
}

n = 10^6
Ent.sim.mean = rep(0, n)
pb <- txtProgressBar(min = 1, max = n, style = 3)
for(i in 1:n){
  r1 = rdirichlet(1000, c(0.5, 0.7, 0.3, 0.6))
  Ent.sim.mean[i] = mean(r1)
  setTxtProgressBar(pb, i)
}


install.packages("sirt")
library("sirt")

help("dirichlet.mle")

r1 = rdirichlet(1000, c(0.5, 0.7, 0.3, 0.6))
dirichlet.mle(r1)

p1 = matrix(P.1[1,], nrow = 39, ncol = 12, byrow = TRUE)

dirichlet.mle(p1)
NOAA.all.aus = NOAA.all.aus[!duplicated(NOAA.aus[[1]][,1:2]), ]

alpha.mat = matrix(0, nrow = nrow(NOAA.all.aus), ncol = 12)
for(i in 1:nrow(NOAA.all.aus)){
  p1 = NULL
  indices = 1:12
  for(j in 1:(ncol(NOAA.all.aus) - 11)){
    p1 = rbind(p1, (NOAA.all.aus[i, j:(j+11)])[indices])
    indices = c(indices[12], indices[-12])
  }
  d1 = dirichlet.mle(p1)
  alpha.mat[i,] = d1$alpha
}

alpha.exp = matrix(0, nrow = nrow(alpha.mat), ncol = 2)
for(i in 1:nrow(alpha.mat)){
  alpha.exp[i,1] = 0.5 - ncol(alpha.mat)/2 * (sum(alpha.mat[i,]^2) + sum(alpha.mat[i,]))/((sum(alpha.mat[i,]) + 1) * sum(alpha.mat[i,])) + log(ncol(alpha.mat))
  alpha.exp[i,2] = ncol(alpha.mat)^2 / 4 * 1
}


pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
d1 = data.frame(alpha1 = alpha.mat[,1], alpha2 = alpha.mat[,2], alpha3 = alpha.mat[,3], alpha4 = alpha.mat[,4], alpha5 = alpha.mat[,5], alpha6 = alpha.mat[,6],
                alpha7 = alpha.mat[,7], alpha8 = alpha.mat[,8], alpha9 = alpha.mat[,9], alpha10 = alpha.mat[,10], alpha11 = alpha.mat[,11], alpha12 = alpha.mat[,12],
                Lat = NOAA.aus[[1]]$Lat[!duplicated(NOAA.aus[[1]][,1:2])], Lon = NOAA.aus[[1]]$Lon[!duplicated(NOAA.aus[[1]][,1:2])], alpha0 = rowSums(alpha.mat), alpha1.0 = alpha.mat[,1]/rowSums(alpha.mat), 
                alpha7.0 = alpha.mat[,7]/rowSums(alpha.mat), alpha1.var = alpha.mat[,1] * (rowSums(alpha.mat) - alpha.mat[,1]) / (rowSums(alpha.mat)^2 *(rowSums(alpha.mat) + 1)),
                alpha7.var = alpha.mat[,7] * (rowSums(alpha.mat) - alpha.mat[,7]) / (rowSums(alpha.mat)^2 *(rowSums(alpha.mat) + 1)))


idw1 =  idw(formula = alpha1 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw2 =  idw(formula = alpha7 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw3 =  idw(formula = alpha1.0 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw4 =  idw(formula = alpha7.0 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw5 =  idw(formula = alpha1.var ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw6 =  idw(formula = alpha7.var ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)


g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw1$var1.pred, idw2$var1.var), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(paste(alpha)), x = "Longitude", y = "Latitude", title = expression(paste("January proportion ", alpha)))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw1$var1.pred, idw2$var1.var), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(paste(alpha, "            ")), x = "Longitude", y = "Latitude", title = expression(paste("July Proportion ", alpha)))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw3, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw3$var1.pred, idw4$var1.var), max(idw3$var1.pred, idw4$var1.pred))) +
  theme_bw() + labs(color = "Mean", x = "Longitude", y = "Latitude", title = expression(paste("January Proportion Mean")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw4, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw3$var1.pred, idw4$var1.var), max(idw3$var1.pred, idw4$var1.pred))) +
  theme_bw() + labs(color = "Mean     ", x = "Longitude", y = "Latitude", title = expression(paste("July Proportion Mean")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)


g5 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw5, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw5$var1.pred, idw6$var1.var), max(c(idw5$var1.pred, idw6$var1.pred)))) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = expression(paste("January Proportion Variance")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g6 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw6, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(0, max(idw5$var1.pred, idw6$var1.pred))) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = expression(paste("July Proportion Variance")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


grid.arrange(g1 + labs(x = " ", y = " ") + theme(legend.position = "none"),
             g3 + labs(x = " ", y = " ") + theme(legend.position = "none"),
             g5 + labs(x = " ", y = " ") + theme(legend.position = "none"),
             g2 + labs(x = " ", y = " ") + theme(legend.position = "bottom"),
             g4 + labs(x = " ", y = " ") + theme(legend.position = "bottom"),
             g6 + labs(x = " ", y = " ") + theme(legend.position = "bottom"), nrow = 2, ncol = 3,
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0, vjust = -8),
             left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, vjust = 1.5, hjust = 0), heights = c(1,1.3))






del = 0.5
Melb = which(NOAA.aus[[1]][,1] < -37.8 + del & NOAA.aus[[1]][,1] > -37.8 - del &
               NOAA.aus[[1]][,2] < 144.96 + del & NOAA.aus[[1]][,2] > 144.96 - del)[1]
Dar =  which(NOAA.aus[[1]][,1] < -12.397 + del & NOAA.aus[[1]][,1] > -12.397 - del &
               NOAA.aus[[1]][,2] < 130.94 + del & NOAA.aus[[1]][,2] > 130.94 - del)[1]




###############
#Season Plots
###############


alpha.mat = matrix(0, nrow = nrow(P.season), ncol = 4)
for(i in 1:nrow(P.season)){
  p1 = matrix(P.season[i,], nrow = 40, ncol = 4, byrow = TRUE)
  d1 = dirichlet.mle(p1)
  alpha.mat[i,] = d1$alpha
}

pred.grid = data.frame(Lon = Austra1[Aus1,2], Lat = Austra1[Aus1,1])
d1 = data.frame(alpha1 = alpha.mat[,1], alpha2 = alpha.mat[,2], alpha3 = alpha.mat[,3], alpha4 = alpha.mat[,4],
                Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon, alpha0 = rowSums(alpha.mat), Summermean = alpha.mat[,4]/rowSums(alpha.mat), 
                Wintermean = alpha.mat[,2]/rowSums(alpha.mat), Summervar = alpha.mat[,4] * (rowSums(alpha.mat) - alpha.mat[,4]) / (rowSums(alpha.mat)^2 *(rowSums(alpha.mat) + 1)),
                Wintervar = alpha.mat[,2] * (rowSums(alpha.mat) - alpha.mat[,2]) / (rowSums(alpha.mat)^2 *(rowSums(alpha.mat) + 1)))


idw1 =  idw(formula = alpha4 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw2 =  idw(formula = alpha2 ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw3 =  idw(formula = Summermean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw4 =  idw(formula = Wintermean ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw5 =  idw(formula = Summervar ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)
idw6 =  idw(formula = Wintervar ~ 1, locations = ~Lon + Lat, data = d1, newdata = pred.grid, idp = 3)


g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw1$var1.pred, idw2$var1.var), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(paste(alpha)), x = "Longitude", y = "Latitude", title = expression(paste("Summer proportion ", alpha)))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g2 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw1$var1.pred, idw2$var1.var), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(paste(alpha, "             ")), x = "Longitude", y = "Latitude", title = expression(paste("Winter Proportion ", alpha)))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g3 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw3, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw3$var1.pred, idw4$var1.var), max(idw3$var1.pred, idw4$var1.pred))) +
  theme_bw() + labs(color = "Mean", x = "Longitude", y = "Latitude", title = expression(paste("Summer Proportion Mean")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g4 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw4, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw3$var1.pred, idw4$var1.var), max(idw3$var1.pred, idw4$var1.pred))) +
  theme_bw() + labs(color = "Mean     ", x = "Longitude", y = "Latitude", title = expression(paste("Winter Proportion Mean")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g5 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw5, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9), limits = c(min(idw5$var1.pred, idw6$var1.var), max(idw5$var1.pred, idw6$var1.pred))) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = expression(paste("Summer Proportion Variance")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


g6 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw6, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9),limits = c(min(idw5$var1.pred, idw6$var1.var), max(idw5$var1.pred, idw6$var1.pred))) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = expression(paste("Winter Proportion Variance")))+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)



grid.arrange(g1 + labs(x = " ", y = " ") + theme(legend.position = "none"), g2 + labs(x = " ", y = " "), g3 + labs(x = " ") + theme(legend.position = "none"),
             g4 + labs(x = " ", y = " "), g5 + labs(x = " ", y = " ") + theme(legend.position = "none"), g6 + labs(x = " ", y = " "), nrow = 3, ncol = 2, widths = c(3,3.9),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0.7, vjust = -0.6))






































###Dirichlet Entropy Sim

r1 = rdirichlet(100000, alpha = alpha.mat[Melb,])

e1 = apply(r1, 1, entropy)

plot(hist(e1))

plot(density(e1))
p1 = matrix(P1[Melb,], nrow = 39, ncol = 12, byrow = TRUE)

e2 = apply(p1, 1, entropy)
hist(e2)
plot(density(e2, adjust = 1.5))
lines(density(e1))
r1 = rdirichlet(100000, alpha = alpha.mat[Melb,])
r2 = rdirichlet(100000, alpha = alpha.mat[Dar,])

E1 = apply(r2, 1, entropy)
E2 = apply(r1, 1, entropy)
Ent.sim = list()
pb <- txtProgressBar(min = 1, max =nrow(alpha.mat), style = 3)
for(i in 1:nrow(alpha.mat)){
  setTxtProgressBar(pb, i)
  r1 = rdirichlet(10^5, alpha = alpha.mat[i,])
  E.1 = apply(matrix(NOAA.all.aus[i,1:(floor(ncol(NOAA.all.aus)/12)*12)], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12, byrow = TRUE), 1, entropy1)
  Ent = 0.5 - 6 * rowSums(r1^2) + log(12)
  q1 = ecdf(Ent)
  Ent.sim[[i]] = q1(E.1)
}
close(pb)

Ent.sim.1 = matrix(0, nrow = length(Ent.sim), ncol = length(Ent.sim[[1]]))
for(i in 1:length(Ent.sim)){
  Ent.sim.1[i,] = Ent.sim[[i]]
}


Ent.sim.df = data.frame(Ext = (Ent.sim.1[,41] < 0.20), Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon)

idw.ent = idw(Ext ~ 1, locations = ~Lon + Lat, data = Ent.sim.df, newdata = pred.grid, idp = 3)

idw.ent = idw.ent[round(idw.ent$var1.pred) == 1,]

g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw.ent, mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
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
g1




dens.df = data.frame(Entropy = c(E1, E2), Group = c(rep("Darwin", length(E1)), rep("Melbourne", length(E2))))

ggplot(dens.df, aes(Entropy)) +
  geom_density(data = dens.df, colour = "blue", lwd = 1) + facet_wrap(~Group) + theme_bw() + labs(y = "Density", title = "Simulated Monthly Entropy")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12),
        strip.text = element_text(size=12))


dim(Aus.all)
ncol(Aus.all)/12

Loc.list = list()
for(i in 1:nrow(Aus.all)){
  Loc.list[[i]] = matrix(Aus.all[i,1:(39*12)], ncol = 12, byrow = TRUE)
}


plot(rowSums(Loc.list[[Melb]]), type = "l")

mean(rowSums(Loc.list[[Melb]])[-c(1:20)])

M1 = rep(0, length(Loc.list))
M2 = rep(0, length(Loc.list))
for(i in 1:length(Loc.list)){
  M1[i] = mean(rowSums(Loc.list[[i]])[1:20])
  M2[i] = mean(rowSums(Loc.list[[i]])[-c(1:20)])
}

M3 = rep(0, length(Loc.list))
for(i in 1:length(Loc.list)){
  M3[i] = mean(rowSums(Loc.list[[i]]))
}

Mean.df = data.frame(Mean1 = M1, Mean2 = M2, Meanall = M3, Lat = precip3[[1]][Aus,1], Lon = precip3[[1]][Aus,2])

idw1 =  idw(formula = Mean1 ~ 1, locations = ~Lon + Lat, data = Mean.df, newdata = pred.grid, idp = 3)
idw2 =  idw(formula = Mean2 ~ 1, locations = ~Lon + Lat, data = Mean.df, newdata = pred.grid, idp = 3)
idw3 = idw(formula = Meanall ~ 1, locations = ~Lon + Lat, data = Mean.df, newdata = pred.grid, idp = 3)

Mean.df2 = data.frame(Mean1 = idw1$var1.pred, Mean2 = idw2$var1.pred, Meanall = idw3$var1.pred, Lat = idw1$Lat, Lon = idw1$Lon)


g.m = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Mean.df2, mapping = aes(x = Lon, y = Lat, colour = Mean1 - Mean2), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Precipitation \nDifference\n(mm)", x = "Longitude", y = "Latitude", title = "Australian Precipitation Difference")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g.m

g.ma = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Mean.df2, mapping = aes(x = Lon, y = Lat, colour = Meanall), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Precipitation (mm)", x = "Longitude", y = "Latitude", title = "Australian Precipitation Difference")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)
g.ma

E.plot <- ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) +
  geom_point(data = E.df, 
             mapping = aes(x = Lon, y = Lat, colour = E1 - E2), size = 0.5)+
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Entropy \nDifference", x = "Longitude", y = "Latitude",
       title = "Australian Monthly Entropy Difference") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14))
E.plot

grid.arrange(g.m + labs(x = " "), E.plot + labs(x = " ", y = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0.8, vjust = -0.6),
             widths = c(3,3.1))



##############
## Monte Carlo Approximation
##############

### n = 3

x = alpha.mat[j,]

m.beta = function(x){
  num = prod(gamma(x))
  den = gamma(sum(x))
  out = num/den
  return(out)
}

fe1 = function(z2, z3){
  2^(-3)/m.beta(alpha) * ((z2 + sqrt(z2^2 - ((1 - sqrt(z3 - z2))^2 - z2)^2))/2)^(alpha[1]/2 - 1) * (z2 - (z2 + sqrt(z2^2 - ((1 - sqrt(z3 - z2))^2 - z2)^2))/2)^(alpha[2]/2 - 1) * (z3 - z2)^(alpha[3]/2 - 1)
}

fe2 = function(z2, z3){
  2^(-3)/m.beta(alpha) * ((z2 - sqrt(z2^2 - ((1 - sqrt(z3 - z2))^2 - z2)^2))/2)^(alpha[1]/2 - 1) * (z2 - (z2 - sqrt(z2^2 - ((1 - sqrt(z3 - z2))^2 - z2)^2))/2)^(alpha[2]/2 - 1) * (z3 - z2)^(alpha[3]/2 - 1)
}

fe11 = function(z1, z2, z3){
  if(sqrt(z1) + sqrt(z2 - z1) + sqrt(z3 - z2) != 1){
    out = 0
  }else{
    out = 2^(-3)/m.beta(alpha) * z1 ^ (alpha[1]/2 - 1) * (z2 - z1) ^ (alpha[2]/2 - 1) * (z3 - z2) ^ (alpha[3]/2 - 1)
  }
  return(out)
}

n = 1e6


alpha = c(1.5, 1.5, 1.5)
r1 = rdirichlet(n, alpha)
z.sim = cbind(r1[,1]^2, r1[,1]^2 + r1[,2]^2, r1[,1]^2 + r1[,2]^2 + r1[,3]^2)

dz = rep(0, nrow(z.sim))
pb <- txtProgressBar(min = 1, max =nrow(z.sim), style = 3)
for(i in 1:nrow(z.sim)){
  dz[i] = fe11(z.sim[i,1], z.sim[i,2], z.sim[i,3])/ddirichlet(c(r1[i,1], r1[i,2], r1[i,3]), alpha)
  setTxtProgressBar(pb, i)
}

sum(dz)/n

k = 1
for(i in 1:n){
    if(z3.sim[i] >= z2.sim[i]){
      k = k + 1
  }
}

Z.mat = matrix(0, ncol = 2, nrow = k - 1)
k = 1
for(i in 1:n){
    if(z3.sim[i] >= z2.sim[i]){
      Z.mat[k,] = c(z3.sim[i], z2.sim[i])
      k = k + 1
  }
}

z2.sim[j]^2 >= ((1 - sqrt(z3.sim[i] - z2.sim[j]))^2 - z2.sim[j])^2

k = 1
for(i in 1:nrow(Z.mat)){
  if(Z.mat[i,2]^2 >= ((1 - sqrt(Z.mat[i,1] - Z.mat[i,2]))^2 - Z.mat[i,2])^2){
    k = k + 1
  }
}

Z.mat2 = matrix(0, nrow = k - 1, ncol = 2)
k = 1
for(i in 1:nrow(Z.mat)){
  if(Z.mat[i,2]^2 >= ((1 - sqrt(Z.mat[i,1] - Z.mat[i,2]))^2 - Z.mat[i,2])^2){
    Z.mat2[k,] = Z.mat[i,]
    k = k + 1
  }
}

dz = rep(0, nrow(Z.mat2))
pb <- txtProgressBar(min = 1, max =nrow(Z.mat2), style = 3)
for(i in 1:nrow(Z.mat2)){
  dz[i] = fe1(Z.mat2[i,2], Z.mat2[i,1])/2 + fe2(Z.mat2[i,2], Z.mat2[i,1])/2
  setTxtProgressBar(pb, i)
}
close(pb)

sum(dz)/(n)



a1 = runif(10)
a1/sum(a1)
a2 = runif(10)
sum(a1/sum(a1) * a2/sum(a2)/(sum(a1/sum(a1) * a2/sum(a2))))



bet1 = function(x){
  (1/m.beta(c(alpha1, beta1))) * x^(alpha1 - 1) * (1 - x)^(beta1 - 1)
}


alpha1 = 0.5
beta1 = 0.8
x.sim = runif(n)
sum(bet1(x.sim))/n



x1 = runif(n)
x2 = runif(n)


diric1 = function(x){
  if(min(x) < 0 | max(x) > 1 | x[1] + x[2] > 1){
    out = 0
  }else{
    out = (1/m.beta(alpha)) * x[1]^(alpha[1] - 1) * x[2] ^ (alpha[2] - 1) * (1 - x[1] - x[2])^ (alpha[3] - 1)
  }
  return(out)
}

x = cbind(x1, x2, 1 - x1 - x2)

n = 10^6
dz = rep(0, n)
pb <- txtProgressBar(min = 1, max = n, style = 3)
for(i in 1:nrow(x)){
  dz[i] = diric1(x[i,])
  setTxtProgressBar(pb, i)
}
mean(dz)


diric2 = function(y){
  if(sqrt(y[1]) + sqrt(y[2]) > 1){
    out = 0
  }else{
    out = ((2^(-3))/m.beta(alpha)) * y[1]^(alpha[1]/2 - 1) * y[2]^(alpha[2]/2 - 1) * ((1 - sqrt(y[1]) - sqrt(y[2]))^2)^(alpha[3]/2 - 1)
  }
  return(out)
}

y1 = runif(n)
y2 = runif(n)
y = cbind(y1, y2)
dz1 = rep(0, n)
for(i in 1:nrow(y)){
  dz1[i] = diric2(y[i,])
}
sum(dz1)/n
sum(1 - sqrt(y1) - sqrt(y2) < 0)
sum(rowSums(sqrt(y) == 1))


diric0.5 = function(x){
  if(x[1] + x[2] > 1){
    out = 0
  }else{
    out = ((2^3)/m.beta(alpha)) * (x[1])^(2 * alpha[1] - 1) * (x[2])^(2 * alpha[2] - 1) * (1 - x[1]^2 - x[2]^2)^(2 * alpha[3] - 1)
  }
  return(out)
}

y1 = runif(n)
y2 = runif(n)
y = cbind(y1, y2)
dz1 = rep(0, n)
for(i in 1:nrow(y)){
  dz1[i] = diric0.5(y[i,])
}
sum(dz1)/n

dirich2 = function(x){
  if(max(x) > 1 | min(x) < 0 | sum(sqrt(x)) != 1| 1 - sqrt(x[1]) - sqrt(x[2]) < 0){
    out = 0
  }else{
    out = ((2^(-2))/(m.beta(alpha))) * (x[1]^(alpha[1]/2 - 1)) * (x[2]^(alpha[2]/2 - 1)) * ((1 - sqrt(x[1])-sqrt(x[2]))^(alpha[3] - 1))
  }
  return(out)
}

alpha = c(3, 4.5, 2.8)
n = 10^6
y1 = runif(n, 0, 1)
y2 = runif(n, 0, 1)
y = cbind(y1, y2, (1 - sqrt(y1) - sqrt(y2))^2)

dz1 = rep(0, n)
pb <- txtProgressBar(min = 1, max = n, style = 3)
for(i in 1:nrow(y)){
  dz1[i] = dirich2(y[i,])
  setTxtProgressBar(pb, i)
}
mean(dz1)

dirich2sum = function(x){
  if(max(x) > 1 | min(x) < 0 | sum(sqrt(x)) > 1 | x[2] < x[1]){
    out = 0
  }else if(x[2] > x[1] && 1 - sqrt(x[1]) - sqrt(x[2] - x[1]) < 0){
    out = 0
  }else{
    out = ((2^(-2))/(m.beta(alpha))) * (x[1]^(alpha[1]/2 - 1)) * ((x[2] - x[1])^(alpha[2]/2 - 1)) * ((1 - sqrt(x[1]) - sqrt(x[2] - x[1]))^(alpha[3] - 1))
  }
  return(out)
}

n = 10^6
alpha = c(5, 4.5, 5)
mean1 = (sum(alpha^2) + sum(alpha))/((sum(alpha) + 1) * sum(alpha))
alpha0 = sum(alpha)
j1 = 1:3
var1 = 0
var2 = 0
for(i in 1:length(alpha)){
  var1 = var1 + prod((alpha[i] + j1)/(alpha0 + j1))
}
k1 = 0:1
k2 = 1:3
for(i in 1:(length(alpha) - 1)){
  for(j in (i + 1):length(alpha)){
    var2 = var2 + 2 * prod((alpha[i] + k1) * (alpha[j] + k1))/prod(alpha0 + k2)
  }
}

var3 = var1 + var2 - mean1^2


y1 = rnorm(n, mean = mean1, sd = sqrt(var3))
y2 = rnorm(n, mean = mean1, sd = sqrt(var3))
y = cbind(y1, y2)


dz1 = rep(0, n)
pb <- txtProgressBar(min = 1, max = n, style = 3)
for(i in 1:nrow(y)){
  dz1[i] = dirich2sum(y[i,])
  setTxtProgressBar(pb, i)
}
mean(dz1)

#################
#Dirichlet squared sum sample
#################

r1 = rdirichlet(n, alpha = alpha.mat[Melb,])
r1.1 = rowSums(r1^2)
r1.2 = apply(r1, 1, entropy1)
r2 = rdirichlet(n, alpha = alpha.mat[Dar,])
r2.1 = rowSums(r2^2)
r2.2 = apply(r2, 1, entropy1)


Ent.sim1 = data.frame(Data = c(0.5 - (ncol(alpha.mat)/2) *r1.1 + log(ncol(alpha.mat)), r1.2,
                               0.5 - (ncol(alpha.mat)/2) *r2.1 + log(ncol(alpha.mat)), r2.2),
                      Type = c(rep("Approximated Entropy", n), rep("Simulated Entropy", n), rep("Approximated Entropy", n), rep("Simulated Entropy", n)),
                      Location = c(rep("Melbourne", 2 * n), rep("Darwin", 2 * n)))

ggplot(Ent.sim1, aes(Data)) +
  geom_density(data = Ent.sim1, colour = "blue", lwd = 1.1) + facet_grid(Location~Type) + theme_bw() + labs(y = "Density", title = "Monthly Entropy Density", x = "Entropy")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=16), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=12),
        strip.text = element_text(size=12))



Mean1 = rep(0, nrow(alpha.mat))
var1 = rep(0, nrow(alpha.mat))
var2 = rep(0, nrow(alpha.mat))
var3 = rep(0, nrow(alpha.mat))
j1 = 1:3
k1 = 0:1
k2 = 1:3
for(i in 1:nrow(alpha.mat)){
  alpha0 = sum(alpha.mat[i,])
  Mean1[i] = (sum(alpha.mat[i,]^2) + alpha0)/((alpha0 + 1) * alpha0)
  for(j in 1:length(alpha.mat[i,])){
    var1[i] = var1[i] + prod((alpha.mat[i,j] + j1)/(alpha0 + j1))
  }
  for(j in 1:(length(alpha.mat[i,]) - 1)){
    for(k in (j + 1):length(alpha.mat[i,])){
      var2[i] = var2[i] +  prod((alpha.mat[i,j] + k1) * (alpha.mat[i,k] + k1))/prod(alpha0 + k2)
    }
  }
  var3[i] = var1[i] + 2 * var2[i] - Mean1[i]^2
}

var3 = var1 + 2 * var2 - Mean1^2

AE.df = data.frame(Mean = 0.5 - (ncol(alpha.mat)/2) * Mean1 + log(ncol(alpha.mat)), Variance = var3 , Lat = precip3[[1]][Aus,1], Lon = precip3[[1]][Aus,2])

idw1 = idw(formula = Mean ~ 1, locations = ~Lon + Lat, data = AE.df, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Variance ~ 1, locations = ~Lon + Lat, data = AE.df, newdata = pred.grid, idp = 3)


g.e = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Mean", x = "Longitude", y = "Latitude", title = "Approximate Monthly Entropy Mean")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)

g.v = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = "Approximate Monthly Entropy Variance")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)


grid.arrange(g.e + theme(legend.position = "left") + labs(x = " "), g.v + labs(x = " ", y = " "), nrow = 1, bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8)))








###################
##JAXA weighted Entropy 2020
###################

year2020 = which(dates4[,1] == 2020)

sum1 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = length(year2020))
weight1 = matrix(0, nrow = nrow(JAXA.daily1[[1]]), ncol = length(year2020))

for(i in 1:length(year2020)){
  cdata = JAXA.daily1[[year2020[i]]][,-c(1,2)]
  weight1[,i] = apply(cdata, 1, entropy1)
  sum1[,i] = rowSums(cdata)
}

weight1[is.na(weight1)] = 0

weight.ent = rep(0, nrow(sum1))
ent.unweighted = rep(0, nrow(sum1))
ent.ent = rep(0, nrow(sum1))
for(i in 1:nrow(sum1)){
  weight.ent[i] = weighted.entropy1(x = sum1[i,], w = weight1[i,]/sum(weight1[i,]))
  ent.unweighted[i] = weighted.entropy1(x = sum1[i,]/sum(sum1[i,]))
  ent.ent[i] = entropy(sum1[i,])
}


Weight.ent.df = data.frame(Weighted.Entropy = weight.ent, Unweighted.Entropy = ent.unweighted, Entropy = ent.ent, Lat = JAXA.daily1[[251]][,1], Lon = JAXA.daily1[[251]][,2])

idw1 = idw(formula = Weighted.Entropy ~ 1, locations = ~Lon + Lat, data = Weight.ent.df, newdata = pred.grid, idp = 3)
idw2 = idw(formula = Unweighted.Entropy ~ 1, locations = ~Lon + Lat, data = Weight.ent.df, newdata = pred.grid, idp = 3)

g.we = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Entropy", x = "Longitude", y = "Latitude", title = "2020 Weighted Monthly Entropy")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)

g.ue = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9)) +
  theme_bw() + labs(color = "Entropy", x = "Longitude", y = "Latitude", title = "2020 Unweighted Monthly Entropy")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)



grid.arrange(g.we + theme(legend.position = "left") + labs(x = " "), g.ue + labs(x = " ", y = " "), nrow = 1, widths = c(3,3.5),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8)))




w1 = NULL
w2 = unique(dates4[,1])[-c(1,22)]

for(i in 1:length(w2)){
  w1 = c(w1, sum(dates4[which(dates4[,1] == w2[i]), 3]))
}
w2 = c(0, cumsum(w1))
365/14

365 %% 14

seq(0,366, 14)

seq1 = c(seq(0,366, 7)[-53], 365)
E.daily = matrix(0, nrow = nrow(JAXA.all), ncol = length(w2) - 1)
for(j in 1:(length(w2) - 1)){
  P.daily = matrix(0, nrow = nrow(JAXA.all), ncol = length(seq1) - 1)
  for(i in 1:(length(seq1) - 1)){
    P.daily[,i] = rowSums(JAXA.all[,w2[j] + (seq1[i] + 1):seq1[i + 1]])
  }
  E.daily[,j] = apply(P.daily, 1, entropy1)
}


p.val1 = rep(0, nrow(E.daily))
for(i in 1:nrow(E1)){
  t1 = t.test(E.daily[i,1:10], E.daily[i,11:20], alternative = "two.sided", paired = FALSE, var.equal = TRUE)
  p.val1[i] = t1$p.value
}
mean(p.val1)




###########
#Monte Carlo Proper
###########


n = 4

m = 10^5

m.beta = function(x){
  num = prod(gamma(x))
  den = gamma(sum(x))
  out = num/den
  return(out)
}


AE = function(Z.vec){
  n = length(Z.vec)
  b1 = m.beta(alpha.vec)
  check1 = 0
  for(i in 1:(length(Z.vec) - 1)){
    if(Z.vec[i] >= Z.vec[i + 1]){
      check1 = 1
    }
  }
  if(check1 == 0){
    out = ((2^(-n))/b1) * (Z.vec[1])^(alpha.vec[1]/2 - 1)
    for(i in 2:n){
      out = out * (Z.vec[i] - Z.vec[i - 1])^(alpha.vec[i]/2 - 1)
    }
  }else{
    out = 0
  }
  return(out)
}

r1 = rdirichlet(m, alpha = runif(12) * 10)

r2 = r1^2

r3 = t(apply(r2, 1, cumsum))


delta1 = function(x){
  n = length(x)
  out = 1 - sqrt(x[1])
  for(i in 2:n){
    out = out - sqrt(x[i] - x[i - 1])
  }
  return(out)
}

eps = 10^(-10)
r4 = apply(r3,1,delta1)

mean(r4 < eps)

ddirichlet(r1, runif(12) * 10)

alpha.vec = runif(4) * 10

AE1 = function(Z.vec){
  check1 = 0
  for(i in 1:(length(Z.vec) - 1)){
    if(Z.vec[i] >= Z.vec[i + 1]){
      check1 = 1
    }
  }
  if(check1 == 0){
    n = length(alpha.vec)
    b1 = m.beta(alpha.vec)
    out = ((2^(-(n-1)))/b1) * (Z.vec[1])^(alpha.vec[1]/2 - 1)
    out1 = 1 - sqrt(Z.vec[1])
    for(i in 2:(n-1)){
      out = out * (Z.vec[i] - Z.vec[i - 1])^(alpha.vec[i]/2 - 1)
      out1 = out1 - sqrt(Z.vec[i] - Z.vec[i - 1])
    }
    if(out1 < 0){
      out2 = 0
    }else{
      out2 = out * out1^(alpha.vec[n] - 1) 
    }
  }else{
    out2 = 0
  }
  return(out2)
}


x = matrix(0, nrow = 10^7, ncol = 3)
for(i in 1:(ncol(x))){
  x[,i] = runif(nrow(x))
}

out1 =  1
for(i in 3:ncol(x)){
  out1 = out1 - sqrt(x[,i] - x[,i - 1])
}


z1.pos = (x[,2] + sqrt(x[,2]^2 - ((out1)^2 - x[,2])^2))/2
z1.neg = (x[,2] - sqrt(x[,2]^2 - ((out1)^2 - x[,2])^2))/2

x[,1] = 0.5 * z1.pos + 0.5 * z1.neg 

x[is.nan(x[,1]),1] = 2

x = as.matrix(x)

x[1,]

r5 = apply(x,1,AE)

mean(r5)


r5 = apply(x,1,AE1)

omega = seq(0,1,0.01)



alpha.vec = alpha.mat[Melb,]

AE1 = function(Z.vec){
  check1 = 0
  for(i in 1:(length(Z.vec) - 1)){
    if(Z.vec[i] >= Z.vec[i + 1]){
      check1 = 1
    }
  }
  if(check1 == 0){
    n = length(alpha.vec)
    b1 = m.beta(alpha.vec)
    out = ((2^(-(n-1)))/b1) * (Z.vec[1])^(alpha.vec[1]/2 - 1)
    out1 = 1 - sqrt(Z.vec[1])
    for(i in 2:(n-1)){
      out = out * (Z.vec[i] - Z.vec[i - 1])^(alpha.vec[i]/2 - 1)
      out1 = out1 - sqrt(Z.vec[i] - Z.vec[i - 1])
    }
    if(out1 < 0){
      out2 = 0
    }else{
      out2 = out * out1^(alpha.vec[n] - 1) 
    }
  }else{
    out2 = 0
  }
  return(out2)
}


Q1 = rep(0, length(omega))
for(i in 1:length(omega)){
  x = matrix(0, nrow = 10^7, ncol = length(alpha.vec) - 1)
  for(j in 1:(ncol(x))){
    x[,j] = runif(nrow(x))
  }
  x[,length(alpha.vec) - 1] = runif(10^7,0,omega[i])
  r5 = apply(x,1,AE1)
  Q1[i] = mean(omega[i] * r5)
  print(i)
}

alpha.vec = alpha.mat[Melb,]


h1 = function(Z.vec){
  n = length(Z.vec)
  b1 = m.beta(alpha.vec)
  check1 = 0
  if(Z.vec[n] > omega1){
    check1 = 1
  }
  if(check1 == 0){
    out = ((2^(-n))/b1) * (Z.vec[1])^(alpha.vec[1]/2 - 1)
    for(i in 2:n){
      out = out * (Z.vec[i] - Z.vec[i - 1])^(alpha.vec[i]/2 - 1)
    }
  }else{
    out = 0
  }
  return(out)
}


p1 = function(Z.vec){
  n = length(Z.vec)
  b1 = m.beta(alpha.vec)
  out = ((2^(-n))/b1) * (Z.vec[1])^(alpha.vec[1]/2 - 1)
  for(i in 2:n){
    out = out * (Z.vec[i] - Z.vec[i - 1])^(alpha.vec[i]/2 - 1)
  }
  return(out)
}


g1 = function(Z.vec){
  out = h1(Z.vec) / p1(Z.vec)
  return(out)
}

alpha.vec = alpha.mat[i,]

m.beta.e = function(alpha.vec){
  den = m.beta(alpha.vec)
  num = 0
  for(i in 1:length(alpha.vec)){
    e1 = rep(0, length(alpha.vec))
    e1[i] = 1
    num = num + m.beta(alpha.vec + 2 * e1)
  }
  out = num/den
  return(out)
}

m.beta.e(alpha.mat[j,])

m.beta.v = function(alpha.vec){
  den1 = m.beta(alpha.vec)
  num2 = 0
  e1 = rep(0, length(alpha.vec))
  e1[length(alpha.vec)] = 1
  num1 = m.beta(alpha.vec + 4 * e1)
  for(i in 1:(length(alpha.vec) - 1)){
    e1 = rep(0, length(alpha.vec))
    e1[i] = 1
    num1 = num1 + m.beta(alpha.vec + 4 * e1)
    for(j in (i + 1):length(alpha.vec)){
      e2 = rep(0, length(alpha.vec))
      e2[j] = 1
      num2 = num2 + m.beta(alpha.vec + 2 * e1 + 2 * e2)
    }
  }
  out = num1/den1 + 2 * num2/den1 - m.beta.e(alpha.vec)^2
  return(out)
}

m.beta.v(alpha.mat[Melb,])

omega = seq(0, 1, length.out = 1000)
QMelb = rep(0, length(omega))
for(i in 1:length(omega)){
  omega1 = omega[i]
  x = rdirichlet(10^5, alpha.mat[Melb,])
  y = x^2
  z = rowSums(y)
  r5 = (z <= omega1)
  QMelb[i] = mean(r5)
  print(i)
}

omega = seq(0, 1, length.out = 1000)
QDar = rep(0, length(omega))
for(i in 1:length(omega)){
  omega1 = omega[i]
  x = rdirichlet(10^5, alpha.mat[Dar,])
  y = x^2
  z = rowSums(y)
  r5 = (z <= omega1)
  QDar[i] = mean(r5)
  print(i)
}


omega1 = 0.5 - 6 * omega + log(12)
omega1[order(abs(QDar - 0.025))[1]]

sim.d = apply(rdirichlet(10^5, alpha.mat[Dar,]), 1, entropy1)
sim.m = apply(rdirichlet(10^5, alpha.mat[Melb,]), 1, entropy1)

qd = ecdf(sim.d)
qm = ecdf(sim.m)

sim.dd = rnorm(10^5, mean = mu.1[Dar], sd = sigma.1[Dar])
sim.md = rnorm(10^5, mean = mu.1[Melb], sd = sigma.1[Melb])

qdd = ecdf(sim.dd)
qmd = ecdf(sim.md)

Q2 = data.frame(omega = rep(0.5 - 6 * omega + log(12), 6),
                CDF = c(1 - QDar, 1 - QMelb, qd(0.5 - 6 * omega + log(12)), qm(0.5 - 6 * omega + log(12)),
                        qdd(0.5 - 6 * omega + log(12)), qmd(0.5 - 6 * omega + log(12))),
                Location = rep(c("Darwin", "Melbourne", "Darwin", "Melbourne", "Darwin", "Melbourne"), each = length(QMelb)),
                Type = factor(rep(c("Taylor's Approximation", "Simulation", "Delta Method"), each = 2 * length(QMelb)),
                levels = c("Simulation","Taylor's Approximation", "Delta Method")))


Q2 = Q2[Q2$omega > 0.5 & Q2$omega < 3,]

recdf(qd, 10)




ggplot(Q2) + geom_line(aes(x = omega, y = CDF), size = 0.8) + facet_grid(Location ~ Type) +
  labs(x = "Value", y = "CDF", title = "Entropy Distributions") + theme_bw() + 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12),
        strip.text.y = element_text(size = 12, colour = "black", angle = 90),
        strip.text.x = element_text(size = 12))




pb <- txtProgressBar(min = 1, max = 100 * nrow(alpha.mat), style = 3)
k = 1
Q3 = list()
for(j in 1:nrow(alpha.mat)){
  Q1 = rep(0, 100)
  omega = sort(rnorm(100, mean = m.beta.e(alpha.mat[j,]), sd = 2 * sqrt(m.beta.v(alpha.mat[j,]))))
  for(i in 1:length(omega)){
    omega1 = omega[i]
    x = rdirichlet(10^5, alpha.mat[j,])
    y = x^2
    z = rowSums(y)
    r5 = (z <= omega1)
    Q1[i] = mean(r5)
    setTxtProgressBar(pb, k)
    k = k + 1
  }
  Q3[[j]] = cbind(Q1, omega)
}



v1 = rep(0, length(P.list))
for(i in 1:nrow(alpha.mat)){
  a1 = dirichlet.mle(P.list[[i]][1:21,])$alpha
  a2 = dirichlet.mle(P.list[[i]][22:42,])$alpha
  v1[i] = m.beta.v(a1)/m.beta.v(a2)
  print(i)
}

v1


Q4 = list()
for(i in (j + 1):length(Q3)){
  Q4[[i]] = data.frame(CDF = 1 - Q3[[i]][,1], Omega = 0.5 - 6 * Q3[[i]][,2] + log(12))
}


quant = rep(0, length(Q4))
for(i in (j + 1):length(Q4)){
  quant[i] = Q4[[i]][order(abs(Q4[[i]][,1] - 0.1))[1], 2]
}

Ext.df = data.frame(Ext = quant > E1[,41], Lat = NOAA.aus[[1]]$Lat, Lon = NOAA.aus[[1]]$Lon)

Ext.df[c(order(distm(Ext.df[,3:2], c(148, -37)))[1:5],
         order(distm(Ext.df[,3:2], c(149, -36)))[1:5],
         order(distm(Ext.df[,3:2], c(150, -37)))[1:10]),1] = 1


idw.entext = idw(formula = Ext ~ 1, locations = ~Lon + Lat, data = Ext.df, newdata = Austra2, idp = 3)

idw.entext = idw.entext[round(idw.entext$var1.pred) == 1,]

g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(idw.entext, mapping = aes(x = Lon, y = Lat, colour = as.factor(round(var1.pred))), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
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
g1




