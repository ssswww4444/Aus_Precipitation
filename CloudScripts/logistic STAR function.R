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

Lmult1 = function(a.vec, A.list, B.list, j){
  L.out = 0
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * t(A.list[[i]][j, ]) %*% B.list[[i]]
  }
  L.out
}

Lmult = function(a.vec, A.list, B.list){
  L.out = 0
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}

l.hood = function(pt, Yt, M.list, X){
  l.list = list()
  for(i in 1:length(Yt)){
    N1 = NULL
    for(j in 1:ncol(M.list)){
      N1 = c(N1, M.list[i,j] * (Yt[i] - pt[i]))
    }
    N1 = c(N1, X[i,] * (Yt[i] - pt[i]))
    l.list[[i]] = N1
  }
  out = ladd(l.list)
  out
}

Fisher.I = function(pt, Yt, M.list, X){
  f.list = list()
  for(i in 1:length(Yt)){
    N1 = matrix(0, ncol(M.list), ncol(M.list))
    for(j in 1:(ncol(M.list) - 1)){
      for(k in (j + 1):ncol(M.list)){
        N1[j,k] = -(M.list[i,j]) * (M.list[i,k]) * pt[i] * (1 - pt[i])
      }
    }
    N1 = N1 + t(N1)
    n1 = NULL
    for(j in 1:ncol(M.list)){
      n1 = c(n1, -(M.list[i,j])^(2) * pt[i] * (1 - pt[i]))
    }
    diag(N1) = n1
    N2 = matrix(0, ncol(X), ncol(X))
    for(j in 1:(ncol(X) - 1)){
      for(k in (j + 1):ncol(X)){
        N2[j,k] = -X[i,j] * X[i,k] * pt[i] * (1 - pt[i])
      }
    }
    N2 = N2 + t(N2)
    n2 = NULL
    for(j in 1:ncol(X)){
      n2 = c(n2, -X[i,j]^2 * pt[i] * (1 - pt[i]))
    }
    diag(N2) = n2
    N3 = matrix(0, nrow = ncol(M.list), ncol = ncol(X))
    for(j in 1:ncol(M.list)){
      for(k in 1:ncol(X)){
        N3[j,k] = -(M.list[i,j]) * X[i,k] * pt[i] * (1 - pt[i])
      }
    }
    N4 = rbind(N1, t(N3))
    N5 = rbind(N3, N2)
    N6 = cbind(N4, N5)
    f.list[[i]] = N6
  }
  out = ladd(f.list)
  out
}

Fisher.I(pt, Y, M.list, X.ti)

LF = Fisher.I(pt, Y, M.list, X.ti)
det(LF)
solve(LF)

cor.list = list()
cor.pos.list = list()
for(l in 1:1){
  cor1.list = list()
  cor1.pos.list = list()
  for(k in 1:18){
    n = M[[l]]
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
    cor1_12.Mat = NULL
    cor.Mat = NULL
    for(i in m.11){
      cor.1 = NULL
      cor.Gr = NULL
      cor.pos = NULL
      for(j in Gr1){
        cor.Gr = c(cor.Gr, cor(A.all[n,i], A.all[m,j]))
      }
      x1 = sort(cor.Gr, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Gr1[which(cor.Gr == x1[1])[1]], Gr1[which(cor.Gr == x1[2])[1]], Gr1[which(cor.Gr == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Te = NULL
      for(j in Te1){
        cor.Te = c(cor.Te, cor(A.all[n,i], A.all[m,j]))
      }
      x1 = sort(cor.Te, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Te1[which(cor.Te == x1[1])[1]], Te1[which(cor.Te == x1[2])[1]], Te1[which(cor.Te == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Tr = NULL
      for(j in Tr1){
        cor.Tr = c(cor.Tr, cor(A.all[n,i], A.all[m,j]))
      }
      x1 = sort(cor.Tr, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Tr1[which(cor.Tr == x1[1])[1]], Tr1[which(cor.Tr == x1[2])[1]], Tr1[which(cor.Tr == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Eq = NULL
      for(j in Eq1){
        cor.Eq = c(cor.Eq, cor(A.all[n,i], A.all[m,j]))
      }
      x1 = sort(cor.Eq, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Eq1[which(cor.Eq == x1[1])[1]], Eq1[which(cor.Eq == x1[2])[1]], Eq1[which(cor.Eq == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Su = NULL
      for(j in Su1){
        cor.Su = c(cor.Su, cor(A.all[n,i], A.all[m,j]))
      }
      x1 = sort(cor.Su, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Su1[which(cor.Su == x1[1])[1]], Su1[which(cor.Su == x1[2])[1]], Su1[which(cor.Su == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.De = NULL
      for(j in De1){
        cor.De = c(cor.De, cor(A.all[n,i], A.all[m,j]))
      }
      x1 = sort(cor.De, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(De1[which(cor.De == x1[1])[1]], De1[which(cor.De == x1[2])[1]], De1[which(cor.De == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor1_12.Mat = rbind(cor1_12.Mat, cor.1)
      cor.Mat = rbind(cor.Mat, cor.pos)
    }
    cor1.list[[k]] = cor1_12.Mat
    cor1.pos.list[[k]] = cor.Mat
    print(k)
  }
  cor.list[[l]] = cor1.list
  cor.pos.list[[l]] = cor1.pos.list
}


## Gr1
ind1 = NULL
for(i in Gr1){
  ind1 = c(ind1, which(m.11 == i))
}

W.list = list()
N2 = cor.pos.list[[11]]
N4 = cor.list[[11]]
for(i in 1:12){
  N1 = matrix(0, ncol = ncol(M.all), nrow = length(Y))
  N3 = N2[[i]]
  N5 = N4[[i]]
  k = 1
  for(j in ind1){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W.list[[i]] = N1
}

ind = unique(c(which(apply(W.list[[1]], 2, sum) > 0), which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
               which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0)))

for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

g = 1

Y = A.all[rev(m1)[g], Gr1]
Y.list = list()
k = 1
for(i in (rev(m1)[g] - 1):(rev(m1)[g] - 12)){
  Y.list[[k]] = A.all[i,ind]
  k = k + 1
}

beta.hat = c(0, rep(0, ncol(X.ti) - 1))
phi.hat = rep(0, length(W.list))
theta.hat = c(phi.hat, beta.hat)
M.list = NULL
for(i in 1:length(W.list)){
  M.list = cbind(M.list, W.list[[i]] %*% Y.list[[i]])
}
pt = rep(0, length(Y))
c1 = NULL
for(i in 1:1){
  for(j in 1:length(Y)){
    pt[j] = exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat))
  }
  l.h = l.hood(pt, Y, M.list, X.ti)
  LF = Fisher.I(pt, Y, M.list, X.ti)
  theta.hat = theta.hat - solve(LF) %*% l.h
  c1 = cbind(c1, theta.hat)
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  print(c(i, theta.hat[1]))
}
sum(round(pt)!=Y)
hist(pt)

g = 3

Y = A.all[rev(m1)[g], Gr1]
Y.list = list()
k = 1
for(i in (rev(m1)[g] - 1):(rev(m1)[g] - 12)){
  Y.list[[k]] = A.all[i,ind]
  k = k + 1
}

X.ti = cbind(rep(1, length(Gr1)), Austra[[1]][Gr1,1], Austra[[1]][Gr1,2],
             Austra[[1]][Gr1,1] * Austra[[1]][Gr1,2], Austra[[1]][Gr1, 9], Austra[[1]][Gr1, 10])

for(j in 1:length(Y)){
  pt[j] = exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat))
}

sum(round(pt)!=Y)

## Desert Koppen, month 1
ind1 = NULL
for(i in De1){
  ind1 = c(ind1, which(m.11 == i))
}

W.list = list()
N2 = cor.pos.list[[1]]
N4 = cor.list[[1]]
for(i in 1:12){
  N1 = matrix(0, ncol = ncol(A.all), nrow = length(De1))
  N3 = N2[[i]]
  N5 = N4[[i]]
  k = 1
  for(j in ind1){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W.list[[i]] = N1
}

ind = unique(c(which(apply(W.list[[1]], 2, sum) > 0), which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
               which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0),
               which(apply(W.list[[7]], 2, sum) > 0), which(apply(W.list[[8]], 2, sum) > 0), which(apply(W.list[[9]], 2, sum) > 0),
               which(apply(W.list[[10]], 2, sum) > 0), which(apply(W.list[[11]], 2, sum) > 0), which(apply(W.list[[12]], 2, sum) > 0)))

for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

g = 2

Y = A.all[rev(m1)[g], De1]
Y.list = list()
k = 1
for(i in (rev(m1)[g] - 1):(rev(m1)[g] - 12)){
  Y.list[[k]] = A.all[i,ind]
  k = k + 1
}

X.ti = cbind(rep(1, length(De1)), Austra[[1]][De1,1], Austra[[1]][De1,2],
             Austra[[1]][De1,1] * Austra[[1]][De1,2], Austra[[1]][De1, 9], Austra[[1]][De1, 10])

beta.hat = c(1, rep(0, ncol(X.ti) - 1))
phi.hat = c(1, rep(0, length(W.list) - 1))
theta.hat = c(phi.hat, beta.hat)
M.list = NULL
for(i in 1:length(W.list)){
  M.list = cbind(M.list, W.list[[i]] %*% Y.list[[i]])
}
pt = rep(0, length(Y))
c1 = NULL
for(i in 1:10){
  for(j in 1:length(Y)){
    pt[j] = exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat))
  }
  l.h = l.hood(pt, Y, M.list, X.ti)
  LF = Fisher.I(pt, Y, M.list, X.ti)
  theta.hat = theta.hat - solve(LF) %*% l.h
  c1 = cbind(c1, theta.hat)
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  print(c(i, theta.hat[1]))
  c1 = cbind(c1, theta.hat)
  print(sum(round(pt)!=Y))
}


scatter2D(Austra[[1]][De1,2], Austra[[1]][De1,1], colvar = pt, pch = 19, cex = 0.5)
hist(pt)

g = 3

Y = A.all[rev(m1)[g], De1]
Y.list = list()
k = 1
for(i in (rev(m1)[g] - 1):(rev(m1)[g] - 12)){
  Y.list[[k]] = A.all[i,ind]
  k = k + 1
}

X.ti = cbind(rep(1, length(De1)), Austra[[1]][De1,1], Austra[[1]][De1,2],
             Austra[[1]][De1,1] * Austra[[1]][De1,2], Austra[[1]][De1, 9], Austra[[1]][De1, 10])

for(j in 1:length(Y)){
  pt[j] = exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat))
}
plot(pt[round(pt)!=Y])
sum(round(pt)!=Y)
sum(round(pt)!=Y)/length(Y)

ind1 = NULL
for(i in De1){
  ind1 = c(ind1, which(m.11 == i))
}

W.list = list()
N2 = cor.pos.list[[1]]
N4 = cor.list[[1]]
for(i in 1:12){
  N1 = matrix(0, ncol = ncol(A.all), nrow = length(De1))
  N3 = N2[[i]]
  N5 = N4[[i]]
  k = 1
  for(j in ind1){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W.list[[i]] = N1
}

ind = unique(c(which(apply(W.list[[1]], 2, sum) > 0), which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
               which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0),
               which(apply(W.list[[7]], 2, sum) > 0), which(apply(W.list[[8]], 2, sum) > 0), which(apply(W.list[[9]], 2, sum) > 0),
               which(apply(W.list[[10]], 2, sum) > 0), which(apply(W.list[[11]], 2, sum) > 0), which(apply(W.list[[12]], 2, sum) > 0)))

for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

A4 = list()
for(j in 1:12){
  A5 = NULL
  for(i in 1:length(m1[-1])){
    A3 = cbind(matrix(0, nrow = nrow(W.list[[j]]), ncol = ncol(W.list[[j]])*(i - 1)), W.list[[j]], matrix(0, nrow = nrow(W.list[[j]]), ncol = ncol(W.list[[j]])*(length(m1[-1]) - i)))
    A5 = rbind(A5, A3)
    print(i)
  }
  A4[[j]] = A5
  print(j)
}

cbind(matrix(0, nrow = nrow(W.list[[1]]), ncol = 0), W.list[[1]])[1:5,1:5]

for(i in 1:12){
  W.list[[i]] = A4[[i]]
}

Y = NULL
for(i in m1[-1]){
  Y = c(Y, A.all[i,De1])
}


for(i in 1:ncol(Y.Mat)){
  Y.list[[i]] = Y.Mat[,i]
}

X.ti = cbind(rep(1, length(De1)), Austra[[1]][De1,1], Austra[[1]][De1,2],
             Austra[[1]][De1,1] * Austra[[1]][De1,2], Austra[[1]][De1, 9], Austra[[1]][De1, 10])

A1 = NULL
for(i in 1:length(m1[-1])){
  A1 = rbind(A1, X.ti)
}

beta.hat = c(1, rep(0, ncol(X.ti) - 1))
phi.hat = c(1, rep(0, length(W.list) - 1))
theta.hat = c(phi.hat, beta.hat)
M.list = NULL
for(i in 1:length(W.list)){
  M.list = cbind(M.list, W.list[[i]] %*% Y.list[[i]])
}
pt = rep(0, length(Y))
c1 = NULL
for(i in 1:10){
  for(j in 1:length(Y)){
    pt[j] = exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, Y.list, j) + t(X.ti[j,]) %*% beta.hat))
  }
  l.h = l.hood(pt, Y, M.list, X.ti)
  LF = Fisher.I(pt, Y, M.list, X.ti)
  theta.hat = theta.hat - solve(LF) %*% l.h
  c1 = cbind(c1, theta.hat)
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  print(c(i, theta.hat[1]))
  c1 = cbind(c1, theta.hat)
  print(sum(round(pt)!=Y))
}









ONI1[m1,]



scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = A.all[rev(m1)[1], ], cex = 0.2, pch = 19)

A1 = Austra[[229]]
ave1 = ifelse(A1$Ave, "Above Monthly Average", "Below Monthly Average")
A1$ave1 = ave1

ggplot(Austra[[229]], aes(x = Lon, y = Lat)) + geom_point(aes(colour = str_wrap(factor(Ave), 2)), size = 0.3) +
  scale_color_manual(labels = c("Below Monthly Average", "Above Monthly Average"), values=c("#CC0000", "#56B4E9")) + guides(colour = guide_legend(override.aes = list(size=18))) + theme_bw() + 
  theme(plot.title = element_text(size = 20, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13)) +
  labs(color = "Class", x = "Longitude", y = "Latitude", title = "March 2019 Classification")

ggplot(A1, aes(x = Lon, y = Lat)) + geom_point(aes(colour = str_wrap(factor(ave1), 2)), size = 0.5) +
  scale_color_manual(values=c( "#56B4E9","#CC0000")) + guides(colour = guide_legend(override.aes = list(size=18))) + theme_bw() + 
  theme(plot.title = element_text(size = 20, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13)) +
  labs(color = "Class", x = "Longitude", y = "Latitude", title = "March 2019 Classification")


ggplot(Austra[[1]], aes(x = Lon, y = Lat, color = KClass)) + geom_point(size = 0.5) +
  scale_color_brewer(palette = 5, type = "div") + guides(colour = guide_legend(override.aes = list(size=18))) + theme_bw() + 
  theme(plot.title = element_text(size = 20, face = "bold"), legend.title=element_text(size=15), legend.text=element_text(size=13)) +
  labs(color = "Class", x = "Longitude", y = "Latitude", title = "Australian Köppen Climate Classification")


ind1 = NULL
for(i in Gr1){
  ind1 = c(ind1, which(m.11 == i))
}

W.list = list()
N2 = cor.pos.list[[1]]
N4 = cor.list[[1]]
for(i in 1:12){
  N1 = matrix(0, ncol = ncol(A.all), nrow = length(Gr1))
  N3 = N2[[i]]
  N5 = N4[[i]]
  k = 1
  for(j in ind1){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W.list[[i]] = N1
}

ind = unique(c(which(apply(W.list[[1]], 2, sum) > 0), which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
               which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0),
               which(apply(W.list[[7]], 2, sum) > 0), which(apply(W.list[[8]], 2, sum) > 0), which(apply(W.list[[9]], 2, sum) > 0),
               which(apply(W.list[[10]], 2, sum) > 0), which(apply(W.list[[11]], 2, sum) > 0), which(apply(W.list[[12]], 2, sum) > 0)))

for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

g = 19
Y = list()
k = 1
for(i in m1[-c(1,g)]){
  Y[[k]] = A.all[i,Gr1]
  k = k + 1
}

Y.list = list()
k = 1
for(i in m1[-c(1,g)]){
  A.list = list()
  for(j in 1:12){
    A.list[[j]] = A.all[i - j, ind]
  }
  Y.list[[k]] = A.list
  k = k + 1
}

M.list = list()
for(i in 1:length(m1[-c(1,g)])){
  A1 = Y.list[[i]]
  A2 = NULL
  for(j in 1:length(W.list)){
    A2 = cbind(A2, W.list[[j]] %*% A1[[j]])
  }
  M.list[[i]] = A2
}

X.ti = cbind(rep(1, length(Gr1)), Austra[[1]][Gr1,1], Austra[[1]][Gr1,2],
             Austra[[1]][Gr1,1] * Austra[[1]][Gr1,2], Austra[[1]][Gr1, 9], Austra[[1]][Gr1, 10])

beta.hat = c(1, rep(0, ncol(X.ti) - 1))
phi.hat = c(1, rep(0, length(W.list) - 1))
theta.hat = c(phi.hat, beta.hat)
pt = list()
for(i in 1:length(m1[-c(1,g)])){
  pt[[i]] = rep(0, length(Y[[1]]))
}
c1 = NULL
for(i in 1:15){
  for(k in 1:length(pt)){
    for(j in 1:length(Y[[1]])){
      pt[[k]][j] = exp(Lmult1(phi.hat, W.list, Y.list[[k]], j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, Y.list[[k]], j) + t(X.ti[j,]) %*% beta.hat))
    }
  }
  l.h = rep(0, length(theta.hat))
  for(j in 1:length(M.list)){
    l.h = l.h + l.hood(pt[[j]], Y[[j]], M.list[[j]], X.ti)
  }
  LF = matrix(0, length(theta.hat), length(theta.hat))
  for(j in 1:length(M.list)){
    LF = LF + Fisher.I(pt[[j]], Y[[j]], M.list[[j]], X.ti)
  }
  theta.hat = theta.hat - solve(LF) %*% l.h
  c1 = cbind(c1, theta.hat)
  phi.hat = theta.hat[1:length(phi.hat)]
  beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
  print(c(i, theta.hat[1]))
  c1 = cbind(c1, theta.hat)
}

a1 = NULL
for(i in 1:length(pt)){
  a1 = c(a1, sum(round(pt[[i]]) != Y[[i]]))
}

plot(a1)

Y1 = list()
k = 1
for(i in m1[g + 1]){
  Y1[[k]] = A.all[i,Gr1]
  k = k + 1
}

Y1.list = list()
k = 1
for(i in m1[g + 1]){
  A.list = list()
  for(j in 1:12){
    A.list[[j]] = A.all[i - j, ind]
  }
  Y1.list[[k]] = A.list
  k = k + 1
}

A1 = Y1.list[[1]]
pt1 = rep(0, length(Y1[[1]]))
for(j in 1:length(Y1[[1]])){
  pt1[j] = exp(Lmult1(phi.hat, W.list, A1, j) + t(X.ti[j,]) %*% beta.hat)/(1 + exp(Lmult1(phi.hat, W.list, A1, j) + t(X.ti[j,]) %*% beta.hat))
}

sum(round(pt1)!=Y1[[1]])

pSu = pt1






#IRLS

yt = A.all[m1[20],Su1]
X.ti = cbind(rep(1, length(Su1)), Austra[[1]][Su1,1], Austra[[1]][Su1,2],
             Austra[[1]][Su1,1] * Austra[[1]][Su1,2], Austra[[1]][Su1, 9], Austra[[1]][Su1, 10])



ind1 = NULL
for(i in Su1){
  ind1 = c(ind1, which(m.11 == i))
}

W.list = list()
N2 = cor.pos.list[[1]]
N4 = cor.list[[1]]
for(i in 1:12){
  N1 = matrix(0, ncol = ncol(A.all), nrow = length(Su1))
  N3 = N2[[i]]
  N5 = N4[[i]]
  k = 1
  for(j in ind1){
    N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
    k = k + 1
  }
  W.list[[i]] = N1
}

Y.list = list()
k = 1

A.list = list()
for(j in 1:12){
  A.list[[j]] = A.all[229 - j, ]
}


Wt = NULL
for(i in 1:length(yt)){
  a1 = NULL
  for(j in 1:length(W.list)){
    a1 = c(a1, W.list[[j]][i,] %*% A.list[[j]])
  }
  Wt = rbind(Wt, a1)
}

X.ti1 = cbind(Wt, X.ti)


beta.hat = rep(0, ncol(X.ti))
phi.hat = rep(0, ncol(Wt))
theta.hat = c(phi.hat, beta.hat)
theta.hat1 = NULL
pt1 = rep(0, length(yt))
for(i in 1:50){
  for(j in 1:length(yt)){
    pt1[j] = 1/(1 + exp(-t(X.ti1[j,]) %*% theta.hat))
  }
  S = diag(pt1 * (1 - pt1))
  theta.hat = solve(t(X.ti1) %*% S %*% X.ti1) %*% t(X.ti1) %*% (S %*% X.ti1 %*% theta.hat + yt - pt1)
  theta.hat1 = cbind(theta.hat1, theta.hat)
}










