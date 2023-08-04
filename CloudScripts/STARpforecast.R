M.all = NULL
for(i in length(Austra):1){
  M.all = rbind(M.all, Austra[[i]][,3])
  if(i %% 100 == 0){
    print(i)
  }
}

m1 = sample(1:ncol(M.all), size = 500)
M.all1 = M.all[,m1]
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], col = "blue", cex = 0.5, pch = 19)

period = 1
p.lag = 12
k = 16
M0 = NULL
for(i in 0:k){
  M0 = c(M0, M.all1[i * 12 + period,1])
}

k = 16
W.list = list()
for(i in 1:p.lag){
  W.list[[i]] = matrix(0,ncol = ncol(M.all1), nrow = ncol(M.all1))
}

for(q in 1:ncol(M.all1)){
  M0 = NULL
  for(i in 0:k){
    M0 = c(M0, M.all1[i * 12 + period, q])
  }
  for(l in 2:(p.lag + 1)){
    M2 = NULL
    for(i in 1:ncol(M.all1)){
      M1 = NULL
      for(j in 0:k){
        M1 = c(M1, M.all1[j * 12 + l, i])
      }
      M2 = cbind(M2, M1)
    }
    M3 = NULL
    for(i in 1:ncol(M2)){
      M3 = c(M3, cor(M0,M2[,i]))
    }
    W.list[[l-1]][q, which(M3 >= sort(M3, decreasing = TRUE)[10])] = M3[which(M3 >= sort(M3, decreasing = TRUE)[10])]/sum(M3[which(M3 >= sort(M3, decreasing = TRUE)[10])])
  }
  print(q)
}

Lmult = function(a.vec, A.list, B.list){
  L.out = matrix(0, nrow = nrow(A.list[[1]]))
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}

period.1 = period
Y = M.all1[period.1,]

Y.list = list()
for(i in 1:p.lag){
  Y.list[[i]] = M.all1[i,]
}

C = NULL
for(i in 1:length(Y.list)){
  C = rbind(C, Y.list[[i]])
}
#Omega = cov(C)
#Omega.inv = inv(Omega)

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

X.ti = rep(1, ncol(M.all1))
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

c1 = NULL
p = p.lag
beta.hat = 0
phi.hat = rep(0, p.lag)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:300){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:p){
    phi.hat[j] = (W1.list[[j]] + W2.list[[j]] - beta.hat * W3.list[[j]]
                  - beta.hat * W4.list[[j]] - t(Lmult(phi.hat[-j], W.list[-j], Y.list[-j])) %*% W5.list[[j]]
                  - W6.list[[j]] %*% Lmult(phi.hat[-j], W.list[-j], Y.list[-j]))/(2 * D.list[[j]])
  }
  print(phi.hat[1])
  c1 = cbind(c1, phi.hat)
} 

Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
phi.hat
plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], colvar = Y, cex = 0.2, add = TRUE)
plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], colvar = Y.hat, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], colvar = Y - Y.hat, cex = 0.2, add = TRUE)


ph1 = c(-0.013013916,  0.104243206,  0.062962632,  0.246374883, -0.070202441,  0.016104680, -0.003665514, -0.109519744,  0.836770798,  0.097072494, -0.415248166, -0.253020524)
ph2 = c(0.001380339,  0.093516650,  0.068206064,  0.073472774, -0.055261602,  0.007347431,  0.155077093, -0.153240560,  0.467429573,  0.104516826, -0.205871664, -0.241908395)
ph3 = c(0.398256279,  0.212562847,  0.030001793,  0.099171575, -0.145706706, -0.053595410, -0.003527051, -0.260393708, -0.283650673, -0.070933816, -0.427553838, -0.490832619)
ph4 = c(0.23139547, -0.41558235,  0.10509657, -0.73989914,  0.25499001, -0.02480982, -0.16647685, -0.42515538,  0.18132118, -0.03961991,  0.01844377,  0.01846143)
ph5 = c(-0.43058292,  0.16580131,  0.03059664, -0.01237877,  0.11361190,  0.01803470,  0.15329473, -0.30942449, -1.11108185, -0.28035444, -0.81376297,  0.38380597)
ph6 = c(-0.18451332,  0.07674981, -0.06556098, -0.20854707, -0.08953160,  0.23586070,  0.11672385,  0.01333223, -0.31087318,  0.50572410, -0.38933486, -0.11404839)
ph7 = c(-0.2719144261,  0.5877656475, -0.1704825271, -0.0516588065, -0.0008301768,  0.0268115454,  0.1395835971, -0.6036326882, -0.7065124185,  0.2851975553, -0.1684970944, -0.3101256671)
ph8 = c(-0.15116766,  0.46342129,  0.05371556,  0.16905822, -0.08837098,  0.01606209, -0.09167285,  0.01483371,  0.51578817, -0.28206905, -0.02944817, -0.13081774)
ph9 = c(-0.702524286, -0.051166370,  0.047965160,  0.175154110, -0.001203029,  0.018447142,  0.341040111, -0.185404348, -0.812857852, -0.206426843, -0.453326533, -0.351993765)
ph10 = c(0.20016190,  0.09229870, -0.01536506,  0.05904327, -0.06088205, -0.07078724,  0.01928412,  0.25155623,  0.43111253,  0.01833574, -0.18184405, -0.16984400)
ph11 = c(-0.46774359,  0.12457267,  0.07339942,  1.07381872,  0.14385973, -0.05030161, -0.24768410, -0.23356702, -0.70449966, -0.09287523, -0.46468913, -0.11403873)


Ph = rbind(ph1, ph2, ph3, ph4, ph5, ph6, ph7, ph8, ph9, ph10, ph11)




