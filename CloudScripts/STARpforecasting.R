M.all = NULL
for(i in 1:length(Austra)){
  M.all = rbind(M.all, Austra[[i]][,3])
  if(i %% 100 == 0){
    print(i)
  }
}

c1 = NULL
for(i in 2:ncol(M.all)){
  c1 = c(c1, cor(M.all[,1], M.all[,i]))
}

par(mfrow = c(1,1))
plot(c1, cex = 0.2)

c2 = NULL
for(i in 1:nrow(M.all)){
  c2 = c(c2, cor(M.all[1,], M.all[i,]))
}
plot(c2, cex = 0.2, type = "l")

c3 = NULL
for(i in 1:nrow(M.all)){
  c3 = c(c3, cor(M.all[225,], M.all[i,]))
}
plot(c3, cex = 0.2, type = "l")

rev(c3)
c3
t1 = as.ts(rev(c3)[-1])
t1
a1 = ar(t1, method = "ols")
a1

d1 = apply(M.all,1,sum)
length(d1)
plot(d1, cex = 0.2, type = "l")

M.all = NULL
for(i in 1:length(Austra)){
  M.all = rbind(M.all, Austra[[i]][,3])
  if(i %% 100 == 0){
    print(i)
  }
}

m.11 = sample(1:ncol(M.all), size = 1000)
scatter2D(Austra[[1]][m.11,2], Austra[[1]][m.11,1], col = "blue", cex = 0.5, pch = 19)


k = 16
M0 = NULL
for(i in 0:k){
  M0 = c(M0, M.all1[i * 12 + 1,1])
}

k = 16
W.list = list()
for(i in 1:24){
  W.list[[i]] = matrix(0,ncol = ncol(M.all1), nrow = ncol(M.all1))
}

for(q in 1:ncol(M.all1)){
  M0 = NULL
  for(i in 0:k){
    M0 = c(M0, M.all1[i * 12 + 1, q])
  }
  for(l in 2:25){
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

Y = M.all1[13,]

Y.list = list()
for(i in 1:12){
  Y.list[[i]] = M.all1[i,]
}

W1.list = list()
for(i in 1:12){
  W1.list[[i]] = t(Y) %*% W.list[[i]] %*% Y.list[[i]]
}

W2.list = list()
for(i in 1:12){
  W2.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% Y
}

D.list = list()
for(i in 1:12){
  D.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% W.list[[i]] %*% Y.list[[i]]
}

X.ti = matrix(rep(1, length(samp1)), ncol = 1)
W3.list = list()
for(i in 1:12){
  W3.list[[i]] = X.ti %*% W.list[[i]] %*% Y.list[[i]]
}

W4.list = list()
for(i in 1:12){
  W4.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% X.ti
}

W5.list = list()
for(i in 1:12){
  W5.list[[i]] = W.list[[i]] %*% Y.list[[i]]
}

W6.list = list()
for(i in 1:12){
  W6.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]])
}

c1 = NULL
p = 12
beta.hat = 0
phi.hat = rep(0, 12)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:1000){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:p){
    phi.hat[j] = (W1.list[[j]] + W2.list[[j]] - beta.hat * W3.list[[j]]
                  - beta.hat * W4.list[[j]] - t(Lmult(phi.hat[-j], W.list[-j], Y.list[-j])) %*% W5.list[[j]]
                  - W6.list[[j]] %*% Lmult(phi.hat[-j], W.list[-j], Y.list[-j]))/(2 * D.list[[j]])
    print(c(i,j))
  }
  print(phi.hat[1])
  c1 = cbind(c1, phi.hat)
} 

Y.hat = beta.hat * X.ti + Lmult(phi.hat, W.list, Y.list)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], colvar = Y, cex = 0.2, add = TRUE)
plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], colvar = Y.hat, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][m1,2], Austra[[1]][m1,1], colvar = Y - Y.hat, cex = 0.2, add = TRUE)



dim(M.all1)

mod1 = lm(rev(M.all1[m1,1]) - mean(M.all1[m1,1]) ~ IOD1[m1,2])
mod2 = lm(rev(M.all1[m2,1]) - mean(M.all1[m2,1]) ~ IOD1[m2,2])

mod1.coef = NULL
for(i in 1:length(m11)){
  mod1.coef = rbind(mod1.coef, lm(rev(M.all1[m5,i]) - mean(M.all1[m5,i]) ~ IOD1[m5,2])$coefficients)
}

scatter2D(Austra[[1]][m11,2], Austra[[1]][m11,1], colvar = mod1.coef[,2], cex = 0.3, pch = 19)

M = list()
M[[1]] = m1
M[[2]] = m2
M[[3]] = m3
M[[4]] = m4
M[[5]] = m5
M[[6]] = m6
M[[7]] = m7
M[[8]] = m8
M[[9]] = m9
M[[10]] = m10
M[[11]] = m11
M[[12]] = m12

IOD.coef = list()
for(i in 1:length(M)){
  mod1.coef = NULL
  for(j in 1:length(m.11)){
    mod1.coef = rbind(mod1.coef, lm(rev(M.all1[M[[i]],j]) - mean(M.all1[M[[i]],j]) ~ IOD1[M[[i]],2])$coefficients)
  }
  IOD.coef[[i]] = mod1.coef
}

SOI.coef = list()
for(i in 1:length(M)){
  mod1.coef = NULL
  for(j in 1:length(m.11)){
    mod1.coef = rbind(mod1.coef, lm(rev(M.all1[M[[i]],j]) - mean(M.all1[M[[i]],j]) ~ SOI1[M[[i]],2])$coefficients)
  }
  SOI.coef[[i]] = mod1.coef
}

scatter2D(Austra[[1]][m.11,2], Austra[[1]][m.11,1], colvar = SOI.coef[[12]][,2], cex = 0.3, pch = 19)

head(SeaIce)

SI.coef = list()
for(i in 1:length(M)){
  mod1.coef = NULL
  for(j in 1:length(m.11)){
    mod1.coef = rbind(mod1.coef, lm(rev(M.all1[M[[i]],j]) - mean(M.all1[M[[i]],j]) ~ SeaIce[M[[i]],5])$coefficients)
  }
  SI.coef[[i]] = mod1.coef
}

scatter2D(Austra[[1]][m.11,2], Austra[[1]][m.11,1], colvar = SI.coef[[2]][,2], cex = 0.3, pch = 19)


cor1 = NULL
for(j in 1:length(m.11)){
  cor1 = c(cor1, cor(rev(M.all1[m1[-1],1]), rev(M.all1[m2[-1],j])))
}

Tr1 = NULL
for(i in 1:length(m.11)){
  if(length(which(Tr == m.11[i])) > 0){
    Tr1 = c(Tr1, m.11[i])
  }
}

Te1 = NULL
for(i in 1:length(m.11)){
  if(length(which(Te == m.11[i])) > 0){
    Te1 = c(Te1, m.11[i])
  }
}

Su1 = NULL
for(i in 1:length(m.11)){
  if(length(which(Su == m.11[i])) > 0){
    Su1 = c(Su1, m.11[i])
  }
}

De1 = NULL
for(i in 1:length(m.11)){
  if(length(which(De == m.11[i])) > 0){
    De1 = c(De1, m.11[i])
  }
}

Eq1 = NULL
for(i in 1:length(m.11)){
  if(length(which(Eq == m.11[i])) > 0){
    Eq1 = c(Eq1, m.11[i])
  }
}

Gr1 = NULL
for(i in 1:length(m.11)){
  if(length(which(Gr == m.11[i])) > 0){
    Gr1 = c(Gr1, m.11[i])
  }
}

cor.list = list()
cor.pos.list = list()
for(l in 1:1){
  cor1.list = list()
  cor1.pos.list = list()
  indices = c(l:1,12:(l+1))[-1]
  if(max(indices) == 13){
    indices = indices[-length(indices)]
  }
  for(k in indices){
    n = M[[l]]
    m = M[[k]]
    if(max(m) >= max(n)){
      m = m[-length(m)]
    }
    if(length(m) < length(n)){
      n = n[-1]
    }
    cor1_12.Mat = NULL
    cor.Mat = NULL
    for(i in m.11){
      cor.1 = NULL
      cor.Gr = NULL
      cor.pos = NULL
      for(j in Gr1){
        cor.Gr = c(cor.Gr, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Gr, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Gr1[which(cor.Gr == x1[1])], Gr1[which(cor.Gr == x1[2])], Gr1[which(cor.Gr == x1[3])])
      cor.pos = c(cor.pos, x2)
      cor.Te = NULL
      for(j in Te1){
        cor.Te = c(cor.Te, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Te, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Te1[which(cor.Te == x1[1])], Te1[which(cor.Te == x1[2])], Te1[which(cor.Te == x1[3])])
      cor.pos = c(cor.pos, x2)
      cor.Tr = NULL
      for(j in Tr1){
        cor.Tr = c(cor.Tr, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Tr, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Tr1[which(cor.Tr == x1[1])], Tr1[which(cor.Tr == x1[2])], Tr1[which(cor.Tr == x1[3])])
      cor.pos = c(cor.pos, x2)
      cor.Eq = NULL
      for(j in Eq1){
        cor.Eq = c(cor.Eq, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Eq, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Eq1[which(cor.Eq == x1[1])], Eq1[which(cor.Eq == x1[2])], Eq1[which(cor.Eq == x1[3])])
      cor.pos = c(cor.pos, x2)
      cor.Su = NULL
      for(j in Su1){
        cor.Su = c(cor.Su, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Su, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Su1[which(cor.Su == x1[1])], Su1[which(cor.Su == x1[2])], Su1[which(cor.Su == x1[3])])
      cor.pos = c(cor.pos, x2)
      cor.De = NULL
      for(j in De1){
        cor.De = c(cor.De, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.De, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(De1[which(cor.De == x1[1])], De1[which(cor.De == x1[2])], De1[which(cor.De == x1[3])])
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




rev(m1[-1])

i = 3

c(i:1,12:(i+1))[-1]


Lmult = function(a.vec, A.list, B.list){
  L.out = matrix(0, nrow = nrow(A.list[[2]]))
  for(i in 2:length(A.list)){
    L.out = L.out + a.vec[i - 1] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}

Lmult(phi.hat, W.list, Y.list)

ind = unique(c(which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
  which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0),
  which(apply(W.list[[7]], 2, sum) > 0), which(apply(W.list[[8]], 2, sum) > 0), which(apply(W.list[[9]], 2, sum) > 0),
  which(apply(W.list[[10]], 2, sum) > 0), which(apply(W.list[[11]], 2, sum) > 0), which(apply(W.list[[12]], 2, sum) > 0)))

W.list = list()
N2 = cor.pos.list[[1]]
for(i in 2:12){
  N1 = matrix(0, ncol = ncol(M.all), nrow = length(Y))
  N3 = N2[[i]]
  for(j in 1:length(Y)){
    N1[j,N3[j,]] = 1/ncol(N3)
  }
  W.list[[i]] = N1
}

g = 4

Y = sqrt(M.all[rev(m1[-1])[g], Gr1])
Y.list = list()
k = 2
for(i in (rev(m1[-1])[g] - 1):(rev(m1[-1])[g] - 11)){
  Y.list[[k]] = sqrt(M.all[i,ind])
  k = k + 1
}

N1 = matrix(0, nrow = length(Y.list[[1]]), ncol = length(Y))

for(i in 2:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

W1.list = list()
for(i in 2:12){
  W1.list[[i]] = t(Y) %*% W.list[[i]] %*% Y.list[[i]]
}

W2.list = list()
for(i in 2:12){
  W2.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% Y
}

D.list = list()
for(i in 2:12){
  D.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% W.list[[i]] %*% Y.list[[i]]
}

X.ti = cbind(rep(1, length(Gr1)), Austra[[1]][Gr1,1], Austra[[1]][Gr1,2],
             Austra[[1]][Gr1,1] * Austra[[1]][Gr1,2], Austra[[1]][Gr1, 9], Austra[[1]][Gr1, 10],
             ONI.coef[,1] + ONI.coef[,2] * rev(ONI.Mat[m1,2])[g], IOD.coef[,1] + IOD.coef[,2] * rev(IOD.Mat[m1,2])[g],
             (ONI.coef[,1] + ONI.coef[,2] * rev(ONI.Mat[m1,2])[g]) * (IOD.coef[,1] + IOD.coef[,2] * rev(IOD.Mat[m1,2])[g]))
W3.list = list()
for(i in 2:12){
  W3.list[[i]] = t(X.ti) %*% W.list[[i]] %*% Y.list[[i]]
}

W4.list = list()
for(i in 2:12){
  W4.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% X.ti
}

W5.list = list()
for(i in 2:12){
  W5.list[[i]] = W.list[[i]] %*% Y.list[[i]]
}

W6.list = list()
for(i in 2:12){
  W6.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]])
}

c1 = NULL
p = 12
beta.hat = matrix(rep(0, ncol(X.ti)), ncol = 1)
phi.hat = rep(0, p - 1)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:1000){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:(p-1)){
    phi.hat[j] = (W1.list[[j + 1]] + W2.list[[j + 1]] - t(beta.hat) %*% W3.list[[j + 1]]
                  - W4.list[[j + 1]] %*% beta.hat - t(Lmult(phi.hat[-j], W.list[-(j + 1)], Y.list[-(j + 1)])) %*% W5.list[[j + 1]]
                  - W6.list[[j + 1]] %*% Lmult(phi.hat[-j], W.list[-(j + 1)], Y.list[-(j + 1)]))/(2 * D.list[[j + 1]])
  }
  print(c(i,phi.hat[1]))
  c1 = cbind(c1, phi.hat)
} 


Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
sqrt(sum((Y^2 - Y.hat^2)^2)/length(Y))


scatter2D(Austra[[1]][Gr1,2], Austra[[1]][Gr1,1], colvar = Y^2, pch = 19, cex = 0.5)




ONI.coef = NULL
for(i in 1:length(Gr1)){
  mat1 = NULL
  for(j in m1){
    mat1 = c(mat1, Austra[[j]][Gr1[i],3])
  }
  mod = lm(mat1 ~ ONI.Mat[m1,2])
  ONI.coef = rbind(ONI.coef, mod$coefficients)
}

IOD.coef = NULL
for(i in 1:length(Gr1)){
  mat1 = NULL
  for(j in m1){
    mat1 = c(mat1, Austra[[j]][Gr1[i],3])
  }
  mod = lm(mat1 ~ IOD.Mat[m1,2])
  IOD.coef = rbind(IOD.coef, mod$coefficients)
}


cor.list = list()
cor.pos.list = list()
for(l in 1:1){
  cor1.list = list()
  cor1.pos.list = list()
  for(k in 1:12){
    n = M[[l]]
    n = n[ONI1[n,2] > 0]
    m = n - k
    if(max(m) > max(n)){
      m = m[-length(m)]
    }
    if(length(m) < length(n)){
      n = n[-1]
    }
    if(min(m) < 1){
      n = n[-1]
      m = m[-1]
    }
    cor1_12.Mat = NULL
    cor.Mat = NULL
    for(i in m.11){
      cor.1 = NULL
      cor.Gr = NULL
      cor.pos = NULL
      for(j in Gr1){
        cor.Gr = c(cor.Gr, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Gr, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Gr1[which(cor.Gr == x1[1])[1]], Gr1[which(cor.Gr == x1[2])[1]], Gr1[which(cor.Gr == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Te = NULL
      for(j in Te1){
        cor.Te = c(cor.Te, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Te, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Te1[which(cor.Te == x1[1])[1]], Te1[which(cor.Te == x1[2])[1]], Te1[which(cor.Te == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Tr = NULL
      for(j in Tr1){
        cor.Tr = c(cor.Tr, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Tr, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Tr1[which(cor.Tr == x1[1])[1]], Tr1[which(cor.Tr == x1[2])[1]], Tr1[which(cor.Tr == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Eq = NULL
      for(j in Eq1){
        cor.Eq = c(cor.Eq, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Eq, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Eq1[which(cor.Eq == x1[1])[1]], Eq1[which(cor.Eq == x1[2])[1]], Eq1[which(cor.Eq == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.Su = NULL
      for(j in Su1){
        cor.Su = c(cor.Su, cor(M.all[n,i], M.all[m,j]))
      }
      x1 = sort(cor.Su, decreasing = TRUE)[1:3]
      cor.1 = c(cor.1, x1)
      x2 = c(Su1[which(cor.Su == x1[1])[1]], Su1[which(cor.Su == x1[2])[1]], Su1[which(cor.Su == x1[3])[1]])
      cor.pos = c(cor.pos, x2)
      cor.De = NULL
      for(j in De1){
        cor.De = c(cor.De, cor(M.all[n,i], M.all[m,j]))
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


Lmult = function(a.vec, A.list, B.list){
  L.out = matrix(0, nrow = nrow(A.list[[2]]))
  for(i in 1:length(A.list)){
    L.out = L.out + a.vec[i] * A.list[[i]] %*% B.list[[i]]
  }
  L.out
}


ind = unique(c(which(apply(W.list[[1]], 2, sum) > 0), which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
               which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0),
               which(apply(W.list[[7]], 2, sum) > 0), which(apply(W.list[[8]], 2, sum) > 0), which(apply(W.list[[9]], 2, sum) > 0),
               which(apply(W.list[[10]], 2, sum) > 0), which(apply(W.list[[11]], 2, sum) > 0), which(apply(W.list[[12]], 2, sum) > 0)))

W.list = list()
N2 = cor.pos.list[[1]]
N4 = cor.list[[1]]
for(i in 1:12){
  N1 = matrix(0, ncol = ncol(M.all), nrow = length(Y))
  N3 = N2[[i]]
  N5 = N4[[i]]
  for(j in 1:length(Y)){
    N1[j, N3[j,]] = N5[j,]/sum(N5[j,])
  }
  W.list[[i]] = N1
}

ind = unique(c(which(apply(W.list[[1]], 2, sum) > 0), which(apply(W.list[[2]], 2, sum) > 0), which(apply(W.list[[3]], 2, sum) > 0), 
               which(apply(W.list[[4]], 2, sum) > 0), which(apply(W.list[[5]], 2, sum) > 0), which(apply(W.list[[6]], 2, sum) > 0),
               which(apply(W.list[[7]], 2, sum) > 0), which(apply(W.list[[8]], 2, sum) > 0), which(apply(W.list[[9]], 2, sum) > 0),
               which(apply(W.list[[10]], 2, sum) > 0), which(apply(W.list[[11]], 2, sum) > 0), which(apply(W.list[[12]], 2, sum) > 0)))

g = 1

Y = A.all[rev(m1)[g], Gr1]
Y.list = list()
k = 1
for(i in (rev(m1[ONI1[n,2] > 0])[g] - 1):(rev(m1[ONI1[n,2] > 0])[g] - 12)){
  Y.list[[k]] = A.all[i,ind]
  k = k + 1
}

N1 = matrix(0, nrow = length(Y.list[[1]]), ncol = length(Y))

for(i in 1:length(W.list)){
  W.list[[i]] = W.list[[i]][,ind]
}

W1.list = list()
for(i in 1:12){
  W1.list[[i]] = t(Y) %*% W.list[[i]] %*% Y.list[[i]]
}

W2.list = list()
for(i in 1:12){
  W2.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% Y
}

D.list = list()
for(i in 1:12){
  D.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% W.list[[i]] %*% Y.list[[i]]
}

X.ti = cbind(rep(1, length(Gr1)), Austra[[1]][Gr1,1], Austra[[1]][Gr1,2],
             Austra[[1]][Gr1,1] * Austra[[1]][Gr1,2], Austra[[1]][Gr1, 9], 
             Austra[[1]][Gr1, 10], Austra[[1]][Gr1, 9] * Austra[[1]][Gr1, 10])

W3.list = list()
for(i in 1:12){
  W3.list[[i]] = t(X.ti) %*% W.list[[i]] %*% Y.list[[i]]
}

W4.list = list()
for(i in 1:12){
  W4.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]]) %*% X.ti
}

W5.list = list()
for(i in 1:12){
  W5.list[[i]] = W.list[[i]] %*% Y.list[[i]]
}

W6.list = list()
for(i in 1:12){
  W6.list[[i]] = t(Y.list[[i]]) %*% t(W.list[[i]])
}

c1 = NULL
p = 12
beta.hat = matrix(rep(0, ncol(X.ti)), ncol = 1)
phi.hat = rep(0, p)
A1 = solve(t(X.ti) %*% X.ti)%*%t(X.ti)
for(i in 1:500){
  beta.hat = A1 %*% (Y - Lmult(phi.hat, W.list, Y.list))
  for(j in 1:(p)){
    phi.hat[j] = (W1.list[[j]] + W2.list[[j]] - t(beta.hat) %*% W3.list[[j]]
                  - W4.list[[j]] %*% beta.hat - t(Lmult(phi.hat[-j], W.list[-(j)], Y.list[-(j)])) %*% W5.list[[j]]
                  - W6.list[[j]] %*% Lmult(phi.hat[-j], W.list[-(j)], Y.list[-(j)]))/(2 * D.list[[j]])
  }
  print(c(i,phi.hat[1]))
  c1 = cbind(c1, phi.hat)
} 


Y.hat = X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)
sum(Y!=round(Y.hat))


g = 8

Y = sqrt(M.all[rev(m1[ONI1[n,2] > 0])[g], Gr1])
Y.list = list()
k = 1
for(i in (rev(m1[ONI1[n,2] > 0])[g] - 1):(rev(m1[ONI1[n,2] > 0])[g] - 12)){
  Y.list[[k]] = sqrt(M.all[i,ind])
  k = k + 1
}

Y.hat = exp(X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list))/(exp(X.ti %*% beta.hat + Lmult(phi.hat, W.list, Y.list)) + 1)
sqrt(sum((Y - Y.hat)))

scatter2D(Austra[[1]][Gr1,2], Austra[[1]][Gr1,1], colvar = Y, pch = 19, cex = 0.5)
scatter2D(Austra[[1]][Gr1,2], Austra[[1]][Gr1,1], colvar = round(Y.hat), pch = 19, cex = 0.5)
scatter2D(Austra[[1]][Gr1,2], Austra[[1]][Gr1,1], colvar = Y - round(Y.hat), pch = 19, cex = 0.5)


#### Chance of exceeding average rainfall
in1 = function(x){
  ifelse(x >= mean(x),1,0)
}

aver = rep(0, nrow(Austra[[1]]))
for(i in 1:length(Austra)){
  Austra[[i]]$Ave = aver
}


M4 = list()
for(i in 1:length(M)){
  M4[[i]] = apply(M.all[M[[i]], ], 2, in1)
}

for(i in 1:length(M)){
  n1 = M[[i]]
  for(j in 1:length(n1)){
    Austra[[n1[j]]]$Ave = M4[[i]][j, ]
  }
}

A.all = NULL
for(i in 1:length(Austra)){
  A.all = rbind(A.all, Austra[[i]][,11])
  if(i %% 100 == 0){
    print(i)
  }
}

SeaIce = read.csv("SeaIceM.csv")
SeaIce = SeaIce[257:489,]

ave.coef = NULL
ave.p = NULL
for(i in 1:nrow(Austra[[1]])){
  mod1 = glm(A.all[m1,i] ~ (ONI1[m1,2] + IOD1[m1,2])^2 + SeaIce[m1, 5], family = binomial(link = "logit"))
  ave.coef = rbind(ave.coef, c(Austra[[1]][i,1], Austra[[1]][i,2], mod1$coefficients, sum(mod1$residuals^2)))
  ave.p = rbind(ave.p, summary(mod1)$coefficients[,4]) 
  if(i %% 1000 == 0){
    print(i)
  }
}



scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = ave.p[,1], cex = 0.2, pch = 19)


exp(mod1$fitted.values)/(exp(mod1$fitted.values) + 1)
hist(ave.coef[,2])

ave.coef = ave.coef[ave.coef[,3] < 20 & ave.coef[,3] > -15,]
ave.coef = ave.coef[ave.coef[,4] < 5 & ave.coef[,4] > -5,]
ave.coef = ave.coef[ave.coef[,5] < 10 & ave.coef[,5] > -10,]
ave.coef = ave.coef[ave.coef[,6] < 20 & ave.coef[,6] > -10,]
hist(ave.coef[,3])

scatter2D(ave.coef[,2], ave.coef[,1], colvar = ave.coef[,6], cex = 0.2, pch = 19)
round(mod1$fitted.values)











