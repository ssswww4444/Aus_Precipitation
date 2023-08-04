theta.hat2 = NULL
for(z in 1:length(M)){
  k.start = 1
  q = k.start + 5
  W.list = list()
  N2 = cor.pos.list[[z]]
  N4 = cor.list[[z]]
  h = 1
  for(i in k.start:q){
    N1 = matrix(0, ncol = ncol(A.all), nrow = length(m.11))
    N3 = N2[[i]]
    N5 = N4[[i]]
    k = 1
    for(j in 1:length(m.11)){
      N1[k, N3[j,]] = N5[j,]/sum(N5[j,])
      k = k + 1
    }
    W.list[[h]] = N1
    h = h + 1
  }
  
  ind = ind.function(W.list)
  
  for(i in 1:length(W.list)){
    W.list[[i]] = W.list[[i]][,ind]
  }
  
  Y = list()
  k = 1
  for(i in M[[z]][-c(1)]){
    Y[[k]] = A.all[i,m.11]
    k = k + 1
  }
  
  Y.list = list()
  k = 1
  for(i in M[[z]][-c(1)]){
    A.list = list()
    h = 1
    for(j in k.start:q){
      A.list[[h]] = A.all[i - j, ind]
      h = h + 1
    }
    Y.list[[k]] = A.list
    k = k + 1
  }
  
  M.list = list()
  for(i in 1:length(M[[z]][-c(1)])){
    A1 = Y.list[[i]]
    A2 = NULL
    for(j in 1:length(W.list)){
      A2 = cbind(A2, W.list[[j]] %*% A1[[j]])
    }
    M.list[[i]] = A2
  }
  
  X.ti = cbind(rep(1, length(m.11)), Austra[[1]][m.11,1], Austra[[1]][m.11,2])
  pt3 = list()
  beta.hat = c(0.5, rep(0, ncol(X.ti) - 1))
  phi.hat = c(0.5, rep(0, length(W.list) - 1))
  theta.hat = c(phi.hat, beta.hat)
  pt = list()
  for(i in 1:length(M[[z]][-c(1)])){
    pt[[i]] = rep(0, length(Y[[1]]))
  }
  c1 = NULL
  l1 = Inf
  i = 0
  while(abs(l1 - theta.hat[1]) > 10^(-6) & i < 50){
    l1 = theta.hat[1]
    for(k in 1:length(pt)){
      pt[[k]] = exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti %*% beta.hat)/(1 + exp(Lmult(phi.hat, W.list, Y.list[[k]]) + X.ti %*% beta.hat))
      pt3[[k]] = Lmult(phi.hat, W.list, Y.list[[k]])
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
    phi.hat = theta.hat[1:length(phi.hat)]
    beta.hat = theta.hat[(length(phi.hat) + 1):length(theta.hat)]
    i = i + 1
    print(c(i, theta.hat[1]))
    c1 = cbind(c1, theta.hat)
  }
  theta.hat2 = cbind(theta.hat2, theta.hat)
  print(z)
}

colnames(theta.hat2) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
row.names(theta.hat2) = NULL

theta.hat3 = NULL
for(i in 1:ncol(theta.hat2)){
  theta.hat3 = c(theta.hat3, theta.hat2[,i])
}

theta.hat3 = cbind(theta.hat3, as.factor(rep(1:9,12)))
theta.hat3 = as.data.frame(theta.hat3)
theta.hat3$Coef = as.factor(theta.hat3$Coef)

colnames(theta.hat3) = c("Value", "Coef")




p10 <- ggplot(theta.hat3, aes(x = Coef, y = Value)) +
  geom_boxplot(fill = "#4271AE", alpha = 0.7) + 
  scale_x_discrete(labels = c("1" = expression(phi[1]),"2" = expression(phi[2]),"3" = expression(phi[3]),"4" = expression(phi[4]),
                                               "5" = expression(phi[5]),"6" = expression(phi[6]), "7" = expression(beta[0]),
                                               "8" = expression(beta[1]),"9" = expression(beta[2])), name = "Coefficent") +
  scale_y_continuous(name = "Value") + ggtitle("Boxplot of each Coefficient for each month")

p10


theta.hat4 = NULL
for(i in 1:nrow(theta.hat2)){
  theta.hat4 = c(theta.hat4, theta.hat2[i,])
}

theta.hat4 = cbind(theta.hat4, as.factor(rep(colnames(theta.hat2), 9)))
theta.hat4 = as.data.frame(theta.hat4)
colnames(theta.hat4) = c("Value", "Month")
theta.hat4$Time = rep(1:12, 9)
theta.hat4$group = as.factor(rep(1:9, each = 12))
theta.hat4$Month = rep(row.names(theta.hat4))

ggplot(theta.hat4, aes(x=Time, y=Value, colour = group)) + geom_line(size=1) +
  scale_color_discrete(labels = c("1" = expression(phi[1]),"2" = expression(phi[2]),"3" = expression(phi[3]),"4" = expression(phi[4]),
                              "5" = expression(phi[5]),"6" = expression(phi[6]), "7" = expression(beta[0]),
                              "8" = expression(beta[1]),"9" = expression(beta[2])), name = "Coefficent") +
  theme_bw() + ggtitle("Time Series For Each Coefficient") + scale_x_continuous()

dim(theta.hat4)
head(theta.hat4)

