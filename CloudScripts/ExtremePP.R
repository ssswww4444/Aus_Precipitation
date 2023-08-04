#Extreme Point Process
dim(S1[[1]])
q0.025 = apply(S1[[1]], 1, quantile, probs = 0.025)
EX1 = matrix(0, nrow = nrow(S1[[1]]), ncol = ncol(S1[[1]]))
for(i in 1:nrow(S1[[1]])){
  EX1[i,] = S1[[1]][i,] <= q0.025[i]
}
ex1 = rep(0,18)
for(i in 1:18){
  ex1[i] = sum(EX1 == i)
}
plot(1:18, ex1, cex = 0.6, pch = 19)

q0.975 = apply(S1[[1]], 1, quantile, probs = 0.975)
EX1 = matrix(0, nrow = nrow(S1[[1]]), ncol = ncol(S1[[1]]))
for(i in 1:nrow(S1[[1]])){
  EX1[i,] = S1[[1]][i,] >= q0.975[i]
}

plot(1:18, EX1[,1], cex = 0.6, pch = 19)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = EX1[,18], cex = 0.2, pch = 19)
