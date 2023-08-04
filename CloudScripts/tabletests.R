#################
##SLM vs ML EStimates
#################

##############
#Univariate, one parameter
##############

#Create data
n = 100
int = rep(1, n)
eps = rnorm(100)

l2 = seq(-1,1, 0.25)
b2 = seq(-10,10,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
 for(j in 1:nrow(loc1)){
   if(i == j){
     W1[i,j] = 0
   }else{
     W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
   }
 }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b2)){
    tryCatch({
    Y[[k]] = solve(diag(n) - l2[i] * W1) %*% int * b2[j] + solve(diag(n) - l2[i] * W1) %*% eps
    Act = rbind(Act, c(l2[i],b2[j]))
    k = k + 1
    }, error = function(e){})
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, int)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int
  l.2 = 2 * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% (W1) %*% Y[[i]]
  b.1 = 2 * t(int) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] %*% t(int) %*% W1 %*% Y[[i]]
  b.2 = 2 * n * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1/b.2))
}

A1 = ""
for(i in 1:length(round(Est1[seq(3,24,3),2], 3))){
  A1 = paste0(A1, round(Est2[seq(3,24,3),2], 3)[i], " & ")
}
A1


#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b2)){
    tryCatch({
      Y[[k]] = solve(diag(n) - l2[i] * W1) %*% int * b2[j] + solve(diag(n) - l2[i] * W1) %*% eps
      Act = rbind(Act, c(l2[i],b2[j]))
      k = k + 1
    }, error = function(e){})
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, int)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int
  l.2 = 2 * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% (W1) %*% Y[[i]]
  b.1 = 2 * t(int) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] %*% t(int) %*% W1 %*% Y[[i]]
  b.2 = 2 * n * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1/b.2))
}

A1 = ""
for(i in 1:length(round(Est1[seq(3,27,3),2], 3))){
  A1 = paste0(A1, round(Est2[seq(3,27,3),2], 3)[i], " & ")
}
A1


#random locations
set.seed(1998)
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c((runif(1) + 1) * 50,(runif(1) + 1) * 50))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b2)){
    tryCatch({
      Y[[k]] = solve(diag(n) - l2[i] * W1) %*% int * b2[j] + solve(diag(n) - l2[i] * W1) %*% eps
      Act = rbind(Act, c(l2[i],b2[j]))
      k = k + 1
    }, error = function(e){})
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, int)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int
  l.2 = 2 * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% (W1) %*% Y[[i]]
  b.1 = 2 * t(int) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] %*% t(int) %*% W1 %*% Y[[i]]
  b.2 = 2 * n * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1/b.2))
}


A1 = ""
for(i in 1:length(round(Est1[seq(3,24,3),2], 3))){
  A1 = paste0(A1, round(Est2[seq(3,24,3),2], 3)[i], " & ")
}
A1



#random locations

W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled

Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b2)){
    tryCatch({
      Y[[k]] = solve(diag(n) - l2[i] * W1) %*% int * b2[j] + solve(diag(n) - l2[i] * W1) %*% eps
      Act = rbind(Act, c(l2[i],b2[j]))
      k = k + 1
    }, error = function(e){})
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, int)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int
  l.2 = 2 * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - (2/n) * t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% (W1) %*% Y[[i]]
  b.1 = 2 * t(int) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% (W1 + t(W1)) %*% Y[[i]] %*% t(int) %*% W1 %*% Y[[i]]
  b.2 = 2 * n * t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]] - t(Y[[i]]) %*% t(W1) %*% int %*% t(int) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1/b.2))
}


A1 = ""
for(i in 1:length(round(Est1[seq(3,24,3),2], 3))){
  A1 = paste0(A1, round(Est2[seq(3,24,3),2], 3)[i], " & ")
}
A1




##############
#Univariate, Three parameter (intercept, continuous and binary)
##############

#Create data
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
x2 = rbinom(100, 1, 0.5)
eps = rnorm(100)

l2 = seq(-1,1, 0.25)
b1 = seq(-10,10,10)
b2 = c(0,10)
b3 = c(-15, 0)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1, x2)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      for(q in 1:length(b3)){
        tryCatch({
          Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l], b3[q]) + solve(diag(n) - l2[i] * W1) %*% eps
          Act = rbind(Act, c(l2[i],b1[j], b2[l], b3[q]))
          k = k + 1
        }, error = function(e){})
      }
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(3) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:4], Est2[i,1:4])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(X > 25, round(X), X)
  X
}

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex", include.rownames = FALSE)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
eps = rnorm(100)

#beta0 = -10
l2 = seq(-1,1, 0.25)
b1 = c(-10)
b2 = c(-5, 0,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
        tryCatch({
          Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
          Act = rbind(Act, c(l2[i],b1[j], b2[l]))
          k = k + 1
        }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(0)
b2 = c(-5, 0,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(X > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(10)
b2 = c(-5, 0,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)


#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
eps = rnorm(100)

#beta0 = -10
l2 = seq(-1,1, 0.25)
b1 = c(-10)
b2 = c(-5, 0,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(0)
b2 = c(-5, 0,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnorm(100, 10, 5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(10)
b2 = c(-5, 0,10)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)

#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,0.5)
eps = rnorm(100)

#beta0 = -10
l2 = seq(-1,1, 0.25)
b1 = c(-10)
b2 = c(-15, 0, 3)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,0.5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(0)
b2 = c(-15, 0, 3)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)

#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,0.5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(10)
b2 = c(-15, 0, 3)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}

X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)





#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,0.5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(-10)
b2 = c(-15, 0, 3)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)


#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,0.5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(0)
b2 = c(-15, 0, 3)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder))
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)

#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,0.5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(10)
b2 = c(-15, 0, 3)

#grid
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c(i,j))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#unscaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder))
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)


#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnrom(100,10,5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(-10)
b2 = c(-5, 0, 10)

#random locations
set.seed(1998)
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c((runif(1) + 1) * 50,(runif(1) + 1) * 50))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-15} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{3} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)



#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnrom(100,10,5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(0)
b2 = c(-5, 0, 10)

#random locations
set.seed(1998)
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c((runif(1) + 1) * 50,(runif(1) + 1) * 50))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)

#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rnrom(100,10,5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(10)
b2 = c(-5, 0, 10)

#random locations
set.seed(1998)
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c((runif(1) + 1) * 50,(runif(1) + 1) * 50))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
for(i in 1:nrow(W1)){
  W1[i,] = W1[i,]/sum(W1[i,])
}
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 8)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder), NA)
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)


#Bivariate, set beta 0 to constant
n = 100
int = rep(1, n)
set.seed(1998)
x1 = rbinom(100,1,.5)
eps = rnorm(100)

#beta0 = 0
l2 = seq(-1,1, 0.25)
b1 = c(10)
b2 = c(-15, 0, 3)

#random locations
set.seed(1998)
loc1 = NULL
k = 1
for(i in 1:10){
  for(j in 1:10){
    loc1 = rbind(loc1, c((runif(1) + 1) * 50,(runif(1) + 1) * 50))
  }
}


W1 = matrix(0,n,n)
for(i in 1:(nrow(loc1))){
  for(j in 1:nrow(loc1)){
    if(i == j){
      W1[i,j] = 0
    }else{
      W1[i, j] = 1/sqrt(sum((loc1[i,] - loc1[j,])^2))^2 
    }
  }
}

#scaled
X = cbind(int, x1)

set.seed(1998)
Y = list()
k = 1
Act = NULL
for(i in 1:length(l2)){
  for(j in 1:length(b1)){
    for(l in 1:length(b2)){
      tryCatch({
        Y[[k]] = solve(diag(n) - l2[i] * W1) %*% X %*% c(b1[j], b2[l]) + solve(diag(n) - l2[i] * W1) %*% eps
        Act = rbind(Act, c(l2[i],b1[j], b2[l]))
        k = k + 1
      }, error = function(e){})
    }
  }
}

Est1 = NULL
Est2 = NULL
for(i in 1:length(Y)){
  smod = SPE1(Y[[i]], W1, X)
  Est1 = rbind(Est1, c(smod$lambda.hat, smod$beta.hat))
  l.1 = t(Y[[i]]) %*% (t(W1) + W1 - 2 * t(W1) %*% X %*% solve(t(X) %*% X) %*% t(X)) %*% Y[[i]]
  l.2 = 2 * t(Y[[i]]) %*% (t(W1) %*% W1 - W1 %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W1) %*% Y[[i]]
  b.1 = solve(((t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * diag(2) - solve(t(X) %*% X) %*% t(X) %*% Y[[i]] %*% t(Y[[i]]) %*% t(W1) %*% X))
  b.2 = (t(Y[[i]]) %*% t(W1) %*% W1 %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% Y[[i]] - (1/2) * (t(Y[[i]]) %*% (t(W1) %*% W1) %*% Y[[i]])[1,1] * solve(t(X) %*% X) %*% t(X) %*% W1 %*% Y[[i]] 
  Est2 = rbind(Est2, c(l.1/l.2, b.1 %*% b.2))
}

t2 = NULL
for(i in 1:nrow(Est1)){
  t2 = c(t2, Est1[i,1:3], Est2[i,1:3])
}

t1 = matrix(t2, ncol = 9)
cbind(t1, NA)
round(t1, 3)
rounder = function(X){
  X = ifelse(abs(X) > 25, round(X), X)
  X
}

addtorow <- list()
addtorow$pos <- list(c(0,0), c(1,0), c(2,0), c(3,0), c(4,0), c(5,0), c(6,0), c(7,0), c(8,0), c(9,0), c(10, 0), c(11,0), c(12, 0),
                     c(13,0), c(14,0), c(15,0), c(16,0), c(17,0))
addtorow$command <- c("\\multicolumn{1}{ |c  }{\\multirow{18}{*}{${\\beta}_1$ value} } & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{-5} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{0} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{2-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{\\multirow{6}{*}{10} } & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{Algorithm} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &",
                      "\\cline{3-13} \\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{\\multirow{3}{*}{OLS} } & \\multicolumn{1}{|c|}{$\\hat{\\lambda}$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_0$} &",
                      "\\multicolumn{1}{ |c  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{ |c|  }{} & \\multicolumn{1}{|c|}{$\\hat{\\beta}_1$} &")

t3 = cbind(apply(round(t1, 3), 2, rounder))
print(xtable(t3, type = "latex"), file = "filename2.tex",include.rownames=FALSE, include.colnames = FALSE, add.to.row = addtorow)
