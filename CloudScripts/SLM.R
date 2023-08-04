Opt.Weight = function(formula, data, locations = NULL, WMatrix = NULL, k.start = 1, alpha.start = 0, increment = 0.1, intercept = TRUE, Durbin = FALSE, 
                      complete = FALSE, tol = 10^(-5), omit = FALSE, method = c("IDW", "Gaussian"), LR = FALSE){
  d1 = 0
  s3 = Inf
  alpha = alpha.start
  while(d1 == 0){
    Smod1 = SLM(formula, data, locations = locations, k = k.start, alpha = alpha, intercept = intercept, Durbin = Durbin, 
                complete = complete, tol = 10^(-5), omit = omit, method = method, LR = LR)
    s2 = Smod1$sigma
    if(s2 < s3){
      s3 = s2
      alpha = abs(alpha + increment)
    }else{
      d1 = 1
      s4 = s2
    }
  }
  k2 = k.start
  alpha1 = alpha
  alpha2 = 1
  k = k.start + 1
  d3 = 0
  while(d3 == 0){
    s3 = Inf
    d2 = 0
    int = 0
    Smod1 = SLM(formula, data, locations = locations, k = k, alpha = alpha, intercept = intercept, Durbin = Durbin, 
                complete = complete, tol = 10^(-5), omit = omit, method = method, LR = LR)
    ss2 = Smod1$sigma
    Smod2 = SLM(formula, data, locations = locations, k = k, alpha = alpha + increment, intercept = intercept, Durbin = Durbin, 
                complete = complete, tol = 10^(-5), omit = omit, method = method, LR = LR)
    ss3 = Smod2$sigma
    if(ss3 < ss2){
      ss2 = ss3
      int = 1
      alpha = alpha + increment
      alpha2 = alpha
    }else if(alpha > 0){
      Smod3 = SLM(formula, data, locations = locations, k = k, alpha = abs(alpha - increment), intercept = intercept, Durbin = Durbin, 
                  complete = complete, tol = 10^(-5), omit = omit, method = method, LR = LR)
      ss4 = Smod3$sigma
      if(ss4 < ss2){
        ss2 = ss4
        int = -1
        alpha = abs(alpha - increment)
        alpha2 = alpha
      }
    }
    if(int != 0){
      while(d2 == 0){
        alpha = abs(alpha + int * increment)
        Smod1 = SLM(formula, data, locations = locations, k = k, alpha = alpha, intercept = intercept, Durbin = Durbin, 
                    complete = complete, tol = 10^(-5), omit = omit, method = method, LR = LR)
        ss22 = Smod1$sigma
        if(ss22 > ss2){
          alpha2 = abs(alpha - int * increment)
          d2 = 1
        }else{
          ss2 = ss22
          if(alpha + int * increment < 0){
            d2 = 1
            alpha2 = alpha
          }
        }
      }
    }
    if(ss2 < s4){
      s4 = ss2
      alpha1 = alpha2
      k2 = k
      k = k + 1
    }else{
      d3 = 1
    }
  }
  c(k2, alpha1)
}

Weight.Matrix = function(locations, k = NULL, alpha = 1, method = c("IDW", "Gaussian"), fun = distCosine, scale = TRUE){
  if(is.null(k)){
    k = nrow(locations) - 1
  }
  if(!is.null(k) & k %% 1 != 0){
    stop("k must be an integer")
  }
  if(alpha < 0){
    stop("alpha must be non-negative")
  }
  coords = NULL
  ds = NULL
  for(i in 1:nrow(locations)){
    newX <- distm(locations[i,], locations, fun = distCosine)
    newX1 = sort(newX[1,])[2:(k+1)]
    m3 = NULL
    if(newX1[1]!=0){
      for(j in 1:(k)){
        m3 = c(m3, which(newX[1,] == newX1[j])[1])
      }
    }else{
      newX1 = sort(newX[1,])[3:(k+2)]
      for(j in 1:k){
        m3 = c(m3, which(newX[1,] == newX1[j])[1])
      }
    }
    coords = rbind(coords, matrix(m3, nrow = 1, k))
    ds = rbind(ds, newX1)
    
  }
  new.W = matrix(0, nrow(locations), nrow(locations))
  if(method == "IDW"){
    for(i in 1:nrow(locations)){
      new.W[i,coords[i,]] = 1/ds[i,]^alpha
    }
  }
  if(method == "Gaussian"){
    for(i in 1:nrow(locations)){
      new.W[i,coords[i,]] = exp(-ds[i,]^2/alpha)
    }
  }
  if(scale){
    for(i in 1:nrow(new.W)){
      new.W[i,] = new.W[i,]/sum(new.W[i,])
    }
  }
  W = new.W
  W
}



Sep.Weight.Matrix = function(Oldlocations, Newlocations, k = NULL, alpha = 1, method = c("IDW", "Gaussian"), fun = distCosine, scale = TRUE){
  if(is.null(k)){
    k = nrow(Oldlocations) - 1
  }
  if(!is.null(k) & k %% 1 != 0){
    stop("k must be an integer")
  }
  if(alpha < 0){
    stop("alpha must be non-negative")
  }
  coords = NULL
  ds = NULL
  for(i in 1:nrow(Newlocations)){
    newX <- distm(Newlocations[i,], Oldlocations, fun = distCosine)
    newX1 = sort(newX[1,])[1:(k + 1)]
    m3 = NULL
    if(newX1[1]!=0){
      for(j in 1:k){
        m3 = c(m3, which(newX[1,] == newX1[j])[1])
        newX1 = newX1[1:k]
      }
    }else{
      for(j in 2:(k + 1)){
        m3 = c(m3, which(newX[1,] == newX1[j])[1])
        newX1 = newX1[2:(k + 1)]
      }
    }
    coords = rbind(coords, matrix(m3, nrow = 1, k))
    ds = rbind(ds, newX1)
  }
  new.W = matrix(0, nrow = nrow(Newlocations), ncol = nrow(Oldlocations))
  if(method == "IDW"){
    for(i in 1:nrow(new.W)){
      new.W[i,coords[i,]] = 1/ds[i,]^alpha
    }
  }
  if(method == "Gaussian"){
    for(i in 1:nrow(Newlocations)){
      new.W[i,coords[i,]] = exp(-ds[i,]^2/alpha)
    }
  }
  if(scale){
    for(i in 1:nrow(new.W)){
      new.W[i,] = new.W[i,]/sum(new.W[i,])
    }
  }
  W = new.W
  W
}

SPE1 = function(y, W, X, tol = 10 ^ (-5), intercept = TRUE){
  n = length(y)
  if(nrow(W) != n | ncol(W) != n){
    stop("Dimensions of W do not match the length of y")
  }
  if(is.null(nrow(X))){
    X = matrix(X, nrow = n)
  }
  if(nrow(X) != n){
    stop("Number of rows of X does not match the length of y")
  }
  if(tol <= 0){
    stop("Tol must be a positive number")
  }
  beta.hat = matrix(rep(0, ncol(X)), ncol = 1)
  lambda.hat = 0
  l0 = Inf
  n1 = t(y) %*% (t(W) + W) %*% y
  d1 = 2 * t(y) %*% t(W) %*% W %*% y
  n2 = t(X) %*% W %*% y
  n3 = t(y) %*% t(W) %*% X
  n4 = solve(t(X) %*% X) %*% t(X) %*% y
  n5 = solve(t(X) %*% X) %*% t(X) %*% W %*% y
  iter = 0
  while(abs(lambda.hat - l0) > tol){
    l0 = lambda.hat
    lambda.hat = ((n1 - (t(beta.hat) %*% n2 + n3 %*% beta.hat))/d1)[1,1]
    beta.hat = n4 - lambda.hat * n5
    iter = iter + 1
  }
  ret = list(lambda.hat = lambda.hat, beta.hat = beta.hat, iter = iter)
  ret
}

SPE2 = function(y, W, X, tol = 10^(-5), intercept = TRUE){
  n = length(y)
  if(nrow(W) != n | ncol(W) != n){
    stop("Dimensions of W do not match the length of y")
  }
  if(is.null(nrow(X))){
    X = matrix(X, nrow = n)
  }
  if(nrow(X) != n){
    stop("Number of rows of X does not match the length of y")
  }
  if(tol <= 0){
    stop("Tol must be a positive number")
  }
  lambda.hat = 0
  rho.hat = 0
  beta.hat = matrix(rep(0, ncol(X)), ncol = 1)
  l0 = Inf
  iter = 0
  while(abs(lambda - l0)>tol){
    Omega3 = (diag(n) - rho.hat * t(W)) %*% (diag(n) - rho.hat * W)
    l0 = lambda.hat
    lambda.hat = ((t(y) %*% t(W) %*% Omega3 %*% y + t(y) %*% Omega3 %*% W %*% y - t(y) %*% (W) %*% Omega3 %*% X %*% beta.hat + t(beta.hat) %*% t(X) %*% Omega3 %*% W %*%y)/(
      2 * t(y) %*% t(W) %*% Omega3 %*% W %*% y
    ))[1,1]
    Omega1 = (diag(n) - rho.hat * t(W)) %*% (diag(n) - rho.hat * W) %*% (diag(n) - lambda.hat * W)
    beta.hat = solve(t(X) %*% Omega3 %*% X) %*%t(X) %*% Omega1 %*% y
    Psi1 = (diag(n) - lambda.hat * t(W)) %*% (t(W) + W) %*% (diag(n) - lambda.hat * W)
    Psi2 = (diag(n) - lambda.hat * t(W)) %*% (t(W) + W)
    Psi3 = t(W) + W
    Upsilon1 = (diag(n) - lambda.hat * t(W)) %*% t(W) %*% W %*% (diag(n) - lambda.hat * W)
    Upsilon2 = (diag(n) - lambda.hat * t(W)) %*% t(W) %*% W
    Upsilon3 = t(W) %*% W
    rho.hat = ((t(y) %*% Psi1 %*% y - t(y) %*% Psi2 %*% X %*% beta.hat - t(beta.hat) %*% t(X) %*% t(Psi2) %*% y + t(beta.hat) %*% t(X) %*% Psi3 %*% X %*% beta.hat)/(
      2*(t(y) %*% Upsilon1 %*% y - t(y) %*% Upsilon2 %*% X %*% beta.hat - t(beta.hat) %*% t(X) %*% t(Upsilon2) %*% y + t(beta.hat) %*% t(X) %*% Upsilon3 %*% X %*% beta.hat)
    ))[1,1]
    iter = iter + 1
  }
  ret = list(lambda.hat = lambda.hat, rho.hat = rho.hat, beta.hat = beta.hat, iter = iter)
  ret
}

SPE3 = function(y, W, X, tol = 10^(-5), intercept = TRUE){
  n = length(y)
  if(nrow(W) != n | ncol(W) != n){
    stop("Dimensions of W do not match the length of y")
  }
  if(is.null(nrow(X))){
    X = matrix(X, nrow = n)
  }
  if(nrow(X) != n){
    stop("Number of rows of X does not match the length of y")
  }
  if(tol <= 0){
    stop("Tol must be a positive number")
  }
  if((t(y) %*% t(W) %*% X %*% solve(t(X) %*% X) %*% t(X) %*% W %*% y)/(t(y) %*% t(W) %*% W %*% y)>1){
    stop("Algorithm will not converge due to eigenvalues of the weight matrix W")
  }
  A1 = t(W) + W
  A2 = t(W) %*% W
  rho.hat = 0
  beta.hat = matrix(rep(0, ncol(X)), ncol = 1)
  r0 = Inf
  iter = 0
  while(abs(rho.hat - r0) > tol){
    X1 = X %*% beta.hat
    r0 = rho.hat
    rho.hat = ((t(y - X1) %*% A1 %*% (y - X1))/(2*t(y - X1) %*% A2 %*% (y - X1)))[1,1]
    Omega = t(X) %*% (diag(n) -  rho.hat * t(W)) %*%  (diag(n) -  rho.hat * W)
    beta.hat = solve(Omega %*% X) %*% Omega %*% y
    iter = iter + 1
  }
  ret = list(rho.hat = rho.hat, beta.hat = beta.hat, iter = iter)
  ret
}

SLM.logLik = function(y, W, X, coef.hat, fitted.values, complete = FALSE, tol = 10^ (-5), intercept = TRUE, omit = FALSE){
  n = length(y)
  if(is.null(dim(X))){
    X = as.matrix(X)
  }
  if(!complete){
    s2 = (1/(n - ncol(X) - 1))*sum((y - fitted.values)^2)
    logLik1 = -(n/2) * log(2*pi*s2) - determinant(solve(diag(n) - coef.hat[[1]] * W))[[1]][1] - 
      (1/(2 * s2)) * t(y - fitted.values) %*% (diag(n) - coef.hat[[1]] * t(W))%*%(diag(n) - coef.hat[[1]] * W)%*% (y - fitted.values)
    if(ncol(X) == 1 & intercept){
      mod0 = lm(y ~ 1)
    }else if(ncol(X) == 1 & !intercept){
      mod0 = lm(y ~ X - 1)
    }else{
      mod0 = lm(y ~ X[,-1])
    }
    logLik0 = logLik(mod0)[1]
    Lambda.y = 2 * (logLik1 - logLik0)
    p.value = pchisq(q = Lambda.y, df = 1, lower.tail = FALSE)
    ret = list(LRS = Lambda.y, p.value = p.value)
  }else if(complete & !omit){
    s2 = (1/(n - ncol(X) - 2))*sum((y - fitted.values)^2)
    logLik1 = -(n/2) * log(2*pi*s2) + determinant(diag(n) - coef.hat[[1]] * W)[[1]][1] + determinant(diag(n) - coef.hat[[2]] * W)[[1]][1]
    - (1/(2 * s2)) * t(y - fitted.values) %*% (diag(n) - coef.hat[[1]] * t(W)) %*% (diag(n) 
                                                                                    - coef.hat[[2]] * t(W)) %*% (diag(n) - coef.hat[[2]] * W) %*% (diag(n) - coef.hat[[1]] * W)%*% (y - fitted.values)
    ncoef1 = SPE1(y = y, W = W, X = X, tol = tol, intercept = intercept)
    ny1 = X %*% ncoef1[[2]] + ncoef1[[1]] * W %*% y
    s2.lambda = (1/(n - ncol(X) - 1))*sum((y - ny1)^2)
    logLik0.lambda = -(n/2) * log(2*pi*s2.lambda) + log(det(diag(n) - coef.hat[[1]] * W)^2)/2 - (1/(2 * s2.lambda)) * t(y - ny1) %*% (y - ny1)
    Lambda.y.lambda = 2 * (logLik1 - logLik0.lambda)
    p.value.lambda = pchisq(q = Lambda.y.lambda, df = 1, lower.tail = FALSE)
    ncoef2 = SPE3(y = y, W = W, X = X, tol = tol, intercept = intercept)
    ny2 = X %*% ncoef1[[2]]
    s2.rho = (1/(n - ncol(X) - 1))*sum((y - ny2)^2)
    logLik0.rho = -(n/2) * log(2*pi*s2.rho) + determinant(diag(n) - coef.hat[[1]] * W)[[1]][1] - (1/(2 * s2.rho)) * t(y - ny2) %*% (y - ny2)
    Lambda.y.rho = 2 * (logLik1 - logLik0.rho)
    p.value.rho = pchisq(q = Lambda.y.rho, df = 1, lower.tail = FALSE)
    ret = list(LRS.lambda = Lambda.y.lambda, p.value.lambda = p.value.lambda, LRS.rho = Lambda.y.rho, p.value.rho = p.value.rho)
  }else{
    s2 = (1/(n - ncol(X) - 1))*sum((y - fitted.values)^2)
    logLik1 = -(n/2) * log(2*pi*s2) + determinant(diag(n) - coef.hat[[1]] * W)[[1]][1]
    - (1/(2 * s2)) * t(y - fitted.values) %*% (diag(n) - coef.hat[[1]] * t(W)) %*% (diag(n) - coef.hat[[1]] * W) %*% (y - fitted.values)
    mod0 = lm(y ~ X[,-1])
    logLik0 = logLik(mod0)[1]
    Lambda.y = 2*(logLik1 - logLik0)
    p.value = pchisq(q = Lambda.y, df = 1, lower.tail = FALSE)
    ret = list(LRS = Lambda.y, p.value = p.value)
  }
  ret
}

SLM = function(formula, data, locations = NULL, WMatrix = NULL, k = NULL, alpha = 1, intercept = TRUE, Durbin = FALSE, 
               complete = FALSE, tol = 10^(-5), omit = FALSE, method = c("IDW", "Gaussian"), LR = TRUE){
  d = data
  m = model.frame(formula, data)
  var.names = colnames(m)
  p = ncol(m)
  y = m[,1]
  if(!complete & omit){
    stop("No spatial parameters to be estimated, used ordinary linear model (lm)")
  }
  if(is.null(WMatrix) & is.null(locations)){
    stop("Need either the locations to be given or a predefined weight matrix in WMatrix")
  }
  if(sum(duplicated(locations))>0){
    print("Identical locations found, removing duplicated location")
    d = d[!duplicated(locations),]
    locations = locations[!duplicated(locations),]
    m = model.frame(formula, d)
  }
  if(is.null(WMatrix) & !is.null(locations)){
    WMatrix = Weight.Matrix(locations = locations, k = k, alpha = alpha, method = method)
    #print("Weight Matrix Created")
  }
  if(Durbin){
    WX = WMatrix %*% m[,-1]
    colnames(WX) = paste0("Weighted.",var.names[-1])
    d1 = c(var.names, colnames(WX))
    f1 = d1[2]
    for(i in 3:length(d1)){
      f1 = paste(f1, d1[i], sep = " + ")
    }
    f2 = as.formula(paste(d1[1], f1, sep = " ~ "))
    d = cbind(d, WX)
    m = model.frame(f2, d)
    y = m[,1]
    formula = f2
  }
  mat = model.matrix(formula, m)
  if(!intercept){
    mat = mat[,-1]
  }
  X = mat
  W = WMatrix
  y = m[,1]
  if(!complete){
    coef.hat = SPE1(y = y, W = W, X = X, tol = tol, intercept = intercept)
    #print(paste("Coefficients Estimated in ", coef.hat[[3]], "iterations", sep = " "))
    lambda.hat = coef.hat[[1]]
    beta.hat = coef.hat[[2]]
    iter = coef.hat[[3]]
    fitted.values = X %*% beta.hat + lambda.hat * W %*% y
    if(LR){
      if(determinant(diag(nrow(W)) - lambda.hat * W, logarithm = FALSE)[[1]][1] == 0){
        print("I - lambda.hat * W is not invertible, LR is invalid")
        print("Try different weight matrix")
      }
      LR = SLM.logLik(y = y, W = W, X = X, coef.hat = coef.hat, fitted.values = fitted.values, complete = complete, tol = tol, intercept = intercept, omit = omit)
    }
  }else if(complete & !omit){
    coef.hat = SPE2(y = y, W = W, X = X, tol = tol, intercept = intercept)
    #print(paste("Coefficients Estimated in ", coef.hat[[4]], "iterations", sep = " "))
    lambda.hat = coef.hat[[1]]
    rho.hat = coef.hat[[2]]
    beta.hat = coef.hat[[3]]
    iter = coef.hat[[4]]
    fitted.values = X %*% beta.hat + lambda.hat * W %*% y
    if(LR){
      if(determinant(diag(nrow(W)) - lambda.hat * W, logarithm = FALSE)[[1]][1] == 0 | determinant(diag(nrow(W)) - rho.hat * W, logarithm = FALSE)[[1]][1] == 0){
        print("I - lambda.hat * W is not invertible or I - rho.hat * W, LR is invalid")
        print("Try different weight matrix")
      }
      LR = SLM.logLik(y = y, W = W, X = X, coef.hat = coef.hat, fitted.values = fitted.values, complete = complete, tol = tol, intercept = intercept, omit = omit)
    }
  }else{
    coef.hat = SPE3(y = y, W = W, X = X, tol = tol, intercept = intercept)
    #print(paste("Coefficients Estimated in ", coef.hat[[3]], "iterations", sep = " "))
    rho.hat = coef.hat[[1]]
    beta.hat = coef.hat[[2]]
    iter = coef.hat[[3]]
    fitted.values = X %*% beta.hat
    if(LR){
      if(determinant(diag(nrow(W)) - rho.hat * W, logarithm = FALSE)[[1]][1] == 0){
        print("I - rho.hat * W is not invertible, LR is invalid")
        print("Try different weight matrix")
      }
      LR = SLM.logLik(y = y, W = W, X = X, coef.hat = coef.hat, fitted.values = fitted.values, complete = complete, tol = tol, intercept = intercept, omit = omit)
    }
  }
  residuals = y - fitted.values
  df = length(y) - length(beta.hat) - 1 * !omit - 1 * complete
  sigma2 = (1/df) * sum((residuals)^2)
  C = sigma2 * solve(t(X) %*% X)
  t.stat = NULL
  p.value = NULL
  for(i in 1:length(beta.hat)){
    t.stat = c(t.stat, beta.hat[i]/sqrt(C[i,i]))
    p.value = c(p.value, pt(abs(beta.hat[i]/sqrt(C[i,i])), df = df, lower.tail = FALSE))
  }
  ret = list(LR = LR, coef = coef.hat, locations = locations, fitted.values = fitted.values, 
             residuals = residuals, Weight.Matrix = W, X = X, t.stat = t.stat, p.value = p.value, sigma2 = sigma2, sigma = sqrt(sigma2), y = y, class = c("SLM"))
  ret
}
