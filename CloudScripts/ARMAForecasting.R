#####################
#ARMA Forecasting Oscillations
#####################



SOI = read.table("https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/soi", header = TRUE)
IOD = read.table("https://ds.data.jma.go.jp/tcc/tcc/products/elnino/index/sstindex/sliding_30year_period/DMI/3rmean", header = TRUE)

years1 = c(rep(1946:2022, each = 12))
months1 = c(rep(1:12, length(unique(years1))))
dates1 = cbind(years1, months1)
days1 = NULL
for(i in 1:nrow(dates1)){
  if(dates1[i,2] == 1 | dates1[i,2] == 3 | dates1[i,2] == 5 | dates1[i,2] == 7 | dates1[i,2] == 8 | dates1[i,2] == 10 | dates1[i,2] == 12){
    days1 = c(days1, 31)
  }else if(dates1[i,2] == 2){
    if(dates1[i,1] %% 4 == 0){
      days1 = c(days1, 29)
    }else{
      days1 = c(days1, 28)
    }
  }else if(dates1[i,2] == 4 | dates1[i,2] == 6 | dates1[i,2] == 9 | dates1[i,2] == 11){
    days1 = c(days1, 30)
  }
}

datesSOI = cbind(dates1, days1)

years1 = c(rep(1950:2021, each = 12))
months1 = c(rep(1:12, length(unique(years1))))
dates1 = cbind(years1, months1)
days1 = NULL
for(i in 1:nrow(dates1)){
  if(dates1[i,2] == 1 | dates1[i,2] == 3 | dates1[i,2] == 5 | dates1[i,2] == 7 | dates1[i,2] == 8 | dates1[i,2] == 10 | dates1[i,2] == 12){
    days1 = c(days1, 31)
  }else if(dates1[i,2] == 2){
    if(dates1[i,1] %% 4 == 0){
      days1 = c(days1, 29)
    }else{
      days1 = c(days1, 28)
    }
  }else if(dates1[i,2] == 4 | dates1[i,2] == 6 | dates1[i,2] == 9 | dates1[i,2] == 11){
    days1 = c(days1, 30)
  }
}

datesIOD = cbind(dates1, days1)



SOI1 = as.vector(t(as.matrix(SOI)))
datesIOD = datesIOD[SOI1 < 90,]
SOI1 = SOI1[SOI1 < 90]
SOI.ave = NULL
dates3.ave = NULL
for(i in 2:(length(SOI1) - 1)){
  SOI.ave = c(SOI.ave, mean(SOI1[(i-1):(i + 1)]))
  dates3.ave = c(dates3.ave, paste0(datesSOI[i,1], "-", datesSOI[i,2], "-", "15"))
}

IOD1 = as.vector(t(as.matrix(IOD)))
SOI.ave = SOI.ave[-c(1:11)]
dates3.ave = dates3.ave[-c(1:11)]

SOI3 = cbind(SOI.ave, dates3.ave)

datesIOD = datesIOD[IOD < 90,]

datesIOD = paste0(datesIOD[,1], "-", datesIOD[,2], "-", "15")

IOD1 = IOD1[IOD1 < 90]

IOD.ave = IOD1

IOD3 = cbind(IOD.ave, datesIOD)


SOI.cor = rep(0, nrow(NOAA.all.aus))
w1 = which(dates3.ave =="1979-1-15")
w2 = which(dates3.ave == "2021-7-15")
for(i in 1:nrow(NOAA.all.aus)){
  SOI.cor[i] = cor(SOI.ave[w1:w2], NOAA.all.aus[i,])
}

SOI1 = SOI.ave[w1:w2]

IOD.cor = rep(0, nrow(NOAA.all.aus))
w1 = which(datesIOD[,1] == "1979")[1]
w2 = which(datesIOD[,1] == "2021")[7]
for(i in 1:nrow(NOAA.all.aus)){
  IOD.cor[i] = cor(IOD1[w1:w2], NOAA.all.aus[i,])
}

IOD1 = IOD1[w1:w2]


ARMAForecast = function(Y, k, p, q){
  Y.response = Y[(2*k + p + q + 1):length(Y)]
  Y.mat = matrix(0, nrow = length(Y.response), ncol = p + 1)
  Y.mat[,1] = rep(1, length(Y.response))
  for(i in 0:(p - 1)){
    Y.mat[,i + 2] = Y[(k + p + q + 1 - i):(length(Y) - k - i)]
  }
  pi.hat = solve(t(Y.mat) %*% Y.mat) %*% t(Y.mat) %*% Y.response
  E.mat = matrix(0, nrow = length(Y.response), ncol = q)
  for(i in 0:(nrow(E.mat) - 1)){
    for(j in 0:(ncol(E.mat) - 1)){
      E.mat[i + 1,j + 1] = Y[k + p + q + 1 - j + i] - sum(pi.hat * Y[(k + p + q + 1 - j + i):(k + q + 1 - j + i)])
    }
  }
  phi.hat = rep(0, p + 1)
  theta.hat = rep(0, q)
  k1 = 0
  phi.old = Inf
  while(abs(phi.hat[1] - phi.old[1]) > 0.0000001){
    phi.old = phi.hat
    phi.hat = solve(t(Y.mat) %*% Y.mat) %*% t(Y.mat) %*% (Y.response - E.mat %*% theta.hat)
    theta.hat = solve(t(E.mat) %*% E.mat) %*% t(E.mat) %*% (Y.response - Y.mat %*% phi.hat)
    k1 = k1 + 1
  }
  fitted = Y.mat %*% phi.hat + E.mat %*% theta.hat
  sigma2 = sum((fitted - Y.response)^2)/(length(Y.response))
  AIC = -log(sigma2) + 2 * (p + q)/length(Y.response)
  out = list(phi = phi.hat, theta = theta.hat, fitted = fitted, k1 = k1, y = Y.response, AIC = AIC)
  return(out)
}

Y = SOI.ave
k = 1
p = 5
q = 10

Ar1 = ARMAForecast(SOI.ave, 1, 5, 10)

plot(SOI.ave[(2 * 3 + 5 + 10 + 1):length(SOI.ave)] - Ar1$fitted, type = "l")

auto.arima(SOI.ave)


ARMA.CV = function(Y, k, p.max, q.max){
  CV.Mat = matrix(0, nrow = p.max, ncol = q.max)
  for(p in 1:p.max){
    for(q in 1:q.max){
      Y.response = Y[(2*k + p + q + 1):length(Y)]
      Y.mat = matrix(0, nrow = length(Y.response), ncol = p + 2)
      Y.mat[,1] = rep(1, length(Y.response))
      for(i in 0:(p)){
        Y.mat[,i + 2] = Y[(k + p + q + 1 - i):(length(Y) - k - i)]
      }
      CV.list = list()
      CV.list[[1]] = sample(1:length(Y.response), size = floor(length(Y.response) / 10))
      non = NULL
      for(h in 2:9){
        non = c(non, as.vector(CV.list[[h - 1]]))
        CV.list[[h]] = sort(sample((1:length(Y.response))[-non], floor(length(Y.response) / 10)))
      }
      CV.list[[10]] = sort((1:length(Y.response))[-non])
      err = rep(0, 10)
      for(h in 1:10){
        CV.vec = NULL
        for(i in (1:10)[-h]){
          CV.vec = c(CV.vec, CV.list[[i]])
        }
        CV.vec = sort(CV.vec)
        Y.response.CV = Y.response[CV.vec]
        Y.mat.CV = Y.mat[CV.vec, ]
        pi.hat = solve(t(Y.mat.CV) %*% Y.mat.CV) %*% t(Y.mat.CV) %*% Y.response.CV
        E.mat = matrix(0, nrow = length(Y.response.CV), ncol = q)
        for(i in 1:(nrow(E.mat))){
          for(j in 0:(ncol(E.mat) - 1)){
            E.mat[i,j + 1] = Y[CV.vec[i] + (2*k + p + q) - j] - 
              sum(pi.hat * c(1, Y[(CV.vec[i] + (2*k + p + q) - j - k):(CV.vec[i] + (2*k + p + q) - j - k - p)]))
          }
        }
        phi.hat = rep(0, p + 2)
        theta.hat = rep(0, q)
        g = 0
        phi.old = Inf
        while(abs(phi.hat[1] - phi.old[1]) > 0.0000001){
          phi.old = phi.hat
          phi.hat = solve(t(Y.mat.CV) %*% Y.mat.CV) %*% t(Y.mat.CV) %*% (Y.response.CV - E.mat %*% theta.hat)
          theta.hat = solve(t(E.mat) %*% E.mat) %*% t(E.mat) %*% (Y.response.CV - Y.mat.CV %*% phi.hat)
          g = g + 1
        }
        E.new = matrix(0, nrow = length(CV.list[[h]]), ncol = q)
        for(i in 1:nrow(E.new)){
          for(j in 0:(ncol(E.new) - 1)){
            E.new[i, j + 1] = Y[CV.list[[h]][i] + (2*k + p + q) - j] - 
              sum(pi.hat * c(1, Y[(CV.list[[h]][i] + (2*k + p + q) - j - k):(CV.list[[h]][i] + (2*k + p + q) - j - k - p)]))
          }
        }
        err[h] = sum((Y.response[CV.list[[h]]] - Y.mat[CV.list[[h]],] %*% phi.hat - E.new %*% theta.hat)^2)/(length(CV.list[[h]]) - p -  q)
      }
      CV.Mat[p, q] = mean(err)
    }
  }
  CV.Mat
}



Ar1 = ARMA.CV(SOI.ave, 1, 15, 15)
AR.cv = matrix(0, 15, 15)
for(i in 1:15){
  for(j in 1:15){
    Ar1 = ARMAForecast(SOI.ave, 6, i, j)
    AR.cv[i,j] = Ar1$AIC
  }
}

which(Ar1 == min(Ar1), arr.ind = TRUE)

Ar2 = ARMAForecast(SOI.ave, 1, 2, 15)

plot(SOI.ave[(2 * 1 + 2 + 15 + 1):length(SOI.ave)] - Ar2$fitted, type = "l")

sum((SOI.ave[(2 * 1 + 2 + 15 + 1):length(SOI.ave)] - Ar2$fitted)^2/ (length(Ar2$fitted) - 17))

ar1 = auto.arima(SOI.ave)

split1 = split(1:100, 1:10, drop = TRUE)
as.vector(split1[1:2])

Best.SOI = NULL
for(i in 1:6){
  Ar1 = ARMA.CV(SOI.ave, i, 15, 15)
  Best.SOI = rbind(Best.SOI, which(Ar1 == min(Ar1), arr.ind = TRUE))
}


Ar2 = ARMAForecast(SOI.ave, 1, 2, 15)
Ar3 = ARMAForecast(SOI.ave, 3, 1, 8)
Ar6 = ARMAForecast(SOI.ave, 6, 1, 8)
plot(Ar2$fitted - SOI.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)], type = "l")


SOI.ar.df1 = data.frame(SOI = c(SOI.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)], Ar2$fitted),
                        k = rep(1, length(c(SOI.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)], Ar2$fitted))),
                        Type = rep(c("Acutal", "Fitted"), each = length(Ar2$fitted)),
                        Dates = as.Date(rep(dates3.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)], 2)))


SOI.ar.df3 = data.frame(SOI = c(SOI.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)], Ar3$fitted),
                        k = rep(1, length(c(SOI.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)], Ar3$fitted))),
                        Type = rep(c("Acutal", "Fitted"), each = length(Ar3$fitted)),
                        as.Date(rep(dates3.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)], 2)))


SOI.ar.df6 = data.frame(SOI = c(SOI.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)], Ar6$fitted),
                        k = rep(1, length(c(SOI.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)], Ar6$fitted))),
                        Type = rep(c("Acutal", "Fitted"), each = length(Ar6$fitted)),
                        as.Date(rep(dates3.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)], 2)))

SOI.ar.df1.1 = data.frame(SOI = c(SOI.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)]),
                          k = rep(1, length(c(SOI.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)]))),
                          Type = rep(c("Acutal"), length(Ar2$fitted)),
                          Dates = as.Date(dates3.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)]))

SOI.ar.df1.2 = data.frame(SOI = c(Ar2$fitted),
                          k = rep(1, length(c(SOI.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)]))),
                          Type = rep(c("Fitted"), length(Ar2$fitted)),
                          Dates = as.Date(dates3.ave[(2*1 + 2 + 15 + 1):length(SOI.ave)]))


SOI.ar.df1.1 = SOI.ar.df1.1[-c(1:which(SOI.ar.df1.1$Dates == "1981-01-15")[1] - 1),]
SOI.ar.df1.2 = SOI.ar.df1.2[-c(1:which(SOI.ar.df1.2$Dates == "1981-01-15")[1] - 1),]

SOI.ar.df3.1 = data.frame(SOI = c(SOI.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)]),
                          k = rep(3, length(c(SOI.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)]))),
                          Type = rep(c("Acutal"), length(Ar3$fitted)),
                          Dates = as.Date(dates3.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)]))

SOI.ar.df3.2 = data.frame(SOI = c(Ar3$fitted),
                          k = rep(3, length(c(SOI.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)]))),
                          Type = rep(c("Fitted"), length(Ar3$fitted)),
                          Dates = as.Date(dates3.ave[(2*3 + 1 + 8 + 1):length(SOI.ave)]))

SOI.ar.df3.1 = SOI.ar.df3.1[-c(1:which(SOI.ar.df3.1$Dates == "1981-01-15")[1] - 1),]
SOI.ar.df3.2 = SOI.ar.df3.2[-c(1:which(SOI.ar.df3.2$Dates == "1981-01-15")[1] - 1),]

SOI.ar.df6.1 = data.frame(SOI = c(SOI.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)]),
                          k = rep(6, length(c(SOI.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)]))),
                          Type = rep(c("Acutal"), length(Ar6$fitted)),
                          Dates = as.Date(dates3.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)]))

SOI.ar.df6.2 = data.frame(SOI = c(Ar6$fitted),
                          k = rep(6, length(c(SOI.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)]))),
                          Type = rep(c("Fitted"), length(Ar6$fitted)),
                          Dates = as.Date(dates3.ave[(2*6 + 1 + 8 + 1):length(SOI.ave)]))

SOI.ar.df6.1 = SOI.ar.df6.1[-c(1:which(SOI.ar.df6.1$Dates == "1981-01-15")[1] - 1),]
SOI.ar.df6.2 = SOI.ar.df6.2[-c(1:which(SOI.ar.df6.2$Dates == "1981-01-15")[1] - 1),]

SOI.ar.df = rbind(SOI.ar.df1.1, SOI.ar.df1.2, SOI.ar.df3.1, SOI.ar.df3.2, SOI.ar.df6.1, SOI.ar.df6.2)

ggplot(SOI.ar.df) + geom_line(aes(x = Dates, y = SOI, colour = Type, linetype = Type), size = 0.5) +
  theme_bw() + facet_wrap(~k, nrow = 3, labeller = as_labeller(c(`1` = "One Month Lead", `3` = "Three Month Lead", `6` = "Six Month Lead"))) +
  scale_color_manual(values = c("blue","red"))  +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right",
        strip.text = element_text(size = 12, colour = "black")) +
  labs(x = "Date", y = "Index", colour = "Type", title = "Southern Oscillation Index Forecasting")

plot(SOI.ar.df1.1$SOI[100:120], type = "l")
lines(SOI.ar.df1.2$SOI[100:120], col = "blue")
plot(SOI.ar.df1.1$SOI[100:120]- SOI.ar.df1.2$SOI[100:120])

which(SOI.ar.df1.1$SOI[100:120]- SOI.ar.df1.2$SOI[100:120] == max(SOI.ar.df1.1$SOI[100:120]- SOI.ar.df1.2$SOI[100:120]))

SOI.ar.df1.1$SOI[112]
SOI.ar.df1.2$SOI[112]


Best.IOD = NULL
for(i in 1:6){
  Ar1 = ARMA.CV(IOD.ave, i, 15, 15)
  Best.IOD = rbind(Best.IOD, which(Ar1 == min(Ar1), arr.ind = TRUE))
  print(i)
}


Ar2 = ARMAForecast(IOD.ave, 1, 13, 12)
Ar3 = ARMAForecast(IOD.ave, 3, 15, 11)
Ar6 = ARMAForecast(IOD.ave, 6, 15, 11)
plot(Ar2$fitted - IOD.ave[(2*1 + 13 + 12 + 1):length(IOD.ave)], type = "l")

IOD.ar.df1.1 = data.frame(IOD = c(IOD.ave[(2*1 + 13 + 12 + 1):length(IOD.ave)]),
                          k = rep(1, length(c(IOD.ave[(2*1 + 13 + 12 + 1):length(IOD.ave)]))),
                          Type = rep(c("Acutal"), length(Ar2$fitted)),
                          Dates = as.Date(datesIOD[(2*1 + 13 + 12 + 1):length(IOD.ave)]))

IOD.ar.df1.2 = data.frame(IOD = c(Ar2$fitted),
                          k = rep(1, length(c(IOD.ave[(2*1 + 13 + 12 + 1):length(IOD.ave)]))),
                          Type = rep(c("Fitted"), length(Ar2$fitted)),
                          Dates = as.Date(datesIOD[(2*1 + 13 + 12 + 1):length(IOD.ave)]))


IOD.ar.df1.1 = IOD.ar.df1.1[-c(1:which(IOD.ar.df1.1$Dates == "1981-01-15")[1] - 1),]
IOD.ar.df1.2 = IOD.ar.df1.2[-c(1:which(IOD.ar.df1.2$Dates == "1981-01-15")[1] - 1),]

dates.IOD.ave = dates3.ave[(2*1 + 13 + 12 + 1):length(IOD.ave)]

IOD.ar.df3.1 = data.frame(IOD = c(IOD.ave[(2*3 + 15 + 11 + 1):length(IOD.ave)]),
                          k = rep(3, length(c(IOD.ave[(2*3 + 15 + 11 + 1):length(IOD.ave)]))),
                          Type = rep(c("Acutal"), length(Ar3$fitted)),
                          Dates = as.Date(datesIOD[(2*3 + 15 + 11 + 1):length(IOD.ave)]))

IOD.ar.df3.2 = data.frame(IOD = c(Ar3$fitted),
                          k = rep(3, length(c(IOD.ave[(2*3 + 15 + 11 + 1):length(IOD.ave)]))),
                          Type = rep(c("Fitted"), length(Ar3$fitted)),
                          Dates = as.Date(datesIOD[(2*3 + 15 + 11 + 1):length(IOD.ave)]))

IOD.ar.df3.1 = IOD.ar.df3.1[-c(1:which(IOD.ar.df3.1$Dates == "1981-01-15")[1] - 1),]
IOD.ar.df3.2 = IOD.ar.df3.2[-c(1:which(IOD.ar.df3.2$Dates == "1981-01-15")[1] - 1),]

IOD.ar.df6.1 = data.frame(IOD = c(IOD.ave[(2*6 + 15 + 11 + 1):length(IOD.ave)]),
                          k = rep(6, length(c(IOD.ave[(2*6 + 15 + 11 + 1):length(IOD.ave)]))),
                          Type = rep(c("Acutal"), length(Ar6$fitted)),
                          Dates = as.Date(datesIOD[(2*6 + 15 + 11 + 1):length(IOD.ave)]))

IOD.ar.df6.2 = data.frame(IOD = c(Ar6$fitted),
                          k = rep(6, length(c(IOD.ave[(2*6 + 15 + 11 + 1):length(IOD.ave)]))),
                          Type = rep(c("Fitted"), length(Ar6$fitted)),
                          Dates = as.Date(datesIOD[(2*6 + 15 + 11 + 1):length(IOD.ave)]))

IOD.ar.df6.1 = IOD.ar.df6.1[-c(1:which(IOD.ar.df6.1$Dates == "1981-01-15")[1] - 1),]
IOD.ar.df6.2 = IOD.ar.df6.2[-c(1:which(IOD.ar.df6.2$Dates == "1981-01-15")[1] - 1),]

IOD.ar.df = rbind(IOD.ar.df1.1, IOD.ar.df1.2, IOD.ar.df3.1, IOD.ar.df3.2, IOD.ar.df6.1, IOD.ar.df6.2)

ggplot(IOD.ar.df) + geom_line(aes(x = Dates, y = IOD, colour = Type, linetype = Type), size = 0.5) +
  theme_bw() + facet_wrap(~k, nrow = 3, labeller = as_labeller(c(`1` = "One Month Lead", `3` = "Three Month Lead", `6` = "Six Month Lead"))) +
  scale_color_manual(values = c("blue", "red")) +
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=16), legend.position = "right",
        strip.text = element_text(size = 12, colour = "black")) +
  labs(x = "Date", y = "Index", colour = "Type", title = "Dipole Mode Index Forecasting")


#####################
#Comparison
#####################

ts1 = arima.sim(n = 500, list(ar = c(0.8897, -0.4858), ma = c(-0.2279, 0.2488), d = 1), sd = sqrt(0.1796))


Ar2 = arima(ts1, order = c(2,0,2))


Ar1 = ARMAForecast(ts1, k = 1, p = 2, q = 2)

length(ts1)
length(Ar2$residuals)

sum(Ar2$residuals^2)
sum((Ar1$fitted - ts1[-c(1:6)])^2)



##################
#Difference ARIMA
##################

Arima1 = ARMA.CV(diff(SOI.ave), k = 1, p.max = 15, q.max = 15)

Ar1 = ARMAForecast(diff(SOI.ave), k = 1, p = 2, q = 9)

plot(Ar1$fitted - diff(SOI.ave)[-c(1:13)], type = "l")

plot(diff(SOI.ave), type = "l")


























