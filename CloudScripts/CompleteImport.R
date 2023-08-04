years = c(rep(2000,10),sort(rep(seq(2001,2018),12)), rep(2019,6))
months = c(rep(c("01","02","03","04","05","06","07","08","09","10","11","12"),19), "01", "02", "03", "04", "05", "06")
days = c(rep(c("31","28","31","30","31","30","31","31","30","31","30","31"),19),"31","28","31","30", "31", "30")
months = cbind(months, days)
months = months[-(1:2),]
dates = cbind(years, months)
for(i in 1:(dim(dates)[1])){
  if(dates[i,2] == "02"){
    if(dates[i,1] == "2004" | dates[i,1] == "2008" | dates[i,1] == "2012" | dates[i,1] == "2016"){
      dates[i,3] = "29"
    }
  }
}

AsiaEE = list()
for(i in 1:nrow(dates)){
  AsiaEE[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AsiaEE/master/AsiaEE", dates[i,1], dates[i,2]))
  print(i)
}

AsiaSE = list()
for(i in 1:nrow(dates)){
  AsiaSE[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AsiaSE/master/AsiaSE", dates[i,1], dates[i,2]))
  print(i)
}
getwd()
aus = readOGR(".", "COM20111216_ELB_region")
Austra = list()
for(i in 1:nrow(dates)){
  Austra[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/Aus.Monthly/master/AusGrid/Grid", dates[i,1], dates[i,2]))
  print(i)
}

A1 = Austra[[1]]
coordinates(A1) =~Lon + Lat
proj4string(A1) = "+proj=longlat +ellps=GRS80 +no_defs"
a3 = over(A1, aus)

A2 = as.data.frame(A1)
A2 = A2[,-1]
A2 = A2[!is.na(a3[,1]),]
dim(A2)

for(i in 1:length(Austra)){
  Austra[[i]] = Austra[[i]][,-1]
}

for(i in 1:length(Austra)){
  Austra[[i]] = Austra[[i]][!is.na(a3[,1]),]
}

AsiaCC = list()
for(i in 1:nrow(dates)){
  AsiaCC[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AsiaCC/master/AsiaCC", dates[i,1], dates[i,2]))
  print(i)
}

AsiaSS = list()
for(i in 1:nrow(dates)){
  AsiaSS[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AsiaSS/master/AsiaSS", dates[i,1], dates[i,2]))
  print(i)
}

AsiaSW = list()
for(i in 1:nrow(dates)){
  AsiaSW[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AsiaSW/master/AsiaSW", dates[i,1], dates[i,2]))
  print(i)
}

Europe = list()
for(i in 1:nrow(dates)){
  Europe[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/Europe/master/Europe", dates[i,1], dates[i,2]))
  print(i)
}

AfriNW = list()
for(i in 1:nrow(dates)){
  AfriNW[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AfriNW/master/AfriNW", dates[i,1], dates[i,2]))
  print(i)
}

AfriSN = list()
for(i in 1:nrow(dates)){
  AfriSN[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AfriSN/master/AfriSN", dates[i,1], dates[i,2]))
  print(i)
}

AfriSS = list()
for(i in 1:nrow(dates)){
  AfriSS[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/AfriSS/master/AfriSS", dates[i,1], dates[i,2]))
  print(i)
}

UsaCon = list()
for(i in 1:nrow(dates)){
  UsaCon[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/UsaCon/master/UsaCon", dates[i,1], dates[i,2]))
  print(i)
}

C_Amer = list()
for(i in 1:nrow(dates)){
  C_Amer[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/C_Amer/master/C_Amer", dates[i,1], dates[i,2]))
  print(i)
}

SAmerN = list()
for(i in 1:nrow(dates)){
  SAmerN[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/SAmerN/master/SAmerN", dates[i,1], dates[i,2]))
  print(i)
}

SAmerC = list()
for(i in 1:nrow(dates)){
  SAmerC[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/SAmerC/master/SAmerC", dates[i,1], dates[i,2]))
  print(i)
}

SAmerS = list()
for(i in 1:nrow(dates)){
  SAmerS[[i]] = read.csv(paste0("https://raw.githubusercontent.com/hinestein/SAmerS/master/SAmerS", dates[i,1], dates[i,2]))
  print(i)
}





#Checking Dimensions of each Section
nrowEE = NULL
for(i in 1:length(AsiaEE)){
  nrowEE = c(nrowEE, nrow(AsiaEE[[i]]))
}
table(nrowEE)
unique(nrowEE)

nrowSE = NULL
for(i in 1:length(AsiaSE)){
  nrowSE = c(nrowSE, nrow(AsiaSE[[i]]))
}
unique(nrowSE)

nrowAustra = NULL
for(i in 1:length(Austra)){
  nrowAustra = c(nrowAustra, nrow(Austra[[i]]))
}
unique(nrowAustra)

nrowCC = NULL
for(i in 1:length(AsiaCC)){
  nrowCC = c(nrowCC, nrow(AsiaCC[[i]]))
}
unique(nrowCC)

nrowSS = NULL
for(i in 1:length(AsiaSS)){
  nrowSS = c(nrowSS, nrow(AsiaSS[[i]]))
}
unique(nrowSS)

nrowSW = NULL
for(i in 1:length(AsiaSW)){
  nrowSW = c(nrowSW, nrow(AsiaSW[[i]]))
}
unique(nrowSW)

nrowEU = NULL
for(i in 1:length(Europe)){
  nrowEU = c(nrowEU, nrow(Europe[[i]]))
}
unique(nrowEU)

nrowAfriNW = NULL
for(i in 1:length(AfriNW)){
  nrowAfriNW = c(nrowAfriNW, nrow(AfriNW[[i]]))
}
unique(nrowAfriNW)

nrowAfriSN = NULL
for(i in 1:length(AfriSN)){
  nrowAfriSN = c(nrowAfriSN, nrow(AfriSN[[i]]))
}
unique(nrowAfriSN)

nrowAfriSS = NULL
for(i in 1:length(AfriSS)){
  nrowAfriSS = c(nrowAfriSS, nrow(AfriSS[[i]]))
}
unique(nrowAfriSS)

nrowUS = NULL
for(i in 1:length(UsaCon)){
  nrowUS = c(nrowUS, nrow(UsaCon[[i]]))
}
unique(nrowUS)

nrowCAmer = NULL
for(i in 1:length(C_Amer)){
  nrowCAmer = c(nrowCAmer, nrow(C_Amer[[i]]))
}
unique(nrowCAmer)

nrowSAmerN = NULL
for(i in 1:length(SAmerN)){
  nrowSAmerN = c(nrowSAmerN, nrow(SAmerN[[i]]))
}
unique(nrowSAmerN)

nrowSAmerC = NULL
for(i in 1:length(SAmerC)){
  nrowSAmerC = c(nrowSAmerC, nrow(SAmerC[[i]]))
}
unique(nrowSAmerC)

nrowSAmerS = NULL
for(i in 1:length(SAmerS)){
  nrowSAmerS = c(nrowSAmerS, nrow(SAmerS[[i]]))
}
unique(nrowSAmerS)




Austra.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, Austra[[12*i + j]][,3])
    Austra.M[[j]] = S2
  }
}

AsiaSE.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, AsiaSE[[12*i + j]][,3])
    AsiaSE.M[[j]] = S2
  }
}




VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.1.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.1.cor = c(VB.1.AsiaSE.1.cor, cor(VB.1, AsiaSE.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

AsiaCC.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, AsiaCC[[12*i + j]][,3])
    AsiaCC.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaCC.1.cor = NULL
for(i in 1:nrow(AsiaCC.M[[1]])){
  VB.1.AsiaCC.1.cor = c(VB.1.AsiaCC.1.cor, cor(VB.1, AsiaCC.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

SAmerS.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, SAmerS[[12*i + j]][,3])
    SAmerS.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.SAmerS.1.cor = NULL
for(i in 1:nrow(SAmerS.M[[1]])){
  VB.1.SAmerS.1.cor = c(VB.1.SAmerS.1.cor, cor(VB.1, SAmerS.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

SAmerC.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, SAmerC[[12*i + j]][,3])
    SAmerC.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.SAmerC.1.cor = NULL
for(i in 1:nrow(SAmerC.M[[1]])){
  VB.1.SAmerC.1.cor = c(VB.1.SAmerC.1.cor, cor(VB.1, SAmerC.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

SAmerN.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, SAmerN[[12*i + j]][,3])
    SAmerN.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.SAmerN.1.cor = NULL
for(i in 1:nrow(SAmerN.M[[1]])){
  VB.1.SAmerN.1.cor = c(VB.1.SAmerN.1.cor, cor(VB.1, SAmerN.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}



#Australian Correlation
VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.1.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.1.cor = c(VB.1.Austra.1.cor, cor(VB.1, Austra.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.2.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.2.cor = c(VB.1.Austra.2.cor, cor(VB.1, Austra.M[[2]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.3.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.3.cor = c(VB.1.Austra.3.cor, cor(VB.1, Austra.M[[3]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.4.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.4.cor = c(VB.1.Austra.4.cor, cor(VB.1, Austra.M[[4]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.5.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.5.cor = c(VB.1.Austra.5.cor, cor(VB.1, Austra.M[[5]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.6.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.6.cor = c(VB.1.Austra.6.cor, cor(VB.1, Austra.M[[6]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.7.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.7.cor = c(VB.1.Austra.7.cor, cor(VB.1, Austra.M[[7]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.8.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.8.cor = c(VB.1.Austra.8.cor, cor(VB.1, Austra.M[[8]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.9.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.9.cor = c(VB.1.Austra.9.cor, cor(VB.1, Austra.M[[9]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.10.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.10.cor = c(VB.1.Austra.10.cor, cor(VB.1, Austra.M[[10]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.11.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.11.cor = c(VB.1.Austra.11.cor, cor(VB.1, Austra.M[[11]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Austra.12.cor = NULL
for(i in 1:nrow(Austra.M[[1]])){
  VB.1.Austra.12.cor = c(VB.1.Austra.12.cor, cor(VB.1, Austra.M[[12]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.1.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.2.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.3.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.4.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.5.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.6.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.7.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.8.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.9.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.10.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.11.cor, cex = 0.2, add = TRUE)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][,2], Austra[[1]][,1], colvar = VB.1.Austra.12.cor, cex = 0.2, add = TRUE)


#AsiaSE correlation
AsiaSE.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, AsiaSE[[12*i + j]][,3])
    AsiaSE.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.1.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.1.cor = c(VB.1.AsiaSE.1.cor, cor(VB.1, AsiaSE.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.2.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.2.cor = c(VB.1.AsiaSE.2.cor, cor(VB.1, AsiaSE.M[[2]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.3.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.3.cor = c(VB.1.AsiaSE.3.cor, cor(VB.1, AsiaSE.M[[3]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.4.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.4.cor = c(VB.1.AsiaSE.4.cor, cor(VB.1, AsiaSE.M[[4]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.5.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.5.cor = c(VB.1.AsiaSE.5.cor, cor(VB.1, AsiaSE.M[[5]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.6.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.6.cor = c(VB.1.AsiaSE.6.cor, cor(VB.1, AsiaSE.M[[6]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.7.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.7.cor = c(VB.1.AsiaSE.7.cor, cor(VB.1, AsiaSE.M[[7]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.8.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.8.cor = c(VB.1.AsiaSE.8.cor, cor(VB.1, AsiaSE.M[[8]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.9.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.9.cor = c(VB.1.AsiaSE.9.cor, cor(VB.1, AsiaSE.M[[9]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.10.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.10.cor = c(VB.1.AsiaSE.10.cor, cor(VB.1, AsiaSE.M[[10]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.11.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.11.cor = c(VB.1.AsiaSE.11.cor, cor(VB.1, AsiaSE.M[[11]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AsiaSE.12.cor = NULL
for(i in 1:nrow(AsiaSE.M[[1]])){
  VB.1.AsiaSE.12.cor = c(VB.1.AsiaSE.12.cor, cor(VB.1, AsiaSE.M[[12]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}


par(mfrow = c(1,1))
scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.1.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.2.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.3.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.4.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.5.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.6.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.7.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.8.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.9.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.10.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.11.cor, cex = 0.2)

scatter2D(AsiaSE[[1]][,2], AsiaSE[[1]][,1], colvar = VB.1.AsiaSE.12.cor, cex = 0.2)

#C_Amer correlation
C_Amer.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, C_Amer[[12*i + j]][,3])
    C_Amer.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.1.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.1.cor = c(VB.1.C_Amer.1.cor, cor(VB.1, C_Amer.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.2.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.2.cor = c(VB.1.C_Amer.2.cor, cor(VB.1, C_Amer.M[[2]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.3.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.3.cor = c(VB.1.C_Amer.3.cor, cor(VB.1, C_Amer.M[[3]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.4.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.4.cor = c(VB.1.C_Amer.4.cor, cor(VB.1, C_Amer.M[[4]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.5.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.5.cor = c(VB.1.C_Amer.5.cor, cor(VB.1, C_Amer.M[[5]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.6.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.6.cor = c(VB.1.C_Amer.6.cor, cor(VB.1, C_Amer.M[[6]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.7.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.7.cor = c(VB.1.C_Amer.7.cor, cor(VB.1, C_Amer.M[[7]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.8.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.8.cor = c(VB.1.C_Amer.8.cor, cor(VB.1, C_Amer.M[[8]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.9.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.9.cor = c(VB.1.C_Amer.9.cor, cor(VB.1, C_Amer.M[[9]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.10.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.10.cor = c(VB.1.C_Amer.10.cor, cor(VB.1, C_Amer.M[[10]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.11.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.11.cor = c(VB.1.C_Amer.11.cor, cor(VB.1, C_Amer.M[[11]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.C_Amer.12.cor = NULL
for(i in 1:nrow(C_Amer.M[[1]])){
  VB.1.C_Amer.12.cor = c(VB.1.C_Amer.12.cor, cor(VB.1, C_Amer.M[[12]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}


par(mfrow = c(1,1))
scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.1.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.2.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.3.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.4.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.5.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.6.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.7.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.8.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.9.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.10.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.11.cor, cex = 0.2)

scatter2D(C_Amer[[1]][,2], C_Amer[[1]][,1], colvar = VB.1.C_Amer.12.cor, cex = 0.2)


#AfriNW correlation
AfriNW.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, AfriNW[[12*i + j]][,3])
    AfriNW.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.1.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.1.cor = c(VB.1.AfriNW.1.cor, cor(VB.1, AfriNW.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.2.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.2.cor = c(VB.1.AfriNW.2.cor, cor(VB.1, AfriNW.M[[2]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.3.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.3.cor = c(VB.1.AfriNW.3.cor, cor(VB.1, AfriNW.M[[3]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.4.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.4.cor = c(VB.1.AfriNW.4.cor, cor(VB.1, AfriNW.M[[4]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.5.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.5.cor = c(VB.1.AfriNW.5.cor, cor(VB.1, AfriNW.M[[5]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.6.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.6.cor = c(VB.1.AfriNW.6.cor, cor(VB.1, AfriNW.M[[6]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.7.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.7.cor = c(VB.1.AfriNW.7.cor, cor(VB.1, AfriNW.M[[7]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.8.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.8.cor = c(VB.1.AfriNW.8.cor, cor(VB.1, AfriNW.M[[8]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.9.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.9.cor = c(VB.1.AfriNW.9.cor, cor(VB.1, AfriNW.M[[9]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.10.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.10.cor = c(VB.1.AfriNW.10.cor, cor(VB.1, AfriNW.M[[10]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.11.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.11.cor = c(VB.1.AfriNW.11.cor, cor(VB.1, AfriNW.M[[11]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.AfriNW.12.cor = NULL
for(i in 1:nrow(AfriNW.M[[1]])){
  VB.1.AfriNW.12.cor = c(VB.1.AfriNW.12.cor, cor(VB.1, AfriNW.M[[12]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}


par(mfrow = c(1,1))
scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.1.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.2.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.3.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.4.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.5.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.6.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.7.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.8.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.9.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.10.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.11.cor, cex = 0.2)

scatter2D(AfriNW[[1]][,2], AfriNW[[1]][,1], colvar = VB.1.AfriNW.12.cor, cex = 0.2)



#Europe correlation
Europe.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, Europe[[12*i + j]][,3])
    Europe.M[[j]] = S2
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.1.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.1.cor = c(VB.1.Europe.1.cor, cor(VB.1, Europe.M[[1]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.2.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.2.cor = c(VB.1.Europe.2.cor, cor(VB.1, Europe.M[[2]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.3.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.3.cor = c(VB.1.Europe.3.cor, cor(VB.1, Europe.M[[3]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.4.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.4.cor = c(VB.1.Europe.4.cor, cor(VB.1, Europe.M[[4]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.5.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.5.cor = c(VB.1.Europe.5.cor, cor(VB.1, Europe.M[[5]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.6.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.6.cor = c(VB.1.Europe.6.cor, cor(VB.1, Europe.M[[6]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.7.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.7.cor = c(VB.1.Europe.7.cor, cor(VB.1, Europe.M[[7]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.8.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.8.cor = c(VB.1.Europe.8.cor, cor(VB.1, Europe.M[[8]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.9.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.9.cor = c(VB.1.Europe.9.cor, cor(VB.1, Europe.M[[9]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.10.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.10.cor = c(VB.1.Europe.10.cor, cor(VB.1, Europe.M[[10]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.11.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.11.cor = c(VB.1.Europe.11.cor, cor(VB.1, Europe.M[[11]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}

VB = 56577
VB.1 = Austra.M[[1]][VB,]
VB.1.Europe.12.cor = NULL
for(i in 1:nrow(Europe.M[[1]])){
  VB.1.Europe.12.cor = c(VB.1.Europe.12.cor, cor(VB.1, Europe.M[[12]][i,]))
  if(i %% 10000 == 0){
    print(i)
  }
}


par(mfrow = c(1,1))
scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.1.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.2.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.3.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.4.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.5.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.6.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.7.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.8.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.9.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.10.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.11.cor, cex = 0.2)

scatter2D(Europe[[1]][,2], Europe[[1]][,1], colvar = VB.1.Europe.12.cor, cex = 0.2)




#One month prediction May
Austra.M = list()
for(j in 1:12){
  S2 = NULL
  for(i in 0:18){
    S2 = cbind(S2, Austra[[12*i + j]][,3])
    Austra.M[[j]] = S2
  }
}


set.seed(1998)
samp1 = sample(1:nrow(Austra.M[[3]]), size = 3000)
S3 = Austra.M[[3]][samp1,]
plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], col = "blue", cex = 0.2, add = TRUE)

sampEU = sample(1:nrow(Europe.M[[2]]), size = 1000)
EU2 = Europe.M[[2]][sampEU,]
Europe.2.cor = NULL
for(i in 1:nrow(S3)){
  cor1 = NULL
  for(j in 1:nrow(EU2)){
    cor1 = c(cor1, cor(S3[i,], EU2[j,]))
  }
  Europe.2.cor = rbind(Europe.2.cor, cor1)
  if(i %% 100 == 0){
    print(i)
  }
}

dim(Europe.2.cor)

EUmax = NULL
for(i in 1:nrow(Europe.2.cor)){
  EUmax = c(EUmax, which(Europe.2.cor[i,] == max(Europe.2.cor[i,])))
}
EUmin = NULL
for(i in 1:nrow(Europe.2.cor)){
  EUmin = c(EUmin, which(Europe.2.cor[i,] == min(Europe.2.cor[i,])))
}

mod1 = lm((S3[,18])^(1/2) ~ EU2[EUmax,18] + EU2[EUmin,18])
summary(mod1)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = S3[,18] - (mod1$fitted.values)^2, cex = 0.2, add = TRUE)




sampANW = sample(1:nrow(AfriNW.M[[2]]), size = 1000)
ANW2 = AfriNW.M[[2]][sampANW,]
ANW.2.cor = NULL
for(i in 1:nrow(S3)){
  cor1 = NULL
  for(j in 1:nrow(ANW2)){
    cor1 = c(cor1, cor(S3[i,], ANW2[j,]))
  }
  ANW.2.cor = rbind(ANW.2.cor, cor1)
  if(i %% 100 == 0){
    print(i)
  }
}

ANWmax = NULL
for(i in 1:nrow(ANW.2.cor)){
  ANWmax = c(ANWmax, which(ANW.2.cor[i,] == max(ANW.2.cor[i,])))
}
ANWmin = NULL
for(i in 1:nrow(ANW.2.cor)){
  ANWmin = c(ANWmin, which(ANW.2.cor[i,] == min(ANW.2.cor[i,])))
}

mod1 = lm((S3[,18])^(1/2) ~ EU2[EUmax,18] + EU2[EUmin,18] + ANW2[ANWmax,18] + ANW2[ANWmin, 18])
summary(mod1)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = (mod1$fitted.values)^2, cex = 0.2, add = TRUE)

sampCAM = sample(1:nrow(C_Amer.M[[2]]), size = 1000)
CAM2 = C_Amer.M[[2]][sampCAM,]
CAM.2.cor = NULL
for(i in 1:nrow(S3)){
  cor1 = NULL
  for(j in 1:nrow(CAM2)){
    cor1 = c(cor1, cor(S3[i,], CAM2[j,]))
  }
  CAM.2.cor = rbind(CAM.2.cor, cor1)
  if(i %% 100 == 0){
    print(i)
  }
}

CAMmax = NULL
for(i in 1:nrow(CAM.2.cor)){
  CAMmax = c(CAMmax, which(CAM.2.cor[i,] == max(CAM.2.cor[i,])))
}
CAMmin = NULL
for(i in 1:nrow(CAM.2.cor)){
  CAMmin = c(CAMmin, which(CAM.2.cor[i,] == min(CAM.2.cor[i,])))
}

mod1 = lm((S3[,18])^(1/2) ~ EU2[EUmax,18] + EU2[EUmin,18] + ANW2[ANWmax,18] + ANW2[ANWmin, 18] + CAM2[CAMmax,18] + CAM2[CAMmin,18])
summary(mod1)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = (mod1$fitted.values)^2 - (S3[,18]), cex = 0.2, add = TRUE)

sampASE = sample(1:nrow(AsiaSE.M[[2]]), size = 1000)
ASEM2 = AsiaSE.M[[2]][sampASE,]
ASE.2.cor = NULL
for(i in 1:nrow(S3)){
  cor1 = NULL
  for(j in 1:nrow(ASEM2)){
    cor1 = c(cor1, cor(S3[i,], ASEM2[j,]))
  }
  ASE.2.cor = rbind(ASE.2.cor, cor1)
  if(i %% 100 == 0){
    print(i)
  }
}

ASEmax = NULL
for(i in 1:nrow(ASE.2.cor)){
  ASEmax = c(ASEmax, which(ASE.2.cor[i,] == max(ASE.2.cor[i,])))
}
ASEmin = NULL
for(i in 1:nrow(ASE.2.cor)){
  ASEmin = c(ASEmin, which(ASE.2.cor[i,] == min(ASE.2.cor[i,])))
}

d1 = data.frame()

mod1 = lm((S3[,18])^(1/2) ~ EU2[EUmax,18] + EU2[EUmin,18] + ANW2[ANWmax,18] + 
            ANW2[ANWmin, 18] + CAM2[CAMmax,18] + CAM2[CAMmin,18] + 
            ASEM2[ASEmax,18] + ASEM2[ASEmin,18]) 
summary(mod1)

plot(newmap, xlim = c(109.4,154.6), ylim = c(-43.65,-10.58))
scatter2D(Austra[[1]][samp1,2], Austra[[1]][samp1,1], colvar = (mod1$fitted.values)^2, cex = 0.2, add = TRUE)

