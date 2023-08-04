

slid.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  slid.mat[,i] = slid(matrix(as.numeric(New.Gauge1[,i]), ncol = 1), 750)[,1]
  print(i)
}

l1 = 0.935

slid.matA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
for(i in 1:(ncol(slid.mat)/2)){
  A = slid.mat[,i]
  A1 = quantile(A, l1)
  slid.matA1[A > A1, i] = 1
}

slid.matA2 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
k = 1
for(i in (ncol(slid.mat)/2 + 1):ncol(slid.mat)){
  A = slid.mat[,i]
  A1 = quantile(A, l1)
  slid.matA2[A > A1, k] = 1
  k = k + 1
}

slid.diffA = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  slid.diffA[,i] = -(rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) - rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)]))
}

slid.diffA = slid.diffA[,c(10:12,1:9)]


slid.diffA1 = data.frame(Difference = as.vector(slid.diffA), Month = rep(1:12, each = nrow(New.Gauge1)),
                         Lon = rep(FusedLoc$Lon, 12), Lat = rep(FusedLoc$Lat, 12))

Precip.diff = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
D2 = New.Gauge1[,(ncol(slid.mat)/2 + 1):(ncol(slid.mat))]
D1 = New.Gauge1[,1:(ncol(slid.mat)/2)]
for(i in 1:12){
  Precip.diff[,i] = rowSums(D2[,seq(i, ncol(D2), 12)]) - rowSums(D1[, seq(i, ncol(D1), 12)])
}

Precip.diff = Precip.diff[,c(10:12, 1:9)]

D2 = slid.mat[,(ncol(slid.mat)/2 + 1):(ncol(slid.mat))]
D1 = slid.mat[,1:(ncol(slid.mat)/2)]
slid.diff.total = matrix(0, nrow = nrow(slid.mat), ncol = 12)
for(i in 1:12){
  slid.diff.total[,i] = rowSums(D2[,seq(i, ncol(D2), 12)]) - rowSums(D1[, seq(i, ncol(D1), 12)])
}


slid.diff.total = slid.diff.total[,c(10:12, 1:9)]


which(slid.diffA1$Difference < 0)




EXOFI = data.frame(Precip = as.vector(Precip.diff)[slid.diffA1$Difference < 0], sLID = as.vector(slid.diff.total)[slid.diffA1$Difference < 0],
                   sLIDA = as.vector(slid.diffA)[slid.diffA1$Difference < 0],
                   Month = rep(1:12, each = nrow(Precip.diff))[slid.diffA1$Difference < 0], Lon = rep(FusedLoc$Lon, 12)[slid.diffA1$Difference < 0], 
                   Lat = rep(FusedLoc$Lat, 12)[slid.diffA1$Difference < 0])

EXOFI$Months = factor(ifelse(EXOFI$Month == 1, "January", 
                             ifelse(EXOFI$Month == 2, "February",
                                    ifelse(EXOFI$Month == 3, "March", 
                                           ifelse(EXOFI$Month == 4, "April", 
                                                  ifelse(EXOFI$Month == 5, "May", 
                                                         ifelse(EXOFI$Month == 6, "June", 
                                                                ifelse(EXOFI$Month == 7, "July", 
                                                                       ifelse(EXOFI$Month == 8, "August", 
                                                                              ifelse(EXOFI$Month == 9, "September", 
                                                                                     ifelse(EXOFI$Month == 10, "October", 
                                                                                            ifelse(EXOFI$Month == 11, "November", "December"))))))))))),
                      levels = c("January", "February", "March", "April",
                                 "May", "June", "July", "August",
                                 "September", "October", "November", "December"))



EXOFI1 = EXOFI

EUC1 = NULL
for(i in 1:12){
  EUC1 = c(EUC1, sqrt(scale(EXOFI1$Precip[EXOFI1$Month == i])^2 +
                        scale(EXOFI1$sLID[EXOFI1$Month == i])^2 +
                        scale(EXOFI1$sLIDA)[EXOFI1$Month == i]^2))
}

EXOFI1$Euclidean = EUC1


ggplot() + 
  geom_point(EXOFI1, mapping = aes(x = Lon, y = Lat, colour = Euclidean), size = 1.25) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Negative", colour = "Index")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)






