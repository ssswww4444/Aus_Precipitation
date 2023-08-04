####################
#Forecasting with sLID
####################

New.Gauge1 = read.csv("FusedData")


data("NOAA_df_1990") # load data
Tmax <- subset(NOAA_df_1990, # subset the data
               month %in% 7 & # May to July
                 year == 1993) # year of 1993

Tmax <- within(Tmax,
               {time = as.Date(paste(year,month,day,sep="-"))}) # create Date field


slid = function(x, s){
  out = rep(0, nrow(x))
  d1 = dist(x)
  d2 = as.matrix(d1)
  for(i in 1:nrow(x)){
    nh = order(d2[i,])[2:(s + 1)]
    d3 = d2[i,nh]
    d3[d3 == 0] = 1e-16
    out[i] = max(- ( ( 1 / s ) * sum( log( d3/max( d3 ) ) ) ) ^ ( -1 ), 0)
  }
  out = cbind(out, out > mean(out))
  return(out)
}

slid.vals = c(750)
SLID.TS2 = NULL
k = 1
for(i in 1:ncol(New.Gauge1)){
  for(j in slid.vals){
    s1 = slid(matrix(New.Gauge1[,i], ncol = 1), j)
    SLID.TS2 = rbind(SLID.TS2, cbind(NOAA.aus[[1]]$Lon, NOAA.aus[[1]]$Lat, s1[,1], s1[,2], rep(j, nrow(New.Gauge1)), New.Gauge1[,i],
                                                                              NOAA.aus[[i + 255]]$Year, NOAA.aus[[i + 255]]$Month))
    
    k = k + 1
  }
  print(i)
}

colnames(SLID.TS2) = c("Lon", "Lat", "sLID", "Class", "s", "Precipitation", "Year", "Month")
SLID.TS2 = as.data.frame(SLID.TS2)

sLIDNOW = NULL
for(i in 1:length(slid.vals)){
  Temp = SLID.TS2[SLID.TS2$s == slid.vals[i],]
  idw1 = gstat::idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  sLIDNOW = rbind(sLIDNOW, idw1)
}

sLIDNOW$s = rep(slid.vals, each = nrow(idw1))

ggplot() + 
  geom_point(sLIDNOW, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~s) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "2022-03 Precipitaion sLID Values", colour = "sLID")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

ggplot(data = SLID.TS2, aes(x = sLID, y = Precipitation)) + geom_point() + facet_wrap(~s) +
  theme_bw() + labs(y = "Precipitation (mm)", x = "sLID", title = "2022-03 sLID-Precipitation") + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right",
        plot.margin = margin(1,0.5,0.5,0.5, "cm"))


SLID.TS2$Date = as.Date(paste(SLID.TS2$Year, ifelse(nchar(SLID.TS2$Month) == 1, paste0("0", SLID.TS2$Month), SLID.TS2$Month), "15", sep = "-"))

dateSLID = unique(SLID.TS$Date)

Tmax = SLID.TS

for(i in 1:length(unique(SLID.TS$id))){
  for(j in 1:12){
    temp = log(SLID.TS[SLID.TS$id == i & SLID.TS$Month == j,3] + 1)
    Tmax[SLID.TS$id == i & SLID.TS$Month == j,3] = log(Tmax[SLID.TS$id == i & SLID.TS$Month == j,3] + 1) - mean(temp)
  }
  print(i)
}

STObj <- stConstruct(x = Tmax, # dataset
                     space = c("Lon","Lat"), # spatial fields
                     time="Date", # time field
                     interval=TRUE) 

STObj$std <- 2


grid_BAUs <- auto_BAUs(manifold=STplane(), # spatio-temporal process on the plane
                       data=STObj, # data
                       cellsize = c(1,1,1), # BAU cell size
                       type="grid", # grid or hex?
                       convex=-0.1, # parameter for hull construction
                       tunit="months", # time unit
                       nonconvex_hull=FALSE) # convex hull
grid_BAUs$fs = 1 # fine-scale variation


G_spatial <- auto_basis(manifold = plane(), # spatial functions on the plane
                        data=as(STObj,"Spatial"), # remove the temporal dimension
                        nres = 1, # three resolutions
                        type = "bisquare", # bisquare basis functions
                        regular = 1) # regular basis functions


print(head(grid_BAUs@time))


G_temporal <- local_basis(manifold = real_line(), # functions on the real line
                          type = "Gaussian", # Gaussian functions
                          loc = matrix(seq(1,ncol(New.Gauge1),by=12)), # locations of functions
                          scale = rep(3,length(seq(1,ncol(New.Gauge1),by=12)))) # scales of functions

basis_s_plot <- show_basis(G_spatial) + xlab("lon (deg)") + ylab("lat (deg)")
basis_t_plot <- show_basis(G_temporal) + xlab("time index") + ylab(expression(phi(t)))

G <- TensorP(G_spatial,G_temporal) 


f <- log(sLID + 1) ~ 1 + (Lat + Lon)^2 # fixed effects part
S <- SRE(f = f, # formula
         data = list(STObj), # data (can have a list of data)
         basis = G, # basis functions
         BAUs = grid_BAUs, # BAUs
         est_error = FALSE) # do not estimate measurement-error variance
S1 <- SRE.fit(S, # estimate parameters in the SRE model S
             n_EM = 10, # maximum no. of EM iterations
             tol = 0.1, # tolerance on log-likelihood
             print_lik=FALSE) # print log-likelihood trace
grid_BAUs <- predict(S1, obs_fs = FALSE)

GB = as.data.frame(grid_BAUs)

pred.grid1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat)
G1 = NULL
k = 4
dd1 = NULL
for(i in 1:length(unique(GB$time))){
  s2 = GB[GB$time == GB$time[i],]
  idw1 = gstat::idw(formula = mu ~ 1, locations = ~Lon + Lat, data = s2, newdata = pred.grid1, idp = 3)
  idw2 = gstat::idw(formula = sd ~ 1, locations = ~Lon + Lat, data = s2, newdata = pred.grid1, idp = 3)
  G1 = rbind(G1, cbind(idw1$var1.pred, idw2$var1.pred, idw1$Lon, idw1$Lat, rep(unique(GB$time)[i], nrow(idw1)), 1:nrow(idw1), k))
  if(k == 12){
    k = 1
  }else{
    k = k + 1
  }
  print(i)
}

colnames(G1) = c("mu", "sd", "Lon", "Lat", "Date", "id", "Month")
G1 = as.data.frame(G1)

G2 = NULL
G3 = NULL

for(i in 1:length(unique(G1$id))){
  for(j in 1:12){
    temp = SLID.TS[SLID.TS$id == i & SLID.TS$Month == j, 3]
    G2 = c(G2, G1[G1$id == i & G1$Month == j,1] - 2 * sd(temp))
    G3 = c(G3, G1[G1$id == i & G1$Month == j,1] + 2 * sd(temp))
    G1[G1$id == i & G1$Month == j,1] = G1[G1$id == i & G1$Month == j,1] + mean(log(temp + 1))
  }
  print(i)
}

dd1 = NULL
for(i in 1:length(unique(GB$time))){
  dd1 = c(dd1, rep(as.character(unique(GB$time)[i]), nrow(idw1)))
  print(i)
}

G1$Date = as.Date(dd1)

G1$Lower = G2
G1$Upper = G3

Pred.slid = G1[G1$Date == "2022-03-01",]

Act.slid = SLID.TS[SLID.TS$Date == "2022-03-15",]

Pred.slid1 = data.frame(Lon = c(Pred.slid$Lon, Act.slid$Lon, Pred.slid$Lon, Pred.slid$Lon),
                        Lat = c(Pred.slid$Lat, Act.slid$Lat, Pred.slid$Lat, Pred.slid$Lat),
                        sLID = c(exp(Pred.slid$mu), Act.slid$sLID, exp(Pred.slid$mu - 2 * Pred.slid$sd),
                                 exp(Pred.slid$mu + 2 * Pred.slid$sd)),
                        Class = rep(c("Forecasted", "Actual", "Lower", "Upper"), each = nrow(Pred.slid)))

Pred.slid2 = NULL
for(i in 1:length(unique(Pred.slid1$Class))){
  Temp = Pred.slid1[Pred.slid1$Class == unique(Pred.slid1$Class)[i], ]
  idw1 = gstat::idw(formula = sLID ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  Pred.slid2 = rbind(Pred.slid2, idw1)
}

cl1 = rep(c("Forecasted", "Actual", "Lower", "Upper"), each = nrow(idw1))

Pred.slid2$Class = factor(cl1, levels = c("Forecasted", "Actual", "Lower", "Upper"))

ggplot() + 
  geom_point(Pred.slid2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~Class) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "2022-03 sLID", colour = "sLID")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


month.mean = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in c(10:12,1:9)){
  month.mean[,i] = rowMeans(New.Gauge1[,seq(i, ncol(New.Gauge1), 12)])
}

slid.mat = New.Gauge1
for(i in 1:ncol(New.Gauge1)){
  slid.mat[,i] = slid(matrix(as.numeric(New.Gauge1[,i]), ncol = 1), 750)[,1]
  print(i)
}

sLID.Prec.cor = list()
Temp = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:nrow(slid.mat)){
  for(j in 1:12){
    Temp[i, j] = cor(as.numeric(slid.mat[i, seq(j, ncol(New.Gauge1), 12)]), as.numeric(New.Gauge1[i, seq(j, ncol(New.Gauge1), 12),]))
  }
  print(i)
}

sLID.Prec.cor = NULL
for(i in 1:12){
  Df1 = data.frame(Correlation = Temp[,i], Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Month = JAXA.Monthly[[1]]$Month[1])
  idw1 = gstat::idw(formula = Correlation ~ 1, locations = ~Lon + Lat, data = Df1, newdata = pred.grid.aus, idp = 3)
  sLID.Prec.cor = rbind(sLID.Prec.cor, cbind(idw1, JAXA.Monthly[[i]]$Month[1]))
  print(i)
}

mean(sLID.Prec.cor > 0.5, na.rm = TRUE)

colnames(sLID.Prec.cor) = c("Lon", "Lat", "Correlation", "Variance", "Month")

S2 = as.data.frame(sLID.Prec.cor)



S2$Months = factor(ifelse(S2$Month == 1, "January", 
       ifelse(S2$Month == 2, "February",
              ifelse(S2$Month == 3, "March", 
                     ifelse(S2$Month == 4, "April", 
                            ifelse(S2$Month == 5, "May", 
                                   ifelse(S2$Month == 6, "June", 
                                          ifelse(S2$Month == 7, "July", 
                                                 ifelse(S2$Month == 8, "August", 
                                                        ifelse(S2$Month == 9, "September", 
                                                               ifelse(S2$Month == 10, "October", 
                                                                      ifelse(S2$Month == 11, "November", "December"))))))))))),
       levels = c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December"))

ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = Correlation), size = 0.1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Monthly 750-LID Precipitation Correlation", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


Temp = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:nrow(slid.mat)){
  for(j in c(10:12,1:9)){
    Temp[i, j] = cor(as.numeric(slid.mat[i, seq(j, ncol(New.Gauge1), 12)]), as.numeric(New.Gauge1[i, seq(j, ncol(New.Gauge1), 12),]))
  }
  print(i)
}

r.squared = NULL
for(i in 1:12){
  sub1 = G1[which(G1$Month == i),]
  m2 = rowMeans(slid.mat, seq(c(10:12, 1:9)[i], ncol(slid.mat), 12))
  sub2 = SLID.TS2[SLID.TS2$Month == i, ]
  sub2 = sub2[1:nrow(sub1),]
  m2 = rep(sqrt(m2), sum(sub1$id == 1))
  m1 = rep(sqrt(month.mean[,i]), sum(sub1$id == 1))
  #sub2 = sub2[rep(Temp[,i] > 0.75, sum(sub1$id == 1)), ]
  #m1 = m1[rep(Temp[,i] > 0.75, sum(sub1$id == 1))]
  #sub1 = sub1[rep(Temp[,i] > 0.75, sum(sub1$id == 1)), ]
  mu1 = sqrt(exp(sub1$mu)) - m2
  precip1 = sqrt(sub2$Precipitation) - m1
  df1 = data.frame(sLID = mu1, Precipitation = precip1)
  mod1 = lm(Precipitation ~ sLID + m1, data = df1)
  s1 = summary(mod1)
  r.squared = c(r.squared, s1$r.squared)
}


i = 1


newdata1 = data.frame(sLID = sqrt(G1[G1$Date == "2022-01-01",1]), m1 = sqrt(month.mean[,1]))

hist(predict(mod1, newdata1)^2)



i = 1


sub1 = G1[which(G1$Month == i),]
m2 = rowMeans(slid.mat, seq(c(10:12, 1:9)[i], ncol(slid.mat), 12))
sub2 = SLID.TS2[SLID.TS2$Month == i, ]
sub2 = sub2[1:nrow(sub1),]
m2 = rep(sqrt(m2), sum(sub1$id == 1))
m1 = rep(sqrt(month.mean[,i]), sum(sub1$id == 1))
#sub2 = sub2[rep(Temp[,i] > 0.75, sum(sub1$id == 1)), ]
#m1 = m1[rep(Temp[,i] > 0.75, sum(sub1$id == 1))]
#sub1 = sub1[rep(Temp[,i] > 0.75, sum(sub1$id == 1)), ]
mu1 = sqrt(exp(sub1$mu)) - m2
precip1 = sqrt(sub2$Precipitation) - m1
df1 = data.frame(sLID = mu1, Precipitation = precip1)
mod1 = lm(Precipitation ~ sLID, data = df1)

pr1 = sqrt(New.Gauge1[,262]) - sqrt(month.mean[,1])
sl1 = sqrt(G1[G1$Date == "2022-01-01",1]) - sqrt(rowMeans(slid.mat, seq(c(10:12, 1:9)[1])))

mod2 = lm(sqrt(New.Gauge1[,262]) ~ bs(sqrt(exp(G1[G1$Date == "2022-01-01",1])), df = 3))

newdata1 = data.frame(sLID = sqrt(G1[G1$Date == "2022-01-01",1]) - sqrt(rowMeans(slid.mat, seq(c(10:12, 1:9)[i], ncol(slid.mat), 12))))


p1 = (predict(mod1, newdata1) + sqrt(month.mean[,i]))^2

New.Precip = data.frame(Precipitation = c(mod2$fitted.values^2, New.Gauge1[,262]),
                        Class = rep(c("Prediction", "Actual"), each = nrow(New.Gauge1)),
                        Lon = rep(FusedLoc$Lon, 2), Lat = rep(FusedLoc$Lat, 2))

N1 = data.frame(Precipitation = mod2$fitted.values^2,
                Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)
N2 = data.frame(Precipitation = New.Gauge1[,262],
                Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)

idw1 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N1, newdata = pred.grid.aus, idp = 3)
idw2 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N2, newdata = pred.grid.aus, idp = 3)


mod2 = lm(sqrt(New.Gauge1[,263]) ~ bs(sqrt(exp(G1[G1$Date == "2022-02-01",1])), df = 3))

N1 = data.frame(Precipitation = mod2$fitted.values^2,
                Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)
N2 = data.frame(Precipitation = New.Gauge1[,263],
                Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)


idw3 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N1, newdata = pred.grid.aus, idp = 3)
idw4 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N2, newdata = pred.grid.aus, idp = 3)

mod2 = lm(sqrt(New.Gauge1[,264]) ~ bs(sqrt(exp(G1[G1$Date == "2022-03-01",1])), df = 3))

N1 = data.frame(Precipitation = mod2$fitted.values^2,
                Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)
N2 = data.frame(Precipitation = New.Gauge1[,264],
                Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)


idw5 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N1, newdata = pred.grid.aus, idp = 3)
idw6 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = N2, newdata = pred.grid.aus, idp = 3)


Df1 = rbind(idw1, idw2, idw3, idw4, idw5, idw6)

colnames(Df1) = c("Lon", "Lat", "Precipitation", "Variance")
Df1 = as.data.frame(Df1)

Df1$Class = factor(rep(c("January 2022 Forecast", "January 2022 Actual",
                  "February 2022 Forecast", "February 2022 Actual",
                  "March 2022 Forecast", "March 2022 Actual"), each = nrow(idw1)), 
                  levels = c("January 2022 Forecast", "January 2022 Actual",
                             "February 2022 Forecast", "February 2022 Actual",
                             "March 2022 Forecast", "March 2022 Actual"))

ggplot() + 
  geom_point(Df1, mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 0.1) + facet_wrap(~Class, nrow = 3) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Precipitation Forecast", colour = "Precipitation\n(mm)")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)




0.8650 - 0.4257


predict(mod2, )





ggplot() + 
  geom_point(New.Precip, mapping = aes(x = Lon, y = Lat, colour = Precipitation), size = 1) + facet_wrap(~Class, nrow = 1) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Monthly 750-LID Precipitation Correlation", colour = "Correlation")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)




summary(mod1)



slid.mat = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
for(i in 1:ncol(New.Gauge1)){
  slid.mat[,i] = slid(matrix(New.Gauge1[,i], ncol = 1), 750)[,1]
}

slid.matA = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
for(i in 1:ncol(slid.mat)){
  A = slid.mat[,i]
  A1 = order(A)
  slid.matA[A1[1:139], i] = 1
}


slid.matA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
for(i in 1:(ncol(slid.mat)/2)){
  A = slid.mat[,i]
  A1 = order(A)
  slid.matA1[A1[1:139], i] = 1
}

slid.matA2 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
k = 1
for(i in (ncol(slid.mat)/2 + 1):ncol(slid.mat)){
  A = slid.mat[,i]
  A1 = order(A)
  slid.matA2[A1[1:139], k] = 1
  k = k + 1
}


slid.diffA = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  slid.diffA[,i] = rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) - rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)])
}


slid.diffA1 = data.frame(Difference = as.vector(slid.diffA), Month = rep(1:12, each = nrow(New.Gauge1)),
                         Lon = rep(FusedLoc$Lon, 12), Lat = rep(FusedLoc$Lat, 12))

S2 = as.data.frame(slid.diffA1)

S2.months = NULL
for(i in 1:nrow(S2)){
  m1 = S2$Month[i]
  if(m1 == 1){
    S2.months = c(S2.months, "January")
  }else if(m1 == 2){
    S2.months = c(S2.months, "February")
  }else if(m1 == 3){
    S2.months = c(S2.months, "March")
  }else if(m1 == 4){
    S2.months = c(S2.months, "April")
  }else if(m1 == 5){
    S2.months = c(S2.months, "May")
  }else if(m1 == 6){
    S2.months = c(S2.months, "June")
  }else if(m1 == 7){
    S2.months = c(S2.months, "July")
  }else if(m1 == 8){
    S2.months = c(S2.months, "August")
  }else if(m1 == 9){
    S2.months = c(S2.months, "September")
  }else if(m1 == 10){
    S2.months = c(S2.months, "October")
  }else if(m1 == 11){
    S2.months = c(S2.months, "November")
  }else if(m1 == 12){
    S2.months = c(S2.months, "December")
  }
}

S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))

slid.diffA2 = NULL
for(i in 1:length(unique(slid.diffA1$Month))){
  Temp = slid.diffA1[slid.diffA1$Month == i, ]
  idw1 = gstat::idw(formula = Difference ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  slid.diffA2 = rbind(slid.diffA2, idw1)
  print(i)
}

slid.diffA2$Month = rep(1:12, each = nrow(pred.grid.aus))

S2 = as.data.frame(slid.diffA2)

S2.months = rep(c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December"), each = nrow(idw1))
S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))


ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Decadal Difference", colour = "Value")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


dim(slid.mat)

V1 = apply(slid.mat, 1, var)

V1.df = data.frame(Variance = V1, Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)

idw1 = gstat::idw(formula = Variance ~ 1, locations = ~Lon + Lat, data = V1.df, newdata = pred.grid.aus, idp = 3)

M1 = rowMeans(slid.mat)
M1.df = data.frame(Mean = M1, Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)
idw2 = gstat::idw(formula = Mean ~ 1, locations = ~Lon + Lat, data = M1.df, newdata = pred.grid.aus, idp = 3)



V1.plot = ggplot() + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "750-LID Variance", colour = "Variance")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)

M1.plot = ggplot() + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + 
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "750-LID Mean", colour = "Mean")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "left") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)


grid.arrange(M1.plot + labs(x = " ", y = " "), V1.plot + labs(x = " ", y = " "), nrow = 1,
             left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, hjust = 0, vjust = 10),
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), vjust = 0, hjust = 0.1))



##################
#Changing A
##################

fusedLoc = NOAA.aus[[1]][,2:1]
order(abs(fusedLoc$Lon -153.294) + abs(fusedLoc$Lat + 28.810))
fusedLoc[1391,]


slid.mat = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
for(i in 1:ncol(New.Gauge1)){
  slid.mat[,i] = slid(matrix(New.Gauge1[,i], ncol = 1), 750)[,1]
}

setA1 = seq(10, 100, 1)

lis = NULL
lis1 = NULL
for(l1 in setA1){
  
  slid.matA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
  for(i in 1:(ncol(slid.mat)/2)){
    A = slid.mat[,i]
    A1 = order(A, decreasing = TRUE)
    slid.matA1[A1[1:floor(nrow(slid.mat)/l1)], i] = 1
  }
  
  slid.matA2 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
  k = 1
  for(i in (ncol(slid.mat)/2 + 1):ncol(slid.mat)){
    A = slid.mat[,i]
    A1 = order(A, decreasing = TRUE)
    slid.matA2[A1[1:floor(nrow(slid.mat)/l1)], k] = 1
    k = k + 1
  }
  
  slid.diffA = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
  for(i in 1:12){
    slid.diffA[,i] = rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) - rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)])
  }
  
  slid.diffA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
  for(i in 1:12){
    slid.diffA1[,i] = rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) + rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)])
  }
 lis = rbind(lis, slid.diffA[1391,])
 lis1 = rbind(lis1, slid.diffA1[1391,])
}

lis = lis[,c(10:12, 1:9)]
lis1 = lis1[,c(10:12, 1:9)]



S2 = as.data.frame(slid.diffA1)

S2.months = NULL
for(i in 1:nrow(S2)){
  m1 = S2$Month[i]
  if(m1 == 1){
    S2.months = c(S2.months, "January")
  }else if(m1 == 2){
    S2.months = c(S2.months, "February")
  }else if(m1 == 3){
    S2.months = c(S2.months, "March")
  }else if(m1 == 4){
    S2.months = c(S2.months, "April")
  }else if(m1 == 5){
    S2.months = c(S2.months, "May")
  }else if(m1 == 6){
    S2.months = c(S2.months, "June")
  }else if(m1 == 7){
    S2.months = c(S2.months, "July")
  }else if(m1 == 8){
    S2.months = c(S2.months, "August")
  }else if(m1 == 9){
    S2.months = c(S2.months, "September")
  }else if(m1 == 10){
    S2.months = c(S2.months, "October")
  }else if(m1 == 11){
    S2.months = c(S2.months, "November")
  }else if(m1 == 12){
    S2.months = c(S2.months, "December")
  }
}

S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))

l1 = 14

slid.matA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
for(i in 1:(ncol(slid.mat)/2)){
  A = slid.mat[,i]
  A1 = order(A, decreasing = TRUE)
  slid.matA1[A1[1:floor(nrow(slid.mat)/l1)], i] = 1
}

slid.matA2 = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1)/2)
k = 1
for(i in (ncol(slid.mat)/2 + 1):ncol(slid.mat)){
  A = slid.mat[,i]
  A1 = order(A, decreasing = TRUE)
  slid.matA2[A1[1:floor(nrow(slid.mat)/l1)], k] = 1
  k = k + 1
}

slid.diffA = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  slid.diffA[,i] = rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) - rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)])
}

slid.diffA = slid.diffA[,c(10:12, 1:9)]


slid.diffA1 = data.frame(Difference = as.vector(slid.diffA), Month = rep(1:12, each = nrow(New.Gauge1)),
                         Lon = rep(FusedLoc$Lon, 12), Lat = rep(FusedLoc$Lat, 12))

S2 = slid.diffA1

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))



slid.diffA2 = NULL
for(i in 1:length(unique(slid.diffA1$Month))){
  Temp = slid.diffA1[slid.diffA1$Month == i, ]
  idw1 = gstat::idw(formula = Difference ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  slid.diffA2 = rbind(slid.diffA2, idw1)
  print(i)
}

slid.diffA2$Month = rep(1:12, each = nrow(pred.grid.aus))

S2 = as.data.frame(slid.diffA2)

S2.months = rep(c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December"), each = nrow(idw1))
S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))


ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = Difference), size = 0.1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Decadal Difference", colour = "Value")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA)




lis = NULL
lis1 = NULL

setA1 = seq(0.9, 0.99, 0.005)
for(l1 in setA1){
  
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
    slid.diffA[,i] = rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) - rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)])
  }
  
  slid.diffA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
  for(i in 1:12){
    slid.diffA1[,i] = rowSums(slid.matA1[,seq(i,ncol(slid.matA1), 12)]) + rowSums(slid.matA2[,seq(i,ncol(slid.matA1), 12)])
  }
  lis = rbind(lis, slid.diffA[1391,])
  lis1 = rbind(lis1, slid.diffA1[1391,])
}

lis = lis[,c(10:12, 1:9)]
lis1 = lis1[,c(10:12, 1:9)]



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

S2 = slid.diffA1

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))



slid.diffA2 = NULL
for(i in 1:length(unique(slid.diffA1$Month))){
  Temp = slid.diffA1[slid.diffA1$Month == i, ]
  idw1 = gstat::idw(formula = Difference ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  slid.diffA2 = rbind(slid.diffA2, idw1)
  print(i)
}

slid.diffA2$Month = rep(1:12, each = nrow(pred.grid.aus))

S2 = as.data.frame(slid.diffA2)

S2.months = rep(c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December"), each = nrow(idw1))
S2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))


ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Decadal Difference", colour = "Difference")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1)


D2 = slid.mat[,(ncol(slid.mat)/2 + 1):(ncol(slid.mat))]
D1 = slid.mat[,1:(ncol(slid.mat)/2)]
slid.diff.total = matrix(0, nrow = nrow(slid.mat), ncol = 12)
for(i in 1:12){
  slid.diff.total[,i] = rowSums(D2[,seq(i, ncol(D2), 12)]) - rowSums(D1[, seq(i, ncol(D1), 12)])
}


slid.diff.total = slid.diff.total[,c(10:12, 1:9)]

slid.diff.total.df = data.frame(Difference = as.vector(slid.diff.total), Month = rep(1:12, each = nrow(New.Gauge1)),
                         Lon = rep(FusedLoc$Lon, 12), Lat = rep(FusedLoc$Lat, 12))


slid.diff.total.df2 = NULL
for(i in 1:length(unique(slid.diff.total.df$Month))){
  Temp = slid.diff.total.df[slid.diff.total.df$Month == i, ]
  idw1 = gstat::idw(formula = Difference ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  slid.diff.total.df2 = rbind(slid.diff.total.df2, idw1)
  print(i)
}



slid.diff.total.df2$Month = rep(1:12, each = nrow(pred.grid.aus))


S2.months = rep(c("January", "February", "March", "April",
                  "May", "June", "July", "August",
                  "September", "October", "November", "December"), each = nrow(idw1))
slid.diff.total.df2$Months = factor(S2.months, levels = c("January", "February", "March", "April",
                                         "May", "June", "July", "August",
                                         "September", "October", "November", "December"))


ggplot() + 
  geom_point(slid.diff.total.df2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "750-LID Decadal Difference", colour = "Difference")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1)



Precip.diff = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
D2 = New.Gauge1[,(ncol(slid.mat)/2 + 1):(ncol(slid.mat))]
D1 = New.Gauge1[,1:(ncol(slid.mat)/2)]
for(i in 1:12){
  Precip.diff[,i] = rowSums(D2[,seq(i, ncol(D2), 12)]) - rowSums(D1[, seq(i, ncol(D1), 12)])
}

Precip.diff = Precip.diff[,c(10:12, 1:9)]

Precip.diff


k1 = 8

plot_ly(x=Precip.diff[,k1], y=slid.diff.total[,k1], z=slid.diffA[,k1], type="scatter3d", size = 0.1, col = "red")


EXOFI = data.frame(Precip = as.vector(Precip.diff), sLID = as.vector(slid.diff.total), sLIDA = as.vector(slid.diffA),
                   Month = rep(1:12, each = nrow(Precip.diff)), Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)

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


EXOFI2 = EXOFI[EXOFI$sLIDA != 0,]

ggplot() + 
  geom_point(EXOFI2, mapping = aes(x = Precip, y = sLID, colour = sLIDA), size = 0.5) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Precipitation Difference (mm)", y = "sLID Difference", colour = "Difference")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right",
        strip.text = element_text(size = 14)) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))



rowSums(sign(EXOFI[,1:3])) > 0

EXOFI1 = EXOFI[rowSums(sign(EXOFI[,1:3])) > 0,]

EXOFI1$Euclidean = sqrt(EXOFI1$Precip^2 + EXOFI1$sLID^2 + EXOFI1$sLIDA^2)


ggplot() + 
  geom_point(EXOFI1, mapping = aes(x = Lon, y = Lat, colour = Euclidean), size = 0.5) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI", colour = "Euclidean\nDistance")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1)


EXOFI1 = EXOFI[rowSums(sign(EXOFI[,1:3])) == 3,]

EUC1 = NULL
for(i in 1:12){
  EUC1 = c(EUC1, sqrt(scale(EXOFI1$Precip[EXOFI1$Month == i])^2 +
                scale(EXOFI1$sLID[EXOFI1$Month == i])^2 +
                scale(EXOFI1$sLIDA)[EXOFI1$Month == i]^2))
}

EXOFI1$Euclidean = EUC1


ggplot() + 
  geom_point(EXOFI1, mapping = aes(x = Lon, y = Lat, colour = Euclidean), size = 0.5) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Standardised", colour = "Euclidean\nDistance")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1)















l1 = 0.935

slid.matA = matrix(0, nrow = nrow(New.Gauge1), ncol = ncol(New.Gauge1))
for(i in 1:(ncol(slid.mat))){
  A = slid.mat[,i]
  A1 = quantile(A, l1)
  slid.matA[A > A1, i] = 1
}

slid.matA1 = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  slid.matA1[,i] = rowSums(slid.matA[,seq(i, ncol(slid.mat), 12)])
}

slid.matA1 = slid.matA1[,c(10:12, 1:9)]

S.df = data.frame(Count = as.vector(slid.matA1), Month = rep(1:12, each = nrow(slid.matA1)), Lon = FusedLoc$Lon, Lat = FusedLoc$Lat)

S2 = S.df

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))

ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = Count), size = 1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Count", colour = "Count")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.1)

SetAtotal = NULL
for(i in 1:12){
  Temp = S.df[S.df$Month == i,]
  idw1 = gstat::idw(formula = Count ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  SetAtotal = rbind(SetAtotal, idw1)
}

SetAtotal$Month = rep(1:12, each = nrow(idw1))

S2 = SetAtotal

S2$Months = factor(ifelse(S2$Month == 1, "January", 
                          ifelse(S2$Month == 2, "February",
                                 ifelse(S2$Month == 3, "March", 
                                        ifelse(S2$Month == 4, "April", 
                                               ifelse(S2$Month == 5, "May", 
                                                      ifelse(S2$Month == 6, "June", 
                                                             ifelse(S2$Month == 7, "July", 
                                                                    ifelse(S2$Month == 8, "August", 
                                                                           ifelse(S2$Month == 9, "September", 
                                                                                  ifelse(S2$Month == 10, "October", 
                                                                                         ifelse(S2$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))

ggplot() + 
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Set A Count", colour = "Count")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)





####Table

slid.matA2 = NULL
for(i in 1:12){
  slid.matA2 = colSums(slid.matA[, seq(i, ncol(slid.matA)/2), 12)])
}


#Look at locations with greatest changes at each month
m1 = apply(slid.diffA, 2, order)[1,]
m2 = apply(slid.diffA, 2, order, decreasing = TRUE)[1,]



S1 = slid.mat[m1,]
P1 = New.Gauge1[m1,]

S2 = slid.mat[m2,]
P2 = New.Gauge1[m2,]

A1 = auto.arima(S1[1,])
A2 = auto.arima(as.numeric(P1[1,]))



















