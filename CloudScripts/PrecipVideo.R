####################
#Precipitation Video
####################

datesidw = dates4


IDW.mat = matrix(0, dim(Austra2) * 12 * 22, 6)
pred.grid.aus = data.frame(Lon = Austra2$Lon, Lat = Austra2$Lat)
k = 1
for(i in 1:nrow(datesidw)){
  for(j in 1:1){
    s2 = SLID.TS2[which(SLID.TS2$Year == datesidw[i,1] & SLID.TS2$Month == datesidw[i,2] & SLID.TS2$s == slid.vals[j]), ]
    idw1 = gstat::idw(formula = Precipitation ~ 1, locations = ~Lon + Lat, data = s2, newdata = pred.grid.aus, idp = 3)
    IDW.mat[((k - 1) * nrow(Austra2) + 1):(k * nrow(Austra2)),] = cbind(Austra2$Lon, Austra2$Lat, 
                                                                        idw1$var1.pred, rep(slid.vals[j], nrow(Austra2)),
                                                                        rep(datesidw[i,1], nrow(Austra2)),
                                                                        rep(datesidw[i,2], nrow(Austra2)))
    k = k + 1
  }
  print(i)
}

colnames(IDW.mat) = c("Lon", "Lat", "Precipitation", "s", "Year", "Month")

IDW.mat = as.data.frame(IDW.mat)



IDW.mat$Months = factor(ifelse(IDW.mat$Month == 1, "January", 
                          ifelse(IDW.mat$Month == 2, "February",
                                 ifelse(IDW.mat$Month == 3, "March", 
                                        ifelse(IDW.mat$Month == 4, "April", 
                                               ifelse(IDW.mat$Month == 5, "May", 
                                                      ifelse(IDW.mat$Month == 6, "June", 
                                                             ifelse(IDW.mat$Month == 7, "July", 
                                                                    ifelse(IDW.mat$Month == 8, "August", 
                                                                           ifelse(IDW.mat$Month == 9, "September", 
                                                                                  ifelse(IDW.mat$Month == 10, "October", 
                                                                                         ifelse(IDW.mat$Month == 11, "November", "December"))))))))))),
                   levels = c("January", "February", "March", "April",
                              "May", "June", "July", "August",
                              "September", "October", "November", "December"))


#write.csv(IDW.mat, row.names = FALSE, file = "IDW.matprecip")


ganim = ggplot(IDW.mat, aes(x = Lon, y = Lat, colour = Precipitation)) +
  geom_point(alpha = 0.7, size = 0.01) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Year : {closest_state}", x = 'Longitude', y = 'Latitude', colour = "Precipitation\n(mm)") +
  transition_states(Year) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) 

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/PrecipVid")
animate(ganim, nframes = 110)

5

IDW.mat = read.csv("IDW.matprecip")

ggplot(IDW.mat[IDW.mat$Year == 2021,], aes(x = Lon, y = Lat, colour = Precipitation)) +
  geom_point(alpha = 0.7, size = 0.01) +
  theme_bw() +
  facet_wrap(~s) +
  # Here comes the gganimate specific bits
  labs(title = "Year : {closest_state}", x = 'Longitude', y = 'Latitude', colour = "Precipitation\n(mm)") +
  transition_states(Year) +
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) +
  theme(plot.margin = margin(1,0.5,0.5,0.5, "cm")) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) 




