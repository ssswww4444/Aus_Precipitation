month.mean = matrix(0, nrow = nrow(New.Gauge1), ncol = 12)
for(i in 1:12){
  month.mean[,i] = rowMeans(New.Gauge1[,seq(i, ncol(New.Gauge1), 12)])
}

month.mean = month.mean[,c(10:12, 1:9)]

head(month.mean)

MM.df = data.frame(Mean = as.vector(month.mean), Lon = FusedLoc$Lon, Lat = FusedLoc$Lat, Month = rep(1:12, each = nrow(New.Gauge1)))


MM1.df = NULL
for(i in 1:12){
  Temp = MM.df[MM.df$Month == i,]
  idw2 = gstat::idw(formula = Mean ~ 1, locations = ~Lon + Lat, data = Temp, newdata = pred.grid.aus, idp = 3)
  MM1.df = rbind(MM1.df, idw2)
}

MM1.df$Month = rep(1:12, each = nrow(idw2))

S2 = MM1.df

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
  geom_point(S2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 1) + facet_wrap(~Months, nrow = 4) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "Mean Monthly Precipitation", colour = "Precipitation\n(mm)")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)







