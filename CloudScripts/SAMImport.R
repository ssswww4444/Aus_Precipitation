SAM = read.table("SouthernAnnularMode.txt", header = TRUE)

SAM.years = SAM[,1]
SAM = SAM[,-1]

Years.Sam = rep(SAM.years, each = 12)
SAM.df = data.frame(Year = Years.Sam, Month = rep(1:12, length(SAM.years)), SAM = as.vector(t(as.matrix(SAM))))

write.csv(SAM.df, file = "SouthernAnnularModeIndex", row.names = FALSE)

###########################
#SSP oscillation Testing

SSP.location = as.data.frame(SSP.location)

ggplot() + 
  geom_point(SSP.location, mapping = aes(x = Lon, y = Lat), size = 2) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = " ")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") + 
  geom_polygon(data = shape, aes(x = long, y = lat, group = group), colour = "black", fill = NA)



