######Fire data
fireyears = 2000:2020
Fire = list()
for(i in 1:length(fireyears)){
  Fire1 = read.csv(paste0("https://raw.githubusercontent.com/hinestein/Firedata/main/modis_", fireyears[i], "_Australia.csv"))
  Fire2 = Fire1[,c(1:6)]
  Fire[[i]] = Fire2
  print(i)
}


nr1 = 0
for(i in 1:length(Fire)){
  nr1 = c(nr1, nrow(Fire[[i]]))
}
nr2 = cumsum(nr1)

Fire2 = matrix(0, nrow = sum(nr1), ncol = ncol(Fire[[1]]))
for(i in 1:length(Fire)){
  Fire2[(nr2[i] + 1):nr2[i + 1], ] = as.matrix(Fire[[i]])
}

Fire3 = matrix(0, nrow = nrow(Fire2), ncol = ncol(Fire2))
for(i in 1:(ncol(Fire2) - 1)){
  Fire3[,i] = as.numeric(Fire2[,i])
}

colnames(Fire3) = colnames(Fire[[1]])
Fire3 = as.data.frame(Fire3)

for(i in 1:length(Fire)){
  Fire3[(nr2[i] + 1):nr2[i + 1], 6] = Fire[[i]][,6]
}

Fire2 = Fire3[-c(1:(which(Fire3[,6] == "2020-01-01")[1] -1)),]

Firesum2019.20 = Fire3[(which(Fire3[,6] == "2019-12-01")[1]):(which(Fire3[,6] == "2020-03-01")[1] - 1),]

g1 = ggplot() + geom_polygon(data = ausmap, aes(x = long, y = lat, group = group), colour = "black", fill = NA) + 
  geom_point(Firesum2019.20, mapping = aes(x = longitude, y = latitude, colour = brightness), size = 0.2, alpha = 0.25) +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "2019-2020 Australian Summer Fires")+ 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none") +
  xlim(113, 155) + ylim(-45,-9) + scale_colour_gradientn(colours=c("#FF7F00", "red", "#7F0000"),
                         values = seq(1,0, length.out = 9))
g1



entropy1 = function(x){
  out = -sum(ifelse(x > 0, (x/sum(x))*log(x/sum(x)), 0))
  return(out)
}













