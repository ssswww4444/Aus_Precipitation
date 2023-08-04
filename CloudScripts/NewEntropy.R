#################
#New Entropy work


ncol(NOAA.all.aus)/12

NOAA.all.aus = matrix(0, nrow = nrow(NOAA.aus[[1]]), ncol = length(NOAA.aus))
for(i in 1:length(NOAA.aus)){
  NOAA.all.aus[,i] = NOAA.aus[[i]][,3]
}

NOAA.year = matrix(0, nrow = nrow(NOAA.all.aus), ncol = floor(ncol(NOAA.all.aus)/12))
for(i in 1:floor(ncol(NOAA.all.aus)/12)){
  NOAA.year[,i] = rowSums(NOAA.all.aus[,((i - 1) * 12 + 1):(i * 12)])
}

AnnualNOAA.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Mean = rowMeans(NOAA.year))
pred.grid = data.frame(Lon = Austra2[,2], Lat = Austra2[,1])
idw1 = idw(formula = Mean ~ 1, locations = ~Lon + Lat, data = AnnualNOAA.df, newdata = pred.grid, idp = 3)

ggplot() + 
  geom_point(data = idw1, 
             mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9)) + theme_bw() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  labs(x = "Longitude", y = "Latitude", colour = "Precipitation\n(mm)", title = "Mean Annual Precipitation") +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  geom_segment(aes(y = -37.78, x = 144.98, yend = -37.78 - 3, xend = 144.98 - 8), size = 0.8) +
  geom_text(aes(x = 144.98 - 10, y = -37.78 - 4, label = "Melbourne"), size = 5) +
  geom_segment(aes(y = -12.45, x = 130.84, yend = -12.45 + 1, xend = 130.84 - 8), size = 0.8) +
  geom_text(aes(x = 130.84 - 10, y = -12.45 + 3 , label = "Darwin"), size = 5) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


##################
#SLIDE hist
##################

Prop = list()
for(i in 1:nrow(NOAA.all.aus)){
  p1 = NULL
  indices = 1:12
  for(j in 1:(ncol(NOAA.all.aus) - 11)){
    p1 = rbind(p1, ((NOAA.all.aus[i, j:(j+11)])[indices])/sum((NOAA.all.aus[i, j:(j+11)])[indices]) )
    indices = c(indices[12], indices[-12])
  }
  Prop[[i]] = p1
}

div1 = function(x){
  if(sum(x) > 0){
    a = x/sum(x)
  }else{
    a = 0
  }
  a
}

Prop = list()
for(i in 1:nrow(NOAA.all.aus)){
  Temp = matrix(0, nrow = floor(ncol(NOAA.all.aus)/ 12), ncol = 12)
  for(j in 1:(floor(ncol(NOAA.all.aus)/ 12))){
    
    Temp[j,] = NOAA.all.aus[i, ((j - 1) * 12 + 1):(j * 12)]/sum(NOAA.all.aus[i, ((j - 1) * 12 + 1):(j * 12)])
  }
  Prop[[i]] = Temp
}

P.list = list()
for(i in 1:nrow(NOAA.all.aus)){
  P1 = matrix(NOAA.all.aus[i, 1:(floor(ncol(NOAA.all.aus)/12) * 12)], nrow = floor(ncol(NOAA.all.aus)/12), ncol = 12, byrow = TRUE)
  P1[P1 == 0] = 10^(-16)
  P2 = t(apply(P1,1,div1))
  P.list[[i]] = P2
}

E1 = matrix(0, nrow = length(P.list), ncol = nrow(P.list[[1]]))

for(i in 1:length(P.list)){
  E1[i,] = apply(P.list[[i]], 1, entropy1)
}

del = 0.5
Melb = which(NOAA.aus[[1]][,1] < -37.8 + del & NOAA.aus[[1]][,1] > -37.8 - del & NOAA.aus[[1]][,2] < 144.96 + del & NOAA.aus[[1]][,2] > 144.96 - del)[1]
Dar =  which(NOAA.aus[[1]][,1] < -12.397 + del & NOAA.aus[[1]][,1] > -12.397 - del & NOAA.aus[[1]][,2] < 130.94 + del & NOAA.aus[[1]][,2] > 130.94 - del)[1]

Hist.df = data.frame(Dar.Jan = Prop[[Dar]][,1], Dar.Jul = Prop[[Dar]][,7], Melb.Jan = Prop[[Melb]][,1], Melb.Jul = Prop[[Melb]][,7])

(g.dar1 = ggplot(Hist.df, mapping = aes(x = Dar.Jan)) + geom_histogram(bins = 15, color="black", fill="white") +
    labs(x = "Proportion", y = "Count", title = "Darwin January") + theme_bw() + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
          axis.title=element_text(size=14),  axis.text=element_text(size=12),
          strip.text.y = element_text(size = 14, colour = "black", angle = 90)))

(g.dar2 = ggplot(Hist.df, mapping = aes(x = Dar.Jul)) + geom_histogram(bins = 15, color="black", fill="white") +
    labs(x = "Proportion", y = "Count", title = "Darwin July") + theme_bw() + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
          axis.title=element_text(size=14),  axis.text=element_text(size=12),
          strip.text.y = element_text(size = 14, colour = "black", angle = 90)))

(g.Melb1 = ggplot(Hist.df, mapping = aes(x = Melb.Jan)) + geom_histogram(bins = 15, color="black", fill="white") +
    labs(x = "Proportion", y = "Count", title = "Melbourne January") + theme_bw() + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
          axis.title=element_text(size=14),  axis.text=element_text(size=12),
          strip.text.y = element_text(size = 14, colour = "black", angle = 90)))

(g.Melb2 = ggplot(Hist.df, mapping = aes(x = Melb.Jul)) + geom_histogram(bins = 15, color="black", fill="white") +
    labs(x = "Proportion", y = "Count", title = "Melbourne July") + theme_bw() + 
    theme(plot.title = element_text(size = 15, face = "bold"),
          legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
          axis.title=element_text(size=14),  axis.text=element_text(size=12),
          strip.text.y = element_text(size = 14, colour = "black", angle = 90)) )

grid.arrange(g.dar1 + labs(x = " ", y = " "), g.dar2 + labs(x = " ", y = " "),
             g.Melb1 + labs(x = " ", y = " "), g.Melb2 + labs(x = " ", y = " "), nrow = 2, 
             left = textGrob("Count", gp=gpar(fontsize=16,font=8), rot = 90, hjust = 0),
             bottom = textGrob("Proportion", gp=gpar(fontsize=16,font=8), vjust = 0, hjust = 0.27))



alpha.mat = matrix(0, nrow = nrow(NOAA.all.aus), ncol = 12)
for(i in 1:nrow(NOAA.all.aus)){
  p1 = NULL
  indices = 1:12
  for(j in 1:(ncol(NOAA.all.aus) - 11)){
    p1 = rbind(p1, (NOAA.all.aus[i, j:(j+11)])[indices])
    indices = c(indices[12], indices[-12])
  }
  d1 = dirichlet.mle(p1)
  alpha.mat[i,] = d1$alpha
}

alpha.exp = matrix(0, nrow = nrow(alpha.mat), ncol = 2)
for(i in 1:nrow(alpha.mat)){
  alpha.exp[i,1] = 0.5 - ncol(alpha.mat)/2 * (sum(alpha.mat[i,]^2) + sum(alpha.mat[i,]))/((sum(alpha.mat[i,]) + 1) * sum(alpha.mat[i,])) + log(ncol(alpha.mat))
  alpha.exp[i,2] = ncol(alpha.mat)^2 / 4 * 1
}


pred.grid = data.frame(Lon = Austra2[,2], Lat = Austra2[,1])
d1 = data.frame(alpha1 = alpha.mat[,1], alpha2 = alpha.mat[,2], alpha3 = alpha.mat[,3], alpha4 = alpha.mat[,4], alpha5 = alpha.mat[,5], alpha6 = alpha.mat[,6],
                alpha7 = alpha.mat[,7], alpha8 = alpha.mat[,8], alpha9 = alpha.mat[,9], alpha10 = alpha.mat[,10], alpha11 = alpha.mat[,11], alpha12 = alpha.mat[,12],
                Lat = NOAA.aus[[1]]$Lat[!duplicated(NOAA.aus[[1]][,1:2])], Lon = NOAA.aus[[1]]$Lon[!duplicated(NOAA.aus[[1]][,1:2])], alpha0 = rowSums(alpha.mat), alpha1.0 = alpha.mat[,1]/rowSums(alpha.mat), 
                alpha7.0 = alpha.mat[,7]/rowSums(alpha.mat), alpha1.var = alpha.mat[,1] * (rowSums(alpha.mat) - alpha.mat[,1]) / (rowSums(alpha.mat)^2 *(rowSums(alpha.mat) + 1)),
                alpha7.var = alpha.mat[,7] * (rowSums(alpha.mat) - alpha.mat[,7]) / (rowSums(alpha.mat)^2 *(rowSums(alpha.mat) + 1)))


idw1 =  idw(formula = alpha1 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw2 =  idw(formula = alpha7 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw3 =  idw(formula = alpha1.0 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw4 =  idw(formula = alpha7.0 ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw5 =  idw(formula = alpha1.var ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)
idw6 =  idw(formula = alpha7.var ~ 1, locations = ~Lon + Lat, data = d1, newdata = Austra2, idp = 3)


g1 = ggplot() + 
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9), limits = c(min(idw1$var1.pred, idw2$var1.var), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(paste(alpha)), x = "Longitude", y = "Latitude", title = expression(paste("January proportion ", alpha)))+ 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


g2 = ggplot() + 
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9), limits = c(min(idw1$var1.pred, idw2$var1.var), max(idw1$var1.pred, idw2$var1.pred))) +
  theme_bw() + labs(color = expression(paste(alpha, "            ")), x = "Longitude", y = "Latitude", title = expression(paste("July Proportion ", alpha)))+ 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


g3 = ggplot() +
  geom_point(idw3, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9), limits = c(min(idw3$var1.pred, idw4$var1.var), max(idw3$var1.pred, idw4$var1.pred))) +
  theme_bw() + labs(color = "Mean", x = "Longitude", y = "Latitude", title = expression(paste("January Proportion Mean")))+ 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


g4 = ggplot() + 
  geom_point(idw4, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9), limits = c(min(idw3$var1.pred, idw4$var1.var), max(idw3$var1.pred, idw4$var1.pred))) +
  theme_bw() + labs(color = "Mean     ", x = "Longitude", y = "Latitude", title = expression(paste("July Proportion Mean")))+ 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


g5 = ggplot() +  
  geom_point(idw5, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9), limits = c(min(idw5$var1.pred, idw6$var1.var), max(c(idw5$var1.pred, idw6$var1.pred)))) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = expression(paste("January Proportion Variance")))+ 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


g6 = ggplot() + 
  geom_point(idw6, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.3) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9), limits = c(0, max(idw5$var1.pred, idw6$var1.pred))) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = expression(paste("July Proportion Variance")))+ 
  theme(plot.title = element_text(size = 14, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=11),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14)) +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


grid.arrange(g1 + labs(x = " ", y = " ") + theme(legend.position = "none"),
             g2 + labs(x = " ", y = " ") + theme(legend.position = "right"),
             g3 + labs(x = " ", y = " ") + theme(legend.position = "none"),
             g4 + labs(x = " ", y = " ") + theme(legend.position = "right"),
             g5 + labs(x = " ", y = " ") + theme(legend.position = "none"),
             g6 + labs(x = " ", y = " ") + theme(legend.position = "right"), nrow = 3, ncol = 2,
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0.8, vjust = 0),
             left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, vjust = 1.5, hjust = 0), widths = c(1,1.3))




E.mean = rowMeans(E1)
E.var = apply(E1, 1, var)
Ent.mean.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Entropy = E.mean, Variance = E.Var)
idw1 =  idw(formula = E.mean ~ 1, locations = ~Lon + Lat, data = Ent.mean.df, newdata = Austra2, idp = 3)
idw2 =  idw(formula = E.var ~ 1, locations = ~Lon + Lat, data = Ent.mean.df, newdata = Austra2, idp = 3)

g1 = ggplot() +
  geom_point(idw1, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9)) +
  theme_bw() + labs(color = "Mean", x = "Longitude", y = "Latitude", title = "Entropy Mean")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)
g2 = ggplot() +
  geom_point(idw2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9)) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = "Entropy Variance")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)

grid.arrange(g1 + labs(x = " ", y = " ") + theme(legend.position = "left"),
             g2 + labs(x = " ", y = " ") + theme(legend.position = "right"), nrow = 1, ncol = 2,
             bottom = textGrob("Longitude", gp=gpar(fontsize=14,font=8), hjust = 0, vjust = 0),
             left = textGrob("Latitude", gp=gpar(fontsize=14,font=8), rot = 90, vjust = 10, hjust = 0), widths = c(1,1))


p.val = NULL
for(i in 1:nrow(E1)){
  x1 = 1:43
  mod1 = lm(E1[i,] ~ x1)
  s1 = summary(mod1)
  p.val = c(p.val, s1$coefficients[2,4])
}


p.val.df = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, pval = p.val)
idw1 =  idw(formula = pval ~ 1, locations = ~Lon + Lat, data = p.val.df, newdata = Austra2, idp = 6)

idw1$Class = (idw1$var1.pred < 0.05)

idw1 = idw1[idw1$Class == 1,]


g1 = ggplot() +  
  geom_point(idw1, mapping = aes(x = Lon, y = Lat), size = 0.1, colour = "#ff0d23") + 
  theme_bw() + labs(color = "Entropy", x = "Longitude", y = "Latitude", title = "Significant Entropy Trend")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)+ scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)
g1


##############################
#Dirichlet Entropy sim
##############################

m.beta = function(alpha.vec){
  prod(gamma(alpha.vec))/gamma(sum(alpha.vec))
}

e.vec = function(i,n){
  out = rep(0, n)
  out[i] = 1
  return(out)
}

dir.exp = function(alpha.vec){
  n = length(alpha.vec)
  a0 = sum(alpha.vec)
  out = 0
  for(i in 1:length(alpha.vec)){
    out = out + (m.beta(alpha.vec + e.vec(i,n))/m.beta(alpha.vec)) * (digamma(a0 + 1) - digamma(alpha.vec[i] + 1))
  }
  return(out)
}


dir.exp(c(10,5,6,8,3))

d1 = rdirichlet(100000, c(10,5,6,8,3))

d2 = (apply(d1, 1, entropy1))

t.test(d2, mu = dir.exp(c(10,5,6,8,3)))


dir.var1 = function(alpha.vec){
  n = length(alpha.vec)
  a0 = sum(alpha.vec)
  out = 0
  for(i in 1:length(alpha.vec)){
    out = out + (m.beta(alpha.vec + 2 * e.vec(i,n))/m.beta(alpha.vec)) * ((digamma(a0 + 2) - digamma(alpha.vec[i] + 2))^2 + trigamma(alpha.vec[i] + 2) - trigamma(a0 + 2))
  }
  return(out)
}


dir.var2 = function(alpha.vec){
  n = length(alpha.vec)
  a0 = sum(alpha.vec)
  out = 0
  for(i in 1:(n - 1)){
    for(j in (i + 1):n){
      out = out + (m.beta(alpha.vec + e.vec(i,n) + e.vec(j,n))/m.beta(alpha.vec)) *
        ((digamma(alpha.vec[i] + 1) - digamma(a0 + 2)) * (digamma(alpha.vec[j] + 1) - digamma(a0 + 2)) - trigamma(a0 + 2))
    }
  }
  return(out)
}


dir.var = function(alpha.vec){
  out = dir.var1(alpha.vec) + 2 * dir.var2(alpha.vec) - dir.exp(alpha.vec)^2
  return(out)
}


d1 = rdirichlet(100000, c(10,5,6,8,3))

d2 = (apply(d1, 1, entropy1))

t.test(d2, mu = dir.exp(c(10,5,6,8,3)))


c((length(d2) - 1) * var(d2)/(qchisq(0.975, df = length(d2) - 1)), (length(d2) - 1) * var(d2)/(qchisq(0.025, df = length(d2) - 1)))

dir.var(c(10,5,6,8,3))

alpha.mat = matrix(0, nrow = nrow(NOAA.all.aus), ncol = 12)
for(i in 1:nrow(NOAA.all.aus)){
  p1 = NULL
  indices = 1:12
  for(j in 1:(floor(ncol(NOAA.all.aus)/2) - 11)){
    p1 = rbind(p1, (NOAA.all.aus[i, j:(j+11)])[indices])
    indices = c(indices[12], indices[-12])
  }
  d1 = dirichlet.mle(p1)
  alpha.mat[i,] = d1$alpha
}


alpha.exp = apply(alpha.mat, 1, dir.exp)
alpha.var = apply(alpha.mat, 1, dir.var)

Sim1 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, Expectation = alpha.exp, Variance = alpha.var)

Sim1 = Sim1[Sim1$Expectation > 1, ]
Sim1 = Sim1[complete.cases(Sim1),]

idw1 = gstat::idw(formula = Expectation ~ 1, data = Sim1, locations = ~ Lon + Lat, newdata = Austra2, idp = 3)
idw2 = gstat::idw(formula = Variance ~ 1, data = Sim1, locations = ~ Lon + Lat, newdata = Austra2, idp = 3)

Sim2 = rbind(idw1, idw2)
Sim2$Class = rep(c("Expectation", "Variance"), each = nrow(idw1))


ggplot() +
  geom_point(Sim2, mapping = aes(x = Lon, y = Lat, colour = var1.pred), size = 0.1) + facet_wrap(~ Class) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"),
                         values = seq(0,1, length.out = 9)) +
  theme_bw() + labs(color = "Variance", x = "Longitude", y = "Latitude", title = "Entropy Variance")+ 
  theme(plot.title = element_text(size = 16, face = "bold"),
        legend.title=element_text(size=14), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right") +
  xlim(113, 155) + ylim(-45,-9)+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)



m1 = NULL
for(i in 1:nrow(NOAA.all.aus)){
  p1 = NULL
  for(j in (floor(ncol(NOAA.all.aus)/12/2) + 1):floor(ncol(NOAA.all.aus)/12)){
    p1 = rbind(p1, (NOAA.all.aus[i, (12 * (j - 1) + 1):(j * 12)]))
  }
  d1 = mean(apply(p1, 1, entropy1))
  m1 = c(m1, d1)
}

alpha.mat = matrix(0, nrow = nrow(NOAA.all.aus), ncol = 12)
for(i in 1:nrow(NOAA.all.aus)){
  p1 = NULL
  indices = 1:12
  for(j in 1:floor(ncol(NOAA.all.aus)/12/2)){
    p1 = rbind(p1, (NOAA.all.aus[i, (12 * (j - 1) + 1):(j * 12)]))
  }
  d1 = dirichlet.mle(p1)
  alpha.mat[i,] = d1$alpha
}



alpha.exp = apply(alpha.mat, 1, dir.exp)
alpha.var = apply(alpha.mat, 1, dir.var)


mean(abs((m1 - alpha.exp)/sqrt(alpha.var/ 21)) > 1.96, na.rm = TRUE)


mean(pnorm(abs((m1 - alpha.exp)/sqrt(alpha.var/ 21))) > 0.975, na.rm = TRUE)

Sim2 = data.frame(Lon = NOAA.aus[[1]]$Lon, Lat = NOAA.aus[[1]]$Lat, p.val = 1 - pnorm(abs((m1 - alpha.exp)/sqrt(alpha.var/ 21))))
Sim2 = Sim2[complete.cases(Sim2),]
idw1 = gstat::idw(formula = p.val ~ 1, locations = ~Lon + Lat, data = Sim2, newdata = Austra2, idp = 5)

coordinates(Sim2) = ~Lon + Lat
proj4string(Sim2) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
Austra3 = Austra2
coordinates(Austra3) = ~ Lon + Lat
proj4string(Austra3) = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
idw1 = autoKrige(formula = p.val ~ 1, input_data = Sim2, new_data = Austra2)

idw1$Class = (idw1$var1.pred < 0.025)

Sim3 = idw1[idw1$Class,]


ggplot() + 
  geom_point(data = Sim3, 
             mapping = aes(x = Lon, y = Lat, colour = as.factor(Class)), size = 0.5) +
  scale_colour_manual(
    values = c("1" = "#ff0d23", "0" = "white"),
    aesthetics = c("colour", "fill"),
    labels = c("1" = "Extreme", "0" = "Normal")
  ) +
  coord_cartesian(xlim = c(110,155), ylim = c(-45, -9)) +
  labs(color = "Classification", x = "Longitude", y = "Latitude",
       title = "Asymptotic Z Test") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=15), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "none",
        strip.text.x = element_text(size = 14, colour = "black")) +
  guides(colour = guide_legend(override.aes = list(size=10)))+
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


