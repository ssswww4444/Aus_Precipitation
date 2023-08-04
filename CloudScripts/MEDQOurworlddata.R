#### GDP per capita vs LE emissions per capita

LE <- read.csv("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/life-expectancy.csv")

GDP = read.csv("/mount/autofs/home_ad1/student.unimelb.edu.au/bhines/gdp-per-capita-worldbank.csv")



g1 = ggplot(data = LE.4, mapping = aes(x = Year, y = Emmision, group = Entity)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "LE Emmissions per capita",
       title = "Time Series 1") +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))
g1

g2 = ggplot(data = GDP.4, mapping = aes(x = Year, y = GDPPC, group = Entity)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "GDP per capita",
       title = "Time Series 1") +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))
g2

a1 = NULL
for(i in unique(GDP$Entity)){
  if(min(GDP$Year[GDP$Entity == i]) > 1990){
    a1 = c(a1, i)
  }
}

a1.1 = NULL
for(i in a1){
  a1.1 = c(a1.1, which(GDP$Entity == i))
}

GDP.1 = GDP[-a1.1,]

a2 = NULL
for(i in unique(LE$Entity)){
  if(min(LE$Year[LE$Entity == i]) > 1990){
    a2 = c(a2, i)
  }
}

a2.1 = NULL
for(i in a2){
  a2.1 = c(a2.1, which(LE$Entity == i))
}

LE.1 = LE[LE$Year < 2018, ]


t1 =table(c(unique(GDP.1$Entity), unique(LE.1$Entity)))
n1 = names(t1)[which(as.numeric(t1) == 1)]

a1.2 = NULL
a2.2 = NULL
for(i in n1){
  a1.2 = c(a1.2, which(GDP.1$Entity == i))
  a2.2 = c(a2.2, which(LE.1$Entity == i))
}

GDP.2 = GDP.1[-a1.2,]
LE.2 = LE.1[-a2.2,]

length(unique(GDP.2$Entity))
length(unique(LE.2$Entity))

LE.2 = LE.2[LE.2$Year >1989,]

n2  = c("World", "Yemen")

a1.3 = NULL
a2.3 = NULL
for(i in n2){
  a1.3 = c(a1.3, which(GDP.2$Entity == i))
  a2.3 = c(a2.3, which(LE.2$Entity == i))
}

GDP.3 = GDP.2[-a1.3,]
LE.3 = LE.2[-a2.3,]

t1 = table(GDP.3$Entity)
t2 = table(LE.3$Entity)
n1 = c(names(t1)[which(as.numeric(t1) == 24)], names(t1)[which(as.numeric(t1) == 25)], names(t1)[which(as.numeric(t1) == 27)])

a1.4 = NULL
a2.4 = NULL
for(i in n1){
  a1.4 = c(a1.4, which(GDP.2$Entity == i))
  a2.4 = c(a2.4, which(LE.2$Entity == i))
}

GDP.4 = GDP.3[-a1.4,]
LE.4 = LE.3[-a2.4,]

colnames(GDP.4) = c("Entity", "Code", "Year", "GDPPC")
colnames(LE.4) = c("Entity", "Code", "Year", "Expectancy")

g1 = ggplot(data = LE.4, mapping = aes(x = Year, y = Expectancy, group = Entity)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Life Expectancy (Years)",
       title = "Time Series 1") +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))
g1

g2 = ggplot(data = GDP.4, mapping = aes(x = Year, y = GDPPC, group = Entity)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "GDP per capita",
       title = "Time Series 1") +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))
g2

a1 = matrix(GDP.4$GDPPC, ncol = 28, byrow = TRUE)

X.list = list()
X.list[[1]] = matrix(GDP.4$GDPPC, ncol = 28, byrow = TRUE)
X.list[[2]] = matrix(LE.4$Expectancy, ncol = 28, byrow = TRUE)

p = seq(0,1,0.005)
M1 = MEDQ(X.list = X.list, p = p, method = "Mahalanobis")


TS.data = data.frame(Country = GDP.4$Entity, Year = LE.4$Year, GDP = GDP.4$GDPPC, LE = LE.4$Expectancy)


dd1 = cbind(M1, p)
v1 = NULL
for(i in unique(M1)){
  v1 = c(v1, max(dd1[which(dd1[,1] == i), 2]))
}

TS.dates1 = rep(unique(TS.data$Year), length(unique(M1)))
TS.GDP = as.vector(t(X.list[[1]][unique(M1),]))
TS.LE = as.vector(t(X.list[[2]][unique(M1),]))
TS.group1 = rep(1:nrow(X.list[[1]][unique(M1),]), each = 28)
TS.value1 = rep(v1, each = 28)

TS.data.2 = data.frame(Country = TS.group1, Year = TS.dates1, GDP = TS.GDP, LE = TS.LE, Value = TS.value1)


g.GDP = ggplot(data = TS.data, mapping = aes(x = Year, y = GDP, group = Country)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "GDP Per Capita (US$)") +
  geom_line(data = TS.data.2, mapping = aes(x = Year, y = GDP, colour = Value), size = 1.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12))

g.LE = ggplot(data = TS.data, mapping = aes(x = Year, y = LE, group = Country)) + geom_line() +
  labs(colour = "Quantile", x = "Year", y = "Life Expectancy (Years)") +
  geom_line(data = TS.data.2, mapping = aes(x = Year, y = LE, colour = Value), size = 1.5) +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9)) +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
        legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
        axis.title=element_text(size=14),  axis.text=element_text(size=12), axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 3.5)))
grid.arrange(g.GDP + theme(legend.position = "none"), g.LE + theme(legend.position = "none"), nrow = 1)

a1 = unique(LE.4$Entity)[unique(M1)]
a2 = rep(NA, length(shape$NAME))
for(i in 1:length(a1)){
  b1 = which(shape$NAME == a1[i] | shape$ISO3 == (unique(LE.4$Code)[unique(M1)])[i])[1]
  a2[b1] = v1[i]
}

a1= NULL
for(i in 1:length(shape@polygons)){
  a3 = NULL
  for(j in 1:length(shape@polygons[[i]]@Polygons)){
    a3 = c(a3, nrow(shape@polygons[[i]]@Polygons[[j]]@coords))
  }
  a1 = c(a1, sum(a3))
}

a4 = rep(a2, a1)


map1 = ggplot() + geom_polygon(data = shape, aes(x = long, y = lat, group = group, fill = a4), colour = "black") +
  scale_fill_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(1,0, length.out = 9), na.value="white") +
  labs(fill = "Quantile", x = "Longitude", y = "Latitude", title = "MEDQ Locations") +
  theme_bw() + 
  theme(plot.title = element_text(size = 15, face = "bold"),
                     legend.title=element_text(size=15), legend.text=element_text(size=13), legend.key.size = unit(1, "cm"),
                     axis.title=element_text(size=14),  axis.text=element_text(size=12), legend.position = "bottom")

require(scales)


grid.arrange(g.GDP + theme(legend.position = "none", plot.margin = margin(0, 0, 4, 0.1, "cm"), axis.text.y = element_text(angle = 45)) + scale_y_continuous(labels = comma) + 
               labs(title = "GDP Per Capita"),
             g.LE + theme(legend.position = "none", plot.margin = margin(0, 0, 4, 0.1, "cm")) + labs(title = "Life Expectancy"),
             map1 + theme(plot.margin = margin(-4, 0, 0, 0, "cm"), legend.position = "none"),layout_matrix=rbind(c(1,2), c(1,2), c(3,3)))
             
  








