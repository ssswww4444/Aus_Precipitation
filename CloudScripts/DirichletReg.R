install.packages("DirichletReg")
library(DirichletReg)


ALake <- ArcticLake
ALake$Y <- DR_data(ALake[,1:3])

# fit a quadratic Dirichlet regression models ("common")
res1 <- DirichReg(Y ~ depth + I(depth^2), ALake)

# fit a Dirichlet regression with quadratic predictor for the mean and
# a linear predictor for precision ("alternative")
res2 <- DirichReg(Y ~ depth + I(depth^2) | depth, ALake, model="alternative")

# test both models
anova(res1, res2)

res1
summary(res2)


rowSums(matrix(P1[1,], nrow = 40, ncol = 12, byrow = TRUE))

D1 = matrix(P1[1,], nrow = 40, ncol = 12, byrow = TRUE)

D2 = DR_data(D1)



P.list = list()
for(i in 1:12){
  P.list[[i]] = matrix(0, ncol = 40, nrow = nrow(P1))
}


for(i in 1:nrow(P1)){
  p.null = matrix(P1[i,], nrow = 40, ncol = 12, byrow = TRUE)
  for(j in 1:ncol(p.null)){
    P.list[[j]][i,] = p.null[,j]
  }
}


MEDQ(P.list, p = c(0.05, 0.5, 0.95), weight = FALSE, scale = FALSE)



Sliding.ent = matrix(0, nrow = nrow(Aus.all), ncol = ncol(Aus.all) - 11)
for(i in 1:nrow(Aus.all)){
  for(j in 1:(ncol(Aus.all) - 11)){
    Sliding.ent[i,j] = entropy1(Aus.all[i,(j:(j+11))])
  }
}

par(mfrow = c(1,1))
ts.plot(t(Sliding.ent))

S.list = list()
S.list[[1]] = Sliding.ent
MEDQ(S.list, p = c(0.05, 0.5, 0.95), weight = FALSE)


rowSums(P.list[[Melb]])

P.Melb = P.list[[Melb]]

D2 = DR_data(P.Melb)

colnames(P.Melb) = paste0("month",1:12)
P.Melb = as.data.frame(P.Melb)

P.Melb1 = P.Melb[1:40,]
P.Melb2 = P.Melb[2:41,]
P.Melb3 = P.Melb[3:42,]

auto.arima(P.Melb[,12])

colnames(P.Melb2) = paste0(v, 1:12)
colnames(P.Melb3) = paste0(u, 1:12)

P.Melb4 = cbind(P.Melb1, P.Melb2, P.Melb3)

colnames(P.Melb4) = c(paste0("month", 1:12), paste0("v", 1:12), paste0("u", 1:12))

P.Melb4$Y = DR_data(P.Melb4[,1:12])

res1 <- DirichReg(Y ~ v1 + v2 + v3 + v4 + v5 + v6 + v7 + v8 + v9 + v10 + v11, P.Melb4)


bacf1 <- acf(P.list[[Melb]][,1], plot = FALSE, type = "covariance")
bacfdf1 <- with(bacf1, data.frame(lag, acf))

bacf2 <- acf(P.list[[Melb]][,7], plot = FALSE, type = "covariance")
bacfdf2 <- with(bacf2, data.frame(lag, acf))

bacf3 <- acf(P.list[[Dar]][,1], plot = FALSE, type = "covariance")
bacfdf3 <- with(bacf3, data.frame(lag, acf))

bacf4 <- acf(P.list[[Dar]][,7], plot = FALSE, type = "covariance")
bacfdf4 <- with(bacf4, data.frame(lag, acf))

bacfdf = rbind(bacfdf1, bacfdf2, bacfdf3, bacfdf4)

bacfdf$Location = c(rep("Melbourne", nrow(bacfdf1) + nrow(bacfdf2)),
                   rep("Darwin", nrow(bacfdf3) + nrow(bacfdf4)))

bacfdf$Month = c(rep("January", nrow(bacfdf1)), rep("July", nrow(bacfdf2)),
                 rep("January", nrow(bacfdf3)), rep("July", nrow(bacfdf4)))



q1 <- ggplot(data = bacfdf3, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocorrelation Function",
       title = "Darwin January") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q1

q2 <- ggplot(data = bacfdf4, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocorrelation Function",
       title = "Darwin July") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q2

q3 <- ggplot(data = bacfdf1, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocorrelation Function",
       title = "Melbourne January") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q3

q4 <- ggplot(data = bacfdf2, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0)) + theme_bw() +
  labs(x = "Lag (months)", y = "Autocorrelation Function",
       title = "Melbourne July") +  theme_bw() + 
  theme(plot.title = element_text(size = 18, face = "bold"),
        legend.title=element_text(size=18), axis.text=element_text(size=12), legend.text=element_text(size=13),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=14), legend.position = "right")
q4


grid.arrange(q1 + labs(x = " ",y = " "), q2 + labs(x = " ",y = " "), q3 + labs(x = " ",y = " "), q4 + labs(x = " ",y = " "),
             left = textGrob("Autocovariance Function", vjust = 1, gp=gpar(fontsize=16,font=8), hjust = 0.45, rot = 90), 
             bottom = textGrob("Lag (Years)", gp=gpar(fontsize=16,font=8), vjust = 0, hjust = 0.2), nrow = 2)



P.df = data.frame(Proportion = c(as.vector(t(P.list[[Melb]])), as.vector(t(P.list[[Dar]]))),
                  Location = rep(c("Melbourne", "Darwin"), each = length(as.vector(t(P.list[[Melb]])))),
                                 Year = rep(datesNOAA[1:(42*12),1],2), Month = rep(datesNOAA[1:(42*12),2], 2))



ggplot(P.df, mapping = aes(x = Year, y = Proportion, group = Location, colour = Location)) + geom_line() + facet_wrap(~Month)










