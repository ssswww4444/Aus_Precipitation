######################
#BOM all test
######################




Stations = 1000:400000

Stations = Stations.all

Stations1 = ifelse(nchar(Stations) == 1, paste0("00000", Stations),
                   ifelse(nchar(Stations) == 2, paste0("0000", Stations),
                          ifelse(nchar(Stations) == 3, paste0("000", Stations),
                                 ifelse(nchar(Stations) == 4, paste0("00", Stations),
                                        ifelse(nchar(Stations) == 5, paste0("0", Stations), Stations)))))

Stations = Stations1


library(RCurl)
BOM.list = list()
k1 = 1
for(k in 1:length(Stations)){
  tryCatch({
    Station = Stations[k]
    
    file = paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=", Station)
    
    a1 = getURL(file)
    
    g1 = gregexpr("rainfall", a1)
    if(g1[[1]][1] < 450){
      Lon = as.numeric(substr(a1, gregexpr("Longitude", a1)[[1]][1] + 33, gregexpr("Longitude", a1)[[1]][1] + 38))
      
      Lat = -as.numeric(substr(a1, gregexpr("Latitude", a1)[[1]][1] + 33, gregexpr("Latitude", a1)[[1]][1] + 37))
      
      y1 = NULL
      for(i in 2:length(gregexpr("startYear", a1)[[1]])){
        y1 = c(y1, as.numeric(substr(a1, gregexpr("startYear", a1)[[1]][i] + 10, gregexpr("startYear", a1)[[1]][i] + 13)))
      }
      Precipdf = data.frame(Lon = rep(Lon, length(y1) * 12), Lat = rep(Lat, length(y1) * 12),
                            Year = rep(y1, each = 12), Month = rep(1:12, length(y1)), Station = rep(Station, length(y1) * 12))
      pos1 = c(gregexpr("startYear", a1)[[1]], gregexpr("startYear", a1)[[1]][length(gregexpr("startYear", a1)[[1]])] + 376)
      data2 = NULL
      t2 = NULL
      for(i in 2:(length(pos1)  - 1)){
        
        a2 = substr(a1, pos1[i], pos1[i + 1])
        if(gregexpr("scope", a2)[[1]][1] > 0){
          a2 = substr(a1, pos1[i], pos1[i] + gregexpr("scope", a2)[[1]][1])
        }
        
        data1 = NULL
        mon1 = gregexpr("<td >", a2)[[1]]
        mon2 = gregexpr("qc\">", a2)[[1]] - 1
        mon3 = gregexpr("</td>", a2)[[1]]
        mon4 = sort(c(mon1, mon2))
        mon4 = mon4[mon4 > 0]
        for(j in 1:(length(mon4) - 1)){
          data1 = c(data1, as.numeric(substr(a2, mon4[j] + 5, mon3[j] - 1)))
        }
        t2 = c(t2,length(data1))
        data2 = c(data2, data1)
      }
      
      Precipdf$Rain = data2
      
      BOM.list[[k1]] = Precipdf
      k1 = k1 + 1
      print(k1)
    }
    
  }, error = function(e){})
  print(k)
}

Stations.all = NULL
for(i in 1:length(BOM.list)){
  Stations.all = c(Stations.all, BOM.list[[i]]$Station[1])
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")


Station.Num = data.frame(Number = Stations.all)

write.csv(Station.Num, file = "StationNumbers", row.names = FALSE)

BOM.list[[17000]]

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/BOMGaugeData")


for(i in 1:length(BOM.list)){
  write.csv(BOM.list[[i]], file = paste0("BOM", Stations.all[i]), row.names = FALSE)
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

Station.Num = read.csv("StationNumbers")

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/BOMGaugeData")
BOM.list = list()
for(i in 1:nrow(Station.Num)){
  s1 = Station.Num[i,1]
  if(nchar(s1) == 4){
    s1 = paste0("00", s1)
  }else if(nchar(s1) == 5){
    s1 = paste0("0", s1)
  }
  BOM.list[[i]] = read.csv(paste0("BOM", s1))
}

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

LocationsBOM = NULL
for(i in 1:length(BOM.list)){
  LocationsBOM = rbind(LocationsBOM, BOM.list[[i]][1,1:2])
}



ggplot() + 
  geom_point(LocationsBOM, mapping = aes(x = Lon, y = Lat), size = 1.25)  +
  theme_bw() + labs(x = "Longitude", y = "Latitude", title = "EXOFI Negative", colour = "Index")+ 
  theme(plot.title = element_text(size = 26, face = "bold"),
        legend.title=element_text(size=22), axis.text=element_text(size=14), legend.text=element_text(size=16),
        legend.key.size = unit(1, "cm"), axis.title=element_text(size=18), legend.position = "right", strip.text = element_text(size = 18)) +
  xlim(113, 155) + ylim(-45,-9)  +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))+ 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5)


g1 = getURL("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=139&p_display_type=dataFile&p_startYear=&p_c=&p_stn_num=040913")


gregexpr("rainfall", g1)

BOM.nrow = NULL
for(i in 1:length(BOM.list)){
  BOM.nrow = c(BOM.nrow, nrow(BOM.list[[i]]))
}

BOM.list1 = list()
for(i in 1:length(BOM.list)){
  BOM.list1[[i]] = BOM.list[[i]][complete.cases(BOM.list[[i]]),]
}


BOM.nrow = NULL
for(i in 1:length(BOM.list)){
  BOM.nrow = c(BOM.nrow, nrow(BOM.list1[[i]]))
}


BOM.list1 = list()
k = 1
for(i in 1:length(BOM.list)){
  Temp = BOM.list[[i]]
  temp.date = as.Date(paste0(Temp$Year, "-", Temp$Month, "-", 15))
  if(sum(temp.date > as.Date("1960-01-01")) >= 700){
    BOM.list1[[k]] = Temp[temp.date >  as.Date("1960-01-01"), ]
    k = k + 1
  }
}


Temp = BOM.list1[[1]]
temp.date = as.Date(paste0(Temp$Year, "-", Temp$Month, "-", 15))

length(temp.date)

BOM.mat = matrix(0, nrow = length(BOM.list1), ncol = length(temp.date))
for(i in 1:length(BOM.list1)){
  Temp = BOM.list[[i]]
  t.date = as.Date(paste0(Temp$Year, "-", Temp$Month, "-", 15))
  for(j in 1:length(temp.date)){
    if(sum(t.date == temp.date[j]) > 0){
      BOM.mat[i,j] = Temp[which(t.date == temp.date[j]), 6]
    }else{
      BOM.mat[i,j] = NA
    }
  }
}


slid = function(x, s){
  out = rep(0, nrow(x))
  w1 = which(complete.cases(x))
  x1 = matrix(x[w1,], nrow = length(w1))
  d1 = dist(x1)
  d2 = as.matrix(d1)
  for(i in 1:nrow(x1)){
    nh = order(d2[i,])[2:(s + 1)]
    d3 = d2[i,nh]
    d3[d3 == 0] = 1e-16
    out[w1[i]] = max(- ( ( 1 / s ) * sum( log( d3/max( d3 ) ) ) ) ^ ( -1 ), 0)
  }
  out
}

BOM1.loc = NULL
for(i in 1:length(BOM.list1)){
  BOM1.loc = rbind(BOM1.loc, BOM.list1[[i]][1,1:2])
}


slid(BOM.mat[,100], 100)

BOM.slid = matrix(0, nrow = nrow(BOM.mat) * ncol(BOM.mat), ncol = 6)
for(i in 1:ncol(BOM.mat)){
  s1 = sum(!is.na(BOM.mat[,i])) - 1
  s2 = slid(matrix(BOM.mat[,i], ncol = 1), s1)
  BOM.slid[((i - 1) * nrow(BOM.mat) + 1 ):(i * nrow(BOM.mat)),] = cbind(BOM1.loc[,1], BOM1.loc[,2], rep(temp.date[i], nrow(BOM.mat)), rep(s1, nrow(BOM.mat)), s2, BOM.mat[,i])
  print(i)
}

BOM.slid = BOM.slid[1:(737 * nrow(BOM.mat)),]

BOM.slid = cbind(BOM.slid, 1:nrow(BOM.mat))

tail(BOM.slid)
temp.date.used = temp.date[1:737]

BOM.slid.df = data.frame(Lon = BOM.slid[,1], Lat = BOM.slid[,2], Date = as.Date(rep(temp.date.used, each = nrow(BOM.mat))),
                         s = BOM.slid[,4], sLID = BOM.slid[,5], rain = BOM.slid[,6], id = BOM.slid[,7])

BOM.slid.df1 = BOM.slid.df[complete.cases(BOM.slid.df),]
























