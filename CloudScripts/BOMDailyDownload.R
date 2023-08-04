############BOM Daily

d1 = paste0("scope=\"row\">",
            c("1st", "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th",
              "10th", "11th", "12th", "13th", "14th", "15th", "16th", "17th",
              "18th", "19th", "20th", "21st", "22nd", "23rd", "24th", "25th",
              "26th", "27th", "28th", "29th", "30th", "31st"))
d1 = c(d1, "<th scope=\"row\">Highest Daily")

Stations1 = StationDaily[,1]

Stations1 = Stations.all

Stations.1 = Stations.all[1:6000]
Stations.2 = Stations.all[6001:12000]
Stations.3 = Stations.all[12001:length(Stations.all)]

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines/BOMDaily")

for(j in Stations.3){
  tryCatch({
    stationnum = ifelse(nchar(j) == 4, paste0("00", j), ifelse(nchar(j) == 5, paste0("0", j), j))
    a1 = getURL(paste0("http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear=&p_c=&p_stn_num=", stationnum))
    spcode = substr(a1, gregexpr("amp;p_c", a1)[[1]][1] + 8, gregexpr("&amp;p_n", a1)[[1]][1] - 1)
    Lat = as.numeric(substr(a1, gregexpr("Lat:", a1)[[1]][1] + 23, gregexpr("Lat:", a1)[[1]][1] + 27))
    Lon = as.numeric(substr(a1, gregexpr("Lon:", a1)[[1]][1] + 22, gregexpr("Lon:", a1)[[1]][1] + 27))
    t1 = gregexpr("</option>", a1)[[1]]
    years = NULL
    Temp = NULL
    for(i in 1:(length(t1) - 6)){
      y1 = substr(a1, t1[i] - 4, t1[i] - 1)
      years = c(years, y1)
      file0 = "http://www.bom.gov.au/jsp/ncc/cdio/weatherData/av?p_nccObsCode=136&p_display_type=dailyDataFile&p_startYear="
      file1 = paste0("&p_c=", spcode,"&p_stn_num=", stationnum)
      file2 = paste0(file0, y1, file1)
      a2 = getURL(file2)
      for(k in 1:(length(d1) - 1)){
        mon1 = gregexpr(d1[k], a2)[[1]][1]
        mon2 = gregexpr(d1[k + 1], a2)[[1]][1]
        a3 = substr(a2, mon1, mon2)
        mon3 = gregexpr("<td ", a3)[[1]]
        mon4 = gregexpr("</td>", a3)[[1]]
        for(l in 1:length(mon3)){
          if(mon4[l] == mon3[l] + 5){
            R1 = NA
          }else if(mon4[l] == mon3[l] + 19){
            R1 = NULL
          }else if(gregexpr("class=\"no-qc\"", substr(a3, mon3[l], mon4[l]))[[1]][1] > -1){
            R1 = substr(a3, mon3[l] + 18, mon4[l] - 1)
          }else{
            R1 = substr(a3, mon3[l] + 5, mon4[l] - 1)
          }
          if(!is.null(R1)){
            Temp = rbind(Temp, c(y1, l, k, R1, ifelse(gregexpr("class=\"no-qc\"", substr(a3, mon3[l], mon4[l]))[[1]][1] > -1, "NQC", "QC")))
          }
        }
      }
    }
    
    D1 = as.Date(paste0(Temp[,1],"-", ifelse(nchar(Temp[,2]) == 1, paste0("0", Temp[,2]), Temp[,2]), "-", ifelse(nchar(Temp[,3]) == 1, paste0("0", Temp[,3]), Temp[,3])))
    O1 = order(D1)
    
    Precip.df = data.frame(Lon = as.numeric(rep(Lon, nrow(Temp))), Lat = as.numeric(rep(Lat, nrow(Temp))),
                           Year = as.numeric(Temp[O1,1]), Month = as.numeric(Temp[O1,2]), Day = as.numeric(Temp[O1,3]),
                           Rain = as.numeric(Temp[O1,4]), QualityControl = ifelse(Temp[O1,5] == "QC", TRUE, FALSE),
                           Date = D1[O1], Station = rep(stationnum, nrow(Temp)))
    
    write.csv(Precip.df, file = paste0("BOMDaily", stationnum))
  }, error = function(e){})
  print(j)
}



for(i in 1:length(BOM.daily)){
  write.csv(BOM.daily[[i]], file = paste0("BOMDaily", Stations[i]), row.names = FALSE)
  print(i)
}






