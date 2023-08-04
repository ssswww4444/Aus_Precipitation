Stations = read.table("Stations", header = FALSE)[-1,]


#Set Working Directory to BOMGaugeData folder
BOM.list = list()
for(i in 1:length(Stations)){
  BOM.list[[i]] = read.csv(paste0("BOM", Stations[i]))
}