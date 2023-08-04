download.shapefile<-function(shape_url,layer,outfile=layer)
{
  #written by: jw hollister
  #Oct 10, 2012
  
  #set-up/clean-up variables
  if(length(grep("/$",shape_url))==0)
  {
    shape_url<-paste(shape_url,"/",sep="")
  }
  #creates vector of all possible shapefile extensions
  shapefile_ext<-c(".shp",".shx",".dbf",".prj",".sbn",".sbx",
                   ".shp.xml",".fbn",".fbx",".ain",".aih",".ixs",
                   ".mxs",".atx",".cpg")
  
  #Check which shapefile files exist
  if(require(RCurl))
  {
    xurl<-getURL(shape_url)
    xlogic<-NULL
    for(i in paste(layer,shapefile_ext,sep=""))
    {
      xlogic<-c(xlogic,grepl(i,xurl))
    }
    
    #Set-up list of shapefiles to download
    shapefiles<-paste(shape_url,layer,shapefile_ext,sep="")[xlogic]
    #Set-up output file names
    outfiles<-paste(outfile,shapefile_ext,sep="")[xlogic]   }
  #Download all shapefiles
  if(sum(xlogic)>0)
  {
    for(i in 1:length(shapefiles))
    {
      download.file(shapefiles[i],outfiles[i],
                    method="auto",mode="wb")
    }
  } else
  {
    stop("An Error has occured with the input URL
            or name of shapefile")
  }
}


#Download the NH State Boundaries
download.shapefile("ftp://ftp.granit.sr.unh.edu/pub/GRANIT_Data/Vector_Data/Administrative_and_Political_Boundaries/d-nhsenatedists/2012",
                   "NHSenateDists2012")
#Read shapefiles in SpatialPolygonsDataFrame
NHBnd<-readOGR(".","NHSenateDists2012")
#Plot it
plot(NHBnd)

URLs <- c("http://gis.tirol.gv.at/ogd/umwelt/wasser/wis_gew_pl.zip",
          "http://gis.tirol.gv.at/ogd/umwelt/wasser/wis_tseepeicher_pl.zip")
url_shp_to_spdf <- function(URL) {
  require(rgdal)
  wd <- getwd()
  td <- tempdir()
  setwd(td)
  temp <- tempfile(fileext = ".zip")
  download.file(URL, temp)
  unzip(temp)
  shp <- dir(tempdir(), "*.shp$")
  lyr <- sub(".shp$", "", shp)
  y <- lapply(X = lyr, FUN = function(x) readOGR(dsn=shp, layer=lyr))
  names(y) <- lyr
  unlink(dir(td))
  setwd(wd)
  return(y)
}
y <- lapply(URLs, url_shp_to_spdf)
z <- unlist(unlist(y))
# finally use it:
plot(z[[1]])



# Download the shapefile. (note that I store it in a folder called DATA. You have to change that if needed.)
download.file("https://www.abs.gov.au/statistics/standards/australian-statistical-geography-standard-asgs-edition-3/jul2021-jun2026/access-and-downloads/digital-boundary-files/STE_2021_AUST_SHP_GDA2020.zip" , destfile="world_shape_file.zip")
# You now have it in your current working directory, have a look!

# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
system("unzip world_shape_file.zip")
#  -- > You now have 4 files. One of these files is a .shp file! (TM_WORLD_BORDERS_SIMPL-0.3.shp)

setwd("/nfs/ms_home/home/ad/student.unimelb.edu.au/bhines")

library(rgdal)
my_spdf <- readOGR( 
  dsn= ".",
  layer="STE_2021_AUST_GDA2020"
)


dates2 = rep(0, nrow(IDWmat))
for(i in 1:nrow(IDWmat)){
  m1 = IDWmat$Month[i]
  if(nchar(m1) == 1){
    m1 = paste0("0", m1)
  }
  dates2[i] = paste0(IDWmat$Year[i], "-", m1)
  if(i %% 100000 == 0){
    print(i)
  }
}

IDWmat$Date2 = dates2


# Make a ggplot, but add frame=year: one image per year
ggplot(IDWmat, aes(x = Lon, y = Lat, colour = sLID)) +
  geom_point(alpha = 0.7, size = 0.1) + 
  geom_polygon(data = my_spdf, aes(x = long, y = lat, group = group), colour = "black", fill = NA, size = 0.5) +
  theme_bw() +
  facet_wrap(~s) +
  xlim(113, 155) + ylim(-45,-9) +
  # Here comes the gganimate specific bits
  labs(title = "{closest_state}", x = 'Longitude', y = 'Latitude') +
  transition_states(Date2) +
    
  ease_aes('linear') +
  scale_colour_gradientn(colours=c("#00007F", "blue", "#007FFF", "cyan",
                                   "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"), values = seq(0,1, length.out = 9))




help(spline)

dim(SLID.TS)




























