


library(rgdal)
library(sp)
library(maps)
library(dplyr)
library(DT)

wa.map <- readOGR("EasternNSW_PrimaryStudyArea_Merged.shp", layer="EasternNSW_PrimaryStudyArea_Merged")
sodo <- wa.map[0]


#   #"+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"
#sodo<-spTransform(sodo, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))
proj4string(sodo)<-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0")


dat <- data.frame(Longitude =c(142.2854149,151.0864149,146.2754149) ,
                  Latitude =c(-33.43033554,-33.43233554,-33.43333554),
                  names = c("BKKKKKKKK","abc1","abc2"))


p<-dat

coordinates(p) <- ~ Longitude + Latitude



#p <- SpatialPointsDataFrame(dat[,1:2], dat, proj4string =crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

proj4string(p) <- proj4string(sodo)
#p<-spTransform(p, CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))



x<-p[sodo,]
plot(sodo)
plot(x, col="red" , add=TRUE, lwd=4)
common <- setdiff(dat$names, x@data$names)  
pts<-SpatialPoints(cbind(dat$Longitude,dat$Latitude))
plot(pts, col="blue", add=TRUE)

is.vector(common)
toString(common) 








inFile <- input$file1
infile_df <- read.csv(inFile$datapath,
                      header = T,
                      stringsAsFactors = F)
names(infile_df)[1] <- "sites"

if (check_infile()$env_present) {
  
  env_data <- select(infile_df, sites, one_of(non_floristic), -X)
  
  polygonSF <- read_sf(dsn = "spatial/EasternNSW_PrimaryStudyArea_Merged.shp")
  
  st_crs(polygonSF)=4326
  
  
  dat <- data.frame(Longitude =env_data$Longitude ,
                    Latitude =env_data$Latitude,
                    names = env_data$sites)                  
  
  
  coordinates(dat) <- ~ Longitude + Latitude
  
  pointsSF <- st_as_sf(x = dat, coords = c("Longitude", "Latitude"), crs = st_crs(polygonSF))
  
  st_crs(pointsSF)=4326
  
  pointsSF <- st_transform(pointsSF, crs = st_crs(polygonSF))
  
  numplotsOutsideStudy<-nrow(data.frame(table(st_difference(pointsSF, polygonSF))))
  
} 


