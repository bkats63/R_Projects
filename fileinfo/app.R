
library(zoo)
library(httr)


fp<- file.path(getwd(), "data")
normalizePath(fp)
fi<-file.info(list.files(path=fp,pattern="example.csv$", full.names=TRUE))
nhours<-as.numeric(difftime(fi$ctime ,Sys.Date(), units="hours"))

if (nhours>=720){
  
  #redo csv data
}

r <- GET("https://biodiversity.my.opendatasoft.com/api/odata/testdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")
#status_code(r)
# headers(r)
# str(content(r))
#content(r, "text")

ar<-array(content(r))
ar[2][[1]][[1]]
rcnames<-names(ar[2][[1]][[10]]) #must use names with loop for each record (last [])
rcnames
dt<-unlist(rcnames) #get names as array


rcnames<-names(ar[2][[1]][[2]])
dt<-unlist(rcnames) #get names as array
fileConn<-file("output2.txt")
writeLines(dt, fileConn)
close(fileConn)

df<-data.frame(ar[2][[1]][[2]])

fileConn <- file( "realization1.txt",open="w+" )

write.table(x =df,
      file = fileConn,col.names = TRUE,
      append = TRUE)


close( fileConn )


library(DBI)
# Initialize a temporary in memory database and copy a data.frame into it
con <- dbConnect(RSQLite::SQLite(), ":memory:")
data(USArrests)
dbWriteTable(con, "USArrests", USArrests)
dbListTables(con)

# Fetch all query results into a data frame:
dbGetQuery(con, "SELECT * FROM USArrests")

# Or do it in batches
rs <- dbSendQuery(con, "SELECT * FROM USArrests")
d1 <- dbFetch(rs) #, n = 10)      # extract data in chunks of 10 rows
dbHasCompleted(rs)
d2 <- dbFetch(rs, n = -1)      # extract all remaining data
dbHasCompleted(rs)
dbClearResult(rs)

# clean up
dbDisconnect(con)


library("OData")
pctdata<-retrieveData("https://biodiversity.my.opendatasoft.com/api/odata/pctdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")

#px<-pctdata$value[[1]]$pctname

nextlink<-pctdata[["@odata.nextLink"]]


pctdata2<-retrieveData(nextlink)


library("jsonlite")
fjs <- fromJSON("https://biodiversity.my.opendatasoft.com/api/odata/fsdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")

nextlink<-fjs[["@odata.nextLink"]]

fjs_df<-as.data.frame(fjs$value)

while(!is.null(nextlink))
{
 
  fsj2<-fromJSON(nextlink)
  nextlink<-fsj2[["@odata.nextLink"]]
  fjs2_df<-as.data.frame(fsj2$value)
  fjs_df<-rbind(fjs_df,fjs2_df)
  
}

fjs$value






library(DBI)
library(datamart)
# Initialize a temporary in memory database and copy a data.frame into it
con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadb.sqlite")
#dbWriteTable(con, "fsdata", fjs_df)

#dbExecute(con,"DELETE FROM pctdata")

rs <- dbSendQuery(con, "SELECT * FROM pctdata")
d1 <- dbFetch(rs)
dbHasCompleted(rs)
dbClearResult(rs)

# clean up
dbDisconnect(con)
pid<-as.numeric(d1$pctname)










library("jsonlite")
fjs <- fromJSON("https://biodiversity.my.opendatasoft.com/api/odata/fsdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")

nextlink<-fjs[["@odata.nextLink"]]

fjs_df<-as.data.frame(fjs$value)

while(!is.null(nextlink))
{
  
  fsj2<-fromJSON(nextlink)
  nextlink<-fsj2[["@odata.nextLink"]]
  fjs2_df<-as.data.frame(fsj2$value)
  fjs_df<-rbind(fjs_df,fjs2_df)
  
}



library(DBI)
library(datamart)

# Initialize a temporary in memory database and copy a data.frame into it
con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadb.sqlite")
dbWriteTable(con, "pctdata",fjs_df, overwrite=F, append=T)

# clean up
dbDisconnect(con)







library("OData")
pctdata<-retrieveData("https://biodiversity.my.opendatasoft.com/api/odata/pctdata?apikey=e37a9d51f1cf91617f70085f3c0be6882dd589b497742ce2df604dde")

nextlink<-pctdata[["@odata.nextLink"]]

n<-as.integer(length(pctdata$value))
# Initialize a temporary in memory database and copy a data.frame into it
con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadb.sqlite")
dbExecute(con,"DELETE FROM pctdata")
for (i in 1:n){
  
  fjs_df<-as.data.frame(pctdata$value[i], stringsAsFactors = F)
  dbWriteTable(con, "pctdata",fjs_df, overwrite=F, append=T)
  
}

# clean up
dbDisconnect(con)

while(!is.null(nextlink))
{
  pctdata2<-retrieveData(nextlink)
  nextlink<-pctdata2[["@odata.nextLink"]]
  n2<-as.integer(length(pctdata2$value))
  # Initialize a temporary in memory database and copy a data.frame into it
  con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadb.sqlite")
  
  for (i2 in 1:n2){
    
    fjs_df<-as.data.frame(pctdata$value[i2], stringsAsFactors = F)
    dbWriteTable(con, "pctdata",fjs_df, overwrite=F, append=T)
    
  }
  # clean up
  dbDisconnect(con)
}





library("OData")
fsdata<-retrieveData("https://data.bionet.nsw.gov.au/BioSvcApp/odata/SystematicFloraSurvey_SiteData?
$select=siteID,%20currentClassification,%20currentClassificationDescription,%20surveyName,%20PCTAssignmentCategory,%20decimalLatitude,%20decimalLongitude,%20visitNo,%20ElevationInMeters,
%20annualRainfallInMillimeters,%20annualMeanTemperatureInCelsius")









# 
# 
# siteno                 text,
# pctid                  text,
# pctname                text,
# pctassignmentcategory  text,
# surveyname             text,
# lat                    real,
# long                   real,
# replicatenumber        integer,
# elevation              real,
# rainfall               real,
# "temp"                 real



library("OData")
library(DBI)
library(datamart)
library("jsonlite") 
library(tidyverse)
library(dplyr)


      ## PCT DATA UPDATES FOR FLORA SURVEY

      fjs <-fromJSON("https://data.bionet.nsw.gov.au/BioSvcApp/odata/SystematicFloraSurvey_SiteData?$select=siteID,%20currentClassification,%20currentClassificationDescription,%20surveyName,%20PCTAssignmentCategory,%20decimalLatitude,%20decimalLongitude,%20visitNo,%20ElevationInMeters,%20annualRainfallInMillimeters,%20annualMeanTemperatureInCelsius")

       # n<-as.integer(length(pctdata$value))
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadb.sqlite")
        dbExecute(con,"DELETE FROM fsdata")
       ## for (i in 1:n){

          fjs_df<-as.data.frame(fjs$value, stringsAsFactors = F)
          
          
         
          names(fjs_df)[names(fjs_df) == "siteID"] <- "siteno"
          names(fjs_df)[names(fjs_df) == "currentClassification"] <- "pctid"
          names(fjs_df)[names(fjs_df) == "currentClassificationDescription"] <- "pctname"
          names(fjs_df)[names(fjs_df) == "PCTAssignmentCategory"] <- "pctassignmentcategory"
          names(fjs_df)[names(fjs_df) == "surveyName"] <- "surveyname"
          names(fjs_df)[names(fjs_df) == "decimalLatitude"] <- "lat"
          names(fjs_df)[names(fjs_df) == "decimalLongitude"] <- "long"
          names(fjs_df)[names(fjs_df) == "visitNo"] <- "replicatenumber"
          names(fjs_df)[names(fjs_df) == "ElevationInMeters"] <- "elevation"
          names(fjs_df)[names(fjs_df) == "annualRainfallInMillimeters"] <- "rainfall"
          names(fjs_df)[names(fjs_df) == "annualMeanTemperatureInCelsius"] <- "temp"
          
          
          dbWriteTable(con, "fsdata",fjs_df, overwrite=F, append=T)

       ## }
        # clean up
        dbDisconnect(con)

      
        
        
      
        
        
        ## pct profile data
        
        library("OData")
        library(DBI)
        library(datamart)
        library("jsonlite") 
        library(tidyverse)
        library(dplyr)
        
       
        
        pctjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/VegetationClassification_PCTDefinition?$select=PCTID,PCTName,vegetationDescription,classificationConfidenceLevel,numberOfPrimaryReplicates,numberOfSecondaryReplicates,vegetationFormation,vegetationClass,IBRASubregion,maximumElevationInMeters,minimumElevationInMeters,medianElevationInMeters,maximumAnnualRainfallInMillimeters,minimumAnnualRainfallInMillimeters,medianAnnualRainfallInMillimeters,maximumAnnualMeanTemperatureInCelsius,minimumAnnualMeanTemperatureInCelsius,medianAnnualMeanTemperatureInCelsius,TECAssessed,stateTECFitStatus,medianNativeSpeciesRichness")
        
       
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadb.sqlite")
        dbExecute(con,"DELETE FROM pctprofiledata")
        
        
        pctprofiledt_df<-as.data.frame(pctjson$value, stringsAsFactors = F)
       
        names(pctprofiledt_df)[names(pctprofiledt_df) == "vegetationDescription"] <- "Vegetation_description"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "classificationConfidenceLevel"] <- "Classification_confidence_level"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "numberOfPrimaryReplicates"] <- "Number_of_Primary_replicates"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "numberOfSecondaryReplicates"] <- "Number_of_Secondary_replicates"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "vegetationFormation"] <- "Vegetation_Formation"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "vegetationClass"] <- "Vegetation_Class"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "IBRASubregion"] <- "IBRA_Subregion"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "maximumElevationInMeters"] <- "Elevation_max"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "minimumElevationInMeters"] <- "Elevation_min"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "medianElevationInMeters"] <- "Elevation_median"
        
        names(pctprofiledt_df)[names(pctprofiledt_df) == "maximumAnnualRainfallInMillimeters"] <- "Rainfall_max"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "minimumAnnualRainfallInMillimeters"] <- "Rainfall_min"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "medianAnnualRainfallInMillimeters"] <- "Rainfall_median"
        
        names(pctprofiledt_df)[names(pctprofiledt_df) == "maximumAnnualMeanTemperatureInCelsius"] <- "Temperature_max"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "minimumAnnualMeanTemperatureInCelsius"] <- "Temperature_min"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "medianAnnualMeanTemperatureInCelsius"] <- "Temperature_median"
        
        names(pctprofiledt_df)[names(pctprofiledt_df) == "TECAssessed"] <- "TEC_list"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "stateTECFitStatus"] <- "TEC_Act"
        names(pctprofiledt_df)[names(pctprofiledt_df) == "medianNativeSpeciesRichness"] <- "Median_species_richness"
        
        
        dbWriteTable(con, "pctprofiledata",pctprofiledt_df, overwrite=F, append=T)
        
       
        # clean up
        dbDisconnect(con)
        
        
        
        
        ## pct tec data
        
        library("OData")
        library(DBI)
        library(datamart)
        library("jsonlite") 
        library(tidyverse)
        library(dplyr)
        
        
        
        pcttecjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/VegetationClassification_PCTDefinition?$select=PCTID,TECAssessed,stateTECProfileID,stateTECFitStatus,stateTECDegreeOfFit,countryTECProfileID,countryTECFitStatus,countryTECDegreeOfFit")
        
        
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadbtest.sqlite")
        dbExecute(con,"DELETE FROM pct_tecdata")
        
        
        pctTECdt_df<-as.data.frame(pcttecjson$value, stringsAsFactors = F)
        
       
        
        dbWriteTable(con, "pct_tecdata",pctTECdt_df, overwrite=F, append=T)
        
        
        # clean up
        dbDisconnect(con)
        
        
        ## tec data only
        
        library("OData")
        library(DBI)
        library(datamart)
        library("jsonlite") 
        library(tidyverse)
        library(dplyr)
        
        
        
        tecjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/ThreatenedBiodiversity_EcologicalCommunities?$select=profileID,TECName,stateConservation,countryConservation")
        
        
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadbtest.sqlite")
        dbExecute(con,"DELETE FROM tecdata")
        
        
        TECdt_df<-as.data.frame(tecjson$value, stringsAsFactors = F)
        
        
        
        dbWriteTable(con, "tecdata",TECdt_df, overwrite=F, append=T)
        
        
        # clean up
        dbDisconnect(con)
        
        
        
        
        ## PCT SPP GF data only
        
        library("OData")
        library(DBI)
        library(datamart)
        library("jsonlite") 
        library(tidyverse)
        library(dplyr)
        
        
        
        spgfjson <-fromJSON("https://datatest.bionet.nsw.gov.au/BioSvcApp/odata/VegetationClassification_PCTGrowthForm?$select=PCTID,scientificName,medianCoverScore,speciesFrequency,primaryGrowthFormGroup")
        
        
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadbtest.sqlite")
        dbExecute(con,"DELETE FROM pctspeciesgrowthforms")
        
        
        SPGFdt_df<-as.data.frame(spgfjson$value, stringsAsFactors = F)
        
        names(SPGFdt_df)[names(SPGFdt_df) == "PCTID"] <- "PCT_ID"
        names(SPGFdt_df)[names(SPGFdt_df) == "scientificName"] <- "Scientific_name"
        names(SPGFdt_df)[names(SPGFdt_df) == "medianCoverScore"] <- "Group_score_median"
        names(SPGFdt_df)[names(SPGFdt_df) == "speciesFrequency"] <- "Group_frequency"
        names(SPGFdt_df)[names(SPGFdt_df) == "primaryGrowthFormGroup"] <- "GrowthFormGroup"
        
        
        dbWriteTable(con, "pctspeciesgrowthforms",SPGFdt_df, overwrite=F, append=T)
        
        
        # clean up
        dbDisconnect(con)
        
        
        
        
        library("OData")
        library(DBI)
        library(datamart)
        library("jsonlite") 
        library(tidyverse)
        library(dplyr)
        
        
        # Initialize a temporary in memory database and copy a data.frame into it
        con <- dbConnect(RSQLite::SQLite(), dbname="pctdatadbtest.sqlite")
        
        pctid<-37
        
        rs <- dbSendQuery(con, paste0("SELECT B.profileID,B.TECName,'BC Act' as ACT, A.stateTECFitStatus as TECFitStatus FROM PCT_TECData A 
                                        INNER JOIN TECData B on A.stateTECProfileID like '%'||B.profileID||'%'
                                            where A.PCTID='",pctid,"' "))
        dtStateTEC <- dbFetch(rs)
        dbHasCompleted(rs)
        dbClearResult(rs)
        
        
        rs <- dbSendQuery(con, paste0("SELECT B.profileID,B.TECName, 'EPBC Act' as ACT, A.countryTECFitStatus as TECFitStatus FROM PCT_TECData A 
                                         INNER JOIN TECData B on A.countryTECProfileID like '%'||B.profileID||'%'
                                        where A.PCTID='",pctid,"' "))
        dtComTEC <- dbFetch(rs)
        dbHasCompleted(rs)
        dbClearResult(rs)
        
        # clean up
        dbDisconnect(con)
        
        
        n<-0
        n<-nrow(dtStateTEC)
        StateTECName<-""
        
          if (n>0){
            for (i in 1:n){ 
              
                s<-as.data.frame(strsplit(toString(dtStateTEC$TECFitStatus[n]),";"), stringsAsFactors = F)
                StateTECName <- paste0(dtStateTEC$TECName[n]," ", s[[1]][[n]],"<br/>",StateTECName)
            
            }
          }
        
        
        n<-0
        n<-nrow(dtComTEC)
        CommTECName<-""
        
        if (n>0){
          for (i in 1:n){ 
            
            s<-as.data.frame(strsplit(toString(dtComTEC$TECFitStatus[n]),";"), stringsAsFactors = F)
            CommTECName <- paste0(dtComTEC$TECName[n]," ", s[[1]][[n]],"<br/>",CommTECName)
            
          }
        }

 

library(stringi)    
        
        fruits 
          "(Part);(Part);"
        
        dt<-strsplit("(Part);(Part);",";")   
        
        
        s<-as.data.frame(str_split(toString("(Part);(Part);"),";"), stringsAsFactors = F)
        
        s[[1]][[1]]
        [1] "(Part)"
        > s[[1]][[2]]
        [1] "(Part)"
        


        dt<-strsplit("(Part);(Part);",split=';', fixed=TRUE)

