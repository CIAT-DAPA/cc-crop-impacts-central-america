# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Run EcoCrop for future conditions

# source("D:/_scripts/cc-crop-impacts-central-america/13_Run-EcoCrop-Future.R")
# n <- 1 #crop_id

src.dir <- "E:/_scripts/cc-crop-impacts-central-america"
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select.csv"
cropDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
cDir <- "E:/Tortillas/TORII/EcoCrop_runs/02_climate_change/camexca_2_5min"

sspLs <- c("ssp_126", "ssp_245", "ssp_585")
periodLs <- c("2030s", "2050s", "2070s")
#sowDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/2000_sta_2.asc"
#harDat="//dapadfs/workspace_cluster_3/vulnerability-analysis/EcoCrop-development/Growing_season/end_date.asc"
#crop <- ""

source(paste(src.dir,"/src-ecocrop/EcoCrop-model_WCl.R",sep=""))

#######################################
#######          CURRENT         ######
#######################################

# Reading crop parameters from parameter file
cropPar <- read.csv(cropParamFile, header=T)
parNames <- paste(cropPar$Parameters)
cropNames <- names(cropPar)[2:ncol(cropPar)]
cropPar <- as.data.frame(t(cropPar[,2:ncol(cropPar)]))
row.names(cropPar) <- 1:nrow(cropPar)
names(cropPar) <- parNames
cropPar <- cbind(Crop=cropNames, cropPar)

#number of crops
nTest <- nrow(cropPar) #Number of test into file crop parameters

#loop periods and ssps

for (ssp in sspLs){
  # ssp <- "ssp_126"
  
  gcmList <- list.dirs(paste0(cDir, "/", ssp), recursive = FALSE, full.names = FALSE)
  
  for (period in periodLs){
    
    for (gcm in gcmList){
      
      #loop crops
      for (n in 1:nTest){
        
        testName <- paste(cropPar$Crop[n])  #Name of the tests
        
        #Source scripts and libraries
        #stop("no")
        library(raster);library(maptools);library(rgdal);library(sp)
        
        if (!file.exists(paste(cropDir, "/", testName, "/runs-", ssp, "/", gcm , "/", period, "/", testName, "_suit.tif", sep=""))) {
          
          #Run principal function
          cat("Processing : ",  testName, ssp, gcm, period, "\n")
          
          eco <- suitCalc(climPath=paste0(cDir, "/", ssp, "/", gcm, "/", period), 
                          Gmin=cropPar$Gmin[n], #Minimum lenght of the growing season 
                          Gmax=cropPar$Gmax[n], #Maximum lenght of the growing season
                          Tkmp=cropPar$Tkmp[n], #Killing temperature  
                          Tmin=cropPar$Tmin[n], #Minimum temperature
                          Topmin=cropPar$Topmin[n], #Minimum optimum temperature
                          Topmax=cropPar$Topmax[n], #Maximum optimum temperature
                          Tmax=cropPar$Tmax[n], #Maximum temperature
                          Rmin=cropPar$Rmin[n], #Minimum precipitation
                          Ropmin=cropPar$Ropmin[n], #Minimum optimum precipitation
                          Ropmax=cropPar$Ropmax[n], #Maximum optimum precipitation
                          Rmax=cropPar$Rmax[n], #Maximum precipitation
                          outfolder=paste(cropDir, "/", testName, "/runs-", ssp, "/", gcm, "/", period, sep=""),
                          #sowDat=sowDat,
                          #harDat=harDat,
                          cropname=paste(testName, sep=""),
                          ext=".tif",
                          cropClimate=F
          )
          
        } else {
          cat(paste("Processed : ",  testName, "\n", sep=""))
        }
      }
    }  
    
  }
  
  
}

