# Author: Carlos Navarro
# UNIGIS 2022
# Purpose: Generate plots to evaluate current suitability results comparing these with observed ocurrences

################################
#### Evaluation suitability ####
################################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)
require(geodata)
require(RColorBrewer)
require(sp)
require(stringr)
require(latticeExtra)
require(sf)


# Set parameters
bDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
msk <- "E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/camexca_msk_2_5m_v3.tif"
mask_ctr <- readOGR("E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/CAMEXCA_adm0.shp")
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-all.csv"
cropEquivFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/Ecocorop_Parameters_TOR2.csv"
occDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/ocurrences"
ispamDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/ocurrences/SPAM2010V1r0"
threshold <- 0.001 #Very High data
# threshold <- 0.01 #Very High data
# threshold <- 0.05 #High data
# threshold <- 0.1 #Medium data
# threshold <- 0.2 #Low data
# thresLs <- c(0.001, 0.005, 0.01, 0.05, 0.1)
oDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/evaluation"

mask <- raster(msk)

# List of simulated crops 
cropPar <- read.csv(cropParamFile, header=T)
parNames <- paste(cropPar$Parameters)
cropLs <- names(cropPar)[2:ncol(cropPar)]
cropEqLs <- read.csv(cropEquivFile, header = T)

oDirShp <- paste0(occDir, "/merged")
if(!file.exists(oDirShp)){dir.create(oDirShp, recursive = T)}

oDirMon <- paste0(occDir, "/monfreda_cut")
if(!file.exists(oDirMon)){dir.create(oDirMon, recursive = T)}

oDirGbif <- paste0(occDir, "/gbif_cut")
if(!file.exists(oDirGbif)){dir.create(oDirGbif, recursive = T)}

oDirMapSpam <- paste0(occDir, "/mapspam_cut")
if(!file.exists(oDirMapSpam)){dir.create(oDirMapSpam, recursive = T)}


for(crop in cropLs){
  
  # crop <- cropLs[1]
  cropmod <- strsplit(crop, "_")[[1]][1]
  cropEq <- cropEqLs[which(cropEqLs$Run.ID == crop), ]
  
  cat(" >. Evaluating ", crop, "\n")
  
  # Load occurrences gbif 
  if(!file.exists(paste0(oDirGbif, "/gbif_", cropmod, ".shp"))){
    if(file.exists(paste0(occDir, "/gbif_", cropmod, ".csv"))){
      
      occ <- read.csv(paste0(occDir, "/gbif_", cropmod, ".csv"), header = T)
      occ <- occ[which(occ$decimalLongitude >= xmin(mask) & occ$decimalLongitude <= xmax(mask) &
                         occ$decimalLatitude >= ymin(mask) & occ$decimalLatitude <= ymax(mask)), ]
      occ <- occ[which(occ$year >= 1960 & occ$year <= 2023), ]
      occ <- occ[which(occ$basisOfRecord != "PRESERVED_SPECIMEN"), ]
      # occ <- SpatialPoints(cbind(occ$decimalLongitude, occ$decimalLatitude), proj4string=CRS(as.character(NA)), bbox = NULL)
      occP <- cbind(occ$decimalLongitude, occ$decimalLatitude)
      # points(occ, cex=0.1)
      
      if(nrow(occP) >0){
        occP_exp <- SpatialPoints(occP[,1:2], proj4string=CRS(as.character(NA)), bbox = NULL)
        occP_exp <- SpatialPointsDataFrame(occP_exp, data.frame(row.names=row.names(occP_exp), ID=1:length(occP_exp)))
        writeOGR(obj=occP_exp, dsn=paste0(oDirGbif, "/gbif_", cropmod, ".shp"), layer="ID", driver="ESRI Shapefile") 
      }
      
      cat("   - Gbif ocurrence data loaded \n")
      
    }
  }
  
  # Load monfreda presence area 
  if(!file.exists(paste0(oDirMon, "/monf_", cropmod, ".tif"))){
    
    
    if(cropEq$Monfreda %in% monfredaCrops()$name ) {
      
      mcas <- crop_monfreda(cropEq$Monfreda, var="area_f", path=occDir)
      # monf <- mcas[[3]] #HarvestedAreaFraction
      monf <- mask(crop(raster(mcas), extent(mask)), mask_ctr)
      monf[which(monf[] <= threshold)] = NA
      writeRaster(monf, paste0(oDirMon, "/monf_", cropmod, ".tif"))
      monfP <- rasterToPoints(monf)
      # points(rasterToPoints(monf), cex=0.1)
      unlink(paste0(occDir, "/monfreda"), recursive = T)
      
      cat("   - Monfreda ocurrence data loaded \n")
      
    }
  }
  
  # Load ispam presence area (if exits)
  if(!file.exists(paste0(oDirMapSpam, "/mspam_", cropmod, ".tif"))){
    
    if(file.exists(paste0(ispamDir, "/spam2010V2r0_global_H_", toupper(cropEq$spam_2010), "_R.tif"))){
      
      ispam <- raster(paste0(ispamDir, "/spam2010V2r0_global_H_", toupper(cropEq$spam_2010), "_R.tif"))
      # ispam <- raster(paste0(ispamDir, "/spam2000v3r6_harvested-area_total_", cropmod, ".asc"))
      ispam <- mask(crop(ispam, extent(mask)), mask_ctr)/8574.76 #to calculate fraction area
      ispam[which(ispam[] <= threshold)] = NA
      writeRaster(ispam, paste0(oDirMapSpam, "/mspam_", cropmod, ".tif"))
      ispamP <- rasterToPoints(ispam)
      # points(rasterToPoints(ispam), cex=0.1, col = "blue")
      
      cat("   - iSpam ocurrence data loaded \n")
      
    }
  }
  
  suitH <- mask(crop(raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif")), raster(msk)), raster(msk))
  suitH[which(suitH[] == 0)] = NA
  writeRaster(suitH, paste0(bDir, "/", crop, "/runs/", crop, "_suit_cut.tif"))
  
}

