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
require(ncdf4)

# Set parameters
bDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
msk <- "E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/camexca_msk_2_5m_v3.tif"
mask_ctr <- readOGR("E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/CAMEXCA_adm0.shp")
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-all.csv"
cropEquivFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/Ecocorop_Parameters_TOR2.csv"
occDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/ocurrences"
ispamDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/ocurrences/SPAM2010V1r0"
cropgDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/ocurrences/cropgrids"
# threshold <- 0.001 #Very High data
# threshold <- 0.01 #Very High data
# threshold <- 0.05 #High data
# threshold <- 0.1 #Medium data
# threshold <- 0.2 #Low data
thresLs <- c(0.001, 0.005, 0.01, 0.05, 0.1)
oDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/evaluation"

mask <- raster(msk)

# List of simulated crops 
cropPar <- read.csv(cropParamFile, header=T)
parNames <- paste(cropPar$Parameters)
cropLs <- names(cropPar)[2:ncol(cropPar)]
cropEqLs <- read.csv(cropEquivFile, header = T)

oDirShp <- paste0(occDir, "/merged_all")
if(!file.exists(oDirShp)){dir.create(oDirShp, recursive = T)}

for(threshold in thresLs){
  
  oDirThr <- paste0(oDir, "/eval_thr", str_replace(threshold, "[.]", "_"))
  if(!file.exists(oDirThr)){dir.create(oDirThr, recursive = T)}
  
  for(crop in cropLs){
    
    # crop <- cropLs[1]
    cropmod <- strsplit(crop, "_")[[1]][1]
    cropEq <- cropEqLs[which(cropEqLs$Run.ID == crop), ]
    
    if(!file.exists(paste(oDirThr, "/suitocc_", crop, ".tif", sep=""))){
      
      if(!file.exists(paste0(oDirShp, "/", cropmod, "_", str_replace(threshold, "[.]", "_"), "_merge.shp"))){
        
        cat(" >. Evaluating ", crop, "\n")
        
        # Load occurrences gbif 
        if(file.exists(paste0(occDir, "/gbif_", cropmod, ".csv"))){
          
          occ <- read.csv(paste0(occDir, "/gbif_", cropmod, ".csv"), header = T)
          occ <- occ[which(occ$decimalLongitude >= xmin(mask) & occ$decimalLongitude <= xmax(mask) &
                             occ$decimalLatitude >= ymin(mask) & occ$decimalLatitude <= ymax(mask)), ]
          occ <- occ[which(occ$year >= 1960 & occ$year <= 2023), ]
          occ <- occ[which(occ$basisOfRecord != "PRESERVED_SPECIMEN"), ]
          # occ <- SpatialPoints(cbind(occ$decimalLongitude, occ$decimalLatitude), proj4string=CRS(as.character(NA)), bbox = NULL)
          occP <- cbind(occ$decimalLongitude, occ$decimalLatitude)
          # points(occ, cex=0.1)
          cat("   - Gbif ocurrence data loaded \n")
          
        }
        
        # Load monfreda presence area 
        if(cropEq$Monfreda %in% monfredaCrops()$name ) {
          
          mcas <- crop_monfreda(cropEq$Monfreda, var="area_f", path=occDir)
          # monf <- mcas[[3]] #HarvestedAreaFraction
          monf <- mask(crop(raster(mcas), extent(mask)), mask_ctr)
          monf[which(monf[] <= threshold)] = NA
          monfP <- rasterToPoints(monf)
          # points(rasterToPoints(monf), cex=0.1)
          unlink(paste0(occDir, "/monfreda"), recursive = T)
          
          cat("   - Monfreda ocurrence data loaded \n")
          
        }
        
        # Load cropgrid presence area 
        if(file.exists(paste0(cropgDir, "/CROPGRIDSv1.05_", cropEq$Monfreda, ".nc"))){
          
          cropg <- raster(paste0(cropgDir, "/CROPGRIDSv1.05_", cropEq$Monfreda, ".nc"), layer="harvarea")
          cropg <- mask(crop(cropg, extent(mask)), mask_ctr)/8574.76 #to calculate fraction area
          cropg[which(cropg[] <= threshold)] = NA
          cropgP <- rasterToPoints(cropg)
          # points(rasterToPoints(ispam), cex=0.1, col = "blue")
          
          cat("   - Cropgrid ocurrence data loaded \n")
          
        }
        
        # Load ispam presence area (if exits)
        if(file.exists(paste0(ispamDir, "/spam2010V2r0_global_H_", toupper(cropEq$spam_2010), "_R.tif"))){
          
          ispam <- raster(paste0(ispamDir, "/spam2010V2r0_global_H_", toupper(cropEq$spam_2010), "_R.tif"))
          # ispam <- raster(paste0(ispamDir, "/spam2000v3r6_harvested-area_total_", cropmod, ".asc"))
          ispam <- mask(crop(ispam, extent(mask)), mask_ctr)/8574.76 #to calculate fraction area
          ispam[which(ispam[] <= threshold)] = NA
          ispamP <- rasterToPoints(ispam)
          # points(rasterToPoints(ispam), cex=0.1, col = "blue")
          
          cat("   - iSpam ocurrence data loaded \n")
          
        }
        
        # 
        # if(cropmod == "plantain"){
        #   plot(readOGR(paste0(occDir, "/banana_systems.shp"), layer="banana_systems"), lwd=0.5, border="red", add=T)
        # }
        
        
        sPts <- monfP[,1:2]
        if(exists("occP")){
          sPts <- rbind(sPts, occP[,1:2]) 
        }
        sPts <- cropgP[,1:2]
        if(exists("cropgP")){
          sPts <- rbind(sPts, cropgP[,1:2]) 
        }
        if(exists("ispamP")){
          sPts <- rbind(sPts, ispamP[,1:2]) 
        }
        
        sPtsCoord <- cbind(1:nrow(sPts), sPts[ ,1:2])
        rownames(sPts) <- 1:nrow(sPts)
        
        sPts <- SpatialPoints(sPts, proj4string=CRS(as.character(NA)), bbox = NULL)
        
        sPtsSPDF <- SpatialPointsDataFrame(sPts, data.frame(row.names=row.names(sPts), ID=1:length(sPts)))
        writeOGR(obj=sPtsSPDF, dsn=paste0(oDirShp, "/", cropmod, "_", str_replace(threshold, "[.]", "_"), "_merge.shp"), layer="ID", driver="ESRI Shapefile") 
        
      }
      
      sPts <- readOGR(paste0(oDirShp, "/", cropmod, "_", str_replace(threshold, "[.]", "_"), "_merge.shp"))
      crs(sPts) <- crs(mask_ctr)
      
      # Load current suitability 
      suitH <- mask(crop(raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif")), raster(msk)), raster(msk))
      #suitH[which(suitH[] == 0)] <- NA
      
      plot <- setZ(suitH, "suitability")
      names(plot) <- "suitability"
      zvalues <- seq(0, 100, 10)
      myTheme <- BuRdTheme()
      myTheme=rasterTheme(region=brewer.pal('RdYlGn', n=10))  
      myTheme$strip.border$col = "white"
      myTheme$axis.line$col = 'white'
      
      # Save to file
      tiff(paste(oDirThr, "/suitocc_", crop, ".tif", sep=""), width=2200, height=1400, pointsize=8, compression='lzw',res=200)
      
      print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), xlab="", ylab="", margin = FALSE, par.settings = myTheme,
                      colorkey = list(space = "bottom", width=1.2, height=1)
      )
      + layer(sp.points(sPts, cex=0.1, col="1", pch=16), columns=1)
      + layer(sp.polygons(mask_ctr, lwd=0.1, col="gray50"))
      )
      
      dev.off()
      
      cat("   - Plot done \n")
      
    }
    
  }
  
}

