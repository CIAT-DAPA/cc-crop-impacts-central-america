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
library(RColorBrewer)
require(sp)


# Set parameters
bDir <- "D:/cenavarro/msc_gis_thesis/03_crop_impacts/outputs"
oDir <- "D:/cenavarro/msc_gis_thesis/03_crop_impacts/evaluation/low_ocurrence"
msk <- "D:/cenavarro/msc_gis_thesis/00_admin_data/camexca_msk_2_5m_v2.tif"
mask_ctr <- readOGR("D:/cenavarro/msc_gis_thesis/00_admin_data/CAMEXCA_adm0.shp")
cropParamFile <- "D:/cenavarro/msc_gis_thesis/03_crop_impacts/crop-parameters/crop-parameters-int.csv"
occDir <- "D:/cenavarro/msc_gis_thesis/03_crop_impacts/ocurrences"
ispamDir <- "R:/cropdata/ispam/iSPAM_V3R6/SPAM_harv-area"
# threshold <- 0.1 #Medium data
threshold <- 0.05 #High data
# threshold <- 0.2 #Low data


if(!file.exists(oDir)){dir.create(oDir, recursive = T)}

mask <- raster(msk)

# List of simulated crops 
cropPar <- read.csv(cropParamFile, header=T)
parNames <- paste(cropPar$Parameters)
cropLs <- names(cropPar)[2:ncol(cropPar)]

for(crop in cropLs){
  
  cropmod <- strsplit(crop, "_")[[1]][1]
  if(!file.exists(paste0(occDir, "/", cropmod, "_merge.shp"))){
  cat(" >. Evaluating ", crop, "\n")
  
  # Load current suitability 
  suitH <- raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif"))
  # suitH[which(suitH[] == 0)] <- NA
  
  # Plot with monfreda
  # tiff(paste(oDir, "/suitocc_", crop, ".tif", sep=""), width=2400, height=1200, pointsize=8, compression='lzw',res=200)

  # breaks <- c(seq(0, 100, 10))
  # color <- colorRampPalette(RColorBrewer::brewer.pal(10, "RdYlGn"))(11) 
  # plot(suitH, breaks = breaks, col = color)
  
  # plot(monf, add=T, alpha=0.70, legend= F)
  
  # Load occurrences gbif 
  if(file.exists(paste0(occDir, "/gbif_", cropmod, ".csv"))){
    
    occ <- read.csv(paste0(occDir, "/gbif_", cropmod, ".csv"), header = T)
    occ <- occ[which(occ$decimalLongitude >= xmin(mask) & occ$decimalLongitude <= xmax(mask) &
                       occ$decimalLatitude >= ymin(mask) & occ$decimalLatitude <= ymax(mask)), ]
    occ <- occ[which(occ$year >= 1970 & occ$year <= 2000), ]
    # occ <- SpatialPoints(cbind(occ$decimalLongitude, occ$decimalLatitude), proj4string=CRS(as.character(NA)), bbox = NULL)
    occP <- cbind(occ$decimalLongitude, occ$decimalLatitude)
    # points(occ, cex=0.1)
    cat("   - Gbif ocurrence data loaded \n")
    
  }
  
  # Load monfreda presence area 
  if(cropmod %in% monfredaCrops()$name ) {
    
    mcas <- crop_monfreda(cropmod, path=occDir)
    monf <- mcas[[3]] #HarvestedAreaFraction
    monf <- mask(crop(raster(monf), extent(mask)), mask_ctr)
    monf[which(monf[] <= threshold)] = NA
    monfP <- rasterToPoints(monf)
    # points(rasterToPoints(monf), cex=0.1)
    
    unlink(paste0(occDir, "/monfreda"), recursive = T)
    
    cat("   - Monfreda ocurrence data loaded \n")

  }
  
  # Load ispam presence area (if exits)
  if(file.exists(paste0(ispamDir, "/spam2000v3r6_harvested-area_total_", cropmod, ".asc"))){
    
    ispam <- raster("R:/cropdata/ispam/iSPAM_V3R6/SPAM_physical-area/Spam_all/maize_SPAM_all.tif")
    ispam <- raster(paste0(ispamDir, "/spam2000v3r6_harvested-area_total_", cropmod, ".asc"))
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
  
  plot <- setZ(suitH, "suitability")
  names(plot) <- "suitability"
  zvalues <- seq(0, 100, 10)
  myTheme <- BuRdTheme()
  myTheme=rasterTheme(region=brewer.pal('RdYlGn', n=10))  
  myTheme$strip.border$col = "white"
  myTheme$axis.line$col = 'white'
  
  sPts <- monfP[,1:2]
  if(exists("occP")){
    sPts <- rbind(sPts, occP[,1:2]) 
  }
  if(exists("ispamP")){
    sPts <- rbind(sPts, ispamP[,1:2]) 
  }
  
  sPtsCoord <- cbind(1:nrow(sPts), sPts[ ,1:2])
  rownames(sPts) <- 1:nrow(sPts)
  
  sPts <- SpatialPoints(sPts, proj4string=CRS(as.character(NA)), bbox = NULL)
  

    sPtsSPDF <- SpatialPointsDataFrame(sPts, data.frame(row.names=row.names(sPts), ID=1:length(sPts)))
    writeOGR(obj=sPtsSPDF, dsn=paste0(occDir, "/", cropmod, "_merge.shp"), layer="ID", driver="ESRI Shapefile")    
  }
  
  crs(sPts) <- crs(mask_ctr)
  # sPts <- over(sPts, mask_ctr, returnList = TRUE)
  
  # # Save to file
  # tiff(paste(oDir, "/suitocc_", crop, ".tif", sep=""), width=2400, height=1200, pointsize=8, compression='lzw',res=200)
  # 
  # print(levelplot(plot, at = zvalues, scales = list(draw=FALSE), xlab="", ylab="", margin = FALSE, par.settings = myTheme, 
  #                 colorkey = list(space = "bottom", width=1.2, height=1)
  # )
  # + layer(sp.points(sPts, cex=0.1, col="1", pch=16), columns=1) 
  # + layer(sp.polygons(mask_ctr, lwd=0.2, col="gray50"))
  # )
  # 
  # dev.off()
  
  cat("   - Plot done \n")
  
  # plot(mask_ctr, add=T, lwd=0.2)
  # dev.off()
    
# } 
  
}
