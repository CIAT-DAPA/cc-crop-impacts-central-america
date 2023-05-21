# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Calculate impact metrics for the study area taking JRV functions

library(raster);library(maptools);library(rgdal);library(sp)

src.dir <- "E:/_scripts/cc-crop-impacts-central-america"
bd <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts" 
iDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
oDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/impacts"
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select.csv"
sspLs <- c("ssp_126", "ssp_245", "ssp_585")
periodLs <- c("2030s", "2050s", "2070s")
shpStudyArea <- "E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/CAMEXCA_adm0_diss.shp"
shpSubLevel <- "E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/CAMEXCA_adm0.shp"
sh <- readShapePoly(shpStudyArea)
reg <- "camexca"
# cropLs <- c("avocado")
# cropPar <- read.csv(cropParamFile, header=T)
# cropLs <- names(cropPar)[-1]

#Source scripts and libraries
stop("no")
source(paste(src.dir,"/src-ecocrop/createMask.R",sep=""))
source(paste(src.dir,"/src-ecocrop/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/src-ecocrop/impacts.R",sep=""))
source(paste(src.dir,"/src-ecocrop/uncertainty.R",sep=""))



cat("Calculate impact metrics for the area of study\n")

for(crop in cropLs){
  
  if (!file.exists(paste(bd, "/impact/impacts-", crop, "/impacts-", reg, ".csv", sep=""))) {
    
  # crop <- cropLs[1]
  impDir <- paste(bd, "/impacts/impacts-", crop, sep="")
  if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
  
  for (ssp in sspLs){
    
    sspDir <- paste0(iDir, "/", crop, "/runs-", ssp)
    gls <- list.files(sspDir, full.names = F, include.dirs = F)
    
    for(period in periodLs){
      
        for (gcm in gls) {
          
          cat("\tImpact StudyArea ", gcm, "\n")
          
          od <- paste(bd, "/impacts/impacts-", crop, "/", gcm, "/", ssp, "_", period, sep="")  
          if (!file.exists(od)) {dir.create(od, recursive = T)}
          
          r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif"))
          r2 <- raster(paste0(sspDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
          pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
          
          # im <- cbind(GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          # 
          # if (gcm == gls[1]) {
          #   res.im <- im
          # } else {
          #   res.im <- rbind(res.im, im)
          # }
          # 
          im <- cbind(ssp=rep(ssp,times=nrow(pp$IMPACT)),PERIOD=rep(period,times=nrow(pp$IMPACT)),GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          
          if (gcm == gls[1] && ssp == sspLs[1] && period == periodLs[1]) {
            res.im <- im
          } else {
            res.im <- rbind(res.im, im)
          }
          
          
          rm(im) #; rm(cl)
        }
        

        
      } 
      
      
    }
    
  write.csv(res.im, paste(bd, "/impacts/impacts-", crop, "/impacts-", reg, ".csv", sep=""), quote=T, row.names=F)
  cat("Calcs impact metrics for the area of study done\n")
  
  } else {cat("Calcs impact metrics for the area of study done!\n")}
  
}





########  Calculate impact metrics for subadministrative levels  ######

library(raster);library(maptools);library(rgdal);library(sp)
sh <- readOGR(shpSubLevel)

#Source scripts and libraries
stop("no")
source(paste(src.dir,"/src-ecocrop/createMask.R",sep=""))
source(paste(src.dir,"/src-ecocrop/futureRuns.tmp.R",sep=""))
source(paste(src.dir,"/src-ecocrop/impacts.R",sep=""))
source(paste(src.dir,"/src-ecocrop/uncertainty.R",sep=""))

cat("Calculate impact metrics for subadministrative levels\n")
for(crop in cropLs){
  
  if (!file.exists(paste(bd, "/impact/impacts-", crop, "/impacts-", reg, "-sub.csv", sep=""))) {
    
    impDir <- paste(bd, "/impact/impacts-", crop, sep="")
    if (!file.exists(impDir)) {dir.create(impDir, recursive = T)}
    
    for (ssp in sspLs){
      
      sspDir <- paste0(iDir, "/", crop, "/runs-", ssp)
      gls <- list.files(sspDir, full.names = F, include.dirs = F)
      
      for(period in periodLs){
       
        for (gcm in gls) {
          
          cat("\tImpact StudyArea ", crop, ssp, gcm, period, "\n")
          
          od <- paste(bd, "/impact/impacts-", crop, "/", gcm, "/", ssp, "_", period, sep="")  
          if (!file.exists(od)) {dir.create(od, recursive=T)}
          
          r1 <- raster(paste0(iDir, "/", crop, "/runs/", crop, "_suit.tif"))
          r2 <- raster(paste0(sspDir, "/", gcm, "/", period, "/", crop, "_suit.tif"))
          pp <- iMetrix(r1,r2,sh,od, chggrid=T, impact=T, classes=T)
          
          im <- cbind(ssp=rep(ssp,times=nrow(pp$IMPACT)),PERIOD=rep(period,times=nrow(pp$IMPACT)),GCM=rep(gcm,times=nrow(pp$IMPACT)), pp$IMPACT)
          
          if (gcm == gls[1] && ssp == sspLs[1] && period == periodLs[1]) {
            res.im <- im
          } else {
            res.im <- rbind(res.im, im)
          }
          rm(im) #; rm(cl)
        }

      }
      
    }
    
    write.csv(res.im, paste(bd, "/impact/impacts-", crop, "/impacts-", reg, "-sub.csv", sep=""), quote=T, row.names=F)
    cat("Calcs impact metrics for subadministrative levels done!", crop, "\n")
    
  } else {cat("Calcs impact metrics for subadministrative levels done!", crop, "\n")}
}

