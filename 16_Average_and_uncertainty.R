require(rgdal)
require(raster)

#Create stack, then a function to calculate per pixel the average and the probability
#Uncertainty metrics for a given stack (likely a set of previously loaded GCMs)

#Calculate average, standard deviation, average of first 25%, average of last 25%, agreement (n), agreement (%)

#Basic function to calculate eveything per pixel
uncertain <- function(x) {
  if (is.na(x[1])) {
    return(c(NA,NA,NA,NA,NA,NA))
  } else {
    #stippling
    x <- x[which(!is.na(x))]
    if (mean(x) < 0) {
      st <- length(which(x < 0))
    } else if (mean(x) > 0) {
      st <- length(which(x > 0))
    } else  if (mean(x) == 0) {
      st <- length(which(x == 0))
    } else {st <- NA}
    
    #Get 25% quantile
    q25 <- quantile(x,probs=0.25)
    #Average below that value
    bot25 <- mean(x[which(x<=q25)])
    
    #Get 75% quantile
    q75 <- quantile(x,probs=0.75)
    #Average above that value
    top25 <- mean(x[which(x>=q75)])
    
    #Return values
    return(c(mean(x),sd(x),bot25,top25,st,st/length(x)))
  }
}


#Block calculation
#Final grid naming and creation
uncertainties <- function(instack, outFolder="F:/EcoCrop-development/testing", crop, ssp, period) {
  
  if (!file.exists(outFolder)) {dir.create(outFolder, recursive = T)}
  
  #creating rasters from scratch
  rsmean <- raster(gcmstack, 0)
  rssd <- raster(gcmstack, 0)
  rsb25 <- raster(gcmstack, 0)
  rst25 <- raster(gcmstack, 0)
  rsst <- raster(gcmstack, 0)
  rsstp <- raster(gcmstack, 0)
  
  #Looping through chunks of data
  bs <- blockSize(gcmstack, n=41, minblocks=2)
  cat("(", bs$n, " chunks) \n", sep="")
  pb <- pbCreate(bs$n, type='text', style=3)
  for (b in 1:bs$n) {
    #extracting data and running the function
    rowVals <- getValues(gcmstack, row=bs$row[b], nrows=bs$nrow[b])
    rasVals <- apply(rowVals, 1, uncertain)
    #extracting the outcome of the function
    avgVec <- rasVals[1,]
    sdVec <- rasVals[2,]
    b25Vec <- rasVals[3,]
    t25Vec <- rasVals[4,]
    stVec <- rasVals[5,]
    stpVec <- rasVals[6,]
    rm(rasVals)
    #assigning outcome to corresponding cells
    iniCell <- 1+(bs$row[b]-1)*ncol(rsmean)
    finCell <- (bs$row[b]+bs$nrow[b]-1)*ncol(rsmean)
    rsmean[iniCell:finCell] <- avgVec
    rssd[iniCell:finCell] <- sdVec
    rsb25[iniCell:finCell] <- b25Vec
    rst25[iniCell:finCell] <- t25Vec
    rsst[iniCell:finCell] <- stVec
    rsstp[iniCell:finCell] <- stpVec
    pbStep(pb, b)
  }
  pbClose(pb)
  
  #Writing data
  avgName <- paste(outFolder, "/mean_", crop, "_", ssp, "_", period, ".tif", sep=""); rsmean <- writeRaster(rsmean, avgName, overwrite=TRUE)
  sdName <- paste(outFolder, "/sd_", crop, "_", ssp, "_", period, ".tif", sep=""); rssd <- writeRaster(rssd, sdName, overwrite=TRUE)
  b25Name <- paste(outFolder, "/mean-bottom25p_", crop, "_", ssp, "_", period, ".tif", sep=""); rsb25 <- writeRaster(rsb25, b25Name, overwrite=TRUE)
  t25Name <- paste(outFolder, "/mean-top25p_", crop, "_", ssp, "_", period, ".tif", sep=""); rst25 <- writeRaster(rst25, t25Name, overwrite=TRUE)
  stName <- paste(outFolder, "/agreement_", crop, "_", ssp, "_", period, ".tif", sep=""); rsst <- writeRaster(rsst, stName, overwrite=TRUE)
  stpName <- paste(outFolder, "/agreement-percent_", crop, "_", ssp, "_", period, ".tif", sep=""); rsstp <- writeRaster(rsstp, stpName, overwrite=TRUE)
}

# plot(density(x),ylim=c(0,0.41),col="red",lwd=2)
# lines(density(posmod),col="blue",lwd=2); lines(density(negmod), col="black",lwd=2)
# lines(c(mean(x),mean(x)),c(0,1),col="red",lwd=1)
# lines(c(mean(posmod),mean(posmod)),c(0,1),col="blue",lwd=1)
# lines(c(mean(negmod),mean(negmod)),c(0,1),col="black",lwd=1)


#Creating the stack
iDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
oDir <-"E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/uncertainties"
# cropLs <- c("cocoa", "sugar_cane", "sugar_cane_eitzinger", "panela_cane", "coffee", "coffee_eitzinger", "palmito")
sspLs <- c("ssp_126", "ssp_245", "ssp_585")
periodLs <- c("2030s", "2050s", "2070s")
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select.csv"
cropPar <- read.csv(cropParamFile, header=T)
cropLs <- names(cropPar)[-1]

for(crop in cropLs){

  for (ssp in sspLs){
  
  sspDir <- paste0(iDir, "/", crop, "/runs-", ssp)
  gcmList <- list.files(sspDir, full.names = F, include.dirs = F)
  
  for(period in periodLs){
  
    gcmstack <- stack(paste0(sspDir, "/", gcmList, "/", period, "/", crop, "_suit.tif"))
    uncertainties(gcmstack, oDir, crop, ssp, period)
  }
    
  }
}
