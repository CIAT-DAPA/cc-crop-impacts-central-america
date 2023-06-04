# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Plot suitability changes reclassified

###################################################
###### EcoCrop Plots Changes Reclassified  ########
###################################################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)
require(latticeExtra)
require(RColorBrewer)
require(stringr)

# Set params
bDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
iDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/impact"
oDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/evaluation/suit_change"
uDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/uncertainties"
mask <- readOGR("E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/CAMEXCA_adm0.shp")

sspLs <- c("ssp_126", "ssp_245", "ssp_585")
sspLsMod <- c("SSP1-2.6", "SSP2-4.5", "SSP5-8.5")
yearLs <- c("2030s", "2050s", "2070s")
id <- c("2030s SSP1-2.6", "2050s SSP1-2.6", "2070s SSP1-2.6", 
        "2030s SSP2-4.5", "2050s SSP2-4.5", "2070s SSP2-4.5", 
        "2030s SSP5-8.5", "2030s SSP5-8.5", "2030s SSP5-8.5" 
)

# List of simulated crops 
# cropLs <- list.dirs(path = bDir, full.names = F, recursive = F)
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select_msc.csv"
cropPar <- read.csv(cropParamFile, header=T)
cropLs <- names(cropPar)[-1]
cropNameLs <- str_to_title(cropLs)
grdLs <- expand.grid(yearLs,cropLs)
perSSP <- expand.grid(yearLs, sspLs)
cropLsMod <- c("Aguacate", "Banana", "Frijol", "Yuca", "Chile", "Cítricos", "Cacao", "Café", "Maíz", "Plátano", "Papa", "Arroz", "Caña de Azúcar", "Tomate")

## Plot suit changes
for (crop in cropLs){
  
  for (ssp in sspLs){
    
    for (period in yearLs){
      
      cat(crop, ssp, period, "\n")
      
      if(!file.exists(paste0(iDir, "/impacts-", crop, "/suitchg_", ssp, "-", period, ".tif"))){
        
        suitH <- raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif"))
        suitF <- raster(paste0(uDir, "/mean_", crop, "_", ssp, "_", period,".tif", sep=""))
        # 
        #         if (crop == "cassava"){
        #           thr <- 94
        #         } else if (crop == "cotton") {
        #           thr <- 60
        #         } else {
        #           thr <- 50
        #         }
        
        thr <- 50
        
        #Analysis
        outCon1 = ((suitH >= thr) & (suitF  <  thr)) #Areas nolong suitable (RED)
        outCon1[!outCon1]=NA
        outCon1[!is.na(outCon1)]=1
        
        outCon2 = (suitH >= thr) & (suitF >= thr) & ((suitF - suitH) < 0) #Areas suitable but less suitable inthe fut? (ORANGE)
        outCon2[!outCon2]=NA
        outCon2[!is.na(outCon2)]=2
        
        outCon3 = (suitH >= thr) & (suitF >= thr) & ((suitF - suitH) == 0) #Areas suitable and same suitability in the suitF (YELLOW)
        outCon3[!outCon3]=NA
        outCon3[!is.na(outCon3)]=3
        
        outCon4  = ((suitH < thr) & (suitF >= thr)) #New Areas of suitability (LIGHT GREEN)
        outCon4[!outCon4]=NA#Tells R to give all regions except specified region NA
        outCon4[!is.na(outCon4)]=4 #Gives each region a value of 1 
        
        outCon5 = (suitH >= thr) & (suitF >= thr) & ((suitF - suitH) > 0) #Areas Suitable and more suitable in the fut (DARK GREEN)
        outCon5[!outCon5]=NA
        outCon5[!is.na(outCon5)]=5
        
        ###Merge Layers
        
        pieced_fextent = merge(outCon1,outCon2,outCon3,outCon4,outCon5)
        writeRaster(pieced_fextent,paste0(iDir, "/impacts-", crop, "/suitchg_", ssp, "-", period, ".tif"))
        
      }
      
    }
    
  }
  
}


for(crop in cropLs){
  
  if(!file.exists(paste(oDir, "/suitchgrec_", crop, ".tif", sep=""))){
    
    suitChgR <- stack(paste0(iDir, "/impacts-", crop, "/suitchg_", perSSP[,2], "-", perSSP[,1], ".tif"))
    suitChgR <- mask(crop(suitChgR, extent(mask)), mask)

    # Plot settings
    plot <- setZ(suitChgR, id)
    names(plot) <- id
    zvalues <- seq(0, 5, 1) # Define limits
    myTheme <- BuRdTheme() # Define squeme of colors
    myTheme$regions$col=colorRampPalette(c("red", "orange", "yellow", "green", "darkgreen")) # Set new colors
    myTheme$strip.border$col = "white" # Eliminate frame from maps
    myTheme$axis.line$col = 'white' # Eliminate frame from maps
    # myTheme=rasterTheme(region=brewer.pal('Blues', n=9))  
    
    
    # Plot via levelplot
    tiff(paste(oDir, "/suitchgrec_", crop, ".tif", sep=""), width=1200, height=850, pointsize=8, compression='lzw',res=100)
    
    print(levelplot(plot, at = zvalues,  
                    scales = list(draw=FALSE), 
                    names.attr=c(yearLs, rep("", length(id)-3)),
                    layout=c(3, 3), 
                    main="",
                    xlab="",
                    ylab=list(paste(rev(c(sspLsMod)), sep=""),side=1,line=0.5, cex=1),
                    # par.strip.text=list(cex=0),
                    labels=as.character(c("Areas no longer suitable", "Areas less suitable in the suitF", "Same suitability in the suitF", "New Areas of suitability", "Areas more suitable in the suitF")),
                    par.settings = myTheme, 
                    colorkey = list(space = "bottom", width=1.2, height=1)
    )
    + layer(sp.polygons(mask, lwd=0.8))
    )
    
    dev.off()
    
  }
  
}
