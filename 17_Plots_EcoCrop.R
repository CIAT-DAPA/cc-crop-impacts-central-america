# Carlos Navarro 
# CIAT - CCAFS
# January 2017

##############################
###### EcoCrop Plots  ########
##############################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)
require(latticeExtra)
require(RColorBrewer)

# Set params
bDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
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
# List of simulated crops 
cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select_msc.csv"
cropPar <- read.csv(cropParamFile, header=T)
cropLs <- names(cropPar)[-1]
cropNameLs <- str_to_title(cropLs)
grdLs <- expand.grid(yearLs,cropLs)
perSSP <- expand.grid(yearLs, sspLs)
cropLsMod <- c("Aguacate", "Banana", "Frijol", "Yuca", "Chile", "Citricos", "Cacao", "Cafe", "Maiz", "Platano", "Papa", "Arroz", "Yuca", "Tomate")

for(crop in cropLs){
  
  suitH <- raster(paste0(bDir, "/", crop, "/runs/", crop, "_suit.tif"))
  
  if(!file.exists(paste(oDir, "/suitchg_", crop, ".tif", sep=""))){
    
    # Load future suitability in stack (all years by ssp)
    suitF <- stack(paste0(uDir, "/mean_", crop, "_", perSSP[,2], "_", perSSP[,1], ".tif"))
    
    # Change calculation
    suitC <- suitF - suitH
    suitC_crop <- mask(crop(suitC, extent(mask)), mask)
    
    # Set limits
    suitC_crop[which(suitC_crop[]< (-40))] = (-40)
    suitC_crop[which(suitC_crop[]> 40) ] = 40
    
    # Plot settings
    plot <- setZ(suitC_crop, id)
    names(plot) <- id
    zvalues <- seq(-40, 40, 5)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("darkred", "red", "orange", "snow", "yellowgreen", "forestgreen", "darkolivegreen"))(length(zvalues)-1)
    myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'white'
    
    # Plot via levelplot
    tiff(paste(oDir, "/suitchg_", crop, ".tif", sep=""), width=1200, height=850, pointsize=8, compression='lzw',res=100)

    print(levelplot(plot, at = zvalues,  
                    scales = list(draw=FALSE), 
                    names.attr=c(yearLs, rep("", length(id)-3)),
                    layout=c(3, 3), 
                    main="",
                    xlab="",
                    ylab=list(paste(c(sspLsMod), sep=""),side=1,line=0.5, cex=1),
                    # par.strip.text=list(cex=0),
                    par.settings = myTheme, 
                    colorkey = list(space = "bottom", width=1.2, height=1)
    )
    + layer(sp.polygons(mask, lwd=0.8))
    )
    
    dev.off()
    
  }
  
}




# Plot uncertainty
stats <- c("mean", "mean-bottom25p", "mean-top25p", "agreement", "sd")

if(!file.exists(oDir)){
  dir.create(oDir, recursive = T)
}

for (ssp in sspLs){

  for (period in periodLs){

    for(stati in stats){

      if (stati == "agreement"){
        zvalues <- seq(0, 16, 1) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("yellow", "orange", "blue")) # Set new colors

      } else if (stati == "sd"){
        zvalues <- seq(0, 50, 5) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("white", "black")) # Set new colors

      } else {

        zvalues <- seq(0, 100, 10) # Define limits
        myTheme <- BuRdTheme() # Define squeme of colors
        myTheme$regions$col=colorRampPalette(c("red", "white", "darkgreen")) # Set new colors
      }

      stk_crop <- stack(paste0(uDir, "/", stati, "_", cropLs, "_", ssp, "_", period, ".tif"))
      stk_crop <- mask(crop(stk_crop, extent(mask)), mask)
      
      plot <- setZ(stk_crop, cropLsMod)
      names(plot) <- cropLsMod

      myTheme$strip.border$col = "white" # Eliminate frame from maps
      myTheme$axis.line$col = 'white' # Eliminate frame from maps

      tiff(paste(oDir, "/", stati, "-", ssp, "-", period, ".tif", sep=""), width=1000, height=1200, pointsize=8, compression='lzw',res=100)
      print(levelplot(plot, 
                      at = zvalues, 
                      scales = list(draw=FALSE),  
                      xlab="", 
                      ylab="", par.settings = myTheme, 
                      colorkey = list(space = "bottom"), 
                      # main=paste0(stati, " ", ssp, " ", period)
                      ) 
            + layer(sp.polygons(mask))
            )
      dev.off()

      cat(stati, ssp, period, "\n")

    }
  }
}



