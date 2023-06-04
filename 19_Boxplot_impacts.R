# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Boxplots for overall suitability change, PIA/NIA ratio, percent change in suitable area (based in JRamirez codes)

# Set libraries
library(lubridate)
library(ggplot2)
library(reshape)
library(Rcmdr)
library(maptools)
source("./src-ecocrop/createMask.R")
source("./src-ecocrop/impacts.R")

# Set parameteres
iDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/impact"
oDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/evaluation/suit_change"
rDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/outputs"
shpSubLevel <- "E:/Tortillas/TORII/EcoCrop_runs/00_admin_data/CAMEXCA_adm0.shp"
shp <- readShapePoly(shpSubLevel)
reg <- "camexca"

cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select_msc.csv"
cropPar <- read.csv(cropParamFile, header=T)
cropLs <- names(cropPar)[-1]
crop_experiment <- str_to_title(cropLs)
cropLs_names <- c("Aguacate", "Banana", "Frijol", "Yuca", "Chile", "Cítricos", "Cacao", "Café", "Maíz", "Plátano", "Papa", "Arroz", "Caña de Azúcar", "Tomate")

if(!file.exists(oDir)){  dir.create(oDir, recursive = T)}

f_names <- list("ssp_126"="SSP1-2.6", "ssp_245"="SSP2-4.5", "ssp_585"="SSP5-8.5")
f_labeller <- function(variable, value){return(f_names[value])}
flabel <- "Average suitablility change (%)"

#function to take countries and make mask and run all the above
suitArea <- function(csr, shp) {
  res <- (csr@extent@xmax - csr@extent@xmin)/(csr@ncols) #Resolution
  #Looping polygons
  nPol <- length(shp@polygons)
  for (p in 1:nPol) {
    cat("Pol", p, "\n")
    cname <- shp@data$COUNTRY[p]
    pol <- shp@polygons[p] #extract single polygon
    sh <- SpatialPolygons(pol) #create SP object from extracted feature
    rs <- createMask(sh, res) #create a raster from the SP object
    xy <- xyFromCell(rs, which(!is.na(rs[]))) #extract xy values from raster cells
    cv <- extract(csr, xy)
    cu <- rs; cu[which(!is.na(cu[]))] <- cv; rm(cv)
    #running impact functions
    as.nz <- areaSuitable(cu, threshold=0) #impact for g0 mask
    as.th <- areaSuitable(cu, threshold=50) #impact for g50 mask
    asm <- c(as.nz, as.th) #merge both matrices
    asm <- cbind(CID=rep(p,times=length(asm)), COUNTRY=rep(cname,times=length(asm)), THRESH=c(0,50), AREA.SUIT=asm)		
    rm(cu); rm(rs); rm(pol); rm(sh); rm(xy); gc()
    #create the matrix
    if (p == 1) {outasm <- asm} else {outasm <- rbind(outasm, asm)}
  }
  return(outasm)
}

crop_stat <- data.frame()

for (i in 1:length(cropLs)){
  
  ## Define output metrics file
  mFile <- read.csv(paste0(iDir, "/impacts-", cropLs[i], "/impacts-", reg, "-sub.csv"))
  mFile <- mFile[which(mFile$THRESHOLD == 50), ]
  mFile <- mFile[which(mFile$SCEN == "no.mig"), ]
  ##mFile <- aggregate (cbind(AV.SUIT.CHG, AV.SUIT.INC, AV.SUIT.DEC)  ~SSP+PERIOD+GCM , FUN = mean, data = mFile) 
  # mFile <- mFile[!(mFile$SSP == "SSP60"), ]
  
  csr <- raster(paste0(rDir, "/", cropLs[i], "/runs/", cropLs[i], "_suit.tif"))
  ca <- suitArea(csr, shp)
  ca <- as.data.frame(ca)
  ca <- ca[which(ca$THRESH==50),]
  
  
  x <- merge(mFile,ca,by="CID")
  
  x$CROP <- cropLs_names[i]
  x$CHG.AREA <- (x$AREA.SUIT.x - x$AREA.SUIT.y)/x$AREA.SUIT.y * 100
  x$PIA_NIA_RATIO <- x$AREA.SUIT.INC/x$AREA.SUIT.DEC
  
  crop_stat <- rbind(crop_stat, x)
}

write.csv(crop_stat, paste(iDir, "/impacts-", reg, "-allcrops.csv", sep=""), row.names=F, quote=T)


# Make the boxplots for three variables: 
crop_stat <- read.csv(paste(iDir, "/impacts-", reg, "-allcrops.csv", sep=""), header = T)
# crop_stat <- crop_stat[which(crop_stat$SSP == "SSP45"), ]

##### Overall suitability changev #####

if (!file.exists(paste0(oDir, "/osc_all_crops.tif"))) {
  
  tiff(paste0(oDir, "/osc_all_crops.tif"), width=4200, height=5400, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=CROP, y=AV.SUIT.CHG, fill=PERIOD)) +
    scale_fill_manual(values=c("white", "gray", "darkgrey")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    geom_boxplot(color="black", outlier.size = 1) +
    geom_hline(aes(yintercept=0), linetype = 2, size=0.5) +
    facet_grid(COUNTRY.x ~ ., drop=T, scales="fixed")+ 
    labs(x="", y="Cambio de Idoneidad (%)") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_blank(), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_text(size = 12, color="black"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_blank(),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_blank(), 
          legend.position="none"
    )
  
  # Plot
  print(f)
  dev.off()
  
}

# PIA/NIA ratio

if (!file.exists(paste0(oDir, "/pia_nia_ratio_all_crops.tif"))) {
  
  tiff(paste0(oDir, "/pia_nia_ratio_all_crops.tif"), width=4200, height=5400, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=CROP, y=PIA_NIA_RATIO, fill=PERIOD)) +
    scale_fill_manual(values=c("white", "gray", "darkgrey")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    geom_boxplot(color="black", outlier.size = 1) +
    geom_hline(aes(yintercept=1), linetype = 2, size=0.3) +
    facet_grid(COUNTRY.x ~ ., drop=T, scales="fixed")+ 
    coord_cartesian(ylim=c(0,1000))+
    scale_y_sqrt() +
    labs(x="", y="AIP/AIN") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_blank(), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_text(size = 12, color="black"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_blank(),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_blank(), 
          legend.position="none"
    ) 
  
  # Plot
  print(f)
  dev.off()
  
}


# Percent change in suitable area
# Calculate percent change in area (future-current)/current*100

if (!file.exists(paste0(oDir, "/chg_area_all_crops.tif"))) {
  
  tiff(paste0(oDir, "/chg_area_all_crops.tif"), width=4200, height=5400, units = "px", pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=CROP, y=CHG.AREA, fill=PERIOD)) +
    scale_fill_manual(values=c("white", "gray", "darkgrey")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    geom_boxplot(color="black", outlier.size = 1) +
    # geom_hline(aes(yintercept=0), linetype = 2, size=0.5) +
    facet_grid(COUNTRY.x ~ ., drop=T, scales="fixed")+ 
    coord_cartesian(ylim=c(0,160))+
    scale_y_sqrt() +
    labs(x="", y="Cambios en las áreas idóneas (%)") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_blank(), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_text(size = 12, color="black"),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_blank(),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_blank(), 
          legend.position="none"
    ) 
  
  # Plot
  print(f)
  dev.off()
  
}
