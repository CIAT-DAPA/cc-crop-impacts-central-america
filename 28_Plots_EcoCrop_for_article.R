# Carlos Navarro 
# CIAT - CCAFS
# January 2017

######################################
###### EcoCrop Plots changes  ########
######################################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)
require(stringr)

# Set params
bDir <- "/Users/cenavarro/My Drive (cenavarror@gmail.com)/MSc GIS+/Tesis/03_ecocrop/uncertainties"
oDir <- "/Users/cenavarro/My Drive (cenavarror@gmail.com)/MSc GIS+/Tesis/00_maps"
zDir <- bDir

sspLs <- c("ssp_126", "ssp_245", "ssp_585")
yearLs <- c("2030s", "2050s", "2070s")
# grdLs <- expand.grid(yearLs,sspLs)

# List of simulated crops 
cropParamFile <- "/Users/cenavarro/My Drive (cenavarror@gmail.com)/MSc GIS+/Tesis/03_ecocrop/crop-parameters/crop-parameters-select.csv"
cropPar <- read.csv(cropParamFile, header=T)
cropLs <- names(cropPar)[-1]
cropNameLs <- str_to_title(cropLs)
grdLs <- expand.grid(yearLs,cropLs)


adm <- readOGR( paste0("C:/_tools/AdminBoundaries/Global/10m/10m-admin-0-countries.shp"), layer = paste0("10m-admin-0-countries") )
mask <- readOGR( paste0(zDir, "/Eco-Region del Napo.shp"), layer = paste0("Eco-Region del Napo"))
# ext <- extent(extent(mask)@xmin - buf, extent(mask)@xmax + buf, extent(mask)@ymin - buf, extent(mask)@ymax + buf)
lim <- extent(-80, -71, -11.5, 2.5)

for (ssp in sspLs){
  
  # Load current suitability 
  rsStk <- stack(paste0(bDir, "/", grdLs[,2], "/diffnapo_", ssp, "-", grdLs[,1], ".tif"))
  
  rsStk <- crop(rsStk, lim)
  adm_lim <- crop(adm, lim)
  # rsStk <- mask(crop(rsStk, extent(mask)), mask)
  
  if(!file.exists(paste(oDir, "/fut_suit_change_", ssp, ".tif", sep=""))){
    
    id <- rep("", nlayers(rsStk))
    
    # Plot settings
    plot <- setZ(rsStk, id)
    names(plot) <- id
    zvalues <- c(-100, -30, -1, 1, 30, 100)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("red", "orange", "yellow", "olivedrab3", "darkgreen"))(length(zvalues)-1)
    # myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'gray'
    
    # Plot via levelplot
    # tiff(paste(oDir, "/fut_suit_change_", ssp, "_3periods.tif", sep=""), width=12000, height=2500, pointsize=8, compression='lzw',res=600)
    tiff(paste(oDir, "/fut_suit_change_", ssp, "_2periods.tif", sep=""), width=10000, height=3000, pointsize=8, compression='lzw',res=600)
    
    
    print(levelplot(plot, at = zvalues, layout=c(6, 1), xlab="", ylab="", par.settings = myTheme,  colorkey = FALSE, names.attr=id) 
          + layer(sp.polygons(adm_lim, col= "gray", lwd=0.5))
          + layer(sp.polygons(mask, col= "gray", lwd=0.5)) 
    )
    
    dev.off()
    
    
  }
  
}





#########################################
###### EcoCrop Plots PSuit TSuit ########
#########################################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)

# Set params

bDir <- "D:/OneDrive - CGIAR/CIAT/Articles/mbeltran_crop_exposure/maps"
uDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/uncertainties"
cDir <- "Z:/WORK_PACKAGES/WP2/05_EcoCrop_runs/outputs"
oDir <- bDir
zDir <- bDir

sspLs <- c("ssp26","ssp45","ssp60","ssp85")
ssp <- "ssp85"
yearLs <- c("2020_2049","2040_2069", "2070_2099")
# grdLs <- expand.grid(yearLs,sspLs)
periodLs <- c("Current", "2030s", "2050s", "2080s")
# List of simulated crops 
cropLs <- c("cassava", "maize","plantain")
cropNameLs <- c("Cassava", "Maize", "Plantain")
crop_experiment <- c("cassava", "maize_eitzinger_kai", "plantain_reggata_german")
vrLs <- c("p", "t")
grdLs <- expand.grid(yearLs,crop_experiment, vr)


adm <- readOGR( paste0("C:/_tools/AdminBoundaries/Global/10m/10m-admin-0-countries.shp"), layer = paste0("10m-admin-0-countries") )
mask <- readOGR( paste0(zDir, "/Eco-Region del Napo.shp"), layer = paste0("Eco-Region del Napo"))
# ext <- extent(extent(mask)@xmin - buf, extent(mask)@xmax + buf, extent(mask)@ymin - buf, extent(mask)@ymax + buf)
lim <- extent(-80, -71, -11.5, 2.5)

for (vr in vrLs){
  
  
  # Load current suitability 
  rsStk <- stack(c(paste0(cDir, "/", crop_experiment[1], "/runs/", crop_experiment[1], "_", vr, "suit.tif"), 
                   paste0(uDir, "/mean_", crop_experiment[1], "_", ssp, "_", yearLs, "_", vr, "suit.tif"), 
                   paste0(cDir, "/", crop_experiment[2], "/runs/", crop_experiment[2], "_", vr, "suit.tif"), 
                   paste0(uDir, "/mean_", crop_experiment[2], "_", ssp, "_", yearLs, "_", vr, "suit.tif"), 
                   paste0(cDir, "/", crop_experiment[3], "/runs/", crop_experiment[3], "_", vr, "suit.tif"), 
                   paste0(uDir, "/mean_", crop_experiment[3], "_", ssp, "_", yearLs, "_", vr, "suit.tif")
  )
  )
  
  rsStk <- crop(rsStk, lim)
  adm_lim <- crop(adm, lim)
  rsStk <- mask(crop(rsStk, extent(mask)), mask)
  
  if(!file.exists(paste(oDir, "/p-t_suit_.tif", sep=""))){
    
    id <- c(periodLs, rep("", nlayers(rsStk)-4))
    
    # Plot settings
    plot <- setZ(rsStk, id)
    names(plot) <- id
    # zvalues <- c(-100, -30, -1, 1, 30, 100)
    zvalues <- seq(0, 100, 10)
    myTheme <- BuRdTheme()
    myTheme$regions$col=colorRampPalette(c("red", "orange", "yellow", "olivedrab3", "darkgreen"))(length(zvalues)-1)
    # myTheme$strip.border$col = "white"
    myTheme$axis.line$col = 'gray'
    
    
    
    par.settings=list(layout.heights=list(xlab.key.padding=1))
    
    
    
    # Plot via levelplot
    tiff(paste(oDir, "/", vr, "_suit_.tif", sep=""), width=2200, height=2700, pointsize=300, compression='lzw',res=300)
    
    
    print(levelplot(plot, at = zvalues, layout=c(4, 3), xlab="", 
                    ylab="   Plantain                                            Maize                                           Cassava", 
                    par.settings = myTheme,  
                    colorkey = list(space = "bottom"), names.attr=id, 
                    xlim=c(-80, -71), ylim=c(-11.5, 2.5)
                    ) 
          + layer(sp.polygons(adm_lim, col= "black", lwd=0.5))
          + layer(sp.polygons(mask, col= "black", lwd=0.5)) 
    )
    
    dev.off()
    
  }
  
  
}




######################################
######## EcoCrop Uncertainty  ########
######################################

# Load libraries
require(raster)
require(rgdal)
require(rasterVis)
require(maptools)

# Set params

bDir <- "D:/OneDrive - CGIAR/CIAT/Articles/mbeltran_crop_exposure/maps"
uDir <- "D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/05_EcoCrop_runs/uncertainties"
cDir <- "D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/05_EcoCrop_runs/outputs"
oDir <- bDir
zDir <- bDir


sspLs <- c("ssp26", "ssp45", "ssp85")
ssp <- "ssp85"
period <- "2040_2069"
gcm <- "ensemble"
season <- "ann"
cropLs <- c("cassava", "maize","plantain")
cropNameLs <- c("Cassava", "Maize", "Plantain")
crop_experiment <- c("cassava", "maize_eitzinger_kai", "plantain_reggata_german")
grdLs <- expand.grid(sspLs,crop_experiment)
mask <- readOGR("D:/OneDrive - CGIAR/CIAT/Projects/lat_sal/00_zones/rg_poly_countries.shp", layer= "rg_poly_countries")

fun <- function(x) { sd(x) }

# adm <- readOGR( paste0("C:/_tools/AdminBoundaries/Global/10m/10m-admin-0-countries.shp"), layer = paste0("10m-admin-0-countries") )
# mask <- readOGR( paste0(zDir, "/Eco-Region del Napo.shp"), layer = paste0("Eco-Region del Napo"))
# ext <- extent(extent(mask)@xmin - buf, extent(mask)@xmax + buf, extent(mask)@ymin - buf, extent(mask)@ymax + buf)
# lim <- extent(-80, -71, -11.5, 2.5)

  # Load current suitability 
rsStk <- stack(c(paste0(uDir, "/sd_", crop_experiment[1], "_", ssp, "_", period, ".tif"), 
                 calc(stack(paste0(uDir, "/mean_", crop_experiment[1], "_", sspLs, "_", period, ".tif")), fun), 
                 paste0(uDir, "/sd_", crop_experiment[2], "_", ssp, "_", period, ".tif"), 
                 calc(stack(paste0(uDir, "/mean_", crop_experiment[2], "_", sspLs, "_", period, ".tif")), fun), 
                 paste0(uDir, "/sd_", crop_experiment[3], "_", ssp, "_", period, ".tif"), 
                 calc(stack(paste0(uDir, "/mean_", crop_experiment[3], "_", sspLs, "_", period, ".tif")), fun)
)
)

id <- rep(c("GCM", "ssp"), 3)

# Plot settings
plot <- setZ(rsStk, id)
names(plot) <- id
# zvalues <- c(-100, -30, -1, 1, 30, 100)
zvalues <- seq(0, 35, 5)
myTheme <- BuRdTheme()
myTheme <- rasterTheme(region=brewer.pal('Greys', n=7))
myTheme$axis.line$col = 'gray'

par.settings=list(layout.heights=list(xlab.key.padding=1))

# Plot via levelplot
tiff(paste(oDir, "/crop_std_",period, "_suit_.tif", sep=""), width=3300, height=1200, pointsize=300, compression='lzw',res=300)

print(levelplot(plot, at = zvalues, layout=c(6, 1),
                xlab="", 
                ylab="", 
                names.attr=id, 
                # par.strip.text=list(cex=0),
                par.settings = myTheme, 
                colorkey = list(space = "bottom")
)
+ layer(sp.polygons(mask, col= "gray", lwd=1)) 
)
grid.text(expression("%"), 0.2, 0, hjust=13, vjust=-3.8, gp=gpar(fontsize=12)) 
dev.off()
