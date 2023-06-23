# Author: Carlos Navarro
# UNIGIS 2023
# Purpose: Scatterplots for overall suitability change, PIA/NIA ratio, percent change in suitable area and uncertainties

# Set libraries
library(lubridate)
library(ggplot2)
library(reshape)
library(maptools)
library(stringr)

# Set parameteres
iDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/impact"
oDir <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/evaluation/suit_change"
reg <- "camexca"

cropParamFile <- "E:/Tortillas/TORII/EcoCrop_runs/03_crop_impacts/crop-parameters/crop-parameters-select_msc.csv"
cropPar <- read.csv(cropParamFile, header=T)
cropLs <- names(cropPar)[-1]
crop_experiment <- str_to_title(cropLs)
cropLs_names <- c("Aguacate", "Banana", "Frijol", "Yuca", "Chile", "Cítricos", "Cacao", "Café", "Maíz", "Plátano", "Papa", "Arroz", "Caña de Azúcar", "Tomate")

f_names <- list("ssp_126"="SSP1-2.6", "ssp_245"="SSP2-4.5", "ssp_585"="SSP5-8.5")
f_labeller <- function(variable, value){return(f_names[value])}
flabel <- "Average suitablility change (%)"

# Make the boxplots for three variables: 
crop_stat <- read.csv(paste(iDir, "/impacts-", reg, "-allcrops-stats_by_period_ssp.csv", sep=""), header = T)
# crop_stat <- crop_stat[which(crop_stat$SSP == "SSP45"), ]

##### Overall suitability changev #####

if (!file.exists(paste0(oDir, "/osc_all_crops_scatterplot.tif"))) {
  
  tiff(paste0(oDir, "/osc_all_crops_scatterplot.tif"), width=3000, height=4500, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=Average.of.AV.SUIT.CHG, y=StdDev.of.AV.SUIT.CHG, shape=COUNTRYMOD, color=ssp)) +
    geom_point(size = 2.5) +
    scale_shape_manual(values=seq(0,15)) +
    scale_colour_grey() +
    scale_y_sqrt() +
    # scale_fill_manual(values=c("white", "gray", "darkgrey")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    theme(legend.position = 'bottom', legend.direction = "horizontal") + 
    # geom_boxplot(color="black", outlier.size = 1) +
    geom_vline(aes(xintercept=0), linetype = 2, size=0.3) +
    # geom_hline(aes(yintercept=10), linetype = 2, size=0.3) +
    facet_wrap(~ CROP, ncol = 3, scales="fixed")+ 
    labs(x="Cambio de idoneidad promedio (%)", y="Cambio de idoneidad desviación estándar (%)") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_text(size = 12, color="black"), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_text(size = 12),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_text(size = 12, color="black"),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_text(size = 12, color="black") 
    )
  
  # Plot
  print(f)
  dev.off()
  
}

# PIA/NIA ratio

if (!file.exists(paste0(oDir, "/pia_nia_ratio_all_crops_scatterplot.tif"))) {
  
  
  tiff(paste0(oDir, "/pia_nia_ratio_all_crops_scatterplot.tif"), width=3000, height=4500, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=Average.of.PIA_NIA_RATIO, y=StdDev.of.PIA_NIA_RATIO, shape=COUNTRYMOD, color=ssp)) +
    geom_point(size = 2.5) +
    scale_shape_manual(values=seq(0,15)) +
    scale_colour_grey() +
    coord_cartesian(ylim=c(0,100), xlim=c(0,100))+
    scale_y_sqrt() +
    scale_x_sqrt() +
    # scale_fill_manual(values=c("white", "gray", "darkgrey")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    theme(legend.position = 'bottom', legend.direction = "horizontal") + 
    # geom_boxplot(color="black", outlier.size = 1) +
    geom_vline(aes(xintercept=1), linetype = 2, size=0.3) +
    # geom_hline(aes(yintercept=10), linetype = 2, size=0.3) +
    facet_wrap(~ CROP, ncol = 3, scales="free_y")+ 
    labs(x="AIP/AIN promedio", y="AIP/AIN desviación estándar") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_text(size = 12, color="black"), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_text(size = 12),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_text(size = 12, color="black"),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_text(size = 12, color="black") 
    )
  
  # Plot
  print(f)
  dev.off()
  
}


# Percent change in suitable area
# Calculate percent change in area (future-current)/current*100

if (!file.exists(paste0(oDir, "/chg_area_all_crops_scatterplot.tif"))) {
  
  tiff(paste0(oDir, "/chg_area_all_crops_scatterplot.tif"), width=3000, height=4500, pointsize=20, compression='lzw',res=300)
  
  f <- ggplot(data=crop_stat, aes(x=Average.of.CHG.AREA, y=StdDev.of.CHG.AREA, shape=COUNTRYMOD, color=ssp)) +
    geom_point(size = 2.5) +
    scale_shape_manual(values=seq(0,15)) +
    scale_colour_grey() +
    coord_cartesian(ylim=c(0,40), xlim=c(0,100))+
    scale_y_sqrt() +
    scale_x_sqrt() +
    # scale_fill_manual(values=c("white", "gray", "darkgrey")) +
    # scale_x_discrete(labels= c("2030", "2050", "2080")) +
    # ggtitle(paste0(cropLs_names[i])) +  
    theme_bw() + 
    theme(legend.position = 'bottom', legend.direction = "horizontal") + 
    # geom_boxplot(color="black", outlier.size = 1) +
    geom_vline(aes(xintercept=0), linetype = 2, size=0.3) +
    geom_hline(aes(yintercept=10), linetype = 2, size=0.3) +
    facet_wrap(~ CROP, ncol = 3, scales="free_y")+ 
    labs(x="Cambio en las áreas idóneas (%)", y="Cambio en las áreas idóneas desviación estándar (%)") +
    theme(axis.ticks=element_line(color = "black"), 
          legend.title=element_text(size = 12, color="black"), 
          axis.title.y = element_text(size = 12, color="black"), 
          plot.title = element_text(size = 30), 
          strip.text.y = element_text(size = 12),
          strip.background = element_blank(),
          panel.border = element_rect(color = "black", fill = NA, size = 0.5),
          strip.text.x = element_text(size = 12, color="black"),
          axis.text=element_text(size=12, color="black"),
          axis.title.x=element_text(size = 12, color="black") 
    )
  
  # Plot
  print(f)
  dev.off()
  
}
