getwd()
setwd("C:/Users/grace/GIS/povertyequity")

install.packages("Rcpp")
install.packages("raster")
install.packages("rgdal")
library(raster)
library(sp)
library(rgdal)
library(sf)
library(dplyr)

wd <- getwd()
landcover <- raster(paste0(wd, "/UMD_senegambia_crop.tif"))

lc_pt <- rasterToPoints(landcover, fun=NULL, spatial=FALSE)
lc_pt_df <- as.data.frame(lc_pt)


lc_pt_df$index <- 1:nrow(lc_pt_df)
lc_pt_df <- rename(lc_pt_df, ID_lc = index)

write.csv(lc_pt_df, file = "lc_pt.csv")
