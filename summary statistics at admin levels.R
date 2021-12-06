# ------------- PREPARE WORKSPACE -------------
setwd("C:/Users/grace/GIS/povertyequity")
wd <- getwd()
library(ursa)
library(raster)
library(sp)
library(rgdal)
library(sf)
library(dplyr)
library(rgeos)



# ------------- EXTRACT AND CLEAN DATA ----------------
admin <- readOGR(paste0(wd, '/admin/sen_admbnda_adm2_1m_gov_ocha_20190426.shp'))
admin <- st_as_sf(admin)

traveltimes <- st_read('LC_value/LC_value_mmF10.shp') 
LC90m <- raster(paste0(wd, "/LC_90m.tif"))
crs(LC90m)
travel_raster <- rasterize(traveltimes, LC90m, field='HD1mm', filename='HD1mm.tif')

pop <- read.csv('sen_admpop_adm2_2020.csv')



# ------------- POPULATION TO ADMIN LEVEL 2 ----------------
pop <- merge(x=admin, y=pop, by="ADM2_PCODE")
pop <- pop[ , c(1,3:4,8:9,23)]



# ------------- JOIN: ADMIN 2 TO AGRICULTURAL AREAS ----------------
st_crs(pop)
st_crs(traveltimes)
traveltimes <- st_transform(traveltimes, crs = st_crs(pop))
pop <- pop %>% st_drop_geometry()
class(pop)
traveltimes <- merge(x=traveltimes, y=pop, by="ADM2_PCODE")


# ------------- AVERAGES ----------------
# Multi-modal travel under baseline weather conditions
HD1mm_avg <- NA
HD1mm_avg <- aggregate(traveltimes$HD1mm, list(traveltimes$ADM2_PCODE), FUN=mean, na.rm=TRUE, na.action=NULL)
HD1mm_avg <- rename(HD1mm_avg, c("ADM2_PCODE"="Group.1", "HD1avg"="x"))

# Multi-modal travel under 1-in-10 year flood conditions
HD1F10mm_avg <- NA
HD1F10mm_avg <- aggregate(traveltimes$HD1F10mm, list(traveltimes$ADM2_PCODE), FUN=mean, na.rm=TRUE, na.action=NULL)
HD1F10mm_avg <- rename(HD1F10mm_avg, c("ADM2_PCODE"="Group.1", "HD1F10avg"="x"))

# Change in travel time from baseline to 1-in-10 year flood, in minutes
HD1dif_avg <- NA
HD1dif_avg <- aggregate(traveltimes$HD1dif, list(traveltimes$ADM2_PCODE), FUN=mean, na.rm=TRUE, na.action=NULL)
HD1dif_avg <- rename(HD1dif_avg, c("ADM2_PCODE"="Group.1", "HD1difavg"="x"))

# Change in travel time from baseline to 1-in-10 year flood, in percent
HD1pc_avg <- NA
HD1pc_avg <- aggregate(traveltimes$HD1pc, list(traveltimes$ADM2_PCODE), FUN=mean, na.rm=TRUE, na.action=NULL)
HD1pc_avg <- rename(HD1pc_avg, c("ADM2_PCODE"="Group.1", "HD1pcavg"="x"))


# ------------- MEDIANS ----------------
# Multi-modal travel under baseline weather conditions
HD1mm_med <- NA
HD1mm_med <- aggregate(traveltimes$HD1mm, list(traveltimes$ADM2_PCODE), FUN=median, na.rm=TRUE, na.action=NULL)
HD1mm_med <- rename(HD1mm_med, c("ADM2_PCODE"="Group.1", "HD1med"="x"))

# Multi-modal travel under 1-in-10 year flood conditions
HD1F10mm_med <- NA
HD1F10mm_med <- aggregate(traveltimes$HD1F10mm, list(traveltimes$ADM2_PCODE), FUN=median, na.rm=TRUE, na.action=NULL)
HD1F10mm_med <- rename(HD1F10mm_med, c("ADM2_PCODE"="Group.1", "HD1F10med"="x"))

# Change in travel time from baseline to 1-in-10 year flood, in minutes
HD1dif_med <- NA
HD1dif_med <- aggregate(traveltimes$HD1dif, list(traveltimes$ADM2_PCODE), FUN=median, na.rm=TRUE, na.action=NULL)
HD1dif_med <- rename(HD1dif_med, c("ADM2_PCODE"="Group.1", "HD1difmed"="x"))

# Change in travel time from baseline to 1-in-10 year flood, in percent
HD1pc_med <- NA
HD1pc_med <- aggregate(traveltimes$HD1pc, list(traveltimes$ADM2_PCODE), FUN=median, na.rm=TRUE, na.action=NULL)
HD1pc_med <- rename(HD1pc_med, c("ADM2_PCODE"="Group.1", "HD1pcmed"="x"))



# ------------- MERGE AND WRITE TO FILE ----------------

HD1_ADM2 = Reduce(function(x, y) 
  merge(x, y, by="ADM2_PCODE", all=TRUE), 
  list(HD1mm_avg, HD1mm_med, HD1F10mm_avg, HD1F10mm_med, HD1dif_avg, HD1dif_med, HD1pc_avg, HD1pc_med))
pop <- pop[ , c('ADM2_PCODE', 'Total')]
pop <- rename(pop, c("pop2020"="Total"))
HD1_ADM2 <- merge(x=HD1_ADM2, y=pop, by="ADM2_PCODE")
write.csv(HD1_ADM2, file = "HD1_ADM2.csv")

# As shapefile
admin <- readOGR(paste0(wd, '/admin/sen_admbnda_adm2_1m_gov_ocha_20190426.shp'))
admin <- st_as_sf(admin)
admin <- admin[ , c('ADM2_PCODE')]
HD1_ADM2 <- merge(x=admin, y=HD1_ADM2, by="ADM2_PCODE")
st_write(HD1_ADM2, "HD1_ADM2.shp", driver = "ESRI Shapefile")
