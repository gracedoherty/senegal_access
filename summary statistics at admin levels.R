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
library(naniar) # replace_with_na()



# ------------- PREPARE RESPECTIVE POPULATION FILES ----------------
admin <- readOGR(paste0(wd, '/admin/sen_admbnda_adm2_1m_gov_ocha_20190426.shp')) # Admin 2 level polygons ("departments"). From OCHA via HDX.
admin <- st_as_sf(admin)
census_pop <- read.csv('sen_admpop_adm2_2020.csv') # Population of admin 2, 2020 projection. From census (RGPHAE 2013) via HDX.
hamlet_pop <- st_read('hamlet_extents_pop/hamlet_extents_pop.shp') # Population of each origin. Count from WorldPop (2020 UN-adjusted, constrained). Extents from GRID3 (hamlet).

# Add the ADM2 unique ID to the census-derived department population.
admin <- admin[ , c('ADM2_PCODE')] # Only need the unique ID of each department and their geometry.
census_pop <- merge(x=admin, y=census_pop, by="ADM2_PCODE")
st_write(census_pop, "pop_ADM2.shp", driver = "ESRI Shapefile")
# Note: must reload. Transformed pop to UTM in QGIS because join was having trouble with transform here.
census_pop <- readOGR(paste0(wd, '/pop_ADM2_UTM.shp'))
census_pop <- st_as_sf(census_pop)

# Clean up hamlet population dataframe. Only need the population and the hamlet unique ID.
hamlet_pop <- hamlet_pop[ , c('mgrs_code', 'SUM')]
hamlet_pop <- rename(hamlet_pop, c("ha_pop"="SUM"))



# ------------- PREPARE TRAVEL DATA ----------------
# Agriculture version
traveltimes <- read.csv('ag_to_HDurban_pre-F10-20-50.csv') 
traveltimes <- st_as_sf(traveltimes, coords=c("X","Y"), crs=4326)

# Hamlet version
traveltimes <- read.csv('hamlet_to_HDurban_pre-F10-20-50.csv') 
traveltimes <- st_as_sf(traveltimes, coords=c("X","Y"), crs=4326)

hamlet_pop <- hamlet_pop %>% st_drop_geometry()
class(hamlet_pop)
traveltimes <- merge(x=traveltimes, y=hamlet_pop, by='mgrs_code', all.x=TRUE)

# Travel times and hamlet populations weren't 1 to 1 earlier. Now resolved. Saving this validation code block for reference.
length(unique(traveltimes$mgrs_code)) # Number of unique values within the unique ID column
length(unique(hamlet_pop$mgrs_code))
length(which(table(traveltimes$mgrs_code)>1)) # Number of duplicates within the unique ID column
length(which(table(hamlet_pop$mgrs_code)>1))

# Join adm2 population to travel times
st_crs(census_pop)
st_crs(traveltimes)
traveltimes <- st_transform(traveltimes, crs = st_crs(census_pop))
traveltimes <- st_join(traveltimes, census_pop)



# ------------- FIND ISOLATED ORIGINS AND REMOVE FROM SUMMARY STATISTICS ----------------
# We'll retain the sum values for summary stats.
traveltimes$pre<- replace(traveltimes$pre,traveltimes$pre>240,NA)
traveltimes$F10 <- replace(traveltimes$F10,traveltimes$F10>240,NA)
traveltimes$F20 <- replace(traveltimes$F20,traveltimes$F20>240,NA)
traveltimes$F50 <- replace(traveltimes$F50,traveltimes$F50>240,NA)


sum(is.na(traveltimes$pre))
max(traveltimes$pre, na.rm=TRUE)
min(traveltimes$pre, na.rm=TRUE)
sum(is.na(traveltimes$F10))
max(traveltimes$F10, na.rm=TRUE)
min(traveltimes$F10, na.rm=TRUE)
sum(is.na(traveltimes$F20))
max(traveltimes$F20, na.rm=TRUE)
min(traveltimes$F20, na.rm=TRUE)
sum(is.na(traveltimes$F50))
max(traveltimes$F50, na.rm=TRUE)
min(traveltimes$F50, na.rm=TRUE)


# Save intermediate file
write.csv(traveltimes, file = "hamlet_to_HDurban_pre-F10-20-50.csv")
write.csv(traveltimes, file = "ag_to_HDurban_pre-F10-20-50.csv")



# ------------- ISOLATION BY POPULATION SIZE ----------------
base_isolated <- subset(traveltimes, is.na(pre))
F10_isolated <- subset(traveltimes, is.na(F10))
F20_isolated <- subset(traveltimes, is.na(F20))
F50_isolated <- subset(traveltimes, is.na(F50))

# Total population isolated from markets under baseline conditions, by ADMIN2
base_iso <- 0
# base_iso <- aggregate(base_isolated$ha_pop, list(base_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
base_isolated["Count"] <- 1
base_iso <- aggregate(base_isolated$Count, list(base_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
base_iso <- rename(base_iso, c("ADM2_PCODE"="Group.1", "preiso"="x"))

# Total population isolated from markets under 1-in-10 year flood conditions, by ADMIN2
F10_iso <- 0
# F10_iso <- aggregate(F10_isolated$ha_pop, list(F10_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F10_isolated["Count"] <- 1
F10_iso <- aggregate(F10_isolated$Count, list(F10_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F10_iso <- rename(F10_iso, c("ADM2_PCODE"="Group.1", "F10iso"="x"))

# Total population isolated from markets under 1-in-20 year flood conditions, by ADMIN2
F20_iso <- 0
# F20_iso <- aggregate(F20_isolated$ha_pop, list(F20_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F20_isolated["Count"] <- 1
F20_iso <- aggregate(F20_isolated$Count, list(F20_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F20_iso <- rename(F20_iso, c("ADM2_PCODE"="Group.1", "F20iso"="x"))

# Total population isolated from markets under 1-in-50 year flood conditions, by ADMIN2
F50_iso <- 0
# F50_iso <- aggregate(F50_isolated$ha_pop, list(F50_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F50_isolated["Count"] <- 1
F50_iso <- aggregate(F50_isolated$Count, list(F50_isolated$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F50_iso <- rename(F50_iso, c("ADM2_PCODE"="Group.1", "F50iso"="x"))

all_iso <- merge(x=base_iso, y=F10_iso, by="ADM2_PCODE", all.x=TRUE, all.y=TRUE)
all_iso <- merge(x=all_iso, y=F20_iso, by="ADM2_PCODE", all.x=TRUE, all.y=TRUE)
all_iso <- merge(x=all_iso, y=F50_iso, by="ADM2_PCODE", all.x=TRUE, all.y=TRUE)

# Replace NA with zero so that we can get accurate difference measures.
all_iso[is.na(all_iso)] <- 0

# Increase in population isolated from baseline to each flood
all_iso$F10dif <- all_iso$F10iso - all_iso$preiso
all_iso$F20dif <- all_iso$F20iso - all_iso$preiso
all_iso$F50dif <- all_iso$F50iso - all_iso$preiso
# Hamlets (people)
sum(all_iso$preiso, na.rm=TRUE) # Number of isolated people in Senegal under baseline conditions
sum(all_iso$F10iso, na.rm=TRUE) # Number of isolated people in Senegal under 1-in-10 year flood conditions
sum(all_iso$F10dif, na.rm=TRUE) # Increase in isolated people in Senegal, by count
sum(all_iso$F20iso, na.rm=TRUE) 
sum(all_iso$F20dif, na.rm=TRUE)
sum(all_iso$F50iso, na.rm=TRUE) 
sum(all_iso$F50dif, na.rm=TRUE)
# Agriculture (sq meters cropland)
sum(all_iso$preiso, na.rm=TRUE) * 300 # Square meters of isolated cropland in Senegal under baseline conditions
sum(all_iso$F10iso, na.rm=TRUE) * 300 # Square meters of isolated cropland in Senegal under 1-in-10 year flood conditions
sum(all_iso$F10dif, na.rm=TRUE) * 300 # Increase in isolated cropland in Senegal, by square meters
sum(all_iso$F20iso, na.rm=TRUE) * 300 
sum(all_iso$F20dif, na.rm=TRUE) * 300
sum(all_iso$F50iso, na.rm=TRUE) * 300 
sum(all_iso$F50dif, na.rm=TRUE) * 300

# Percent increase
all_iso$F10pc <- all_iso$F10dif / all_iso$preiso * 100
all_iso$F20pc <- all_iso$F20dif / all_iso$preiso * 100
all_iso$F50pc <- all_iso$F50dif / all_iso$preiso * 100
mean(all_iso$F10pc, na.rm=TRUE)
mean(all_iso$F20pc, na.rm=TRUE) 
mean(all_iso$F50pc, na.rm=TRUE)
median(all_iso$F10pc, na.rm = TRUE)
median(all_iso$F20pc, na.rm = TRUE) 
median(all_iso$F50pc, na.rm = TRUE)


# Save intermediate file
write.csv(all_iso, file = "hamlet_isolation_pre_adm2.csv")
# Note that three admin areas did not experience any hamlet isolation, so there are only 42 out of 45 possible rows.
write.csv(all_iso, file = "ag_isolation_adm2.csv")


# ------------- COMPARE LIKE TO LIKE: BEFORE AVERAGING, REMOVE DATAPOINTS WHERE ONE SCENARIO IS ISOLATED ----------------

traveltimes$baseF10mod <- 0
traveltimes$baseF10mod <- replace(traveltimes$pre,is.na(traveltimes$F10),NA)
sum(is.na(traveltimes$pre))
sum(is.na(traveltimes$F10))
sum(is.na(traveltimes$baseF10mod)) # Should be the same as F10 because any baseline isolation will remain so during floods.

traveltimes$baseF20mod <- 0
traveltimes$baseF20mod <- replace(traveltimes$pre,is.na(traveltimes$F20),NA)
sum(is.na(traveltimes$pre))
sum(is.na(traveltimes$F20))
sum(is.na(traveltimes$baseF20mod))

traveltimes$baseF50mod <- 0
traveltimes$baseF50mod <- replace(traveltimes$pre,is.na(traveltimes$F50),NA)
sum(is.na(traveltimes$pre))
sum(is.na(traveltimes$F50))
sum(is.na(traveltimes$baseF50mod))

# Calculate the differences and percents
traveltimes$dif_10b <- traveltimes$F10 - traveltimes$baseF10mod
traveltimes$dif_20b <- traveltimes$F20 - traveltimes$baseF20mod
traveltimes$dif_50b <- traveltimes$F50 - traveltimes$baseF50mod
traveltimes$pc_10b <- traveltimes$dif_10b / traveltimes$baseF10mod * 100
traveltimes$pc_20b <- traveltimes$dif_20b / traveltimes$baseF20mod * 100
traveltimes$pc_50b <- traveltimes$dif_50b / traveltimes$baseF50mod * 100



# ------------- WEIGHT BY POPULATION ----------------
# In a weighted average, each data point value is multiplied by the assigned weight 
# which is then summed and divided by the number of data points.
# Each hamlet's travel time is multiplied by the hamlet's population
# which is then summed and divided by the number of hamlets within the adm2 area.
# Pseudocode: sum_by_adm(ham_travel * ham_pop) / count(ham_in_adm)

# Hamlets
pre_p <- traveltimes %>% # Base travel time
  group_by(ADM2_PCODE) %>%
  summarise(pre_p = weighted.mean(pre, ha_pop, na.rm=TRUE)) %>% as.data.frame()
F10_p <- traveltimes %>% # 1 in 10-year travel time
  group_by(ADM2_PCODE) %>%
  summarise(F10_p = weighted.mean(F10, ha_pop, na.rm=TRUE)) %>% as.data.frame()
F20_p <- traveltimes %>% # 1 in 20-year travel time
  group_by(ADM2_PCODE) %>%
  summarise(F20_p = weighted.mean(F20, ha_pop, na.rm=TRUE)) %>% as.data.frame()
F50_p <- traveltimes %>% # 1 in 50-year travel time
  group_by(ADM2_PCODE) %>%
  summarise(F50_p = weighted.mean(F50, ha_pop, na.rm=TRUE)) %>% as.data.frame()

dif_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(dif_10b_p = weighted.mean(dif_10b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
dif_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(dif_20b_p = weighted.mean(dif_20b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
dif_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(dif_50b_p = weighted.mean(dif_50b, ha_pop, na.rm=TRUE)) %>% as.data.frame()

pc_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(pc_10b_p = weighted.mean(pc_10b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
pc_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(pc_20b_p = weighted.mean(pc_20b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
pc_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(pc_50b_p = weighted.mean(pc_50b, ha_pop, na.rm=TRUE)) %>% as.data.frame()


# Agriculture
pre_p <- traveltimes %>% # Base travel time
  group_by(ADM2_PCODE) %>%
  summarise(pre_p = weighted.mean(pre, val, na.rm=TRUE)) %>% as.data.frame()
F10_p <- traveltimes %>% # 1 in 10-year travel time
  group_by(ADM2_PCODE) %>%
  summarise(F10_p = weighted.mean(F10, val, na.rm=TRUE)) %>% as.data.frame()
F20_p <- traveltimes %>% # 1 in 20-year travel time
  group_by(ADM2_PCODE) %>%
  summarise(F20_p = weighted.mean(F20, val, na.rm=TRUE)) %>% as.data.frame()
F50_p <- traveltimes %>% # 1 in 50-year travel time
  group_by(ADM2_PCODE) %>%
  summarise(F50_p = weighted.mean(F50, val, na.rm=TRUE)) %>% as.data.frame()

dif_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(dif_10b_p = weighted.mean(dif_10b, val, na.rm=TRUE)) %>% as.data.frame()
dif_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(dif_20b_p = weighted.mean(dif_20b, val, na.rm=TRUE)) %>% as.data.frame()
dif_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(dif_50b_p = weighted.mean(dif_50b, val, na.rm=TRUE)) %>% as.data.frame()

pc_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(pc_10b_p = weighted.mean(pc_10b, val, na.rm=TRUE)) %>% as.data.frame()
pc_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(pc_20b_p = weighted.mean(pc_20b, val, na.rm=TRUE)) %>% as.data.frame()
pc_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM2_PCODE) %>%
  summarise(pc_50b_p = weighted.mean(pc_50b, val, na.rm=TRUE)) %>% as.data.frame()


# Combine and write to file.
orig_adm2 = Reduce(function(x, y) merge(x, y, by="ADM2_PCODE", all=TRUE), list(pre_p, F10_p, F20_p, F50_p, dif_10b_p, dif_20b_p, dif_50b_p, pc_10b_p, pc_20b_p, pc_50b_p))
orig_adm2 <- orig_adm2[ , c('ADM2_PCODE', 'pre_p', 'F10_p', 'F20_p', 'F50_p', 'dif_10b_p', 'dif_20b_p', 'dif_50b_p', 'pc_10b_p', 'pc_20b_p', 'pc_50b_p', 'geometry.x')]
orig_adm2 <- rename(orig_adm2, c("geometry"="geometry.x"))
write.csv(orig_adm2, file = "hamlet_to_HDurban_pre-F10-20-50_adm2.csv")
write.csv(orig_adm2, file = "ag_to_HDurban_pre-F10-20-50_adm2.csv")



# ------------- ISOLATION BY PERCENT OF ORIGINS ----------------
traveltimes["Count"] <- 1
iso_pc <- aggregate(traveltimes$Count, list(traveltimes$ADM2_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
iso_pc <- rename(iso_pc, c("ADM2_PCODE"="Group.1", "orig_ct"="x"))

iso_pc <- merge(x=iso_pc, y=all_iso, by="ADM2_PCODE", all.x=TRUE)
iso_pc$prepca2 <- iso_pc$preiso / iso_pc$orig_ct * 100
iso_pc$F10pca2 <- iso_pc$F10iso / iso_pc$orig_ct * 100
iso_pc$F20pca2 <- iso_pc$F20iso / iso_pc$orig_ct * 100
iso_pc$F50pca2 <- iso_pc$F50iso / iso_pc$orig_ct * 100

iso_pc <- iso_pc[ , c('ADM2_PCODE', 'prepca2', 'F10pca2', 'F20pca2', 'F50pca2')]
# As expected, 1 row is NA for hamlets (Gwediawaye) because there was no isolation. 
# This matches with number of rows in "F50_iso".



# ------------- MERGE AND WRITE AS SHAPEFILE ----------------
# Add admin name for easier summary stats
admin <- readOGR(paste0(wd, '/admin/sen_admbnda_adm2_1m_gov_ocha_20190426.shp'))
admin <- st_as_sf(admin)
admin <- admin %>% st_drop_geometry()
class(admin)
admin <- admin[ , c('ADM2_FR', 'ADM2_PCODE')]

# Combine all admin-level isolation metrics to single file.
all_iso <- merge(iso_pc, all_iso, by="ADM2_PCODE", all.x=TRUE)
all_iso <- merge(census_pop, all_iso, by="ADM2_PCODE", all.x=TRUE)
all_iso <- merge(x=admin, y=all_iso, by='ADM2_PCODE', all.x=TRUE)

# Hamlet
write.csv(all_iso, file = "hamlet_isolation_pre_adm2.csv")
st_write(all_iso, "hamlet_isolation_pre_adm2.shp", driver = "ESRI Shapefile", append=FALSE)
# ADM2_PCODE : Unique ID field for administrative level 2 areas, called departments. 45 departments in Senegal.
# ADM2_FR : Administrative level 2 area name.
# pop2020 : Projected population in 2020 for admin2. From census 2013.
# preiso : Number of isolated hamlets during baseline conditions (no inclement weather; no govt intervention).
# F10iso : Number of isolated hamlets during 10 year flood event (no govt intervention).
# F20iso : Number of isolated hamlets during 20 year flood event (no govt intervention).
# F50iso : Number of isolated hamlets during 50 year flood event (no govt intervention).
# F10dif : Number of additional hamlets isolated from baseline to 10 year flood event (no govt intervention).
# F20dif : Number of additional hamlets isolated from baseline to 20 year flood event (no govt intervention).
# F50dif : Number of additional hamlets isolated from baseline to 50 year flood event (no govt intervention).
# F10pc : Percent increase in isolated hamlets from baseline to 10 year flood event (no govt intervention).
# F20pc : Percent increase in isolated hamlets from baseline to 20 year flood event (no govt intervention).
# F50pc : Percent increase in isolated hamlets from baseline to 50 year flood event (no govt intervention).
# prepca2 : Percent of the admin2 area's hamlets which are isolated during baseline conditions (no govt intervention).
# F10pca2 : Percent of the admin2 area's hamlets which are isolated in 10 year flood event (no govt intervention).
# F20pca2 : Percent of the admin2 area's hamlets which are isolated in 20 year flood event (no govt intervention).
# F50pca2 : Percent of the admin2 area's hamlets which are isolated in 50 year flood event (no govt intervention).

# Agriculture
write.csv(all_iso, file = "ag_isolation_adm2.csv")
st_write(all_iso, "ag_isolation_adm2.shp", driver = "ESRI Shapefile", append=FALSE)
# ADM2_PCODE : Unique ID field for administrative level 2 areas, called departments. 45 departments in Senegal.
# ADM2_FR : Administrative level 2 area name.
# pop2020 : Projected population in 2020 for admin2. From census 2013.
# preiso : Number of isolated cropland cells (300m) during baseline conditions (no inclement weather; no govt intervention).
# F10iso : Number of isolated cropland cells during 10 year flood event (no govt intervention).
# F20iso : Number of isolated cropland cells during 20 year flood event (no govt intervention).
# F50iso : Number of isolated cropland cells during 50 year flood event (no govt intervention).
# F10dif : Number of additional cropland cells isolated from baseline to 10 year flood event (no govt intervention).
# F20dif : Number of additional cropland cells isolated from baseline to 20 year flood event (no govt intervention).
# F50dif : Number of additional cropland cells isolated from baseline to 50 year flood event (no govt intervention).
# F10pc : Percent increase in isolated cropland from baseline to 10 year flood event (no govt intervention).
# F20pc : Percent increase in isolated cropland from baseline to 20 year flood event (no govt intervention).
# F50pc : Percent increase in isolated cropland from baseline to 50 year flood event (no govt intervention).
# prepca2 : Percent of the admin2 area's cropland which are isolated during baseline conditions (no govt intervention).
# F10pca2 : Percent of the admin2 area's cropland which are isolated in 10 year flood event (no govt intervention).
# F20pca2 : Percent of the admin2 area's cropland which are isolated in 20 year flood event (no govt intervention).
# F50pca2 : Percent of the admin2 area's cropland which are isolated in 50 year flood event (no govt intervention).


# Add admin name to aggregated travel times and save to file.
orig_adm2 <- merge(x=orig_adm2, y=census_pop, by="ADM2_PCODE", all.x=TRUE)
orig_adm2 <- merge(x=orig_adm2, y=admin, by='ADM2_PCODE', all.x=TRUE)

# Hamlet
write.csv(orig_adm2, file = "hamlet_to_HDurban_pre-F10-20-50_adm2.csv")
st_write(orig_adm2, "hamlet_to_HDurban_pre-F10-20-50_adm2.shp", driver = "ESRI Shapefile", append=FALSE)
# ADM2_PCODE : Unique ID field for administrative level 2 areas, called departments. 45 departments in Senegal.
# ADM2_FR : Administrative level 2 area name.
# pre_p : Average travel time in minutes--weighted by population at origin location--during baseline conditions (no inclement weather; no govt intervention).
# F10_p : Average travel time in minutes--weighted by population at origin location--during 10 year flood event (no govt intervention).
# F20_p : Average travel time in minutes--weighted by population at origin location--during 20 year flood event (no govt intervention).
# F50_p : Average travel time in minutes--weighted by population at origin location--during 50 year flood event (no govt intervention).
# dif_10b_p : Average increase in travel time in minutes--weighted by population at origin location--from baseline to 10 year flood event (no govt intervention).
# dif_20b_p : Average increase in travel time in minutes--weighted by population at origin location--from baseline to 20 year flood event (no govt intervention).
# dif_50b_p : Average increase in travel time in minutes--weighted by population at origin location--from baseline to 50 year flood event (no govt intervention).
# pc_10b_p : Average percent increase in travel time--weighted by population at origin location--from baseline to 10 year flood event (no govt intervention).
# pc_20b_p : Average percent increase in travel time--weighted by population at origin location--from baseline to 20 year flood event (no govt intervention).
# pc_50b_p : Average percent increase in travel time--weighted by population at origin location--from baseline to 50 year flood event (no govt intervention).

# Agriculture
write.csv(orig_adm2, file = "ag_to_HDurban_pre-F10-20-50_adm2.csv")
st_write(orig_adm2, "ag_to_HDurban_pre-F10-20-50_adm2.shp", driver = "ESRI Shapefile", append=FALSE)
# ADM2_PCODE : Unique ID field for administrative level 2 areas, called departments. 45 departments in Senegal.
# ADM2_FR : Administrative level 2 area name.
# pre_p : Average travel time in minutes--weighted by value at origin location--during baseline conditions (no inclement weather; no govt intervention).
# F10_p : Average travel time in minutes--weighted by value at origin location--during 10 year flood event (no govt intervention).
# F20_p : Average travel time in minutes--weighted by value at origin location--during 20 year flood event (no govt intervention).
# F50_p : Average travel time in minutes--weighted by value at origin location--during 50 year flood event (no govt intervention).
# dif_10b_p : Average increase in travel time in minutes--weighted by value at origin location--from baseline to 10 year flood event (no govt intervention).
# dif_20b_p : Average increase in travel time in minutes--weighted by value at origin location--from baseline to 20 year flood event (no govt intervention).
# dif_50b_p : Average increase in travel time in minutes--weighted by value at origin location--from baseline to 50 year flood event (no govt intervention).
# pc_10b_p : Average percent increase in travel time--weighted by value at origin location--from baseline to 10 year flood event (no govt intervention).
# pc_20b_p : Average percent increase in travel time--weighted by value at origin location--from baseline to 20 year flood event (no govt intervention).
# pc_50b_p : Average percent increase in travel time--weighted by value at origin location--from baseline to 50 year flood event (no govt intervention).


# Remove unnecessary fields and save disaggregated travel times to file.
traveltimes <- select(traveltimes, -c(baseF10mod, baseF20mod, baseF50mod, Count))

# Hamlet
write.csv(traveltimes, file = "hamlet_to_HDurban_pre-F10-20-50.csv")
st_write(traveltimes, "hamlet_to_HDurban_pre-F10-20-50.shp", driver = "ESRI Shapefile", append = FALSE)
# mgrs_code : Unique ID field for origins, called hamlets. 
# NN : Unique ID field for the nearest road node on the network to this hamlet.
# NSnomax : Off-road travel time in minutes from the origin to the nearest road node.
# pre : Travel time in minutes to nearest major market during baseline conditions (no inclement weather; no govt intervention).
# F10 : Travel time in minutes to nearest major market during 10 year flood event (no govt intervention).
# F20 : Travel time in minutes to nearest major market during 20 year flood event (no govt intervention).
# F50 : Travel time in minutes to nearest major market during 50 year flood event (no govt intervention).
# dif_10b : Minutes increase in travel time from baseline to 10 year flood event (no govt intervention).
# dif_20b : Minutes increase in travel time from baseline to 20 year flood event (no govt intervention).
# dif_50b : Minutes increase in travel time from baseline to 50 year flood event (no govt intervention).
# pc_10b : Percent increase in travel time from baseline to 10 year flood event (no govt intervention).
# pc_20b : Percent increase in travel time from baseline to 20 year flood event (no govt intervention).
# pc_50b : Percent increase in travel time from baseline to 50 year flood event (no govt intervention).
# ha_pop : Population of hamlet. Derived from WorldPop for 2020 (UN-adjusted, constrained).
# ADM2_PCODE : Unique ID field for the admin2 area in which the hamlet center falls.
# pop2020 : Projected population for 2020 of the admin2 area in which the hamlet center falls. Derived from census 2013.

# Agriculture
write.csv(traveltimes, file = "ag_to_HDurban_pre-F10-20-50.csv")
st_write(traveltimes, "ag_to_HDurban_pre-F10-20-50.shp", driver = "ESRI Shapefile", append = FALSE)
# ID_ag : Unique ID field for origins. Each one is a cropland raster cell 300m resolution.
# val : Agricultural value of the cropland cell, in international dollars.
# NN : Unique ID field for the nearest road node on the network to this cropland cell.
# NSnomax : Off-road travel time in minutes from the origin to the nearest road node.
# pre : Travel time in minutes to nearest major market during baseline conditions (no inclement weather; no govt intervention).
# F10 : Travel time in minutes to nearest major market during 10 year flood event (no govt intervention).
# F20 : Travel time in minutes to nearest major market during 20 year flood event (no govt intervention).
# F50 : Travel time in minutes to nearest major market during 50 year flood event (no govt intervention).
# dif_10b : Minutes increase in travel time from baseline to 10 year flood event (no govt intervention).
# dif_20b : Minutes increase in travel time from baseline to 20 year flood event (no govt intervention).
# dif_50b : Minutes increase in travel time from baseline to 50 year flood event (no govt intervention).
# pc_10b : Percent increase in travel time from baseline to 10 year flood event (no govt intervention).
# pc_20b : Percent increase in travel time from baseline to 20 year flood event (no govt intervention).
# pc_50b : Percent increase in travel time from baseline to 50 year flood event (no govt intervention).
# ADM2_PCODE : Unique ID field for the admin2 area in which the cropland cell falls.
# pop2020 : Projected population for 2020 of the admin2 area in which the hamlet center falls. Derived from census 2013.


