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
admin <- readOGR(paste0(wd, '/admin/adm1_UTM.shp')) # Admin 1 level polygons ("region"). From OCHA via HDX. I already transformed to projected CRS in QGIS.
admin <- st_as_sf(admin)

hamlet_pop <- st_read('hamlet_extents_pop/hamlet_extents_pop.shp') # Population of each origin. Count from WorldPop (2020 UN-adjusted, constrained). Extents from GRID3 (hamlet).
# Clean up hamlet population dataframe. Only need the population and the hamlet unique ID.
hamlet_pop <- hamlet_pop[ , c('mgrs_code', 'SUM')]
hamlet_pop <- rename(hamlet_pop, c("ha_pop"="SUM"))



# ------------- PREPARE TRAVEL DATA ----------------

# Hamlet version
traveltimes <- read.csv('hamlet_to_HDurban_post-F10-20-50.csv') 
traveltimes <- st_as_sf(traveltimes, coords=c("X","Y"), crs=4326)

hamlet_pop <- hamlet_pop %>% st_drop_geometry()
class(hamlet_pop)
traveltimes <- merge(x=traveltimes, y=hamlet_pop, by='mgrs_code', all.x=TRUE)

# Agriculture version
traveltimes <- read.csv('ag_to_HDurban_post-F10-20-50.csv') 
traveltimes <- st_as_sf(traveltimes, coords=c("X","Y"), crs=4326)

# Join adm1 population to travel times
st_crs(admin)
st_crs(traveltimes)
traveltimes <- st_transform(traveltimes, crs = st_crs(admin))
traveltimes <- st_join(traveltimes, admin)



# ------------- FIND ISOLATED ORIGINS AND REMOVE FROM SUMMARY STATISTICS ----------------
# We'll retain the sum values for summary stats.
traveltimes$post<- replace(traveltimes$post,traveltimes$post>240,NA)
traveltimes$F10 <- replace(traveltimes$F10,traveltimes$F10>240,NA)
traveltimes$F20 <- replace(traveltimes$F20,traveltimes$F20>240,NA)
traveltimes$F50 <- replace(traveltimes$F50,traveltimes$F50>240,NA)



# ------------- ISOLATION BY POPULATION SIZE ----------------
base_isolated <- subset(traveltimes, is.na(post))
F10_isolated <- subset(traveltimes, is.na(F10))
F20_isolated <- subset(traveltimes, is.na(F20))
F50_isolated <- subset(traveltimes, is.na(F50))

# HAMLETS
# Total population & total hamlets isolated from markets under baseline conditions, by ADMIN2
base_iso <- 0
base_isopop <- 0
base_isolated["Count"] <- 1

base_iso <- aggregate(base_isolated$Count, list(base_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
base_iso <- rename(base_iso, c("ADM1_PCODE"="Group.1", "postiso"="x"))
base_isopop <- aggregate(base_isolated$ha_pop, list(base_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
base_isopop <- rename(base_isopop, c("ADM1_PCODE"="Group.1", "postisopop"="x"))

# Total population & total hamlets isolated from markets under 1-in-10 year flood conditions, by ADMIN2
F10_iso <- 0
F10_isopop <- 0
F10_isolated["Count"] <- 1

F10_iso <- aggregate(F10_isolated$Count, list(F10_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F10_iso <- rename(F10_iso, c("ADM1_PCODE"="Group.1", "F10iso"="x"))
F10_isopop <- aggregate(F10_isolated$ha_pop, list(F10_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F10_isopop <- rename(F10_isopop, c("ADM1_PCODE"="Group.1", "F10isopop"="x"))

# Total population & total hamlets isolated from markets under 1-in-20 year flood conditions, by ADMIN2
F20_iso <- 0
F20_isopop <- 0
F20_isolated["Count"] <- 1

F20_iso <- aggregate(F20_isolated$Count, list(F20_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F20_iso <- rename(F20_iso, c("ADM1_PCODE"="Group.1", "F20iso"="x"))
F20_isopop <- aggregate(F20_isolated$ha_pop, list(F20_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F20_isopop <- rename(F20_isopop, c("ADM1_PCODE"="Group.1", "F20isopop"="x"))

# Total population & total hamlets isolated from markets under 1-in-50 year flood conditions, by ADMIN2
F50_iso <- 0
F50_isopop <- 0
F50_isolated["Count"] <- 1

F50_iso <- aggregate(F50_isolated$Count, list(F50_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F50_iso <- rename(F50_iso, c("ADM1_PCODE"="Group.1", "F50iso"="x"))
F50_isopop <- aggregate(F50_isolated$ha_pop, list(F50_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F50_isopop <- rename(F50_isopop, c("ADM1_PCODE"="Group.1", "F50isopop"="x"))

all_iso = Reduce(function(x, y) merge(x, y, by="ADM1_PCODE", all=TRUE), list(base_iso, base_isopop, F10_iso, F10_isopop, F20_iso, F20_isopop, F50_iso, F50_isopop))



# AGRICULTURE
# Total cropland and total production value isolated from markets under baseline conditions, by ADMIN2
base_iso <- 0
base_isoval <- 0
base_isolated["Count"] <- 1

base_iso <- aggregate(base_isolated$Count, list(base_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
base_iso <- rename(base_iso, c("ADM1_PCODE"="Group.1", "postiso"="x"))
base_isoval <- aggregate(base_isolated$val, list(base_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
base_isoval <- rename(base_isoval, c("ADM1_PCODE"="Group.1", "postisoval"="x"))

# Total cropland and total production value isolated from markets under 1-in-10 year flood conditions, by ADMIN2
F10_iso <- 0
F10_isoval <- 0
F10_isolated["Count"] <- 1

F10_iso <- aggregate(F10_isolated$Count, list(F10_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F10_iso <- rename(F10_iso, c("ADM1_PCODE"="Group.1", "F10iso"="x"))
F10_isoval <- aggregate(F10_isolated$val, list(F10_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F10_isoval <- rename(F10_isoval, c("ADM1_PCODE"="Group.1", "F10isoval"="x"))

# Total cropland and total production value isolated from markets under 1-in-20 year flood conditions, by ADMIN2
F20_iso <- 0
F20_isoval <- 0
F20_isolated["Count"] <- 1

F20_iso <- aggregate(F20_isolated$Count, list(F20_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F20_iso <- rename(F20_iso, c("ADM1_PCODE"="Group.1", "F20iso"="x"))
F20_isoval <- aggregate(F20_isolated$val, list(F20_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F20_isoval <- rename(F20_isoval, c("ADM1_PCODE"="Group.1", "F20isoval"="x"))

# Total cropland and total production value isolated from markets under 1-in-50 year flood conditions, by ADMIN2
F50_iso <- 0
F50_isoval <- 0
F50_isolated["Count"] <- 1

F50_iso <- aggregate(F50_isolated$Count, list(F50_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F50_iso <- rename(F50_iso, c("ADM1_PCODE"="Group.1", "F50iso"="x"))
F50_isoval <- aggregate(F50_isolated$val, list(F50_isolated$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
F50_isoval <- rename(F50_isoval, c("ADM1_PCODE"="Group.1", "F50isoval"="x"))

all_iso = Reduce(function(x, y) merge(x, y, by="ADM1_PCODE", all=TRUE), list(base_iso, base_isoval, F10_iso, F10_isoval, F20_iso, F20_isoval, F50_iso, F50_isoval))




# Replace NA with zero so that we can get accurate difference measures.
all_iso[is.na(all_iso)] <- 0

# Increase in locations isolated from baseline to each flood
all_iso$F10dif <- all_iso$F10iso - all_iso$postiso
all_iso$F20dif <- all_iso$F20iso - all_iso$postiso
all_iso$F50dif <- all_iso$F50iso - all_iso$postiso

# Hamlets (locations; people)
# Increase in population isolated from baseline to each flood
all_iso$F10popdif <- all_iso$F10isopop - all_iso$postisopop
all_iso$F20popdif <- all_iso$F20isopop - all_iso$postisopop
all_iso$F50popdif <- all_iso$F50isopop - all_iso$postisopop
sum(all_iso$postiso, na.rm=TRUE) # Number of isolated hamlets in Senegal under baseline conditions
sum(all_iso$F10iso, na.rm=TRUE) # Number of isolated hamlets in Senegal under 1-in-10 year flood conditions
sum(all_iso$F10dif, na.rm=TRUE) # Increase in isolated hamlets in Senegal under 1-in-10 year flood conditions, by count
sum(all_iso$F20iso, na.rm=TRUE) # Number of isolated hamlets in Senegal under 1-in-20 year flood conditions
sum(all_iso$F20dif, na.rm=TRUE) # Increase in isolated hamlets in Senegal under 1-in-20 year flood conditions, by count
sum(all_iso$F50iso, na.rm=TRUE) # Number of isolated hamlets in Senegal under 1-in-50 year flood conditions
sum(all_iso$F50dif, na.rm=TRUE) # Increase in isolated hamlets in Senegal under 1-in-50 year flood conditions, by count
sum(all_iso$postisopop, na.rm=TRUE) # Number of isolated people in Senegal under baseline conditions
sum(all_iso$F10isopop, na.rm=TRUE) # Number of isolated people in Senegal under 1-in-10 year flood conditions
sum(all_iso$F10popdif, na.rm=TRUE) # Increase in isolated people in Senegal under 1-in-10 year flood conditions, by count
sum(all_iso$F20isopop, na.rm=TRUE) # Number of isolated people in Senegal under 1-in-20 year flood conditions
sum(all_iso$F20popdif, na.rm=TRUE) # Increase in isolated people in Senegal under 1-in-20 year flood conditions, by count
sum(all_iso$F50isopop, na.rm=TRUE)# Number of isolated people in Senegal under 1-in-50 year flood conditions
sum(all_iso$F50popdif, na.rm=TRUE) # Increase in isolated people in Senegal under 1-in-50 year flood conditions, by count



# Agriculture (sq meters cropland; value in intl $)
# Increase in value isolated from baseline to each flood
all_iso$F10valdif <- all_iso$F10isoval - all_iso$postisoval
all_iso$F20valdif <- all_iso$F20isoval - all_iso$postisoval
all_iso$F50valdif <- all_iso$F50isoval - all_iso$postisoval
sum(all_iso$postiso, na.rm=TRUE) * 300 # Square meters of isolated cropland in Senegal under baseline conditions
sum(all_iso$F10iso, na.rm=TRUE) * 300 # Square meters of isolated cropland in Senegal under 1-in-10 year flood conditions
sum(all_iso$F10dif, na.rm=TRUE) * 300 # Increase in isolated cropland in Senegal under 1-in-10 year flood conditions, by square meters
sum(all_iso$F20iso, na.rm=TRUE) * 300 # Square meters of isolated cropland in Senegal under 1-in-20 year flood conditions
sum(all_iso$F20dif, na.rm=TRUE) * 300 # Increase in isolated cropland in Senegal under 1-in-20 year flood conditions, by square meters
sum(all_iso$F50iso, na.rm=TRUE) * 300 # Square meters of isolated cropland in Senegal under 1-in-50 year flood conditions 
sum(all_iso$F50dif, na.rm=TRUE) * 300 # Increase in isolated cropland in Senegal under 1-in-50 year flood conditions, by square meters
sum(all_iso$postisoval, na.rm=TRUE) # Number of isolated agricultural production dollars in Senegal under baseline conditions
sum(all_iso$F10isoval, na.rm=TRUE) # Number of isolated agricultural production dollars in Senegal under 1-in-10 year flood conditions
sum(all_iso$F10valdif, na.rm=TRUE) # Increase in isolated agricultural production dollars in Senegal under 1-in-10 year flood conditions, by count
sum(all_iso$F20isoval, na.rm=TRUE) # Number of isolated agricultural production dollars in Senegal under 1-in-20 year flood conditions
sum(all_iso$F20valdif, na.rm=TRUE) # Increase in isolated agricultural production dollars in Senegal under 1-in-20 year flood conditions, by count
sum(all_iso$F50isoval, na.rm=TRUE)# Number of isolated agricultural production dollars in Senegal under 1-in-50 year flood conditions
sum(all_iso$F50valdif, na.rm=TRUE) # Increase in isolated agricultural production dollars in Senegal under 1-in-50 year flood conditions, by count



# Percent increase
# Hamlets
all_iso$F10pc <- all_iso$F10dif / all_iso$postiso * 100 # Increase in hamlets isolated under 1-in-10
all_iso$F20pc <- all_iso$F20dif / all_iso$postiso * 100 # Increase in hamlets isolated under 1-in-20
all_iso$F50pc <- all_iso$F50dif / all_iso$postiso * 100 # Increase in hamlets isolated under 1-in-50
all_iso$F10poppc <- all_iso$F10popdif / all_iso$postisopop * 100 # Increase in people isolated under 1-in-10
all_iso$F20poppc <- all_iso$F20popdif / all_iso$postisopop * 100 # Increase in people isolated under 1-in-20
all_iso$F50poppc <- all_iso$F50popdif / all_iso$postisopop * 100 # Increase in peopleisolated under 1-in-50



# Agriculture
all_iso$F10pc <- all_iso$F10dif / all_iso$postiso * 100 # Increase in cropland area isolated under 1-in-10
all_iso$F20pc <- all_iso$F20dif / all_iso$postiso * 100 # Increase in cropland area isolated under 1-in-20
all_iso$F50pc <- all_iso$F50dif / all_iso$postiso * 100 # Increase in cropland area isolated under 1-in-50
all_iso$F10valpc <- all_iso$F10valdif / all_iso$postisoval * 100 # Increase in dollars isolated under 1-in-10
all_iso$F20valpc <- all_iso$F20valdif / all_iso$postisoval * 100 # Increase in dollars isolated under 1-in-20
all_iso$F50valpc <- all_iso$F50valdif / all_iso$postisoval * 100 # Increase in dollars isolated under 1-in-50



mean(all_iso$F10pc, na.rm=TRUE)
mean(all_iso$F20pc, na.rm=TRUE) 
mean(all_iso$F50pc, na.rm=TRUE)
median(all_iso$F10pc, na.rm = TRUE)
median(all_iso$F20pc, na.rm = TRUE) 
median(all_iso$F50pc, na.rm = TRUE)

# Hamlet
median(all_iso$F10poppc, na.rm = TRUE)
median(all_iso$F20poppc, na.rm = TRUE) 
median(all_iso$F50poppc, na.rm = TRUE)



# Ag
median(all_iso$F10valpc, na.rm = TRUE)
median(all_iso$F20valpc, na.rm = TRUE) 
median(all_iso$F50valpc, na.rm = TRUE)




# ------------- COMPARE LIKE TO LIKE: BEFORE AVERAGING, REMOVE DATAPOINTS WHERE ONE SCENARIO IS ISOLATED ----------------
traveltimes$baseF10mod <- 0
traveltimes$baseF10mod <- replace(traveltimes$post,is.na(traveltimes$F10),NA)
sum(is.na(traveltimes$post))
sum(is.na(traveltimes$F10))
sum(is.na(traveltimes$baseF10mod)) # Should be the same as F10 because any baseline isolation will remain so during floods.

traveltimes$baseF20mod <- 0
traveltimes$baseF20mod <- replace(traveltimes$post,is.na(traveltimes$F20),NA)
sum(is.na(traveltimes$post))
sum(is.na(traveltimes$F20))
sum(is.na(traveltimes$baseF20mod))

traveltimes$baseF50mod <- 0
traveltimes$baseF50mod <- replace(traveltimes$post,is.na(traveltimes$F50),NA)
sum(is.na(traveltimes$post))
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
# which is then summed and divided by the number of hamlets within the ADM1 area.
# Pseudocode: sum_by_adm(ham_travel * ham_pop) / count(ham_in_adm)

# Hamlets
post_p <- traveltimes %>% # Base travel time
  group_by(ADM1_PCODE) %>%
  summarise(post_p = weighted.mean(post, ha_pop, na.rm=TRUE)) %>% as.data.frame()
F10_p <- traveltimes %>% # 1 in 10-year travel time
  group_by(ADM1_PCODE) %>%
  summarise(F10_p = weighted.mean(F10, ha_pop, na.rm=TRUE)) %>% as.data.frame()
F20_p <- traveltimes %>% # 1 in 20-year travel time
  group_by(ADM1_PCODE) %>%
  summarise(F20_p = weighted.mean(F20, ha_pop, na.rm=TRUE)) %>% as.data.frame()
F50_p <- traveltimes %>% # 1 in 50-year travel time
  group_by(ADM1_PCODE) %>%
  summarise(F50_p = weighted.mean(F50, ha_pop, na.rm=TRUE)) %>% as.data.frame()

dif_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(dif_10b_p = weighted.mean(dif_10b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
dif_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(dif_20b_p = weighted.mean(dif_20b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
dif_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(dif_50b_p = weighted.mean(dif_50b, ha_pop, na.rm=TRUE)) %>% as.data.frame()

pc_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(pc_10b_p = weighted.mean(pc_10b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
pc_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(pc_20b_p = weighted.mean(pc_20b, ha_pop, na.rm=TRUE)) %>% as.data.frame()
pc_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(pc_50b_p = weighted.mean(pc_50b, ha_pop, na.rm=TRUE)) %>% as.data.frame()



# Agriculture
post_p <- traveltimes %>% # Base travel time
  group_by(ADM1_PCODE) %>%
  summarise(post_p = weighted.mean(post, val, na.rm=TRUE)) %>% as.data.frame()
F10_p <- traveltimes %>% # 1 in 10-year travel time
  group_by(ADM1_PCODE) %>%
  summarise(F10_p = weighted.mean(F10, val, na.rm=TRUE)) %>% as.data.frame()
F20_p <- traveltimes %>% # 1 in 20-year travel time
  group_by(ADM1_PCODE) %>%
  summarise(F20_p = weighted.mean(F20, val, na.rm=TRUE)) %>% as.data.frame()
F50_p <- traveltimes %>% # 1 in 50-year travel time
  group_by(ADM1_PCODE) %>%
  summarise(F50_p = weighted.mean(F50, val, na.rm=TRUE)) %>% as.data.frame()

dif_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(dif_10b_p = weighted.mean(dif_10b, val, na.rm=TRUE)) %>% as.data.frame()
dif_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(dif_20b_p = weighted.mean(dif_20b, val, na.rm=TRUE)) %>% as.data.frame()
dif_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(dif_50b_p = weighted.mean(dif_50b, val, na.rm=TRUE)) %>% as.data.frame()

pc_10b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 10-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(pc_10b_p = weighted.mean(pc_10b, val, na.rm=TRUE)) %>% as.data.frame()
pc_20b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 20-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(pc_20b_p = weighted.mean(pc_20b, val, na.rm=TRUE)) %>% as.data.frame()
pc_50b_p <- traveltimes %>% # Difference in travel time from baseline to 1 in 50-year flood, excluding any origins that have been isolated
  group_by(ADM1_PCODE) %>%
  summarise(pc_50b_p = weighted.mean(pc_50b, val, na.rm=TRUE)) %>% as.data.frame()




# Combine and write to file.
orig_adm1 = Reduce(function(x, y) merge(x, y, by="ADM1_PCODE", all=TRUE), list(post_p, F10_p, F20_p, F50_p, dif_10b_p, dif_20b_p, dif_50b_p, pc_10b_p, pc_20b_p, pc_50b_p))
orig_adm1 <- orig_adm1[ , c('ADM1_PCODE', 'post_p', 'F10_p', 'F20_p', 'F50_p', 'dif_10b_p', 'dif_20b_p', 'dif_50b_p', 'pc_10b_p', 'pc_20b_p', 'pc_50b_p', 'geometry.x')]
orig_adm1 <- rename(orig_adm1, c("geometry"="geometry.x"))


# ------------- ISOLATION BY PERCENT OF ORIGINS ----------------
traveltimes["Count"] <- 1
iso_pc <- aggregate(traveltimes$Count, list(traveltimes$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
iso_pc <- rename(iso_pc, c("ADM1_PCODE"="Group.1", "orig_ct"="x"))

# Hamlet
isoval_pc <- aggregate(traveltimes$ha_pop, list(traveltimes$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
isoval_pc <- rename(isoval_pc, c("ADM1_PCODE"="Group.1", "hapop_a1"="x"))


# Agriculture
isoval_pc <- aggregate(traveltimes$val, list(traveltimes$ADM1_PCODE), FUN=sum, na.rm=TRUE, na.action=NULL)
isoval_pc <- rename(isoval_pc, c("ADM1_PCODE"="Group.1", "val_a1"="x"))




iso_pc <- merge(x=iso_pc, y=isoval_pc, by="ADM1_PCODE", all.x=TRUE)

all_iso <- merge(x=iso_pc, y=all_iso, by="ADM1_PCODE", all.x=TRUE)
all_iso$post_pca1 <- all_iso$postiso / all_iso$orig_ct * 100
all_iso$F10_pca1 <- all_iso$F10iso / all_iso$orig_ct * 100
all_iso$F20_pca1 <- all_iso$F20iso / all_iso$orig_ct * 100
all_iso$F50_pca1 <- all_iso$F50iso / all_iso$orig_ct * 100

# Hamlet
all_iso$post_pcpop <- all_iso$postisopop / all_iso$hapop_a1 * 100
all_iso$F10_pcpop <- all_iso$F10isopop / all_iso$hapop_a1 * 100
all_iso$F20_pcpop <- all_iso$F20isopop / all_iso$hapop_a1 * 100
all_iso$F50_pcpop <- all_iso$F50isopop / all_iso$hapop_a1 * 100




# Agriculture
all_iso$post_pcval <- all_iso$postisoval / all_iso$val_a1 * 100
all_iso$F10_pcval <- all_iso$F10isoval / all_iso$val_a1 * 100
all_iso$F20_pcval <- all_iso$F20isoval / all_iso$val_a1 * 100
all_iso$F50_pcval <- all_iso$F50isoval / all_iso$val_a1 * 100



# Georeference and fix the issue of the travel times being in multipoint.
class(all_iso)
all_iso <- merge(admin, all_iso, by="ADM1_PCODE", all.x=TRUE)

# orig_adm1 <- orig_adm1 %>% st_drop_geometry() # Only works with sf objects
# Hamlet
st_write(orig_adm1, "hamlet_to_HDurban_post-F10-20-50_adm1.shp", driver = "ESRI Shapefile", append=FALSE)
orig_adm1 <- st_read('hamlet_to_HDurban_post-F10-20-50_adm1.shp')
# Agriculture
st_write(orig_adm1, "ag_to_HDurban_post-F10-20-50_adm1.shp", driver = "ESRI Shapefile", append=FALSE)
orig_adm1 <- st_read('ag_to_HDurban_post-F10-20-50_adm1.shp')

class(orig_adm1)
orig_adm1 <- orig_adm1 %>% st_drop_geometry()
class(orig_adm1)
orig_adm1 <- merge(admin, orig_adm1, by="ADM1_PCODE", all.x=TRUE)



# Hamlet
write.csv(all_iso, file = "hamlet_isolation_post_ADM1.csv")
st_write(all_iso, "hamlet_isolation_post_ADM1.shp", driver = "ESRI Shapefile", append=FALSE)


# Agriculture
write.csv(all_iso, file = "ag_isolation_post_ADM1.csv")
st_write(all_iso, "ag_isolation_post_ADM1.shp", driver = "ESRI Shapefile", append=FALSE)




# Hamlet
write.csv(orig_adm1, file = "hamlet_to_HDurban_post-F10-20-50_adm1.csv")
st_write(orig_adm1, "hamlet_to_HDurban_post-F10-20-50_adm1.shp", driver = "ESRI Shapefile", append=FALSE)


# Agriculture
write.csv(orig_adm1, file = "ag_to_HDurban_post-F10-20-50_adm1.csv")
st_write(orig_adm1, "ag_to_HDurban_post-F10-20-50_adm1.shp", driver = "ESRI Shapefile", append=FALSE)