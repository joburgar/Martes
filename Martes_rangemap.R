#Running this script to create global IUCN range maps - assumes IUCN terrestrial mammal range maps are downloaded and in a sub-folder of this project

#Load Packages
library(tidyverse)
library(sf)
library(Cairo)
library(ggspatial)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)
library(terra) # to plot mdd maps

library(mdd)
citation("mdd")
# Fernandez ALR (2024). _mdd: Download mammal shapefiles of the world_. R package version 0.1.0,
# commit 81792d43c28941853b4f6892487ef2862f8c611c, <https://github.com/alrobles/mdd>.

library(dataone)
citation("dataone")
# Jones M, Slaughter P, Nahf R, Boettiger C, Jones C, Mecum B, Clark J, Read J, Walker L, Hart E,
# Chamberlain S (2022). _dataone: R Interface to the DataONE REST API_. doi:10.5063/F1M61H5X
# <https://doi.org/10.5063/F1M61H5X>, R package version 2.2.2,
# <https://github.com/DataONEorg/rdataone>.

# Function to download Guloninae ranges and turn to sf objects
get_sf_range = function(gulsp="Eira barbara") {
  gulmap <- get_mdd_map(species = gulsp)
  sfgulmap <-sf::st_as_sf(gulmap)
  return(sfgulmap)
}

# Create Guloninae range spatial object
GulSp <- c("Eira barbara","Gulo gulo","Pekania pennanti",
           "Martes flavigula", "Martes gwatkinsii",
           "Martes americana", "Martes caurina", "Martes foina", "Martes martes","Martes melampus", "Martes zibellina")

Eb <- get_sf_range(gulsp = GulSp[1])
Gg <- get_sf_range(gulsp = GulSp[2])
Pp <- get_sf_range(gulsp = GulSp[3])
Mfl <- get_sf_range(gulsp = GulSp[4])
Mg <- get_sf_range(gulsp = GulSp[5])
Ma <- get_sf_range(gulsp = GulSp[6])
Mc <- get_sf_range(gulsp = GulSp[7])
Mfo <- get_sf_range(gulsp = GulSp[8])
Mma <- get_sf_range(gulsp = GulSp[9])
Mme <- get_sf_range(gulsp = GulSp[10])
Mz <- get_sf_range(gulsp = GulSp[11])

Gulranges <- rbind(Eb, Gg, Pp, Mfl, Mg, Ma, Mc, Mfo, Mma, Mme, Mz)

# #Load IUCN Data
# mammals <- st_read("MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")
# 
# mustelid <- mammals %>% filter(family=="MUSTELIDAE")
# guloninae <- mammals %>% filter(family=="MUSTELIDAE") %>% filter(genus %in% c("Gulo", "Martes","Eira"))
# guloninae %>% filter(sci_name=="Martes pennanti")
# glimpse(guloninae)
# 
# guloninae$sci_name <- case_when(guloninae$sci_name == "Martes pennanti" ~ "Pekania pennanti",
#                            TRUE ~ as.character(guloninae$sci_name))
# 
# guldf <- guloninae %>% st_drop_geometry()
# guldf %>% group_by(yrcompiled) %>% count(sci_name)
# # most range maps compiled in 2016, Martes gwatkinsii in 2015, and M. melampus in 2008
# # 1       2008 Martes melampus       8
# # 2       2015 Martes gwatkinsii     1
# # 3       2016 Eira barbara          2
# # 4       2016 Gulo gulo             9
# # 5       2016 Martes americana      7
# # 6       2016 Martes flavigula      7
# # 7       2016 Martes foina          4
# # 8       2016 Martes martes         8
# # 9       2016 Martes zibellina      1
# # 10      2016 Pekania pennanti      2

#Create Range Map
theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = Gulranges, aes(fill=sciname))+
  scale_fill_viridis_d(option="plasma",alpha=0.5, direction=-1)+
  ggtitle("Guloninae Species' Ranges") +
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())

#############################################
#############################################

# Species Table
# assumes you have downloaded mass data and put in projec folder: https://knb.ecoinformatics.org/view/doi%3A10.5063%2FAA%2Fnceas.196.3

# NCEAS 2182: Smith: Body paleoecology: Linking temporal and taxonomic scales size in ecology and pattern and process across spatial, National Center Ecological Analysis for and Synthesis, & Felisa Smith. (2004). Macroecological database of mammalian body mass. Knowledge Network for Biocomplexity. 
# doi:10.5063/AA/nceas.196.3

mamdf <- read.delim(file="mammals.txt", header=F)
colnames(mamdf) <- c("Continent","Status","Order","Family","Genus","Species","LogMass", "CombinedMass","Ref")
# Combined mass in grams (adult body mass averaged across males and females)
mamdf <- as.tibble(mamdf)
Guldf <- mamdf %>% filter(Genus %in% c("Eira", "Martes", "Gulo"))
Guldf %>% group_by (Genus, Species) %>% summarise(mean = mean(CombinedMass), min = min(CombinedMass))
#   Genus  Species      mean    min
# 1 Eira   barbara     3910   3910 
# 2 Gulo   gulo       17013. 14525.
# 3 Martes americana   1250   1250 
# 4 Martes flavigula   1842.  1185 
# 5 Martes foina       1541.  1541.
# 6 Martes gwatkinsii  2043   2043 
# 7 Martes martes      1300   1300 
# 8 Martes melampus    1000   1000 
# 9 Martes nobilis     1950   1950 
# 10 Martes pennanti    4000   4000 # Pekania pennanti
# 11 Martes zibellina   1130   1130 
