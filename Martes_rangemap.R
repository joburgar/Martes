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

get_sf_range = function(gulsp="Eira barbara") {
  gulmap <- get_mdd_map(species = gulsp)
  sfgulmap <-sf::st_as_sf(gulmap)
  return(sfgulmap)
}


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

#### 
# Species Table
library(MASS)
MASS:mammals
