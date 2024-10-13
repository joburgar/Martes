#Running this script to create global IUCN range maps - assumes IUCN terrestrial mammal range maps are downloaded and in a sub-folder of this project

#Load Packages
library(tidyverse)
library(sf)
library(Cairo)
library(ggspatial)
library(ggmap)
library(rnaturalearth)
library(rnaturalearthdata)


#Load Data
mammals <- st_read("MAMMALS_TERRESTRIAL_ONLY/MAMMALS_TERRESTRIAL_ONLY.shp")

mustelid <- mammals %>% filter(family=="MUSTELIDAE")
guloninae <- mammals %>% filter(family=="MUSTELIDAE") %>% filter(genus %in% c("Gulo", "Martes","Eira"))
guloninae %>% filter(sci_name=="Martes pennanti")
glimpse(guloninae)

guloninae$sci_name <- case_when(guloninae$sci_name == "Martes pennanti" ~ "Pekania pennanti",
                           TRUE ~ as.character(guloninae$sci_name))

guloninae %>% select(legend) %>% print(n=50)

#Create Range Map
theme_set(theme_bw())

world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data = world) +
  geom_sf()

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = guloninae, aes(fill=sci_name))+
  scale_fill_viridis_d(option="plasma",alpha=0.5, direction=-1)+
  ggtitle("Guloninae Species' Ranges") +
  theme_classic() +
  theme(legend.position = "bottom")+
  theme(legend.title=element_blank())
