# 

library(tidyverse)
library(ggplot2)
library(cowplot)
library(tidyterra)
library(sf)
library(rgeoboundaries)
library(patchwork)
library(egg)
library(amt)
# do an overview map of all of sweden with all study areas etc combined

theme_set(theme_cowplot() +
            theme(legend.position = "right"))
sweden = gb_adm0("Sweden")

eco = st_read("01_data/shapefiles/eco_zones_final/eco_zones.shp")
mma = st_read("01_data/shapefiles/mma_data/Stratum2024.shp")
final_eco = st_read("01_data/shapefiles/eco_zones_final/eco_zones.shp")
als = st_read("01_data/shapefiles/als_data/grouped_squaresSGD2.shp")

als = als %>% group_by(Block) %>% 
  summarise(geometry = st_union(geometry))

# get shps for chapter III + IV


issf_om = read_rds("C:/Users/lugf0001/Documents/mdbd_sim/01_data/tmp/OM_issf.RDS") %>% 
  mutate(n_winter = map_int(steps_winter, nrow),
         n_summer = map_int(steps_summer, nrow)) %>% 
  filter(n_winter >=10000) %>%  
  filter(n_summer >= 10000)    %>% 
  select(-contains("scaled"), 
         -contains("steps"))

issf_vx = read_rds("C:/Users/lugf0001/Documents/mdbd_sim/01_data/tmp/VX_issf.RDS") %>% 
  mutate(n_winter = map_int(steps_winter, nrow),
         n_summer = map_int(steps_summer, nrow)) %>% 
  filter(n_winter >=10000) %>%  
  filter(n_summer >= 10000)    %>% 
  select(-contains("scaled"), 
         -contains("steps"))

issf = issf_om %>% 
  mutate(area = "OM") %>% 
  bind_rows(issf_vx %>% mutate(area="VX")) %>% 
  filter(year != 2014)

rm(issf_om, issf_vx)

dat = issf %>% 
  dplyr::select(area,data, year, OBJECT_ID) %>% 
  unnest() %>% 
  filter(month%in%c(10,11,12,1,2))

hrs = dat %>% 
  nest(-area) %>% 
  mutate(hr = map(data, function(x) {
    x %>% mk_track(x,y, ts, crs=3021) %>% 
      hr_kde(levels = .99)%>%
      hr_isopleths()
  })) %>% 
  select(area, hr) %>% 
  unnest();hrs

ggplot() +
  geom_sf(data=sweden) +
 #geom_sf(data = mma, fill="grey") +
  geom_sf(data= final_eco, aes(fill=eclgcl_)) +
  scale_fill_manual(values = c("lightblue3", "#01796F", "#FF6F61", "orange2"),
                    breaks  = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", "Southern-Boreal Zone", "Hemi-Boreal Zone"),
                    labels = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", "Southern-Boreal Zone", "Hemi-Boreal Zone"),
                    name="ecological zone")|
  
  ggplot() +
  geom_sf(data=sweden) +
  geom_sf(data = als) +
  geom_sf(data=hrs$geometry, aes(fill=hrs$area)) +
  scale_fill_manual(values=c("red4", "red4"),
                    labels="")
   
