# 

library(tidyverse)
library(ggplot2)
library(cowplot)
library(tidyterra)
library(sf)
library(rgeoboundaries)
library(ggnewscale)
library(patchwork)
library(egg)
library(amt)
# do an overview map of all of sweden with all study areas etc combined

theme_set(theme_bw() +
            theme(legend.position = "right"))
sweden = gb_adm0("Sweden")

eco = st_read("01_data/shapefiles/eco_zones_final/eco_zones.shp")
mma = st_read("01_data/shapefiles/mma_data/Stratum2024.shp")
final_eco = st_read("01_data/shapefiles/eco_zones_final/eco_zones.shp")
als = st_read("01_data/shapefiles/als_data/grouped_squaresSGD2.shp")%>% 
  group_by(Block) %>% 
  summarise(geometry = st_union(geometry))


mma_eco = st_intersection(final_eco, mma)

## prepdata chapter II 


joins = read.csv("C:/Users/lugf0001/Documents/maps-and-moose/01_data/test_train/train.csv") %>% 
  select(mma, ecological_zone, year) %>% distinct()

# get the shapes
shp = st_read("01_data/shapefiles//final_shape_for_calcs/map_final_plot.shp") %>% 
  rename(mma = LANAFO) %>% 
  select(mma) %>% 
  left_join(joins)

# outline shape.. necessary?
shp2 = shp %>% 
  left_join(joins) %>% 
  group_by(ecological_zone) %>% 
  st_buffer(1e3) %>% 
  summarize(geometry = st_union(geometry))

mma_for_study = st_read("01_data/shapefiles/mma_data/Stratum2024.shp")

mma_for_study = mma_for_study %>% 
  rename(id = AFOID_LANS) 
# works?
sw = rgeoboundaries::gb_adm0("Sweden") %>% 
  st_transform(3006)

mma_for_study = (st_intersection(sw,mma_for_study ))

#get data for chapter V

bb = st_read("01_data/shapefiles/bilberry_shapes/final_stands_shapes/final_stands.shp")


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

## combine data chapter IV + III + V

bb_hrs = bb %>%
  st_transform(3021) %>%
  st_buffer(1e3) %>% 
  
  select(geometry) %>% 
  mutate(chapter = "Chapter V") %>% 
  full_join(hrs %>% select(geometry) %>% 
              mutate(chapter= "Chapter III + IV"))

p1 = ggplot() +
  geom_sf(data=sweden, fill="white") +
  #geom_sf(data = mma, fill="grey") +
  geom_sf(data= als %>% 
            mutate(chapter = "Chapter I"),
          aes(fill=chapter)) +
  scale_fill_manual(name="",
                    values="#566573") +
  theme(legend.position = "bottom");p1

p2 = ggplot() +
  geom_sf(data=sweden, fill="white") +
  #geom_sf(data = mma, fill="grey") +
  geom_sf(data= shp %>% mutate(chapter = "Chapter II"),aes(fill=chapter)) +
  theme(legend.position = "none")+

  geom_sf(data=bb_hrs, aes(fill=chapter)) +
  scale_fill_manual(values=c("#7A5C99", "#D98C00", "#2B8CBE"),
                    name="") +
  geom_sf(data = bb %>% st_buffer(1e3), fill="#2B8CBE")+
  theme(legend.position = "bottom");p2

(p1+p2)|  
  
ggplot() +
  #geom_sf(data=sweden) +
  #geom_sf(data = als) +
  geom_sf(data = shp %>% st_crop(st_bbox(bb_hrs %>% st_transform(3006) %>% st_buffer(5e3)))) +
  geom_sf(data=bb_hrs, aes(fill=chapter)) +
  scale_fill_manual(values=c("#D98C00", "#2B8CBE"),
                    name="") +
  geom_sf(data = bb %>% st_buffer(1e3), fill="#2B8CBE") +
  theme(legend.position = "bottom") 
   
