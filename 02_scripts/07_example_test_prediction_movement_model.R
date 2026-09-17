## example for the browsing damage model being applied?

library(tidyverse)
library(glmmTMB)
library(amt)
library(terra)
library(sf)

#read files

#getmodel 

# restart here

model_dat3 = read.csv("C:/Users/lugf0001/Documents/mdbd_sim/01_data/model_dat/modelling_data.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  rename(ud_time = ud) %>% 
  mutate(ud_time = ud_time,
         ud_full = ud_full) %>% 
  mutate(y= prop_damage3)

# Null model
tictoc::tic()
mod0 <- glmmTMB(
  y~ 1 +(1|id),
  ziformula = ~1,
  family = beta_family(link = "logit"),
  data = model_dat3,
)
## -----------------------------
## Zero-inflation model selection
## -----------------------------

mod4 <- update(mod0, ziformula = ~ year + scale(I(ud_full*10000))+(1|id))

## -----------------------------
## proportional model selection
## -----------------------------

mod13 <- update(mod4, ~ scale(log(n_stems)):scale(I(ud_time*10000))+scale(log(n_stems)) + (1 | id))
performance::performance(mod13)

OM_FULL = sprc(list.files("C:/Users/lugf0001/Documents/mdbd_sim/01_data/validation_sample//OM_2months/", full.names = T))  %>% 
  mosaic(fun="sum") 
VX_FULL = sprc(list.files("C:/Users/lugf0001/Documents/mdbd_sim/01_data/validation_sample//VX_2months/", full.names = T))  %>% 
  mosaic(fun="sum") 

ud = function(x) {
  x/sum(values(x), na.rm=T)
}

UD_OM_FULL = ud(OM_FULL)
UD_VX_FULL = ud(VX_FULL)

names(UD_OM_FULL) = "UD_OM_FULL"
names(UD_VX_FULL) = "UD_VX_FULL"


UD_OM_FULL[is.na(UD_OM_FULL)] = 0
UD_VX_FULL[is.na(UD_VX_FULL)] = 0
plot(UD_OM_FULL)

## where are my LAS rasters?

om_las = rast("C:/Users/lugf0001/Documents/mdbd_sim/01_data/tmp/stack_OM_tmp.tif")
vx_las = rast("C:/Users/lugf0001/Documents/mdbd_sim/01_data/tmp/stack_VX_tmp.tif")

#get pine


bb_vx= st_bbox(vx_las) %>% 
  st_as_sfc() %>% 
  st_as_sf() %>% 
  st_transform(3006)

bb_om = st_bbox(om_las) %>% 
  st_as_sfc() %>% 
  st_as_sf()%>% 
  st_transform(3006)

pine_vx = crop( rast("D:/viltfoder_kartor_data/final_maps/SND_v2/raw_maps/pine.tif"), bb_vx, mask=T)
pine_om = crop( rast("D:/viltfoder_kartor_data/final_maps/SND_v2/raw_maps/pine.tif"), bb_om, mask=T)

UD_aligned = project(UD_VX_FULL, pine_vx)

stk = c(UD_aligned, pine_vx,UD_aligned, UD_aligned)

names(stk) = c("ud_time", "n_stems", "ud_full", "year")
values(stk$year) = as.factor(2012)

stk = crop(stk, st_bbox(UD_VX_FULL) %>% st_as_sfc %>% st_as_sf %>% st_transform(3006))
stk %>% plot

stk2 = stk
stk2[project(vx_las$`2014_zq95`, stk) < 4] <- NA
stk2$n_stems = (stk2$n_stems * 12 )

stk2[project(vx_las$`2014_zq95`, stk) < 4] <- NA


stk3 = crop(stk2,stk2 %>% st_bbox() %>% 
  st_as_sfc %>% 
  st_centroid() %>% 
  st_buffer(3000))

library(patchwork)

pred <- predict(
  stk3,
  mod13,
  type="response",
  re.form = NA
)

zi <- predict(
  stk3,
  mod13,
  type="zprob",
  re.form = NA
)


(as.data.frame(stk3$n_stems, xy=T) %>% 
    ggplot(aes(x,y,fill=n_stems)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(na.value="transparent") | 
    
    as.data.frame(stk3$ud_time, xy=T) %>% 
    ggplot(aes(x,y,fill=ud_time)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(na.value="transparent") ) /
(as.data.frame(pred, xy=T) %>% 
    ggplot(aes(x,y,fill=lyr1)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(na.value="transparent", 
                         name="predicted damage proportion") |
    
    as.data.frame(zi, xy=T) %>% 
    ggplot(aes(x,y,fill=lyr1)) +
    theme_bw() +
    geom_tile() +
    scale_fill_viridis_c(na.value="transparent"))

