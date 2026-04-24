library(amt)
library(terra)
library(sf)
library(furrr)
library(ggh4x)

try(dir.create("01_data/simulation_outputs/"))
try(dir.create("01_data/simulation_inputs/"))

# generate a hypthetical moose

# load and modify landscape

try({
  r = c(rast("C:/Users/lugf0001/Documents/browsing_damage/mdbd_sim/01_data/tmp/stack_VX_tmp.tif")$'2012_ras_forest_edge_distance',
        rast("C:/Users/lugf0001/Documents/browsing_damage/mdbd_sim/01_data/tmp/stack_VX_tmp.tif")$'2012_canopy_cover')
  
  names(r) = c("dist_forest_edge",
               "canopy_cover")
  writeRaster(r, "01_data/simulation_inputs/landscape.tif")
  
})


#init pars

NULL_moose = make_issf_model(coefs = c(sl_ = 0.21, 
                                       ta_ = 0.02,
                                       dist_forest_edge_end = -.2),
                          sl = make_gamma_distr(1),
                          ta = make_vonmises_distr())

bilberry_moose = make_issf_model(coefs = c(sl_ = 0.21, 
                                       ta_ = 0.02,
                                       dist_forest_edge_end = -.2,
                                       canopy_cover_end = 0.3),
                                 sl = make_gamma_distr(1),
                                 ta = make_vonmises_distr())



plan("multisession",
     workers=5)

res1  = future_map(1:10, 
           function(x) {
             
          coords = st_coordinates(st_centroid(st_as_sfc(st_bbox(rast("01_data/simulation_inputs/landscape.tif")))), xy=T) %>% 
          as.data.frame()
           
           start = make_start(x = c(coords$X, coords$Y), 
                              dt = hours(3), time = Sys.time())
           
           redistribution = redistribution_kernel(NULL_moose, 
                                                  map = rast("01_data/simulation_inputs/landscape.tif"), 
                                                  start = start,
                                                  n.control = 10)
           
           null_results = simulate_path(redistribution,
                                        n.steps = 100) %>% 
             mutate(run = x)
           },
           .progress = T)


res2  = future_map(1:10, 
                   function(x) {
                     
                     coords = st_coordinates(st_centroid(st_as_sfc(st_bbox(rast("01_data/simulation_inputs/landscape.tif")))), xy=T) %>% 
                       as.data.frame()
                     
                     start = make_start(x = c(coords$X, coords$Y), 
                                        dt = hours(3), time = Sys.time())
                     
                     redistribution = redistribution_kernel(bilberry_moose, 
                                                            map = rast("01_data/simulation_inputs/landscape.tif"), 
                                                            start = start,
                                                            n.control = 10)
                     
                     null_results = simulate_path(redistribution,
                                                  n.steps = 100) %>% 
                       mutate(run = x)
                   },
                   .progress = T)


bind_rows(res1) %>% 
  ggplot(aes(x_, y_, color=as.factor(run))) +
  #geom_spatraster(data = rast("01_data/simulation_inputs/landscape.tif")$canopy_cover) +
  geom_pointpath()

bind_rows(res2) %>% 
  ggplot(aes(x_, y_, color=as.factor(run))) +
  #geom_spatraster(data = rast("01_data/simulation_inputs/landscape.tif")$canopy_cover) +
  geom_pointpath()
