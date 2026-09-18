## figures from different chapters


library(amt)
library(tidyverse)
library(vroom)
library(data.table)
library(terra)
library(sf)
library(ggh4x)
library(tidyterra)
library(momentuHMM)
library(corrplot)
library(lubridate)
library(MetBrewer)
library(cowplot)

library(showtext)
library(ggplot2)
library(extrafont)

font_import()

loadfonts()
font_add_google("EB Garamond", "EB Garamond")
showtext_auto()
#extrafont::font_import()
extrafont::loadfonts(device = "pdf")
set_null_device(cairo_pdf)

showtext_auto()
theme_set(theme(text = element_text(family="EB Garamond")))
set.seed(420)

unspecific= read_rds( "01_data/dat_from_chapter_III/data//unspecific_ISSA.rds")
hmm_iSSA= read_rds( "01_data/dat_from_chapter_III/data//hmm_ISSA.rds")


classes = read.csv( "D:/viltfoder_kartor_data/phd_project/forage_RS/02_data/nmd/classes_lukas.csv",sep= ";") %>% 
  rename(nmd_end = nmd) %>% 
  mutate(open = ifelse(class_new %in% c(1,2,3,4), "forest", "open"))



hmm_iSSA$class <- sapply(hmm_iSSA$steps, function(x) class(x)[1])
hmm_iSSA = hmm_iSSA %>% filter(class!="try-error") %>% 
  dplyr::select(-class)
for(i in 1:nrow(hmm_iSSA)) {
  hmm_iSSA$steps[[i]] =  hmm_iSSA$steps[[i]] %>% left_join(classes) %>% mutate(
    open = ifelse(open=="forest" & zq95_7m_end<=4,"young", open)
  )
}

unspecific$class <- sapply(unspecific$steps, function(x) class(x)[1])
unspecific = unspecific %>% filter(class!="try-error") %>% 
  dplyr::select(-class)
for(i in 1:nrow(unspecific)) {
  unspecific$steps[[i]] =  unspecific$steps[[i]] %>% left_join(classes) %>% mutate(
    open = ifelse(open=="forest" & zq95_7m_end<=4,"young", open)
  )
}

f1_ = case_~sl_+log(sl_) + cos(ta_)+ strata(step_id_)
f2_ = case_~sl_+log(sl_) + cos(ta_)+ strata(step_id_) + birch_end + pine_end + I(aspen_end + oak_end + rowan_end + willow_end)
f3_ = case_~sl_+log(sl_) + cos(ta_)+ strata(step_id_) + open
f4_ = case_~sl_+log(sl_) + cos(ta_)+ strata(step_id_) + birch_end + pine_end + I(aspen_end + oak_end + rowan_end + willow_end) + open

hmm_iSSA = hmm_iSSA %>%
  mutate(steps = map(steps, ~ .x %>% mutate_if(is.numeric, ~ replace(., is.na(.), 0))))

unspecific = unspecific %>%
  mutate(steps = map(steps, ~ .x %>% mutate_if(is.numeric, ~ replace(., is.na(.), 0))))


full_join(hmm_iSSA, 
          unspecific %>% 
            mutate(state_3 = 0)) %>% 
  mutate(steps = map(steps, function(x) {
    x %>% filter(case_==T)
  })) %>% 
  mutate(steps = map_int(steps, nrow)) %>% 
  dplyr::select(-data) %>% 
  group_by(state_3, season) %>% 
  summarize(mean=mean(steps),
            sd=sd(steps)) %>% 
  mutate(number = paste0(round(mean, digits=1), " +- ", round(sd, digits=1), " SD")) %>% 
  dplyr::select(-mean, -sd) %>% 
  pivot_wider(values_from = number, names_from = state_3) %>% sjPlot::tab_df()

hmm_mods = full_join(hmm_iSSA, 
                     unspecific %>% 
                       mutate(state_3 = 0)) %>% 
  mutate(models = map(steps, .progress = T, function(x) {
    
    list = try(list(
      f1_ = x %>% fit_clogit(f1_, model=T),
      f2_ = x %>% fit_clogit(f2_, model=T),
      f3_ = x %>% fit_clogit(f3_, model=T),
      f4_ = x %>% fit_clogit(f4_, model=T)))
    
    # of any is not converging, return no model
    list[[4]] = ifelse(any(is.infinite(summary(list[[4]])$conf.int)), NA, list[[4]])
    list[[1]] = ifelse(any(is.infinite(summary(list[[1]])$conf.int)), NA, list[[1]])
    list[[2]] = ifelse(any(is.infinite(summary(list[[2]])$conf.int)), NA, list[[2]])
    list[[3]] = ifelse(any(is.infinite(summary(list[[3]])$conf.int)), NA, list[[3]])
    
    return(list)
    
  })) %>% 
  
  mutate(issf = map(models, .progress = T, function(x) {
    try(list(f1_ = x[[1]][[1]] %>% AIC,
             f2_ = x[[2]][[1]] %>% AIC,
             f3_ = x[[3]][[1]] %>% AIC,
             f4_ = x[[4]][[1]] %>% AIC) %>% 
          as.data.frame())
  })) 

hmm_mods = hmm_mods %>% 
  mutate(issf = map(models, .progress = T, function(x) {
    try(list(f1_ = x[[1]][[1]] %>% AIC,
             f2_ = x[[2]][[1]] %>% AIC,
             f3_ = x[[3]][[1]] %>% AIC,
             f4_ = x[[4]][[1]] %>% AIC) %>% 
          as.data.frame())
  })) 

hmm_mods$class <- sapply(hmm_mods$issf, function(x) class(x)[1])
hmm_mods = hmm_mods %>% filter(class!="try-error") %>% 
  dplyr::select(-class)

names = tibble(name = c("f1_", "f2_", "f3_", "f4_"),
               name_meaning = c("1movement only", "2forage", "3forest", "4forage + forest"))

names = tibble(name = c("f1_", "f2_", "f3_", "f4_"),
               name_meaning = c("1movement only", "2forage", "3forest", "4forage + forest"))

hmm_mods  %>%# dplyr::select(-data, -steps)%>%
  unnest(issf  ) %>% 
  pivot_longer(cols = contains("f")) %>% 
  group_by(ID, season, state_3) %>% 
  mutate(AIC=value-min(value),
         AIC=ifelse(AIC<=2, 0, AIC),
         sumAIC=ifelse(AIC==0, 1, 0)) %>% 
  ungroup() %>% 
  group_by(name, season, state_3) %>% 
  summarize(sum = sum(sumAIC)) %>% 
  ungroup() %>% group_by(state_3, season) %>% 
  mutate(n = sum(sum)) %>% 
  mutate(sum2=paste0(100*round(sum/n, digits=3), "%")) %>% 
  mutate(sumcolor=100*round(sum/n, digits=3)) %>% 
  mutate(state_3 = case_when(
    state_3 == 0 ~ "behavior unspecific",
    state_3 == 1 ~ "state encamped",
    state_3 == 2 ~ "state foraging",
    state_3 == 3 ~ "state travelling")) %>% 
  left_join(names) %>% 
  ungroup() %>%
  complete(season, nesting(name_meaning, state_3)) %>% 
  ggplot(aes(x=name_meaning, 
             y=season, fill=sumcolor)) +
  scale_x_discrete(name = "models",
                   labels =  c("movement only", "forage", "landcover", "forage + landcover"))+
  geom_raster() +
  geom_text(aes(label=sum2))+
  facet_wrap(~paste0(state_3)) +
  ggthemes::theme_pander(base_size = 16)+
  scale_fill_gradient2(low = "beige", 
                       na.value="beige",
                       #high= "#d49d06", 
                       high ="orange",
                       name="proportion selected model in %",
                       breaks=c(0,10,20,30,40,50,60,70,80,90,100)) +
  theme(legend.position = "bottom",
        legend.title.position = "top",
        legend.key.width = unit(dev.size()[1] / 5, "cm"),
        legend.justification = "left",
        text= element_text(family="EB Garamond")) +
  xlab("model") + ylab("season") 

cowplot::save_plot("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/HMM_model_selection.svg",
                      device = "svg",
                      dpi=600, plot=last_plot(),
                      scale=1.25,
                      base_width = 8, base_height = 8)

magick::image_write(magick::image_convert(magick::image_read("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/HMM_model_selection.svg"),
                      format = "png"),
            "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/HMM_model_selection.png")

coefs=hmm_mods %>% 
  mutate(coefs = map(models, function(x) {
    broom::tidy(x[[1]][[1]]) %>% mutate(name="f1_") %>% 
      bind_rows(broom::tidy(x[[2]][[1]]) %>% mutate(name="f2_"))%>% 
      bind_rows(broom::tidy(x[[3]][[1]]) %>% mutate(name="f3_"))%>% 
      bind_rows(broom::tidy(x[[4]][[1]]) %>% mutate(name="f4_"))
  }, .progress = T))


boot_mean <- function(x, n_boot = 500) {
  # Simple bootstrap: resample x with replacement, compute mean each time
  boot_means <- replicate(n_boot, mean(sample(x, replace = TRUE)))
  tibble(
    boot_mean = mean(boot_means),
    boot_sd = var(boot_means),
    boot_lwr = mean(boot_means) - 1.96 * sd(boot_means),
    boot_upr = mean(boot_means) + 1.96 * sd(boot_means)
  )
}

n_boot =500;option="Hokusai1";hmm_mods %>% 
  mutate(coefs = map(models, function(x) {
    broom::tidy(x[[1]][[1]]) %>% mutate(name="f1_") %>% 
      bind_rows(broom::tidy(x[[2]][[1]]) %>% mutate(name="f2_"))%>% 
      bind_rows(broom::tidy(x[[3]][[1]]) %>% mutate(name="f3_"))%>% 
      bind_rows(broom::tidy(x[[4]][[1]]) %>% mutate(name="f4_"))
  }))  %>% 
  filter(state_3 %in% c(2,0)) %>% 
  mutate(state_3 = case_when(state_3 == 1 ~ "encamped",
                             state_3 == 2 ~ "foraging",
                             state_3 == 0 ~ "behavior unspecific",
                             state_3 == 3 ~ "travelling"))%>% 
  unnest(coefs) %>%
  filter(name == "f4_") %>%
  filter(term %in% c("pine_end", "openyoung")) %>%
  select(-issf, -models, -std.error, -statistic, -p.value) %>%
  pivot_wider(values_from = estimate, names_from = term) %>%
  group_by(season, state_3) %>%
  # bootstrap the coefficient pairs
  summarise(
    boot = list({
      replicate(n_boot, {
        boot_sample <- slice_sample(cur_data(), replace = TRUE)
        tibble(
          pine_availability = seq(0, 2, 0.05),
          logRSS = (mean(boot_sample$openyoung) +mean(boot_sample$pine_end) * seq(0, 4, 0.1))-mean(boot_sample$openyoung),
          mean_rss_open = mean(boot_sample$openyoung)
        )
      }, simplify = FALSE)
    }),
    .groups = "drop"
  ) %>%
  # unpack bootstrap list into tidy data
  mutate(boot = map(boot, bind_rows, .id = "boot_id")) %>%
  unnest(boot) %>%
  group_by(season, state_3, pine_availability) %>%
  summarise(
    mean_open = mean(mean_rss_open),
    mean_logRSS = mean(logRSS),
    
    sd_logRSS = sd(logRSS),
    lwr = (mean_logRSS - sd_logRSS),
    upr = (mean_logRSS + sd_logRSS),
    .groups = "drop"
  ) %>% 
  ggplot(aes(x=pine_availability, y=mean_logRSS , color=season)) +
  geom_ribbon(aes(ymin = lwr, ymax=upr), alpha=.5) +
  geom_line() +
  geom_hline(yintercept = 0, linetype="dashed", size=.7) +
  facet_grid(vars(season), vars(state_3), scales="free") +
  ggthemes::theme_pander(base_size = 16) +
  ylab("mean log-RSS") +xlab(expression(paste("pixel level pine availability")))+
  MetBrewer::scale_color_met_d(name=option)+
  MetBrewer::scale_fill_met_d(name=option)+
  theme(legend.position = "bottom",
        text=element_text(family="EB Garamond"))

cowplot::save_plot("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/incremental_selection_young_forest.svg",
                   device = "svg",
                   dpi=600, plot=last_plot(),
                   scale=1.25,
                   base_width = 5, base_height = 8)

magick::image_write(magick::image_convert(magick::image_read("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/incremental_selection_young_forest.svg"),
                                          format = "png"),
                    "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/incremental_selection_young_forest.png")

###

names = tibble(name = c("f1_", "f2_", "f3_", "f4_"),
               name_meaning = c("1movement only", "2forage", "3landcover", "4forage + landcover"))

coefs %>% dplyr::select(state_3, season, coefs) %>% 
  unnest() %>% 
  nest(-state_3, -season,-name, -term)%>% 
  mutate(iw = lapply(data, function(x) {
    
    mod <- lm(estimate ~ 1, data = x, weights = 1/(std.error)^2)
    
    return(mod)
  })) %>% 
  mutate(pred = lapply(iw, function(x) {
    
    pred <- predict(x, 
                    newdata = data.frame(dummy = NA),
                    se.fit = TRUE)
    
    est <- data.frame(mean = pred$fit,
                      lwr = pred$fit - 1.96 * pred$se.fit,
                      upr = pred$fit + 1.96 * pred$se.fit)
    
    return(est)
  })) %>% 
  unnest(pred) %>% 
  filter(!term %in% c("sl_", "log(sl_)","cos(ta_)", "willow_end")) %>% 
  left_join(names) %>% 
  mutate(term=str_remove(term, "as.factor"),
         term= str_remove(term,"_end"),
         
         term=str_replace(term, "(open)",""),
         term= str_replace(term,"()open", "open")) %>% 
  mutate(state_3 = case_when(state_3 == 1 ~ "encamped",
                             state_3 == 2 ~ "foraging",
                             state_3 == 0 ~ "behavior unspecific",
                             state_3 == 3 ~ "travelling")) %>% 
  #mutate(term = ifelse(str_detect(term, "open"), substr(term, 3, 6), term)) %>% 
  #mutate(term = ifelse(str_detect(term, "young"), substr(term, 3, 7), term)) %>% 
  mutate(sub_facet = ifelse(term %in% c("aspen","rowan","oak","pine","willow","birch"), "forage", "forest")) %>% 
  filter(!str_detect(term, "I\\(")) %>%
  filter(sub_facet=="forest") %>% 
  ggplot(aes(x=term, y=mean, ymin=lwr, ymax=upr, color=season, shape=name_meaning)) +
  geom_pointrange(position = position_dodge2(width=.75)) +
  ggh4x::facet_nested_wrap(vars(state_3), ncol=2, nrow=4, scales="free") +
  ggthemes::theme_pander(base_size = 16) +
  MetBrewer::scale_color_met_d(name=option) +
  scale_shape(
    labels=c("landcover", "forage + landcover"),
    name="model"
  ) +
  geom_hline(yintercept = 0, linetype="dashed") +
  guides(color = guide_legend(nrow=2, title="season"),
         shape = guide_legend(nrow=2))+
  theme(legend.position = "bottom",
        legend.justification = "left",
        text=element_text(family="EB Garamond")) + ylab("log-RSS") + xlab("")

cowplot::save_plot("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/nuanced_selection_youngforest.svg",
                   device = "svg",
                   dpi=300, plot=last_plot(),
                   scale=1.25,
                   base_width = 8, base_height = 8)

magick::image_write(magick::image_convert(magick::image_read("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/nuanced_selection_youngforest.svg"),
                                          format = "png"),
                    "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/nuanced_selection_youngforest.png")



### Chapter IV


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

## add quick check how many moose have been removed

issf %>% 
  select(OBJECT_ID, year) %>% 
  pull(OBJECT_ID) %>% unique() %>% length


init_dat = fread("C:/Users/lugf0001/Documents/mdbd_sim/01_data/OM_20200827.txt")%>%  #%>% filter(str_detect(Sex, "F")) %>% #filter for females
  dplyr::rename(x = LOCALE_E, y = LOCALE_N) %>%  # rename columns for convenience
  mutate(ts = lubridate::ymd_hms(GMT_DATE), # time manipulations
         year = lubridate::year(ts),
         month = lubridate::month(ts), # add hunt_year variable
         juliandate = lubridate::yday(ts)) %>%  # add juliandate
  filter(month %in% c(5,6,7,8,9,10,11,12,1,2)) %>% 
  mutate(year = ifelse(month%in%c(1,2), year-1, year)) %>% 
  filter(year %in% c(2010)) %>% tibble %>% 
  full_join(fread("C:/Users/lugf0001/Documents/mdbd_sim/01_data/VX_20200827.txt") %>%  #%>% filter(str_detect(Sex, "F")) %>% #filter for females
              dplyr::select(-DOP, -Sats_used, -minn,- PubName) %>%  # remove unused columns
              dplyr::rename(x = LOCALE_E, y = LOCALE_N) %>%  # rename columns for convenience
              mutate(ts = lubridate::ymd_hms(GMT_DATE), # time manipulations
                     year = lubridate::year(ts),
                     month = lubridate::month(ts), # add hunt_year variable
                     juliandate = lubridate::yday(ts)) %>%  # add juliandate
              filter(month %in% c(5,6,7,8,9,10,11,12,1,2)) %>% 
              mutate(year = ifelse(month%in%c(1,2), year-1, year)) %>% 
              filter(year %in% c(2010)) %>% tibble) %>% 
  select(OBJECT_ID, year) %>% 
  distinct()

init_dat2 = init_dat %>% 
  filter(OBJECT_ID%in%issf$OBJECT_ID) %>% 
  filter(year!=2014)

issf %>% 
  select(OBJECT_ID, year) %>% 
  group_by(OBJECT_ID) %>% 
  filter(year == min(year)) %>% 
  filter(OBJECT_ID %in% init_dat2$OBJECT_ID)

## plot-level data

rm(issf_vx, issf_om)

issf = issf %>% 
  select(-tidy_res, -n_winter, -n_summer,-data) %>% 
  #left_join(age) %>% mutate(age=year-YearOfBirth) %>% 
  #left_join(life_history) %>% 
  #mutate(n_calves = SumOfBeforeHunt) %>% 
  select(-contains("SumOf")) 

# browsing _damage_
# try new data from F...????? maaaan
locale=locale(encoding="latin1")


dat = vroom::vroom("C:/Users/lugf0001/Documents/mdbd_sim/01_data/abin_new_data_2025/abin_OM.csv", delim = ";",  col_names = T,locale =  vroom::locale(encoding = "UTF-8") ) %>% 
  janitor::clean_names() %>% 
  filter(tolower(trakt)!="sum") %>%   janitor::clean_names()  %>% 
  select(-rojt) %>% 
  full_join(vroom::vroom("C:/Users/lugf0001/Documents/mdbd_sim/01_data/abin_new_data_2025/abin_vx.csv", delim = ";",  col_names = T,locale =  vroom::locale(encoding = "UTF-8") )%>% 
              filter(tolower(Yta)!="sum") %>% rename(Trakt = Yta)  %>%
              mutate(Datum = as.character(Datum)) %>%  janitor::clean_names()) %>% 
  select(-rojt) %>% 
  select(-bestand) %>% 
  filter(!is.na(ost)) %>% 
  rename(half_height = hojd_m) %>% 
  rename(area = omrade) %>% 
  mutate(half_height = as.numeric(gsub("[,]", ".", half_height))) %>% 
  rowwise() %>% 
  mutate(n_stems = sum(t_oskadade, t_topp, t_brott, t_bark)) %>% 
  filter(n_stems !=0) %>% 
  ungroup() %>% 
  mutate(year = substr(datum, 1,4)) 


dat = dat %>% 
  nest(-area, -inventerare)

# now go into EVERY SINGLE ONE AND FIGURE OUT THE FUCKING COORDS...
#1 is in 3021
#2 is in 3021
#3 is in 3021
#4 IS NOT and is in 3006
#5 is in 3006
#6 is in 3006
#7 is is in 4326 and needs to ahve coordinates divided by 100000
#8 is in 3006
#9 is in 3021
#10 is in 3021
#11 is in 3021
#12 is in 3006
# dat$data[[7]] %>% 
#   mutate(ost = ost/100000,
#          nord = nord/100000) %>% 
#   mk_track(ost, nord, crs=4326 , all_cols = T) %>% 
#   amt::as_sf() %>% 
#   ggplot()+
#   geom_sf(data=st_read("C:/Users/lugf0001/Documents/mdbd_sim//01_data/shapes/mma_data/Stratum2024.shp") %>% 
#             st_transform(3021)) +
#   geom_sf()
# 
# dat$data[[4]] %>% 
#   mk_track(ost, nord, crs=3006 , all_cols = T) %>% 
#   amt::as_sf() %>% 
#   ggplot()+
#   geom_sf(data=st_read("C:/Users/lugf0001/Documents/mdbd_sim/01_data/shapes/mma_data/Stratum2024.shp") %>% 
#             st_transform(3021)) +
#   geom_sf()

dat$data[[1]] = dat$data[[1]] %>% 
  mk_track(ost, nord, crs=3021 , all_cols = T) 

dat$data[[2]] = dat$data[[2]] %>% 
  mk_track(ost, nord, crs=3021 , all_cols = T) 

dat$data[[3]] = dat$data[[3]] %>% 
  mk_track(ost, nord, crs=3021 , all_cols = T) 
dat$data[[9]] = dat$data[[9]] %>% 
  mk_track(ost, nord, crs=3021 , all_cols = T) 
dat$data[[10]] = dat$data[[10]] %>% 
  mk_track(ost, nord, crs=3021 , all_cols = T) 
dat$data[[11]] = dat$data[[11]] %>% 
  mk_track(ost, nord, crs=3021 , all_cols = T) 

dat$data[[4]] = dat$data[[4]] %>% 
  mk_track(ost, nord, crs=3006 , all_cols = T) %>% 
  transform_coords(3021)
dat$data[[5]] = dat$data[[5]] %>% 
  mk_track(ost, nord, crs=3006 , all_cols = T) %>% 
  transform_coords(3021)
dat$data[[6]] = dat$data[[6]] %>% 
  mk_track(ost, nord, crs=3006 , all_cols = T) %>% 
  transform_coords(3021)
dat$data[[8]] = dat$data[[8]] %>% 
  mk_track(ost, nord, crs=3006 , all_cols = T) %>% 
  transform_coords(3021)
dat$data[[12]] = dat$data[[12]] %>% 
  mk_track(ost, nord, crs=3006 , all_cols = T) %>% 
  transform_coords(3021)

dat$data[[7]] = dat$data[[7]] %>% 
  mutate(ost = ost/100000,
         nord = nord/100000) %>% 
  mk_track(ost, nord, crs=4326 , all_cols = T) %>% 
  transform_coords(3021)


abin = dat  %>% 
  filter(inventerare!="Sonya Juthberg") %>% 
  #filter(!str_detect(inventerare, "ån")) %>% 
  #filter(!str_detect(inventerare, "ska")) %>% 
  
  unnest(data) %>%
  mk_track(x_, y_, crs=3021, all_cols = T)%>% 
  amt::as_sf()%>% 
  # extract temporally unfiltered general population uds # Meta Moose
  
  janitor::clean_names() %>% 
  mutate(pine_damage_binary = ifelse(t_oskadade==0, 1, 0)) %>%
  mutate(pine_damage_binary_top = ifelse(t_topp==0, 0, 1)) %>% 
  mutate(area = ifelse(str_detect(area,"ster"), "ÖsterMalma", "Växjö"),
         Area = replace_na(area, "Växjö")) %>% 
  mutate(id = paste0(year, trakt, area)) 

# abin prob needs year col


library(boot)
library(future)
library(furrr)

### bootstrapping part
vars = c("canopy_cover_end",
         "zq95_end", "small_tree_cover_end", 
         "forest_edge_end",
         "dist_roads_end", "dist_water_end", 
         "ndvi_end", "visit_frequency")

labels = c("canopy cover",
           "canopy height", "small tree cover", 
           "distance to forest edge","distance to roads", "distance to water", 
           "NDVI","occurence distribution")
#

plan("sequential")
plan("multisession", workers=2)
tictoc::tic()

n.boot =500

area_list =list()
rss_plots_diff = list()

for(j in 1:length(vars)) {
  
  #first calculate the RSS values for the plots in the span of each day
  for(i in 1:nrow(issf)) {
    
    cat("\r", i)
    
    dat =tibble(sl_ = 50, 
                ta_ = 0,
                canopy_cover_end = 100,
                forest_edge_end = 100,
                dist_roads_end = 250,
                ndvi_end = 0.7, 
                zq95_end = 25,
                step_id_ = issf$ssf_winter[[i]]$model$model$`strata(step_id_)`[[1]],
                area = issf$area[[i]],
                year = issf$year[[i]],
                small_tree_cover_end = 100,
                dist_water_end = 100,
                visit_frequency = 25,
                OBJECT_ID = issf$OBJECT_ID[[i]]) %>% 
      crossing(st = seq(from = 0, to= 2*pi, length.out = 240)) %>% 
      mutate(time_of_day=case_when(between(st,.25*pi,.75*pi) ~ "dawn",
                                   between(st,1.25*pi,1.75*pi) ~ "dusk")) 
    
    x1 = predict(issf$ssf_winter[[i]]$model,dat )
    x2 = predict(issf$ssf_winter[[i]]$model,  dat %>% mutate_at(vars(matches(vars[j])), ~ 0.35) )
    
    rss_plots_diff[[i]] = dat %>% mutate(rss = x1-x2)
    
  }
  #how many boots?
  
  boot_df = bind_rows(rss_plots_diff) 
  
  ## first do morning
  IDs = boot_df %>% 
    
    pull(OBJECT_ID) %>% 
    unique()
  
  # apply this n.boot times
  prefer = future_map(1:n.boot, function(x) {
    
    #sample which individuals (with replacement)
    animals <- sample(unique(IDs), replace = TRUE)
    
    ID = animals
    #year = substr(animals,  nchar(animals)-3, nchar(animals))
    
    #this list stores the individual coefs
    list = list()
    for(k in 1:length(animals)) {
      # get individual coefficients and store them in a list
      list[[k]] = boot_df %>%
        filter(OBJECT_ID==ID[k]) %>%  
        #filter(year == year[k]) %>% 
        dplyr::select(rss, area, st, OBJECT_ID, year) 
      cat("\r", k)
    }
    
    
    # return the mean and the run number of each boot for each coef
    bind_rows(list) %>% 
      ungroup() %>% 
      summarise(rss=mean(rss), 
                .by = c("area", 
                        "st")) %>% 
      mutate(run = x)
    
  }, .progress = T)
  
  area_list[[j]] = prefer  %>% 
    bind_rows() %>% 
    group_by(area, st) %>% 
    summarize(upr = mean(rss)+1.96*sd(rss),
              lwr = mean(rss)-1.96*sd(rss),
              boundup =ifelse( max(rss) < 0, 0.2, quantile(rss, 1)),
              boundlow =ifelse( min(rss) > 0, -0.2, quantile(rss, 0)),
              upr95 = quantile(rss, .975),
              lwr95 = quantile(rss,.025),
              rss = mean(rss)) %>% 
    ggplot(aes(x=st, y= (rss), color=area)) +
    geom_rect(aes(xmin = 0, xmax = .5*pi,
                  ymin = boundlow, ymax=boundup),
              inherit.aes = F, fill="grey50", alpha=.5) +
    geom_rect(aes(xmin = 1.5*pi, xmax = 2*pi,
                  ymin = boundlow, ymax=boundup),
              inherit.aes = F, fill="grey50", alpha=.5)+
    geom_hline(yintercept = 0, linetype="dashed")+
    geom_line(data = prefer  %>% 
                bind_rows(), aes(group=paste0(area, run)), size=.2, alpha=.1) +
    geom_line(size=1.4) +
    scale_color_manual(values=c("#2C3E50", "#C27D38"),
                       labels=c("Öster Malma", "Växjö")) +
    theme_bw() +
    theme(legend.position = ifelse(j==8, "bottom", "none"),
          legend.title.position = "top",
          text = element_text(size = 8,
                              family="EB Garamond"),
          legend.key.width = unit(2, "cm")) + 
    scale_x_continuous(breaks=c(0, .5*pi, pi, 1.5*pi, 2*pi),
                       labels = c("midnight", "dawn", "noon", "dusk", "midnight")) +
    xlab("suntime") +
    ylab(paste0("rss for ", labels[j])) 
}

area_list_save = area_list

area_list = area_list_save

area_list[[9]] = get_legend(ggplot(prefer[[1]], aes(x = st, rss, color=area)) + geom_line() +
                              theme_bw(base_family="EB Garamond",
                                       base_size=17) +
                              scale_color_manual(values=c("#2C3E50", "#C27D38"),
                                                 labels=c("Öster Malma", "Växjö")))
area_list[[8]] = area_list[[8]]+theme(legend.position = "none")

plot_grid(plotlist = area_list, ncol=3, nrow=3)

ggsave(plot=last_plot(),
       filename = "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/rss_diel_pattern.svg",
       device = "svg",
       dpi=600, 
       scale=1.25,
       width = 8, height = 8)

magick::image_write(magick::image_convert(magick::image_read("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/rss_diel_pattern.svg"),
                                          format = "png"),
                    "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/rss_diel_pattern.png")




library(glmmTMB)
library(tidyverse)
library(gghalves)
library(performance)
library(DHARMa)

# restart here

model_dat3 = read.csv("C:/Users/lugf0001/Documents/mdbd_sim/01_data/model_dat/modelling_data.csv") %>% 
  mutate(year = as.factor(year)) %>% 
  rename(ud_time = ud) %>% 
  mutate(ud_time = ud_time*10000,
         ud_full = ud_full*10000) %>% 
  mutate(y= prop_damage3)

# Null model
tictoc::tic()
mod0 <- glmmTMB(
  y~ 1 +(1|id),
  ziformula = ~1,
  family = beta_family(link = "logit"),
  data = model_dat3,
)

mod1 <- update(mod0, ziformula = ~ area +(1|id))
mod2<- update(mod0, ziformula = ~ year+(1|id))
mod3 <- update(mod0, ziformula = ~ year + scale(log(n_stems))+(1|id))
mod4 <- update(mod0, ziformula = ~ year + scale(ud_full)+(1|id))
mod5 <- update(mod0, ziformula = ~ year + scale(ud_time)+(1|id))

mod13 <- update(mod4, ~ scale(log(n_stems)) * scale(ud_time) + (1 | id))
mod14 <- update(mod4, ~ scale(log(n_stems)) * scale(ud_full) + (1 | id))


library(patchwork)
library(ggeffects)


ggpredict(mod13, terms = c("n_stems [all]"), type = "fixed", bias_correction = F) %>% 
  tibble() %>% 
  
  ggplot(aes(x=x, y=predicted, fill=group)) + theme_bw() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=.6, color="black") +
  geom_line() +
  xlab("number of stems")+ 
  ylab("predicted proportion of damage")+
  scale_fill_manual(values=c("grey20"),
                    name="number of stems",
                    labels=c("1 pine stem", "3 pine stems")) +
  theme(legend.position = "bottom",
        text=element_text(family="EB Garamond")) +
  guides(fill="none") +
  ylim(c(0,1)) +
  ggpredict(mod13, terms = c("ud_time [all]"), type = "fixed", bias_correction = F) %>% 
  tibble() %>% 
  
  ggplot(aes(x=x/10000, y=predicted, fill=group)) + theme_bw() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=.6, color="black") +
  geom_line() +
  ylim(c(0,1)) +
  
  xlab(expression("predicted UD"["time"]))+ 
  ylab("predicted proportion of damage")+
  scale_fill_manual(values=c("grey20"),
                    name="number of stems",
                    labels=c("1 pine stem", "3 pine stems")) +
  guides(fill="none") +
  theme(legend.position = "bottom",
        text=element_text(family="EB Garamond"))+
  
  ggpredict(mod13, terms = c("ud_time [all]", "n_stems [1,5]"), type = "fixed", bias_correction = F) %>% 
  tibble() %>% 
  
  ggplot(aes(x=x/10000, y=predicted, fill=group)) + theme_bw() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=.6, color="black") +
  geom_line() +
  ylim(c(0,1))+
  xlab(expression("predicted UD"["time"]))+ 
  ylab("predicted proportion of damage")+
  scale_fill_manual(values=c("#2C3E50", "#C27D38"),
                    name="number of stems",
                    labels=c("1 pine stem", "5 pine stems")) +
  theme(legend.position = "bottom",
        text=element_text(family="EB Garamond")) +
  ggpredict(mod13, terms = c("ud_full [all]"), type = "zi_prob") %>% 
  tibble() %>% 
  ggplot(aes(x=x/10000, y=predicted, fill=group)) + theme_bw() +
  geom_ribbon(aes(ymin=conf.low, ymax=conf.high), alpha=.6, color="black") +
  geom_line() +
  ylim(c(0,1)) +
  #scale_y_reverse() +
  
  scale_x_continuous()+
  xlab(expression("predicted UD"["time"]))+ 
  ylab("predicted propability of zero damage")+
  scale_fill_manual(values=c("grey20"),
                    name="")+
  guides(fill="none") +
  theme(legend.position = "bottom",
        text=element_text(family="EB Garamond")) +
  patchwork::plot_annotation(tag_levels = "a",
                             tag_suffix = ")")

ggsave(plot=last_plot(),
       device = "svg",
       width = 8,
       height=8,
       dpi=600,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/model_chapter_IV.svg")

magick::image_write(magick::image_convert(magick::image_read("C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/model_chapter_IV.svg"),
                                          format = "png"),
                    "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/model_chapter_IV.png")

#### Chapter II


library(openxlsx)
library(tidyverse) #packageversion 1.3.0
library(smatr)  #packageversion 3.4.8
library(Metrics) #packageversion 0.1.4
library(betareg)
library(glmmTMB)
library(scales)
#library(sjPlot)
library(AICcmodavg)
library(diagis)
library(brms)
library(future)
library(posterior)
library(brms)
library(bayesplot)
library(ggridges)
library(cowplot)
library(tidybayes)
library(sf)
library(ggplot2)
library(ggridges)
library(dplyr)
library(gghalves)
library(patchwork)

setwd("C:/Users/lugf0001/Documents/maps-and-moose/")

joins = read.csv("01_data/test_train/train.csv") %>% 
  select(mma, ecological_zone, year) %>% distinct()

# 

list_models = list(weights = list(read_rds("01_data/models/calf_weights/NULL_model.rds")),
                   repro = list(read_rds("01_data/models/repro_rates/NULL_model.rds")),
                   proportion= list(read_rds("01_data/models/browsing_damage/damage_NULL.rds")))


test = read.csv("01_data/test_train/test.csv") %>% 
  full_join(read.csv("01_data/test_train/train.csv"))

## 
leg = get_plot_component(ggplot(test, aes(log_scaled_moose_dens, prop_of_adult_bulls, colour = ecological_zone))+ geom_smooth()+
                           scale_color_manual(values = c("lightblue3", "#01796F", "orange2", "#FF6F61"),
                                              breaks  = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", 
                                                          "Central-Boreal Zone", "Southern-Boreal Zone"),
                                              name="ecological zone")+theme(legend.position = "bottom")+
                           guides(color=guide_legend(nrow=2,byrow=TRUE)), "guide-box-bottom")


# Define desired credible interval probabilities
probs <- c(
  0.25, 0.75,   # 50%
  0.10, 0.90,   # 80%
  0.05, 0.95    # 90%
)

number = 1000
width = 12
height = 12

library(bayestestR)

prop = list_models$proportion[[1]]
weight = list_models$weights[[1]]
recr = list_models$repro[[1]]

m_dens = tibble(ecological_zone = test$ecological_zone,
                mma =  NA,
                year = NA,
                log_scaled_pine_dens = 0, 
                scaled_rase_density = 0,
                scaled_nr_days_with_snow = 0,  
                scaled_rase_density_squared = 0,
                scaled_nr_days_with_snow_squared = 0,
                scaled_rase_density_cubed = 0,
                scaled_nr_days_with_snow_cubed = 0,
                scaled_moose_equivalent = 0,
                scaled_moose_equivalent_squared = 0,
                scaled_moose_equivalent_cubed = 0) %>%
  crossing(log_scaled_moose_dens = test$log_scaled_moose_dens) %>% 
  left_join(tibble(log_scaled_moose_dens = test$log_scaled_moose_dens,
                   moose_dens = test$moose_dens) %>% 
              distinct()) %>% 
  #sample_frac(.1) %>% 
  add_epred_draws(prop, allow_new_levels=T, ndraws = number)%>%
  mutate(ecological_zone = factor(ecological_zone,
                                  levels = c(
                                    "Alpine-Northern Boreal Zone",
                                    "Central-Boreal Zone",
                                    "Southern-Boreal Zone",
                                    "Hemi-Boreal Zone"
                                    
                                  ))) %>% 
  ggplot(aes(x = moose_dens, 
             y = .epred, 
             color = ordered(ecological_zone))) +
  theme_bw()+ 
  ylab("expected damage %") + xlab("moose density") +
  stat_lineribbon(.width = c(0.5, 0.8, 0.90),
                  point_interval = "median_qi") +
  facet_wrap(~ecological_zone)+
  scale_color_manual(values = c("lightblue3", "#01796F", "orange2", "#FF6F61"),
                     breaks  = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", 
                                 "Hemi-Boreal Zone", "Southern-Boreal Zone"),
                     name="ecological zone") +
  scale_x_continuous(breaks = c(2,4,6,8,10,12))+
  scale_fill_manual(values=c("grey90","grey60", "grey50"))+ guides(fill = "none") +
  theme(text=element_text(family="EB Garamond"))


m_dens_w = tibble(ecological_zone = test$ecological_zone,
                  mma =  NA,
                  year = NA,
                  log_scaled_pine_dens = 0, 
                  scaled_rase_density = 0,
                  scaled_nr_days_with_snow = 0,  
                  scaled_rase_density_squared = 0,
                  scaled_nr_days_with_snow_squared = 0,
                  scaled_rase_density_cubed = 0,
                  scaled_nr_days_with_snow_cubed = 0,
                  scaled_moose_equivalent = 0,
                  scaled_moose_equivalent_squared = 0,
                  scaled_moose_equivalent_cubed = 0) %>%
  crossing(log_scaled_moose_dens = test$log_scaled_moose_dens) %>% 
  left_join(tibble(log_scaled_moose_dens = test$log_scaled_moose_dens,
                   moose_dens = test$moose_dens) %>% 
              distinct()) %>% 
  #sample_frac(.1) %>% 
  add_epred_draws(weight, allow_new_levels=T, ndraws = number)%>%
  mutate(ecological_zone = factor(ecological_zone,
                                  levels = c(
                                    "Alpine-Northern Boreal Zone",
                                    "Central-Boreal Zone",
                                    "Southern-Boreal Zone",
                                    "Hemi-Boreal Zone"
                                    
                                  ))) %>% 
  ggplot(aes(x = moose_dens, 
             y = .epred, 
             color = ordered(ecological_zone))) +
  theme_bw()+ 
  ylab("expected calf weight") + xlab("moose density") +
  stat_lineribbon(.width = c(0.5, 0.8, 0.90),
                  point_interval = "median_qi") +
  facet_wrap(~ecological_zone)+
  scale_color_manual(values = c("lightblue3", "#01796F", "orange2", "#FF6F61"),
                     breaks  = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", 
                                 "Hemi-Boreal Zone", "Southern-Boreal Zone"),
                     name="ecological zone") +
  scale_x_continuous(breaks = c(2,4,6,8,10,12))+
  scale_fill_manual(values=c("grey90","grey60", "grey50"))+ guides(fill = "none") +
  theme(text=element_text(family="EB Garamond"))


m_dens_r = tibble(ecological_zone = test$ecological_zone,
                  mma =  NA,
                  year = NA,
                  log_scaled_pine_dens = 0, 
                  scaled_rase_density = 0,
                  scaled_nr_days_with_snow = 0,  
                  scaled_rase_density_squared = 0,
                  scaled_nr_days_with_snow_squared = 0,
                  scaled_rase_density_cubed = 0,
                  scaled_nr_days_with_snow_cubed = 0,
                  scaled_moose_equivalent = 0,
                  scaled_moose_equivalent_squared = 0,
                  scaled_moose_equivalent_cubed = 0) %>%
  crossing(log_scaled_moose_dens = test$log_scaled_moose_dens) %>% 
  left_join(tibble(log_scaled_moose_dens = test$log_scaled_moose_dens,
                   moose_dens = test$moose_dens) %>% 
              distinct()) %>% 
  #sample_frac(.1) %>% 
  add_epred_draws(recr, allow_new_levels=T, ndraws = number)%>%
  mutate(ecological_zone = factor(ecological_zone,
                                  levels = c(
                                    "Alpine-Northern Boreal Zone",
                                    "Central-Boreal Zone",
                                    "Southern-Boreal Zone",
                                    "Hemi-Boreal Zone"
                                    
                                  ))) %>% 
  ggplot(aes(x = moose_dens, 
             y = .epred, 
             color = ordered(ecological_zone))) +
  theme_bw()+ 
  ylab("expected calf recruitment") + xlab("moose density") +
  stat_lineribbon(.width = c(0.5, 0.8, 0.90),
                  point_interval = "median_qi") +
  facet_wrap(~ecological_zone)+
  scale_color_manual(values = c("lightblue3", "#01796F", "orange2", "#FF6F61"),
                     breaks  = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", 
                                 "Hemi-Boreal Zone", "Southern-Boreal Zone"),
                     name="ecological zone") +
  scale_x_continuous(breaks = c(2,4,6,8,10,12))+
  scale_fill_manual(values=c("grey90","grey60", "grey50"))+ guides(fill = "none") +
  theme(text=element_text(family="EB Garamond"))

p_dens = tibble(ecological_zone = test$ecological_zone,
                mma =  NA,
                year = NA,
                log_scaled_moose_dens = 0, 
                scaled_rase_density = 0,
                scaled_rase_density_squared = 0,
                scaled_nr_days_with_snow = 0,
                scaled_nr_days_with_snow_squared = 0,
                scaled_rase_density_cubed = 0,
                scaled_nr_days_with_snow_cubed = 0,
                scaled_moose_equivalent = 0,
                scaled_moose_equivalent_squared = 0,
                scaled_moose_equivalent_cubed = 0) %>%
  crossing(log_scaled_pine_dens = test$log_scaled_pine_dens) %>%
  left_join(tibble(log_scaled_pine_dens = test$log_scaled_pine_dens,
                   pine_dens = test$pine_dens) %>% 
              distinct()) %>% 
  add_epred_draws(prop, allow_new_levels=T, ndraws = number)%>%
  mutate(ecological_zone = factor(ecological_zone,
                                  levels = c(
                                    "Alpine-Northern Boreal Zone",
                                    "Central-Boreal Zone",
                                    "Southern-Boreal Zone",
                                    "Hemi-Boreal Zone"
                                    
                                  ))) %>% 
  ggplot(aes(x = pine_dens, 
             y = .epred, 
             color = ordered(ecological_zone))) +
  theme_bw()+ 
  ylab("expected damage %") + xlab("pine density")  +
  stat_lineribbon(.width = c(0.5, 0.8, 0.90),
                  point_interval = "median_qi") +
  facet_wrap(~ecological_zone)+
  scale_color_manual(values = c("lightblue3", "#01796F", "orange2", "#FF6F61"),
                     breaks  = c("Alpine-Northern Boreal Zone", "Central-Boreal Zone", 
                                 "Hemi-Boreal Zone", "Southern-Boreal Zone"),
                     name="ecological zone") +
  scale_fill_manual(values=c("grey90","grey60", "grey50"))+ guides(fill = "none") +
  theme(text=element_text(family="EB Garamond"))



comb = cowplot::plot_grid(m_dens+theme(legend.position = "none",axis.title = element_text(size=8)), 
                          p_dens+theme(legend.position = "none",axis.title = element_text(size=8)), 
                          m_dens_w+theme(legend.position = "none",axis.title = element_text(size=8)),
                          m_dens_r+ theme(legend.position = "none",axis.title = element_text(size=8)),
                          nrow = 2,
                          labels = c("I", "II", "III", "IV"),
                          #label_fontface = "italic",
                          label_fontfamily = "EB Garamond")

comb


cowplot::save_plot(plot=comb,
                   device = "svg",
                   base_width  = 9,
                   base_height = 9,
                   dpi=600,
                   "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/density_params.svg");#beepr::beep(5)


magick::image_write(magick::image_convert(magick::image_read(
  "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/density_params.svg"),
                                          format = "png"),
  "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/density_params.png")

## maps from chapter II

library(dplyr)
library(ggplot2)
library(cowplot)
library(ggnewscale)
library(cowplot)

mma_for_study = st_read("C:/Users/lugf0001/Documents/maps-and-moose//01_data/shapes/mma_data/Stratum2024.shp")

mma_for_study = mma_for_study %>% 
  rename(id = AFOID_LANS) 
# works?
sw = rgeoboundaries::gb_adm0("Sweden") %>% 
  st_transform(3006)

ggplot() +
  geom_sf(data=mma_for_study,fill="white") +
  geom_sf(data=sw, fill="transparent", color="red") +
  geom_sf(data=plot(st_intersection(sw,mma_for_study )) , fill=NA, color="blue")

mma_for_study = (st_intersection(sw,mma_for_study ))


shp=st_read("C:/Users/lugf0001/Documents/maps-and-moose/01_data/shapes/final_shape_for_calcs/map_final_plot.shp")

# Create summarized datasets for each variable
vars <- list(
  moose_dens = "Moose density",
  prop_young_forest = "Young forest proportion",
  calf_weight = "Calf weight",
  rase_density_hectar = "Råse density",
  nr_days_with_snow = "Days with snow",
  moose_equivalent = "Moose equivalent",
  calves_per_adult_female = "Calves per female"
)

model_data = read.csv("C:/Users/lugf0001/Documents/maps-and-moose/01_data/model_data.csv")


browsing_data = model_data %>%  
  filter(coverage_by_als>=0.5) %>% 
  #filter(mma %in% read.csv("C:/Users/lugf0001/Documents/maps-and-moose/01_data/test_train/oskar_base_data.csv")$ID) %>% 
  select(#log_scaled_moose_dens, log_scaled_pine_dens, log_scaled_snow_depth, scaled_nr_days_with_snow,
    year, mma, region, 
    winter_density,
    nrof_pines, Jaktarea,
    Area_ha,
    max_snowdepth,
    nr_days_with_snow,
    mean_scanning_year,
    yearly_damaged_pines,
    young_forest_ha,
    ecological_zone, 
    pine_mean_hectar,
    birch_density_km2,
    rase_density_hectar, 
    # get responses
    rase_sum_km2,
    moose_equivalent,
    female_calf_weight, bull_calf_weight,
    calves_per_adult_female,
    prop_of_adult_bulls, female_calf_weight,
    vegetation_period_length,
    coverage_by_als,
    birch_mean_km2,
    rase_mean_km2,
    pine_mean_km2)  %>%
  mutate(
    
    # some quick replacemenets and calculations that are needed
    
    max_snowdepth = replace_na(max_snowdepth, 0),
    nr_days_with_snow = replace_na(nr_days_with_snow, 0),
    
    time_difference_skanning = year - mean_scanning_year,
    abs_time_difference_skanning = abs(time_difference_skanning),
    numerical_browsing_damage = log(log(1 / (1 - yearly_damaged_pines))),
    browsing_damage_proportion = yearly_damaged_pines,
    
    
    ## preporare predictors
    # proportion forest
    prop_young_forest = (young_forest_ha / Area_ha)*100,
    moose_dens = winter_density,
    # moose and pine density
    pine_dens = nrof_pines / Jaktarea,
    log_scaled_moose_dens = scale(log(winter_density), center = TRUE, scale = TRUE)[,1],
    log_scaled_pine_dens  = scale(log(nrof_pines / Jaktarea), center = TRUE, scale = TRUE)[,1],
    
    
    
    scaled_moose_dens = scale(winter_density, center = TRUE, scale = TRUE)[,1],
    scaled_pine_dens  = scale(nrof_pines / Area_ha , center = TRUE, scale = TRUE)[,1],
    scaled_pine_dens_squared  = scale(I(nrof_pines / Area_ha )^2, center = TRUE, scale = TRUE)[,1],
    scaled_pine_dens_cubed = scale(I(nrof_pines / Area_ha )^3, center = TRUE, scale = TRUE)[,1],
    
    scaled_moose_equivalent =  scale(moose_equivalent, center = TRUE, scale = TRUE)[,1],
    scaled_moose_equivalent_squared =  scale(I(moose_equivalent^2), center = TRUE, scale = TRUE)[,1],
    scaled_moose_equivalent_cubed =  scale(I(moose_equivalent^3), center = TRUE, scale = TRUE)[,1],
    
    log_scaled_moose_equivalent =  scale(log(moose_equivalent), center = TRUE, scale = TRUE)[,1],
    
    birch_density = birch_density_km2,
    
    # climatic preparation
    scaled_nr_days_with_snows = scale(nr_days_with_snow, center = TRUE, scale = TRUE)[,1],
    scaled_nr_days_with_snow_squared = scale(I(nr_days_with_snow^2), center = TRUE, scale = TRUE)[,1],
    scaled_nr_days_with_snow_cubed = scale(I(nr_days_with_snow^3), center = TRUE, scale = TRUE)[,1],
    
    log_scaled_snow_depth = scale(log(max_snowdepth + 0.0001), center = TRUE, scale = TRUE)[,1],
    scaled_snow_depth     = scale(max_snowdepth, center = TRUE, scale = TRUE)[,1],
    log_scaled_nr_days_with_snow = scale(log(nr_days_with_snow + 0.0001), center = TRUE, scale = TRUE)[,1],
    scaled_nr_days_with_snow = scale((nr_days_with_snow), center = TRUE, scale = TRUE)[,1],
    scaled_nr_days_with_snow_squared = scale(I(nr_days_with_snow^2), center = TRUE, scale = TRUE)[,1],
    
    scale_vegetation_period_length = scale(vegetation_period_length),
    # spatial environment
    scaled_log_prop_young_forest = scale(log(prop_young_forest)),
    scaled_prop_young_forest = scale((prop_young_forest)),
    scaled_prop_young_forest_squared = scale(I(prop_young_forest^2)),
    scaled_prop_young_forest_cubed = scale(I(prop_young_forest^3)),
    
    scaled_rase_km2 = scale(rase_mean_km2),
    scaled_rase_km2_squared = scale(I(rase_mean_km2^2)),
    scaled_rase_km2_cubed = scale(I(rase_mean_km2^3)),,
    
    log_scaled_rase_km2 = scale(log(rase_mean_km2+.0001)),
    scaled_rase_km2 = scale(log(birch_mean_km2+.0001)),
    log_scaled_pine_km2 = scale(log(pine_mean_km2+0.0001)),
    rase_density =  I(rase_sum_km2/Jaktarea),
    scaled_rase_density = scale(rase_density),
    scaled_rase_density_squared = scale(I(rase_density^2)),
    scaled_rase_density_cubed =  scale(I(rase_density^3)),
    scaled_birch= scale(birch_mean_km2),
    scaled_birch_squared = scale(I(birch_mean_km2^2)),
    scaled_birch_cubed =  scale(I(birch_mean_km2^3)),
    scaled_birch_density= scale(birch_density_km2),
    scaled_birch_density_squared = scale(I(birch_density_km2^2)),
    scaled_birch_density_cubed =  scale(I(birch_density_km2^3)),
    # scaled_rase_density = scale(rase_density_hectar),
    # scaled_rase_density_squared = scale(I(rase_density_hectar^2)),,
    # scaled_rase_density_cubed = scale(I(rase_density_hectar^3)),
    log_scaled_rase_density= scale(log(rase_density_hectar+.0001)),
    pine_mean_km2 = pine_mean_km2,
    scaled_birch_km2 = scale((birch_mean_km2)),
    scaled_pine_km2 = scale((pine_mean_km2))) %>% 
  rowwise() %>% 
  mutate(calf_weight = mean(c(female_calf_weight,bull_calf_weight), na.rm=T)) %>% 
  ungroup() %>% 
  select(-c(winter_density,nrof_pines, Jaktarea,max_snowdepth,
            #nr_days_with_snow,
            mean_scanning_year,
            yearly_damaged_pines,
            young_forest_ha)) %>% 
  mutate(ecological_zone = ifelse(ecological_zone=="Nemoral Zone", "Hemi-Boreal Zone", ecological_zone)) %>% 
  filter(!is.na(log_scaled_moose_dens)) 
base_data <- shp %>%
  rename(mma= LANAFO) %>% 
  left_join(train)

test= browsing_data %>% 
  # remove everything with a higher time difference than 3.5 (as 3.51 would suggest being closer to 4 years)
  filter(abs_time_difference_skanning>=3.5);nrow(test)
train <- browsing_data %>% 
  # remove everything with a higher time difference than 3.5 (as 3.51 would suggest being closer to 4 years)
  filter(abs_time_difference_skanning<=3.5);nrow(train)#%>%
#  anti_join(test, by = colnames(df)) %>% 
#  mutate(ecological_zone = ifelse(ecological_zone=="Nemoral Zone", "Hemi-Boreal Zone", ecological_zone))
# Base spatial data
base_data <- shp %>%
  rename(mma= LANAFO) %>% 
  left_join(train)


make_plot <- function(var, title, palette_option) {
  tmp <- base_data %>%
    left_join(
      train %>%
        group_by(mma) %>%
        summarize(mean = mean(.data[[var]]), .groups = "drop")
    ) 
  ggplot(tmp) +
    geom_sf(dat =  mma_for_study, fill="grey80", inherit.aes = F) +
    geom_sf(aes(fill = mean)) +
    scale_fill_viridis_c(option = palette_option) +
    labs(title = title, fill = title) +
    theme_minimal()+
    theme(legend.position ="bottom",
          text = element_text(size=26,
                              family = "EB Garamond"),
          # fine here...
          legend.title = element_blank())
}

# Create individual plots with different palettes
p1 <- make_plot("moose_dens", "Moose density", "A")
p2 <- make_plot("prop_young_forest", "Young forest", "B")
p4 <- make_plot("rase_density", "RASE density", "D")
p5 <- make_plot("nr_days_with_snow", "Snow days", "E")
p6 <- make_plot("moose_equivalent", "Moose equivalent", "F")
p8 <- make_plot("pine_dens", "Pine density", "G")
p10 <- make_plot("birch_density", "Birch density", "E")

p3 <- make_plot("calf_weight", "Calf weight", "C")
p9 <- make_plot("browsing_damage_proportion", "Browsing Damage", "G")
p7 <- make_plot("calves_per_adult_female", "Calves per female", "D")


plot = cowplot::plot_grid(
  p1, p8, p5, p2,p4,p8,p10,
  ncol=7)


ggsave2(plot=plot,
       device="png",
       dpi=300,
       width=16,
       height=9,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/predictars_chapter_2.png")

plot = cowplot::plot_grid(
  p3,p7,p9,
  ncol=3)


ggsave2(plot=plot,
        device="png",
        dpi=300,
        width=10,
       height=8,
        "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/responses_ch2.png")
