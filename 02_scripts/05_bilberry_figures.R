## script to show the study design in the BB project


library(tidyverse)
library(sf)
library(ggthemes)
library(ggnewscale)
library(magick)
library(ggpattern)

library(ggplot2)
library(sf)
library(dplyr)
library(ggnewscale)
library(ggpattern)


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


theme_set(theme_bw())
theme_set(
  theme_get() +
    theme(
      text = element_text(family = "EB Garamond"),
      plot.title = element_text(family = "EB Garamond"),
      plot.subtitle = element_text(family = "EB Garamond"),
      plot.caption = element_text(family = "EB Garamond"),
      axis.title = element_text(family = "EB Garamond"),
      axis.text = element_text(family = "EB Garamond"),
      legend.title = element_text(family = "EB Garamond"),
      legend.text = element_text(family = "EB Garamond"),
      strip.text = element_text(family = "EB Garamond")
    )
)


pt = st_point(c(0,0))
pt = (st_sfc(pt))
pt2 = st_point(c(-1,0))
pt2 = (st_sfc(pt2))
cross = crossing(pt,id = c(1,3,5))
cross2 = crossing(pt2,id = c(2,4,6)) %>% 
  rename(pt=pt2)

cross = bind_rows(cross, cross2)

cross = cross %>% 
  st_as_sf() %>% 
  mutate(treatment = ifelse(id %in% c(1,2), "1strong thinning", ifelse(id%in%c(3:4), "2standard thinning", "3no thinning"))) %>% 
  mutate(facet = ifelse(id %in% c(1,2), "strong thinning", ifelse(id%in%c(3:4), "standard thinning", "no thinning"))) %>% 
  
  mutate(browse = ifelse(id %% 2 == 0, "browsed","not browsed"))

right = cross[1,] %>% st_buffer(.2) %>% st_make_grid(n=c(2,2)) %>% st_centroid %>% st_buffer(.08)
left = cross[4,] %>% st_buffer(.2) %>% st_make_grid(n=c(2,2)) %>% st_centroid %>% st_buffer(.08)


## now with browsing treatments
right_browsed = right %>% 
  st_as_sf %>% 
  mutate(id = 1:nrow(.)) %>% 
  mutate(browse = ifelse(id %in% c(1,2), "3control", ifelse(id%in%c(3), "2low browse ", "1high browsing"))) 



## now with browsing treatments
right_browsed = right %>% 
  st_as_sf %>% 
  mutate(id = 1:nrow(.)) %>% 
  mutate(browse = ifelse(id %in% c(1,2), "3control", ifelse(id%in%c(3), "2low browse ", "1high browsing"))) 



# Create coordinates for 6 points in two rows
coords <- data.frame(
  id = 1:6,
  x = c(1, 2, 3, 1, 2, 3)*.5,
  y = c(1, 1, 1, 2, 2, 2)*.5
)

# Convert to sf object
points_sf <- st_as_sf(coords, coords = c("x", "y"))

points_sf$browse <- c("1non", "1non", "1non", "brows", "brows", "brows")
points_sf$thin <- c("1non", "2BAU", "3strong","1non", "2BAU", "3strong")

st_crs(points_sf) <- NA



# Distance to offset
d <- 0.075

# Square offsets (axis-aligned)
square_offsets <- matrix(c(
  d,  d,   # NE corner
  d, -d,   # SE corner
  -d,  d,   # NW corner
  -d, -d    # SW corner
), ncol = 2, byrow = TRUE)

# Create four corner points for each center
around_df <- points_sf %>%
  rowwise() %>%
  do({
    cx <- st_coordinates(.$geometry)[1]
    cy <- st_coordinates(.$geometry)[2]
    
    data.frame(
      id = paste0(.$id, c("_NE","_SE","_NW","_SW")),
      x  = cx + square_offsets[,1],
      y  = cy + square_offsets[,2]
    )
  }) %>% ungroup()


# Convert to sf
around_sf <- st_as_sf(around_df, coords = c("x", "y"))

browsed = around_sf %>% 
  mutate(treat = case_when(    str_detect(id, "1") ~"none",
                               str_detect(id, "2") ~"none",
                               str_detect(id, "3") ~"none",
                               str_detect(id, "N")~"none",
                               str_detect(id, "SE")~"strong",
                               T ~ "weak")) %>% 
  mutate(spacing = ifelse(treat =="none", 1, 0.03))

plot = ggplot() +
  theme_void() +
  geom_sf(data = points_sf %>% st_buffer(0.225),
          aes(fill = thin),
          color = "grey20",
          linewidth = 1.2,
          alpha=.7)+ 
  scale_fill_manual(
    values = c("grey30",
               "#A88F59",
               "#6B8E23"),
    labels = c("control", "30% thinning", "60% thinning"),
    name = "thinning treatment")+ 
  new_scale_fill()+
  geom_sf_pattern(data = browsed %>% st_buffer(.06),
                  aes(pattern = treat,
                      pattern_fill=treat,
                      pattern_type=treat),
                  pattern_frequency=1,
                  pattern_spacing = .025,
                  pattern="polygon_tiling",
                  color = "grey20",
                  linewidth=.8,    alpha = 0.2)+
  scale_pattern_fill_manual(name="browsing treatment",
                            values = c("grey20",#6B8E23
                                       "#A88F59",
                                       "#6B8E23"),
                            labels=c("control","20% browsing", "60% browsing")) +
  scale_pattern_color_manual(name="browsing treatment",
                             values = c("white",#6B8E23
                                        "white",
                                        "white"),
                             labels=c("control","20% browsing", "60% browsing")) +
  scale_pattern_type_manual(
    values = c(NA, "snub_trihexagonal","pythagorean"),
    name = "browsing treatment",
    labels=c("control","20% browsing", "60% browsing"))+
  theme(
    legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        legend.key.size = unit(1, 'cm'), #change legend key size
        legend.key.height = unit(1, 'cm'), #change legend key height
        legend.key.width = unit(1, 'cm'))+
  geom_sf(data = browsed %>% st_buffer(.06),
          color="white", linewidth=.8,
          fill="transparent")+
  theme(text =element_text(family="EB Garamond"));plot


ggsave(plot = plot,
       device = "svg",
       dpi=600,
       width = 10,
       height = 7,
       "03_figures/experimental_setup_BB.svg")

ggsave(plot = plot,
       device = "svg",
       dpi=600,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/experimental_setup_BB.svg")


ggsave(plot = plot,
       device = "svg",
       dpi=600,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/experimental_setup_BB.svg")

ggsave(plot = plot,
       device = "pdf",
       dpi=600,
       width = 10,
       height = 7,
       "03_figures/experimental_setup_BB.pdf")

ggsave(plot = plot,
       device = "pdf",
       dpi=600,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/experimental_setup_BB.pdf")


ggsave(plot = plot,
       device = "pdf",
       dpi=600,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/experimental_setup_BB.pdf")


library(rgeoboundaries)
library(patchwork)

sw = gb_adm1("Sweden")
stands = st_read("C:/Users/lugf0001/Documents/bilberry/01_data/final_stands_shapes/final_stands.shp") %>% 
  st_transform(4326)

bbox = stands %>% st_union() %>% st_bbox() %>% st_as_sfc %>% st_as_sf()%>% 
  #st_buffer(1.6e5) 
  st_buffer(40000) 

sweden = ggplot(sw %>% 
                  mutate(col = ifelse(shapeName %in% c("Skåne län", "Blekinge län", "Kronobergs län"), "1", "0"))) +
  geom_sf(aes(fill=col)) +
  scale_fill_manual(values = c("grey40",
                               "#6B8E23"))+
  theme_void()+
  theme(legend.position = "none") 

stand_plot = ggplot() +
  geom_sf(data=sw %>% st_crop(bbox), fill="grey90",)+
  geom_sf_text(data=sw %>% st_crop(bbox) %>% 
                 filter(shapeName %in% c("Skåne län", "Blekinge län", "Kronobergs län")),
               aes(label = shapeName),
               size=5)+
  geom_sf(data=stands %>% st_buffer(1e3), fill="#6B8E23")+
  theme_map()+
  ggspatial::annotation_north_arrow(location="tr")+
  ggspatial::annotation_scale(location="br")+
  theme(text= element_text(family="EB Garamond"))

(sweden | stand_plot) /
  plot +
  theme_base() + theme(axis.ticks = element_blank(),
                       axis.text = element_blank(),
                       panel.background = element_rect(fill = 'white', colour = 'white'))

ggsave(plot = last_plot(),
       device = "svg",
       dpi=300,
       width = 10,
       height = 7,
       "03_figures/sweden_stands.svg",
)

ggsave(plot = last_plot(),
       device = "svg",
       dpi=300,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/sweden_stands.svg",
)


ggsave(plot = last_plot(),
       device = "pdf",
       dpi=300,
       width = 10,
       height = 7,
       "03_figures/sweden_stands.pdf",
)

ggsave(plot = last_plot(),
       device = "pdf",
       dpi=300,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/sweden_stands.pdf",
)


library(cowplot)

# overlap the two top plots
top <- ggdraw() +
  draw_plot(sweden,     x = 0.00, y = 0, width = 0.55, height = 1) +
  draw_plot(stand_plot, x = 0.45, y = 0, width = 0.55, height = 1)

# combine with the bottom plot
final_plot <- plot_grid(
  top,
  plot+
    theme_base() + theme(axis.ticks = element_blank(),
                         axis.text = element_blank(),
                         panel.background = element_rect(fill = 'white', colour = 'white')),
  ncol = 1,
  rel_heights = c(.8, 1)
)

final_plot

ggsave(plot = final_plot,
       device = "svg",
       dpi=300,
       width = 6,
       height = 9,
       "03_figures/sweden_stands2.svg",
)
ggsave(plot = last_plot(),
       device = "svg",
       dpi=300,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/sweden_stands2.svg",
)



ggsave(plot = final_plot,
       device = "pdf",
       dpi=300,
       width = 6,
       height = 9,
       "03_figures/sweden_stands2.pdf",
)
ggsave(plot = last_plot(),
       device = "pdf",
       dpi=300,
       width = 10,
       height = 7,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/sweden_stands2.pdf",
)

