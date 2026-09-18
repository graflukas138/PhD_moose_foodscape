library(terra)
library(ggplot2)
library(patchwork)
library(tidyverse)

set.seed(1236)

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
      #axis.title = element_text(family = "EB Garamond"),
      #axis.text = element_text(family = "EB Garamond"),
      legend.title = element_text(family = "EB Garamond"),
      #legend.text = element_text(family = "EB Garamond"),
      #strip.text = element_text(family = "EB Garamond"),
      axis.title = element_blank(),
      axis.text = element_blank(),
      #legend.title = element_text(size = 12),
      #legend.text = element_text(size = 11),
      strip.text = element_text(size = 12),
      legend.position = "bottom",
      legend.text = element_blank()
      
    )
)

# Create a raster with 1000 cells (40 x 25 = 1000)
r <- rast(nrows = 500, ncols = 500)

# Function to create spatially autocorrelated raster
simulate_raster <- function(r, smooth) {
  
  # random noise
  x <- rast(r)
  values(x) <- rnorm(ncell(x), 0, 100)
  
  # Gaussian smoothing to induce autocorrelation
  x_smooth <- focal(
    x,
    w = matrix(1, nrow = smooth, ncol = smooth),
    fun = mean,
    na.policy = "omit",
    fillvalue = NA
  )

  
  # force same mean
  x_smooth <- x_smooth + 10
  
  return(x_smooth)
}

# Three levels of spatial autocorrelation
r_low  <- simulate_raster(r, smooth = 3)   # weak autocorrelation
r_mid  <- simulate_raster(r, smooth = 21)   # intermediate autocorrelation
r_high <- simulate_raster(r, smooth = 99)  # strong autocorrelation

mean(na.omit(values(r_low)))
mean(na.omit(values(r_mid)))
mean(na.omit(values(r_high)))

r_split = r

# Left half = 9, right half = 11
values(r_split) <- rep(
  c(rep(0, ncol(r_split) / 2),
    rep(20, ncol(r_split) / 2)),
  each = nrow(r_split)
)

plot(r_split)

r_stack = c(r_mid, r_high, r_split) 

poly = ext(r_stack) |>
  as.polygons() |>
  st_as_sf() |>
  st_centroid() |>
  st_buffer(60)

r_stack = crop(r_stack, vect(poly), mask=F)
r_stack = r_stack %>% scale
 
names(r_stack) = c("low_correlation", "medium_correlation", "perfect_correlation")

r_df = as.data.frame(r_stack, xy=T) 

p1 = ggplot(r_df, aes(x,y, fill=low_correlation)) +
  geom_tile() +
  coord_equal() +
  labs(y="", x="x")+
  scale_fill_gradient2(name="random",
                       low=scales::col_darker("olivedrab4"),
                       high="firebrick4",
                       mid="white")+
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5));p1

p2 = ggplot(r_df, aes(x,y, fill=medium_correlation)) +
  geom_tile() +
  coord_equal() +
  scale_fill_gradient2(name="correlated",
                       low=scales::col_darker("olivedrab4"),
                       high="firebrick4",
                       mid="white",
                       na.value = "transparent")+
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

p3 = ggplot(r_df, aes(x,y, fill=perfect_correlation)) +
  geom_tile() +
  coord_equal()  +
  scale_fill_gradient2(name="perfect seperation",
                       low=scales::col_darker("olivedrab4"),
                       high="firebrick4",
                       mid="white",
                       na.value = "transparent") +
  guides(fill = guide_colourbar(title.position = "top", title.hjust = 0.5))

p1|p2|p3


ggsave(plot=last_plot(),
       device = "svg",
       dpi=500,
       width=10,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/autocorrelation.svg")


ggsave(plot=last_plot(),
       device = "pdf",
       dpi=500,
       width=10,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/autocorrelation.pdf")


