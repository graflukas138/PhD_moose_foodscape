library(terra)
library(tidyverse)
library(scico)
library(patchwork)
library(amt)
library(sf)

# ------------------------------------------------------------
# 1. Create movement kernel raster
# ------------------------------------------------------------

# Example: step-length distribution
# gamma distribution estimated from iSSF movement model
# replace with your fitted parameters

shape <- 3
scale <- 700

habitat_rasters = unwrap(amt::amt_fisher_covar$elevation)
habitat_rasters$popden = project( unwrap(amt::amt_fisher_covar$popden),
                                habitat_rasters)


movement_kernel <- unwrap(amt::amt_fisher_covar$elevation)

distance_raster <- terra::distance(
  movement_kernel,
  st_bbox(movement_kernel) %>% st_as_sfc %>% st_as_sf %>% st_centroid()
)

# predict probability density for each distance
movement_kernel <- app(
  distance_raster,
  fun=function(x){
    dgamma(
      x,
      shape=shape,
      scale=scale)});plot(movement_kernel)

# raster center
center_x <- mean(xmin(movement_kernel), xmax(movement_kernel))
center_y <- mean(ymin(movement_kernel), ymax(movement_kernel))
# coordinates of cells
xy <- crds(movement_kernel, df=TRUE)

angle <- atan2(
  xy$x - 1782211,
  xy$y - 2406707
)

mu <- pi*2    # 45 degrees from north
kappa <- 2      # directional concentration

vm_kernel <- exp(
  kappa * cos(angle - mu)
)


# convert to raster
vm_raster <- movement_kernel
values(vm_raster) <- vm_kernel

movement_kernel = (movement_kernel*vm_raster)


# ------------------------------------------------------------
# 2. Von Mises directional kernel
# ------------------------------------------------------------

mu <- pi/4      # 45 degrees from north
kappa <- 2      # directional concentration


vm_kernel <- exp(
  kappa * cos(angle - mu)
)


# convert to raster
vm_raster <- movement_kernel
values(vm_raster) <- vm_kernel



# ------------------------------------------------------------
# 2. Habitat selection kernel (RSF)
# ------------------------------------------------------------

# Example iSSF habitat coefficients
beta <- c(
  elevation = 0.04,
  #young_forest = 0.5,
  pop_den = -0.0003
)

# Calculate linear predictor
habitat_kernel <- 
  beta["elevation"]  * habitat_rasters$elevation +
  #beta["young_forest"]  * habitat_rasters$young_forest +
  beta["pop_den"]     * habitat_rasters$popden


# exponentiate RSF
habitat_kernel <- exp(habitat_kernel)


# normalize
habitat_kernel <- habitat_kernel / global(
  habitat_kernel,
  "max",
  na.rm=TRUE
)[1,1]


# ------------------------------------------------------------
# 3. Joint iSSF process
# ------------------------------------------------------------

joint_kernel <- movement_kernel * habitat_kernel

joint_kernel <- joint_kernel /
  global(joint_kernel, "sum", na.rm=TRUE)[1,1]


# ------------------------------------------------------------
# 5. Plot
# ------------------------------------------------------------

library(patchwork)
library(tidyterra)

library(terra)
library(ggplot2)
library(patchwork)

# Convert rasters to data frames
habitat_df <- as.data.frame(
  habitat_kernel,
  xy = TRUE,
  na.rm = TRUE
)

movement_df <- as.data.frame(
  movement_kernel,
  xy = TRUE,
  na.rm = TRUE
)

joint_df <- as.data.frame(
  joint_kernel,
  xy = TRUE,
  na.rm = TRUE
)


# Rename raster value column
names(habitat_df)[3] <- "value"
names(movement_df)[3] <- "value"
names(joint_df)[3] <- "value"


# Plot habitat kernel
p1 <- ggplot(habitat_df) +
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = value
    )
  ) +
  theme_bw() +
  labs(x="",
       y="")  +
  scale_fill_gradient2(name="perfect seperation",
                       low=scales::col_darker("olivedrab4"),
                       high="firebrick4",
                       mid="white",
                       na.value = "transparent")+
  ggtitle("Habitat selection kernel")+
  theme(legend.position = "none",
        axis.text = element_blank()) 


# Plot movement kernel
p2 <- ggplot(movement_df) +
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = value
    )
  ) +
  theme_bw()+
  labs(x="",
       y="") +
  scale_fill_gradient2(name="perfect seperation",
                       low=scales::col_darker("olivedrab4"),
                       high="firebrick4",
                       #mid="white",
                       na.value = "transparent") +
  ggtitle("Movement kernel")+
  theme(legend.position = "none",
        axis.text = element_blank()) 


# Plot joint kernel
p3 <- ggplot(joint_df) +
  geom_raster(
    aes(
      x = x,
      y = y,
      fill = value
    )
  ) +
  theme_bw()+
  labs(x="",
       y="")  +
  scale_fill_gradient2(name="perfect seperation",
                       low=scales::col_darker("olivedrab4"),
                       high="firebrick4",
                       #mid="white",
                       na.value = "transparent") +
  ggtitle("Joint movement process")+
  theme(legend.position = "none",
        axis.text = element_blank()) 


# Combine
p1 | p2 | p3


library(amt)
library(lubridate)

steps = amt::deer %>% 
  steps_by_burst()

m1 =amt::deer %>% 
  steps_by_burst() %>% 
  random_steps(1) %>% 
  fit_issf(case_ ~ log(sl_)+sl_+cos(ta_)+strata(step_id_), model=T)

# Tentative step-length distribution
(tent_sl <- sl_distr(m1))
# Updated selection-free step-length distribution
(upd_sl <- update_sl_distr(m1))

# Compile the parameters into a data.frame for plotting with ggplot
tent_df <- data.frame(dist = "tent",
                      shp = tent_sl$params$shape,
                      scl = tent_sl$params$scale)
upd_df <- data.frame(dist = "upd",
                     shp = upd_sl$params$shape,
                     scl = upd_sl$params$scale)

(sl_df <- rbind(tent_df, upd_df))

# Plot
p4 = expand.grid(sl = seq(1, 1000, length.out = 100),
            dist = c("tent", "upd")) %>% 
  left_join(sl_df) %>% 
  filter(dist=="tent") %>% 
  mutate(y = dgamma(sl, shape = shp, scale = scl)) %>% 
  ggplot(aes(x = sl, y = y)) +
  geom_histogram(data = steps,
                 aes(x=sl_, y = ..density..),
                 inherit.aes = F,
                 alpha=.5)+
  geom_line()  +
  xlab("Step Length (m)") +
  ylab("Probability Density") +
  theme_bw()+ 
  ylim(c(0, 0.004))+
  xlim(c(00, 1000));p4

# We can see there is barely a difference between our tentative and updated
# step-length distributions. This is no surprise, given that the betas for
# sl_ and log_sl_ were not significant.

# Tentative turn-angle distribution
(tent_ta <- ta_distr(m1))
# Updated selection-free turn-angle distribution
(upd_ta <- update_ta_distr(m1))

# Compile the parameters into a data.frame for plotting with ggplot
tent_df_ta <- data.frame(dist = "tent",
                         k = tent_ta$params$kappa)
upd_df_ta <- data.frame(dist = "upd",
                        k = upd_ta$params$kappa)

(ta_df <- rbind(tent_df_ta, upd_df_ta))

# Plot
p5= expand.grid(ta = seq(-pi, pi, length.out = 100),
            dist = c("tent", "upd")) %>% 
  left_join(ta_df) %>% 
  # circular::dvonmises is not vectorized
  rowwise() %>% 
  mutate(y = circular::dvonmises(ta, mu = 0, kappa = k)) %>% 
  ggplot(aes(x = ta, y = y)) +
  geom_histogram(data =steps, aes(x=(ta_), ..density..), inherit.aes = F,alpha=.4)+
  geom_line() +
  xlab("Turn Angle (radians)") +
  ylab("Probability Density") +
  scale_x_continuous(breaks = c(-pi, -pi/2, 0, pi/2, pi),
                     labels = expression(-pi, -pi/2, 0, pi/2, pi)) +
  coord_cartesian(ylim = c(0, 0.25)) +
  theme_bw()

coef_df <- data.frame(
  term = c("beta", "beta2"),
  estimate = c(0.3, -0.5),
  lower = c(0.15, -0.75),
  upper = c(0.45, -0.25)
)


p6 = ggplot(coef_df, aes(term, estimate, ymin=lower, ymax=upper)) +
  geom_pointrange()+
  geom_hline(yintercept = 0, linetype="dashed") +
  theme_bw()+scale_x_discrete(
    labels = c(
      beta = expression(beta[1]),
      beta2 = expression(beta[2])
    )
  ) +
  ylab("log-RSS")+
  xlab("")

library(cowplot)

top_row <- plot_grid(
  plot_grid(p6,
  plot_grid(p4, p5, nrow = 2),
  ncol=2),
  plot_grid(p1, p2, ncol = 2),
  nrow = 2
);top_row

ggsave2(plot=last_plot(),
        device = "png",
        dpi=300,
        width = 8,
        height = 8,
        "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/movement_process.png")

ggsave2(plot=last_plot(),
        device = "png",
        dpi=300,
        width = 8,
        height = 8,
        "03_figures/movement_process.png")


plot_grid(
  top_row,
  plot_grid(ggplot()+theme_void(), p3,
            nrow=2,
            align = "hv"),
  nrow = 1,
  ncol =2,
  rel_widths = c(.6666, .3333),
  align="hv"
)

ggsave2(plot=last_plot(),
        device = "png",
        dpi=600,
        width = 12,
        height = 8,
        "03_figures/movement_process_full.png")

ggsave2(plot=last_plot(),
        device = "png",
        dpi=600,
        width = 12,
        height = 8,
        "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/movement_process_full.png")



plot_grid(
  p1,p2,p3,
  ncol =3
)


ggsave2(plot=last_plot(),
        device = "png",
        dpi=600,
        width = 12,
        height = 6,
        "03_figures/movement_process_only_rasters.png")

ggsave2(plot=last_plot(),
        device = "png",
        dpi=600,
        width = 15,
        height =6,
        "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/movement_process_only_rasters.png")

