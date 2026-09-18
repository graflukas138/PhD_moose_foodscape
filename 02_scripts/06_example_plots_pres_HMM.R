library(tidyverse)
library(ggh4x)
set.seed(1235)


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


theme_set(theme_void())
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


#-----------------------------
# Simulate a GPS track
#-----------------------------

n <- 25
n1=7
n2=12
n3=6

# Three behavioural states
state <- c(rep("Resting", n1),
           rep("Foraging", n2),
           rep("Traveling", n3))

# Step lengths for each state
step <- c(
  rgamma(n1, shape = 1, scale = 2*2),     # very short movements
  rgamma(n2, shape = 2, scale = 8),    # intermediate
  rgamma(n3, shape = 5, scale = 12*1.5)    # long movements
)

# Turning angles
angle <- c(
  rnorm(n1, 0, 2.8),    # random turning
  rnorm(n2, 0, 1.8),   # moderate turning
  rnorm(n3, 0, 0.7)    # directional movement
)

heading <- cumsum(angle)

x <- numeric(n)
y <- numeric(n)

for(i in 2:n){
  x[i] <- x[i-1] + step[i] * cos(heading[i])
  y[i] <- y[i-1] + step[i] * sin(heading[i])
}

track <- tibble(
  x,
  y,
  state,
  time = 1:n
)

#-----------------------------
# Plot

#-----------------------------
ggplot(track, aes(x,y)) +
  geom_pointpath()+
  
  scale_color_manual(values=c(
    Resting   ="#1b9e77",
    Foraging  ="#d95f02",
    Traveling ="#7570b3"
  )) +
  #theme_classic(base_size = 15) +
  labs(
    x = "",
    y = "",
    color = "Behavioural state"
  )+
  theme_void()+
  theme(
    panel.background = element_rect(fill="NA",
                                    color="transparent"),
        legend.text = element_text(family = "EB Garamond"))

ggsave(plot=last_plot(),
       device = "svg",
       dpi=600,
       width=5,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/noHMM.svg")

ggplot(track, aes(x,y, color=state,
                  group="1")) +
  #geom_path(color="grey70", linewidth=0.6) +
  #geom_point(aes(color = state), size=1.3) +
  
  geom_pointpath()+
  scale_color_manual(values=c(
    Resting   ="#5B4636",
    Foraging  ="#d95f02",
    Traveling ="olivedrab"
  )) +
  theme_classic(base_size = 15) +
  labs(
    x = "",
    y = "",
    color = "Behavioural state"
  )+
  theme_void()+
  theme(
    panel.background = element_rect(fill="NA",
                                    color="transparent"),
    legend.position = c(0.12, 0.88),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      colour = NA
    ),
    legend.key = element_blank(),
    legend.text = element_text(family = "EB Garamond"),
    
  )

ggsave(plot=last_plot(),
       device = "svg",
       dpi=600,
       width=5,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/HMM.svg")


rand = track %>%
  filter(state == "Traveling") %>%
  mutate(id = row_number()) %>%
  crossing(rand = 1:2) %>%
  group_by(row_number()) %>% 
  mutate(
    dx = rnorm(1, 0, 50),
    dy = rnorm(1, 0, 20),
    x2 = x + dx ,
    y2 = y + dy
  )

obs_points <- track %>%
  transmute(
    x = x,
    y = y,
    component = "Observed location"
  )

rand_points <- rand %>%
  transmute(
    x = x2,
    y = y2,
    component = "Available location"
  )

points_df <- bind_rows(obs_points, rand_points)
obs_steps <- track %>%
  filter(state == "Traveling") %>%
  mutate(
    xend = lead(x),
    yend = lead(y),
    component = "Observed step"
  ) %>%
  filter(!is.na(xend))

rand_steps <- rand %>%
  transmute(
    x = x,
    y = y,
    xend = x2,
    yend = y2,
    component = "Available step"
  )

steps_df <- bind_rows(obs_steps, rand_steps)
ggplot() +
  geom_pointpath(data = track, aes(x,y, color=state,
             group="1"))+
  scale_color_manual(values=c(
    Resting   ="black",
    Foraging  ="black",
    Traveling ="olivedrab"
  ))+  guides(color=F)+

  ggnewscale::new_scale_color() +
  # steps (observed + available)
  geom_segment(
    data = steps_df %>% filter(component=="Available step"),
    aes(x = x,y = y,
        xend = xend,
        yend = yend,
    ),colour = "black",linewidth = 0.6,
    linetype="dotted") +
  
  # locations (observed + available)
  geom_point(
    data = steps_df,
    aes(
      x = x,
      y = y,
      shape =component,
      color=component),
    size = 2
  ) +
  
  # locations (observed + available)
  geom_point(
    data = steps_df,
    aes(
      x = xend,
      y = yend,
      shape =component,
      color=component),
    size = 2
  ) +
  scale_color_manual(values=c( "#5B4636","olivedrab"),
                     name="availability domain")+
  scale_shape(name="availability domain")+
  theme_void(base_size = 15) +
  
  theme(
    legend.position = c(0.12, 0.88),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      colour = NA
    ),
    legend.key = element_blank(),
    legend.title = element_text(family = "EB Garamond"),
    legend.text = element_text(family = "EB Garamond"),
    panel.background = element_rect(fill="NA",
                                    color="transparent"),
  ) +
  xlim(c(min(c(track$x, track$x)), max(c(track$x, track$x)))) +
  ylim(c(min(c(track$y, track$y)), max(c(track$y, track$y))))



ggsave(plot=last_plot(),
       device = "svg",
       dpi=500,
       width=5,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/HMM_issa.svg")

