library(tidyverse)
library(ggh4x)
set.seed(1235)

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
  theme_classic(base_size = 15) +
  labs(
    x = "",
    y = "",
    color = "Behavioural state"
  )+
  theme_void()+
  theme(    panel.background = element_rect(fill="white"))

ggsave(plot=last_plot(),
       device = "png",
       dpi=500,
       width=5,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/noHMM.jpg")

ggplot(track, aes(x,y, color=state,
                  group="1")) +
  #geom_path(color="grey70", linewidth=0.6) +
  #geom_point(aes(color = state), size=1.3) +
  
  geom_pointpath()+
  scale_color_manual(values=c(
    Resting   ="#5B4636",
    Foraging  ="#d95f02",
    Traveling ="#6C757D"
  )) +
  theme_classic(base_size = 15) +
  labs(
    x = "",
    y = "",
    color = "Behavioural state"
  )+
  theme_void()+
  theme(
    panel.background = element_rect(fill="white"),
    legend.position = c(0.12, 0.88),
    legend.justification = c(0, 1),
    legend.background = element_rect(
      fill = scales::alpha("white", 0.7),
      colour = NA
    ),
    legend.key = element_blank()
  )


ggsave(plot=last_plot(),
       device = "png",
       dpi=500,
       width=5,
       height=5,
       "C:/Users/lugf0001/My Drive/papers in writing/PhD - thesis/images/pres_only_figures/HMM.jpg")


## add iSSA figure?

