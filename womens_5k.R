library(tidyverse)
library(here)
library(magrittr)
library(cowplot)

source(here("functions.R"))

########################################
to_seconds <- function(t){
  t_split <- str_split(t, ":")
  map_dbl(t_split, ~ (as.numeric(.x[[1]]) * 60 ) + as.numeric(.x[[2]]))
}
womens_5k <- 
  tribble(
  ~total_meters, ~lap_distance_meters, ~time,
  200, 200,  "0:38.9",
  1000, 800, "3:00.67",
  1400, 400, "4:12.6",
  1800, 400, "5:25.2",
  2000, 200, "6:00.22",
  2200, 200, "6:36.6",
  2600, 400, "7:46.4",
  3000, 400, "8:59.81",
  3400, 400, "10:11.0",
  3800, 400, "11:21.9",
  4000, 200, "11:57.09",
  4200, 200, "12:31.5",
  4600, 400, "13:39.4", 
  4800, 200, "14:09.0",
  5000, 200, "14:36.79"
) %>% 
  mutate(
    seconds = to_seconds(time),
    split_time_seconds = seconds-lag(seconds, 1, default = 0),
    lap_leader_speed_meters_second = lap_distance_meters/split_time_seconds
  )


womens_runners <- 
  tribble(
    ~athlete, ~distance_meters, ~time, 
    "Hassan", 800, "1:56",
    "Hassan", 1000, "2:34",
    "Hassan", 1500, "3:51",
    "Hassan", 1609, "4:12",
    "Hassan", 3000, "8:18",
    "Hassan", 5000, "14:22",
    "Hassan", 10000, "29:06",
    
    "Obiri", 800, "2:00",
    "Obiri", 1500, "3:57",
    "Obiri", 1609, "4:16",
    "Obiri", 3000, "8:20",
    "Obiri", 3218, "9:14",
    "Obiri", 5000, "14:18",
    "Obiri", 10000, "29:59",
    
    "Tsegay", 800, "1:59",
    "Tsegay", 1500, "3:54",
    "Tsegay", 5000, "14:13",
    "Tsegay", 10000, "29:39",
  
) %>% 
  mutate(seconds = to_seconds(time)) %>% 
  group_by(athlete) %>% 
  nest() %>% 
  mutate(cs_mod = map(data, ~lm(distance_meters ~ seconds, data = .x)),
         d_prime_meters = map(cs_mod, ~coef(.x)[[1]]),
         critical_speed_meters_second =  map(cs_mod, ~coef(.x)[[2]]))



d_balance_womens_5k <- calc_d_balance(womens_runners, womens_5k)

d_balance_womens_5k$athlete <- factor(d_balance_womens_5k$athlete, levels = c("Hassan", "Obiri", "Tsegay"))

(plot_womens_5k <- 
  plot_d_balance(d_balance_womens_5k)+
  scale_color_manual(values = c("gold", "grey50", "darkorange"))+
  ggtitle("2021 Olympic Women's 5000m")
)
