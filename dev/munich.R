library(tidyverse)
library(kin697u)
library(magrittr)

munich_5000 <- 
  tribble(
    ~lap_distance_meters, ~time,
    1800, "4:58",
    400, "6:06",
    400, "7:13",
    400, "8:20",
    400, "9:26",
    400, "10:29",
    400, "11:30",
    400, "12:30",
    400, "13:26"
  ) %>% 
  mutate(cumulative_distance = cumsum(lap_distance_meters),
         seconds = to_seconds(time),
         split_time_seconds = seconds-lag(seconds, default = 0),
         lap_leader_speed_meters_second = lap_distance_meters/split_time_seconds)

munich_runners <- 
  tribble(
    ~athlete, ~distance_meters, ~time,   
    "Pre", 1500, "3:39",
    "Pre", 3000, "7:44",
    "Pre", 3218, "8:19",
    "Pre", 5000, "13:22",
   # "Pre", 9656, "27:22",
    
    "Viren", 3000, "7:43",
    "Viren", 3218, "8:26",
    "Viren", 5000, "13:19",
    "Viren", 10000, "27:38",
    
    "Gammoudi", 3000, "7:50",
    "Gammoudi", 5000, "13:27",
    "Gammoudi", 10000, "27:54",
    
    "Stewart", 1500, "3:39",
    "Stewart", 3218, "8:22",
    "Stewart", 5000, "13:22",
    
    
  ) %>% 
  mutate(seconds = to_seconds(time))

cs_df <- calc_critical_speed(munich_runners) %>% 
  dplyr::select(athlete, critical_speed_meters_second, d_prime_meters)

d_balance_1972 <- kin697u::calc_d_balance(cs_df, munich_5000)    



plot_d_balance(d_balance_1972)
