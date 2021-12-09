library(tidyverse)
library(here)

# copy pasta from Kirby 2021 paper
runner_data <- 
  tribble(
    ~place, ~athlete, ~critical_speed_meters_second, ~d_prime_meters, ~fastest_5k_prediction_mmss, ~actual_finish_time_mmss, ~finish_time_behind_leader_mmss,
    1, "Muktar Edris", 5.92, 351, 13:05.1, 13:32.8, "00:00.0",
    2, "Mohammed Farah", 6.08, 268, 12:59.0, 13:33.2, "00:00.4",
    3, "Paul Chelimo", 6.02, 282, 13:03.9, 13:33.3, "00:00.5",
    4, "Yomif Kejelcha", 6.26, 175, 12:51.2, 13:33.5, "00:00.7",
    5, "Selemon Barega", NA, NA, NA, 13:35.3, "00:02.6",
    6, "Mohammed Ahmed", 5.99, 274, 13:08.2, 13:35.4, "00:02.6",
    7, "Aron Kifle", 5.98, 259, 13:13.2, 13:36.9, "00:04.1",
    8, "Andrew Butchart", 6.15, 148, 13:09.4, 13:38.7, "00:05.9",
    9, "Justyn Knight", NA, NA, NA, 13:39.2, "00:06.4",
    10, "Kemoy Campbell", 5.86, 243, 13:31.5, 13:39.7, "00:06.9",
    11, "Patrick Tiernan", 6.00, 167, 13:25.6, 13:40.0, "00:07.2",
    12, "Birhanu Balew", NA, NA, NA, 13:43.3, "00:10.50"
  ) 

calc_d_left <- function(d_cs, t, d_prior){
  (d_cs * t) + d_prior
}

# Estimated lap speeds from digitized graph
race_data <- 
  read_csv(here("kirby-fig1a.csv")) %>% 
  mutate(split = 1:nrow(.),
         lap_split_per_400m = round(400/race_speed_meters_second, 0),
         relative_lap_timer =  map(lap_split_per_400m, ~seq(1, .x, by = 1)))



d_prior <- as.list(runner_data$d_prime_meters)
race_data$d_left_data <- list(tibble(athlete = runner_data$athlete,
                                   d_left = list(NA)))
# d_bal  <- list()
# final_d <- list()
for(runner in seq_len(nrow(runner_data))){
  # get critical speed and D' for current runner
  cs <- runner_data$critical_speed_meters_second[[runner]]
  d_prime <- runner_data$d_prime_meters[[runner]]
  
  for(lap in seq_len(nrow(race_data))){
    # get current lap speed in m/s and calculate D_cs
    lap_speed <- race_data$race_speed_meters_second[[lap]]
    d_cs <- cs - lap_speed
    
    # Calculate D_exp, or d_left
    d_left <- calc_d_left(d_cs = d_cs, 
                          t = race_data$relative_lap_timer[[lap]],
                          d_prior = d_prior[[runner]][[lap]])
    
    d_prior[[runner]][[lap+1]] <- tail(d_left, 1)
    
    race_data$d_left_data[[lap]]$d_left[[runner]] <- d_left
    
    # d_balance <- d_prime - d_expended*exp(-d_cs[[i]]^rel_lap_times[[i]]/d_prime)
    # d_bal[[i]] <- d_balance
    # if(cs <= lap_speed[[i]]){
    #   final_d[[i]] <- d_expended
    # } else {
    #   final_d[[i]] <- d_balance
    # }
  }
}



 