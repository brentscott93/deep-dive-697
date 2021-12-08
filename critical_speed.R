library(tidyverse)
library(here)

# copy pasta from Kirby 2021 paper
kirby_table1 <- 
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

# Estimated lap speeds from digitized graph
lap_speed_df <- 
  read_csv(here("kirby-fig1a.csv")) %>% 
  mutate(split = 1:nrow(lap_speed),
         lap_split_per_400m = round(400/speed_meters_sec, 0),
         relative_lap_time =  map(lap_split_per_400m, ~ seq(.x, by = 1)))

rel_lap_times <- map(lap_times_per_400m, ~seq(1, round(.x, 0), by = 1))


d_cs <- cs-lap_speed
d_prior <- c(351)
d_exp <- list()
d_bal  <- list()
final_d <- list()
for(i in seq_along(rel_lap_times)){

  d_expended <- (d_cs[[i]]*rel_lap_times[[i]]) + d_prior[[i]]
  
  d_prior[[i+1]] <- d_expended[[length(d_expended)]]
  
  d_exp[[i]] <- d_expended
  
  d_balance <- d_prime - d_expended*exp(-d_cs[[i]]^rel_lap_times[[i]]/d_prime)
  d_bal[[i]] <- d_balance
  if(cs <= lap_speed[[i]]){
    final_d[[i]] <- d_expended
  } else {
    final_d[[i]] <- d_balance
  }
}

plot(unlist(d_bal))

 