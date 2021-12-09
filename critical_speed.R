library(tidyverse)
library(here)
library(cowplot)
library(RColorBrewer)

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


# Estimated lap speeds from digitized graph
race_data <- 
  read_csv(here("kirby-fig1a.csv")) %>% 
  mutate(split = 1:nrow(.),
         lap_distance= c(rep(400, 11), 200, 400),
         total_distance = cumsum(lap_distance),
         lap_split = lap_distance/lap_leader_speed_meters_second,
         relative_lap_timer =  map(lap_split, ~seq(1, .x, by = 1)))


calc_d_balance <- function(runner_data, race_data){
  #d_start_of_lap is d_prior
  d_start_of_lap <- as.list(runner_data$d_prime_meters)
  d_balance_data <- vector("list", nrow(runner_data))
  # d_bal  <- list()
  # final_d <- list()
  for(runner in seq_len(nrow(runner_data))){
    # get critical speed and D' for current runner
    cs <- runner_data$critical_speed_meters_second[[runner]]
    d_prime <- runner_data$d_prime_meters[[runner]]
    
    if(is.na(cs)){ next }
    
    runner_d_balance <- list()
    for(lap in seq_len(nrow(race_data))){
      # get current lap speed in m/s and calculate D_cs
      lap_speed <- race_data$lap_leader_speed_meters_second[[lap]]
     
      lap_time <- race_data$relative_lap_timer[[lap]]
      lap_distance <- race_data$lap_distance[[lap]]
      d_cs <- cs - lap_speed
      
      if(lap_speed > cs){
      # Calculate d_drain (kirby calls this d_exp for expended)
      t <- race_data$relative_lap_timer[[lap]] 
      d_sol <- d_start_of_lap[[runner]][[lap]]
      d_drain <- ( d_cs * t ) + d_sol
      
      d_start_of_lap[[runner]][[lap+1]] <- tail(d_drain, 1)
      
      d_balance <- d_drain
      balance_status <- "drain"
      
      } else if(lap_speed<= cs){
      
      d_expended <- d_prime - d_start_of_lap[[runner]][[lap]]
      d_gain <- d_prime - d_expended * exp((-d_cs*lap_time)/d_prime)
      
      d_start_of_lap[[runner]][[lap+1]] <- tail(d_gain, 1)
      
      d_balance <- d_gain
      balance_status <- "gain"
      
      }
      
      
      runner_lap_data <- tibble(athlete = runner_data$athlete[[runner]],
                                lap = lap,
                                lap_time = lap_time,
                                meters_second = lap_speed,
                                d_balance = d_balance,
                                balance_status = balance_status)
      
      runner_d_balance[[lap]] <- runner_lap_data
      
    }
    
  d_balance_data[[runner]] <- 
    do.call("rbind", runner_d_balance) %>% 
    mutate(total_race_time = seq_len(nrow(.)),
           total_race_distance_m = cumsum(meters_second))
  }
  
  d_balance_data <- do.call("rbind", d_balance_data)
}








runner1 <- filter(d_balance_data, athlete %in% c("Muktar Edris", "Mohammed Farah", "Paul Chelimo", "Yomif Kejelcha")) %>% 
  mutate(source = "Replication")



kirby_d_balance <- 
  read_csv(here("kirby-fig1b.csv")) %>% 
  pivot_longer(cols = !x, names_to = "last_name", values_to = "d_balance") %>% 
  mutate(athlete = case_when(
                    last_name == "Edris" ~ "Muktar Edris",
                    last_name == "Farah" ~ "Mohammed Farah",
                    last_name == "Chelimo" ~ "Paul Chelimo",
                    last_name == "Kejelcha" ~ "Yomif Kejelcha"
                    ),
  total_race_distance_m = x,
  source = "Digitized Original")

run <- bind_rows(runner1, kirby_d_balance)

run$athlete <- factor(run$athlete, levels = c("Muktar Edris",
                                              "Mohammed Farah",
                                              "Paul Chelimo",
                                              "Yomif Kejelcha"))

ggplot()+
  geom_line(data = run, 
            aes(total_race_distance_m, 
                d_balance, 
                color = athlete, 
                linetype = source,
                size = source
                ))+
  scale_size_manual(values = c(0.5, 1))+
  ylab("D' Balance")+
  xlab("Race Distance (m)")+
  ggtitle("Replication of Kirby 2021 Figure 1B")+
  scale_color_manual(values = brewer.pal(4, "Dark2"))+
  theme_cowplot()


ggplot(kirby_d_balance)+
  geom_line(aes(x, d_balance))
 