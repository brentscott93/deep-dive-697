#' Calculate D' Balance for runner in race
#'
#' @param runner_data a datafra,e containing runners critical speed and D' prime values.
#' @param race_data  a dataframe containing details of the race to simulate where each row represents a lap split. Needs lap speed & lap distance columns.  
#'
#' @return
#' @export
#' 
#' @import magrittr
#' @examples
calc_d_balance <- function(runner_data, 
                           race_data, 
                           runner_col_names = FALSE, 
                           race_col_names = FALSE){
  
  if(runner_col_names != FALSE){
    if(length(runner_col_names) != 3) stop("runner_col_names must be length 3")
  runner_data %<>% 
    rename("athlete" = runner_col_names[[1]],
           "critical_speed_meters_second" = runner_col_names[[2]],
           "d_prime_meters" = runner_col_names[[3]])
  }
  
  if(race_col_names != FALSE){
    if(length(race_col_names) != 3) stop("race_col_names must be length 2")
  race_data %<>%
    rename("lap_leader_speed_meters_second" = race_col_names[[1]],
           "lap_distance_meters" = race_col_names[[2]])
  }
           
   race_data %<>%
     mutate(
      total_distance_meters = cumsum(lap_distance_meters),
      lap_split = lap_distance_meters/lap_leader_speed_meters_second,
      relative_lap_timer =  map(lap_split, ~seq(1, .x, by = 1)))
  
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
      lap_distance <- race_data$lap_distance_meters[[lap]]
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
             total_race_distance_meters = cumsum(meters_second))
  }
  
  d_balance_data <- do.call("rbind", d_balance_data)
}







plot_d_balance <- function(d_balance_data){
ggplot()+
  geom_line(data = d_balance_data, 
            aes(total_race_distance_meters, 
                d_balance, 
                color = athlete, 
            ))+
  ylab("D' Balance")+
  xlab("Race Distance (m)")+
  theme_cowplot(font_size = 12)
}

to_seconds <- function(t){
  t_split <- str_split(t, ":")
  map_dbl(t_split, ~ (as.numeric(.x[[1]]) * 60 ) + as.numeric(.x[[2]]))
}

plot_lollipop_race <- function(runner_data, race_data){
 
  ggplot()+
  geom_hline(data = runner_data,
             aes(yintercept = critical_speed_meters_second,
                 color = athlete))+
  geom_segment(data = race_data,
               aes(x = meter_mark,
                   xend = meter_mark,
                   y = 0,
                   yend = lap_leader_speed_meters_second),
               size = 1)+
  geom_point(data = race_data,
             aes(x = meter_mark,
                 y = lap_leader_speed_meters_second),
             size = 8,
             shape = 21,
             fill = "grey15",
             stroke = 1.5)+
  geom_text(data = race_data,
            aes(x = meter_mark,
                y = lap_leader_speed_meters_second,
                label = round(lap_leader_speed_meters_second, 2)),
            size = 2.75,
            color = "white")+
  ylab("Lap Speed (meters/second)")+
  xlab("Distance (meters)")+
  scale_y_continuous(expand = expansion(c(0, 0.1)))+
  scale_x_continuous(breaks = race_data$meter_mark)+
  theme_cowplot(font_size = 12)
}
