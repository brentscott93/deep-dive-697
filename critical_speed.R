# library(tidyverse)
library(here)
library(kin697u)
library(cowplot)
library(RColorBrewer)
# library(magrittr)
# 
# source(here("functions.R"))
# copy pasta from Kirby 2021 paper
runner_data_mens_5k <- 
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
race_data_mens_5k <- 
  read_csv(here("kirby-fig1a.csv")) %>% 
  mutate(split = 1:nrow(.),
         lap_distance_meters = c(rep(400, 11), 200, 400))

      

d_balance_mens_5k <- calc_d_balance(runner_data_mens_5k, race_data_mens_5k)




filter_d_balance_mens_5k <- 
  d_balance_mens_5k %>% 
  filter(athlete %in% c("Muktar Edris", "Mohammed Farah", "Paul Chelimo", "Yomif Kejelcha")) %>% 
  mutate(source = "Replication")



kirby_d_balance_mens_5k <- 
  read_csv(here("kirby-fig1b.csv")) %>% 
  pivot_longer(cols = !x, names_to = "last_name", values_to = "d_balance") %>% 
  mutate(athlete = case_when(
                    last_name == "Edris" ~ "Muktar Edris",
                    last_name == "Farah" ~ "Mohammed Farah",
                    last_name == "Chelimo" ~ "Paul Chelimo",
                    last_name == "Kejelcha" ~ "Yomif Kejelcha"
                    ),
  total_race_distance_meters = x,
  source = "Digitized Original")

mens_5k <- bind_rows(filter_d_balance_mens_5k, kirby_d_balance_mens_5k)

mens_5k$athlete <- factor(mens_5k$athlete, levels = c("Muktar Edris",
                                                      "Mohammed Farah",
                                                      "Paul Chelimo",
                                                      "Yomif Kejelcha"))

(plot_mens_5k <- 
  ggplot()+
  geom_line(data = mens_5k, 
            aes(total_race_distance_meters, 
                d_balance, 
                color = athlete, 
                linetype = source,
                size = source
                ))+
  scale_size_manual(values = c(0.5, 1))+
  ylab("D' Balance")+
  xlab("Race Distance (m)")+
  ggtitle("5k Replication of Kirby 2021 Figure 1B")+
    scale_color_manual(name = "Athlete", values = brewer.pal(4, "Dark2"))+
    scale_linetype(name = "Data Source")+
    scale_size_manual(values = c(0.5, 1), guide = "none")+
  theme_cowplot(12)
)



#######################################################
runner_data_mens_10k <- 
  tribble(
    ~place, ~athlete, ~critical_speed_meters_second, ~d_prime_meters,
    1, "Mohammed Farah", 6.08, 268,
    2, "Joshua Cheptegei", 6.07, 250,
    3, "Paul Tanui", 6.14, 164,
    4, "Bedan Karoki", 6.06, 214,
    5, "Jemal Yimer", NA, NA,
    6, "Geoffrey Kamworor", 6.01, 316,
    7, "Abadi Hadis", 5.99, 317, 
    8, "Mohammed Ahmed", 6.00, 274,
    9, "Shadrack Kipchirchir", 6.09, 113,
    10, "Andamlak Belihu", NA, NA,
    11, "Aron Kifle", 5.98, 259,
    12, "Abraham Cheroben", NA, NA,
    13, "Leonard Korir", 6.02, 165,
    14, "Timothy Toroitich", 6.03, 101,
    15, "Hassan Mead", 5.95, 228
 ) 

race_data_mens_10k <- 
  read_csv(here("kirby-fig1c.csv")) %>% 
  mutate(split = 1:nrow(.),
         lap_distance_meters = 400)

race_data_mens_10k$lap_leader_speed_meters_second[[nrow(race_data_mens_10k)]] <- 7.2

kirby_d_balance_10k <- 
  read_csv(here("kirby-fig1d.csv")) %>% 
  pivot_longer(cols = !x, names_to = "last_name", values_to = "d_balance") %>% 
  mutate(athlete = case_when(
    last_name == "Farah" ~ "Mohammed Farah",
    last_name == "Cheptegei" ~ "Joshua Cheptegei",
    last_name == "Tanui" ~ "Paul Tanui",
    last_name == "Karoki" ~ "Bedan Karoki" 
  ),
  total_race_distance_meters = x,
  source = "Digitized Original")



d_balance_mens_10k <- calc_d_balance(runner_data_mens_10k, race_data_mens_10k)


filter_d_balance_mens_10k <- 
  d_balance_mens_10k %>% 
  filter(athlete %in%  c("Mohammed Farah", "Joshua Cheptegei","Paul Tanui", "Bedan Karoki" )) %>% 
  mutate(source = "Replication")


mens_10k <- bind_rows(filter_d_balance_mens_10k, kirby_d_balance_10k)

mens_10k$athlete <- factor(mens_10k$athlete, levels = c("Mohammed Farah",
                                                        "Joshua Cheptegei",
                                                        "Paul Tanui",
                                                        "Bedan Karoki"
                                                      ))

(plot_mens_10k <- 
  ggplot()+
    geom_line(data = mens_10k, 
              aes(total_race_distance_meters, 
                  d_balance, 
                  linetype = source,
                  color = athlete,
                  size = source
              ))+
    ylab("D' Balance")+
    xlab("Race Distance (m)")+
    ggtitle("10k Replication of Kirby 2021 Figure 1D")+
    scale_color_manual(name = "Athlete", values = brewer.pal(4, "Dark2"))+
    scale_linetype(name = "Data Source")+
    scale_size_manual(values = c(0.5, 1), guide = "none")+
    theme_cowplot(10)
    
)


######################

all_rep <- imap_dfr(list("5k" = d_balance_mens_5k, "10k" = d_balance_mens_10k), ~mutate(.x, race = .y))

all_rep$race <- factor(all_rep$race, levels = c("5k", "10k"))
ggplot()+
  geom_line(data = all_rep, 
            aes(total_race_distance_meters, 
                d_balance, 
                color = athlete,
            ))+
  facet_wrap(~race, scales = "free_x")+
  ylab("D' Balance")+
  xlab("Race Distance (m)")+
  ggtitle("Replication of all athletes D' Balance")+
  scale_y_continuous(breaks = seq(-200, 350, by = 50))+
  scale_color_manual(name = "Athlete", values = c(brewer.pal(8, "Dark2"), brewer.pal(9, "Set1"), "black"))+
  theme_cowplot()+
  theme(
    strip.background = element_rect(fill = "black" ),
    strip.text = element_text(color = "white"),
    panel.border = element_rect(color = "black", size = 1.2)
  )


race_data_mens_5k$meter_mark <- c(seq(400, 4400, by = 400), 4600, 5000)
cs_data_overlay_5k <- filter(runner_data_mens_5k, 
                          athlete %in% c("Muktar Edris", "Mohammed Farah", "Paul Chelimo", "Yomif Kejelcha"))

lollipop_5k <- 
ggplot()+
  geom_hline(data = cs_data_overlay_5k,
             aes(yintercept = critical_speed_meters_second, color = fct_reorder(athlete, critical_speed_meters_second)))+
  geom_segment(data = race_data_mens_5k,
               aes(x = meter_mark,
                   xend = meter_mark, 
                   y = 0, 
                   yend = lap_leader_speed_meters_second),
               size = 1)+
  geom_point(data = race_data_mens_5k, 
             aes(x = meter_mark, 
                 y = lap_leader_speed_meters_second), 
             size = 9,
             shape = 21,
             fill = "grey15",
             stroke = 1.5)+
  
  geom_text(data = race_data_mens_5k, 
            aes(x = meter_mark, 
                y = lap_leader_speed_meters_second,
                label = round(lap_leader_speed_meters_second, 2)),
            size = 3,
            color = "white")+
  ylab("Lap Speed (meters/second)")+
  xlab("Distance (meters)")+
  ggtitle("Men's 5k")+
  coord_cartesian(ylim = c(5, 8))+
  scale_y_continuous(expand = expansion(c(0, 0.1)))+
  scale_x_continuous(breaks = race_data_mens_5k$meter_mark)+
  scale_color_manual(name = "Athlete Critical Speed", values = brewer.pal(4, "Dark2"), guide = guide_legend(reverse=TRUE))+
  theme_cowplot(10)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.1, 0.7)
  )
 

race_data_mens_10k$meter_mark <- seq(400, 10000, by = 400)
cs_data_overlay_10k <- filter(runner_data_mens_10k, 
                              athlete %in%  c("Mohammed Farah", "Joshua Cheptegei","Paul Tanui", "Bedan Karoki" ))

(lollipop_10k <- 
ggplot()+
  geom_hline(data = cs_data_overlay_10k,
             aes(yintercept = critical_speed_meters_second, color = fct_reorder(athlete, critical_speed_meters_second)))+
  geom_segment(data = race_data_mens_10k,
               aes(x = meter_mark,
                   xend = meter_mark, 
                   y = 0, 
                   yend = lap_leader_speed_meters_second),
               size = 1)+
  geom_point(data = race_data_mens_10k, 
             aes(x = meter_mark, 
                 y = lap_leader_speed_meters_second), 
             size = 8,
             shape = 21,
             fill = "grey15",
             stroke = 1.2)+
  
  geom_text(data = race_data_mens_10k, 
            aes(x = meter_mark, 
                y = lap_leader_speed_meters_second,
                label = round(lap_leader_speed_meters_second, 2)),
            size = 3,
            color = "white")+
  ylab("Lap Speed (meters/second)")+
  xlab("Distance (meters)")+
  coord_cartesian(ylim = c(5.7, 7.2))+
  scale_y_continuous(expand = expansion(c(0, 0.1)))+
  ggtitle("Men's 10k")+
  scale_x_continuous(breaks = race_data_mens_10k$meter_mark)+
  scale_color_manual(name = "Athlete Critical Speed", values = brewer.pal(4, "Dark2"), guide = guide_legend(reverse=TRUE))+
  theme_cowplot(12)+
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = c(0.1, 0.7)
  )
)

top <- plot_grid(lollipop_5k, plot_mens_5k, nrow = 1, labels = c("A", "B"))
bottom <- plot_grid(lollipop_10k, plot_mens_10k, nrow = 1, labels = c("C", "D"))
middle <-  plot_grid(plot_mens_5k,
                     plot_mens_10k,
                     nrow = 1,
                     labels = c("B", "C"),
                     rel_widths = c(0.45, 0.55))

plot_grid(lollipop_5k, middle, lollipop_10k, nrow = 3, labels = c("A", "", "D"))

