library(tidyverse)
library(showtext)
load("./data/round_table.RData")

font_files() |> 
  filter(str_detect(ps_name, "Mario")) |>
  with(font_add(family, str_c(path, "/", file)))

theme_mk8 <- function(){
  theme_minimal(base_size = 12) + 
    theme(title  = element_text(family = "Mario Kart F2"))
}

# RElative strength

relative_strength_plot <- round_table |>
  filter(!is.na(map_name)) |>
  semi_join(round_table |>
              count(player) |>
              filter(n > 500)) |>
  group_by(player) |>
  mutate(overall_avg = mean(score)) |>
  group_by(player, map_name) %>% 
  summarize(`Relative Average` = mean(score - overall_avg), .groups="drop") |>
  mutate(map_name = fct_rev(map_name)) |>
  ggplot(aes(x = `Relative Average`, y = map_name, group = player, fill = player)) +
    scale_fill_manual("",values = c("Chuck" = "#F8766D", "Neil" = "#7CAE00", "Shawn" = "#00BFC4", "Steve"="#C77CFF")) +
    geom_col(position="dodge") + 
    theme_mk8() + 
    theme(legend.position = "bottom", panel.grid = element_blank()) + 
    labs(y = NULL, title = "Relative Strength", subtitle = "Difference from average score by map") +
    geom_hline(yintercept = seq(1.5, 7.5, by = 1), color = "gray80") +
    scale_x_continuous(breaks = scales::pretty_breaks(3))

ggsave(relative_strength_plot, file = "./docs/_site/img/relative_strength_plot.png", width = 6, height = 6, dpi = 300)

relative_strength_year_plot <- round_table |>
  filter(!is.na(map_name)) |>
  filter(date <= max(date) - 365) |>
  semi_join(round_table |>
              count(player) |>
              filter(n > 500)) |>
  group_by(player) |>
  mutate(overall_avg = mean(score)) |>
  group_by(player, map_name) %>% 
  summarize(`Relative Average` = mean(score - overall_avg), .groups="drop") |>
  mutate(map_name = fct_rev(map_name)) |>
  ggplot(aes(x = `Relative Average`, y = map_name, group = player, fill = player)) +
  scale_fill_manual("",values = c("Chuck" = "#F8766D", "Neil" = "#7CAE00", "Shawn" = "#00BFC4", "Steve"="#C77CFF")) +
  geom_col(position="dodge") + 
  theme_mk8() + 
  theme(legend.position = "bottom", panel.grid = element_blank()) + 
  labs(y = NULL, title = "Relative Strength", subtitle = "In past year") +
  geom_hline(yintercept = seq(1.5, 7.5, by = 1), color = "gray80") +
  scale_x_continuous(breaks = scales::pretty_breaks(3))

ggsave(relative_strength_year_plot, file = "./docs/_site/img/relative_strength_year_plot.png", width = 6, height = 6, dpi = 300)

# Overall round scores by time
avg_round_scores_plot <- round_table |>
  filter(!is.na(map_name)) |>
  semi_join(round_table |>
              count(player) |>
              filter(n > 500)) |>
  ggplot(aes(x = date, y = score, group = player, color = player)) +
  scale_color_manual("",values = c("Chuck" = "#F8766D", "Neil" = "#7CAE00", "Shawn" = "#00BFC4", "Steve"="#C77CFF")) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_mk8() + 
  theme(legend.position = "bottom") + 
  labs(y = NULL, title = "Round Scores", subtitle = "Player averages over time")
ggsave(avg_round_scores_plot, file = "./docs/_site/img/avg_round_scores_plot.png", width = 6, height = 6, dpi = 300)

avg_round_scores_course_plot <- round_table |>
  filter(!is.na(map_name)) |>
  semi_join(round_table |>
              count(player) |>
              filter(n > 500)) |>
  ggplot(aes(x = date, y = score, group = map_name, color = map_name)) +
  facet_wrap(~player) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_mk8() + 
  theme(legend.position = "bottom") + 
  labs(y = NULL, title = "Round Scores", subtitle = "Player averages by map", color = NULL)
ggsave(avg_round_scores_course_plot, file = "./docs/_site/img/avg_round_scores_course_plot.png", width = 6, height = 6, dpi = 300)

#

# Overall round scores by time


avg_round_scores_players_plot <- round_table |>
  filter(!is.na(map_name)) |>
  semi_join(round_table |>
              count(player) |>
              filter(n > 500)) |>
  inner_join(game_table |>
              count(game_id) |> filter(n <=5)) |>
  group_by(player, map_name, n) |>
  summarize(avg_score = mean(score)) |>
  ggplot(aes(x = n, y = avg_score, group = player, color = player)) +
  scale_color_manual("",values = c("Chuck" = "#F8766D", "Neil" = "#7CAE00", "Shawn" = "#00BFC4", "Steve"="#C77CFF")) +
  geom_line() +
  facet_wrap(~map_name, nrow = 4) +
  theme_mk8() + 
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank()) + 
  labs(y = NULL, title = "Average Round Scores", subtitle = "By number of players", x = "Number of players")
ggsave(avg_round_scores_players_plot, file = "./docs/_site/img/avg_round_scores_players_plot.png", width = 5, height = 8, dpi = 300)
