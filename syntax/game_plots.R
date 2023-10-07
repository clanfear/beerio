library(tidyverse)
library(showtext)
load("./data/game_table.RData")
load("./data/beverage_table.RData")

font_files() |> 
  filter(str_detect(ps_name, "Mario")) |>
  with(font_add(family, str_c(path, "/", file)))

theme_mk8 <- function(){
  theme_minimal(base_size = 12) + 
    theme(title  = element_text(family = "Mario Kart F2"))
}
beu_score <- game_table |>
  inner_join(beverage_table |> 
             filter(!is.na(beverage))) |>
  semi_join(game_table |>
              count(player) |>
              filter(n > 500)) |>
  group_by(date, player) |>
  arrange(date, game) |>
  mutate(current_beu = cumsum(beu), .group = "drop") |>
  ggplot(aes(x = current_beu, y = score, group = player, color = player)) +
  scale_color_manual("",values = c("Chuck" = "#F8766D", "Neil" = "#7CAE00", "Shawn" = "#00BFC4", "Steve"="#C77CFF")) +
  geom_smooth(se = FALSE) +
  scale_y_continuous(limits = c(0, NA)) +
  theme_mk8() + 
  theme(legend.position = "bottom") + 
  labs(y = NULL, title = "Declining Functionality", subtitle = "Average game score by current BEU")
ggsave(beu_score, file = "./docs/_site/img/beu_score.png", width = 6, height = 6, dpi = 300)


