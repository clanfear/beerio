library(tidyverse)


# Score discrepancies
score_discrepancies <- round_table |> 
  group_by(date, game, player) |>
  summarize(round_score = sum(score)) |>
  inner_join( game_table |>
    select(date, game, player, game_score = score)) |>
  filter(round_score != game_score)

write_csv(score_discrepancies, file = "./data/score_discrepancies.csv")
