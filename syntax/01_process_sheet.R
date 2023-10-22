library(tidyverse)
library(googlesheets4)

game_table <- read_sheet("https://docs.google.com/spreadsheets/d/1V-7lY3NVqVGGEWL-jwwmjYcpLT8mJMN1MI11y-C3Bl0/", sheet = "Data", guess_max = 99999) |>
  select(-matches("BEU|\\.\\.\\.")) |>
  rename(date = Date) |>
  mutate(game_id = row_number(),
         date = as.Date(date)) |>
  fill(date, .direction = "down") |>
  group_by(date) |>
  mutate(game = row_number()) |>
  ungroup() |>
  pivot_longer(cols = -c(game_id, game, date, Punishment), values_transform = as.character) |>
  mutate(type = ifelse(str_detect(name, "Beverage"), "beverage", "score"),
         name = str_remove(name, "_Beverage")) |> 
  pivot_wider(names_from = type, values_from = value) |>
  filter(!is.na(score)) |>
  mutate(score = as.numeric(score)) |>
  select(game_id, game, date, player = name, score, beverage)


beverage_table <- read_sheet("https://docs.google.com/spreadsheets/d/1V-7lY3NVqVGGEWL-jwwmjYcpLT8mJMN1MI11y-C3Bl0/", sheet = "Beverages", guess_max = 99999) %>%
  select(beverage = Name, volume_oz = Oz, abv = `Alcohol %`, beu = BEU, worst_rank = `Worst Ranking`)

round_table_wide <- read_sheet("https://docs.google.com/spreadsheets/d/1V-7lY3NVqVGGEWL-jwwmjYcpLT8mJMN1MI11y-C3Bl0/", sheet = "Round Data", guess_max = 99999) |>
  select(-contains("...")) |>
  select(Date:Shot) |>
  rename(date = Date, map = Map, game = Game, round = Round, shot = Shot) |>
  mutate(date = as.Date(date),
         round_id = row_number()) |>
  filter_all(any_vars(!is.na(.))) |>
  fill(date, .direction = "down") 

punishment_table <- round_table_wide |>
  select(date, game, round, round_id, shot) |>
  filter(!is.na(shot)) |>
#  filter(str_detect(shot, ",")) |>
  separate_longer_delim(shot, ", ") |>
  mutate(punishment_id = row_number(),
         give = str_squish(str_extract(shot, "^.*?(?=->)")),
         take = str_squish(str_extract(shot, "(?<=->).*$"))) |>
  select(-shot) |>
  left_join(game_table |> distinct(game_id, date, game)) |>
  mutate(shot_type = ifelse(give==take, "self", "given"))

round_table <- round_table_wide |>
  select(-shot) |>
  pivot_longer(-c(round_id, date, game, round, map), 
               names_to = "player", 
               values_to = "score") |>
  filter(!is.na(score)) |>
  left_join(game_table |> distinct(game_id, date, game)) |>
  left_join(punishment_table |>
              count(game_id, round_id, give, name = "shots_given") |>
              rename(player = give)) |>
  left_join(punishment_table |>
              count(game_id, round_id, take, name = "shots_taken") |>
              rename(player = take)) |>
  replace_na(list(shots_given = 0, shots_taken = 0)) |>
  mutate(map_name = c("LM" = "Luigi's Mansion",
                  "DP" = "Dragon Palace",
                  "BC" = "Battle Course 1",
                  "BS" = "Battle Stadium",
                  "SK" = "Sweet Sweet Kingdom",
                  "WT" = "Wuhu Town",
                  "UU"= "Urchin Underpass",
                  "LC"="Lunar Colony")[map])



                        


character_table <- tribble(~player, ~character, ~sex,
                           "Neil", "Black Shy Guy", "Male",
                           "Chuck", "Pink Shy Guy", "Male",
                           "Shawn", "Cat Peach", "Male",
                           "Steve", "Toad", "Male",
                           "Kivan", "Villager", "Male",
                           "Karly", "Lakitu", "Female",
                           "Jacob", "Orange Shy Guy", "Male",
                           "Andy", "Mario", "Male",
                           "Marshall", "Luigi", "Male"
                           )

player_table <- game_table |>
  group_by(player) |>
  summarize(n_games = n(),
            across(score, list(mean = mean, median=median, min=min, max=max), .names = "{.fn}_{.col}")) |>
  left_join(character_table)

save(beverage_table, file = "./data/beverage_table.RData")
save(game_table, file = "./data/game_table.RData")  
save(round_table, file = "./data/round_table.RData")
save(punishment_table, file = "./data/punishment_table.RData") 
