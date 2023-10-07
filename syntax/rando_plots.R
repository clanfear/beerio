round_table |>
  filter(player %in% c("Neil", "Chuck", "Shawn", "Steve")) |>
  ggplot(aes(x = date, y = score, group = player, color = player)) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Round scores by date")

game_table |>
  filter(player %in% c("Neil", "Chuck", "Shawn", "Steve")) |>
  ggplot(aes(x = date, y = score, group = player, color = player)) +
  geom_smooth() +
  theme_minimal() +
  labs(title = "Game scores by date")

regression_data <- game_table |>
  filter(player %in% c("Neil", "Chuck", "Shawn")) |>
  group_by(player) |>
  mutate(past_week_games = sapply(seq(length(date)), 
                                   function(x) sum(between(date[1:x], date[x] - days(7), date[x])))) |>
  ungroup() |>
  mutate(player = fct_relevel(player, "Neil")) |>
  arrange(game_id, player) |>
  filter(row_number() > 30)

lm(score ~ player + player*date, data = regression_data) |>
  summary()

lm(score ~ player*game + player*date, data = regression_data) |>
  summary()


game_table |>
  filter(player %in% c("Neil", "Chuck", "Shawn", "Steve")) |>
  group_by(date) |>
  mutate(date_pr = cur_group_id(),
         game_pr = game / max(game)) |>
  ungroup() |>
  mutate(date_num = date_pr + game_pr) |>
  ggplot(aes(x = date_num, y = score, group = player, color = player)) +
  geom_point(alpha = 0.5) +
  theme_minimal() +
  labs(title = "Game scores by time-date")
