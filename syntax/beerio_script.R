library(tidyverse)
library(googlesheets4)
library(extrafont)
library(ggforce)
# ttf_import("../syntax/fonts")
# font_import()
# Sys.setenv(R_GSCMD="C:/Program Files/gs/gs9.50/bin/gswin64c.exe")
# loadfonts(device = "win")
#sheets_auth("cclanfear@gmail.com")
theme_mk8 <- function(){
  theme_minimal() + 
    theme(title  = element_text(family = "Mario Kart F2"), 
          panel.grid.major.x = element_blank(), 
          panel.grid.minor.x = element_blank())
}

beerio_sheet_raw <- read_sheet("https://docs.google.com/spreadsheets/d/1V-7lY3NVqVGGEWL-jwwmjYcpLT8mJMN1MI11y-C3Bl0/", sheet = "Data")

beerio_data <- beerio_sheet_raw %>%
  select(-contains("...")) %>%
  filter_all(any_vars(!is.na(.))) %>%
  fill(Date) %>%
  group_by(Date) %>%
  mutate(Day = str_replace(format(Date, "%b %e"), "  ", " ")) %>%
  mutate(Game = row_number()) %>%
  pivot_longer(c(-Date, -Punishment, - Game, -Day), names_to = "Name", values_to = "Score") %>%
  filter(!is.na(Score)) %>%
  ungroup() %>%
  group_by(Name) %>%
  mutate(Player_Game = row_number()) %>%
  mutate(Punishment = ifelse(is.na(Punishment), "None", Punishment)) %>%
  mutate(Punished = str_remove(str_extract(Punishment, "-> [A-Za-z]*"), "-> ")) %>% 
  mutate(Punisher = str_remove(str_extract(Punishment, "[A-Za-z]* ->"), " ->")) %>%
  mutate(Punishment = ifelse(Name == Punished, paste0("Punished by ", ifelse(Punisher==Punished, "himself", Punisher)), NA) )

beerio_data %>%
  ggplot(aes(x = Game, y = Score, group = Name, color = Name)) + 
  facet_wrap(~Day) + 
  geom_line(size = 2) + 
  geom_point(data = beerio_data %>% 
               filter(!is.na(Punishment)), 
             aes(x = Game, y = Score), 
             size = 5, fill = "black") + ylab("") +
  geom_mark_circle(aes(label = Name, description = Punishment, filter = !is.na(Punishment))) +
  ggtitle("Beerio Scores", subtitle = "Over the course of evenings") +
  theme_mk8()
  
beerio_data %>%
  ggplot(aes( x = Name, y = Score, fill = Name)) + geom_violin() + xlab("") + ylab("") + theme_mk8() + theme(legend.position = "none") + ggtitle("Beerio Score Distributions")


