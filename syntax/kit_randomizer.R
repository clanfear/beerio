library(tidyverse)
library(googlesheets4)

sheet_columns <- c("name", 
                   paste("speed", c("land", "ag","water","air"), sep = "_"), 
                   "accel", "weight", 
                   paste("handling", c("land", "ag","water","air"), sep = "_"), 
                   "traction", "turbo")
sheet_types   <- paste0(c("c",rep("d", 12)), collapse="")
sheet_address <- "https://docs.google.com/spreadsheets/d/1g7A-38tn9UAIbB2B3sZI-MpILsS3ZS870UTVMRRxh4Q/edit#gid=0"
sheet_index   <- c(character = "Sheet1!A3:M18",
                   vehicle = "Sheet1!A20:M33",
                   wheels  = "Sheet1!A35:M43",
                   glider   = "Sheet1!A45:M48")

mk8_kit <- map_dfr(sheet_index, 
                   ~ read_sheet(sheet_address, 
                                range = .,
                                col_types = sheet_types,
                                col_names = sheet_columns), 
                   .id = "type") %>%
  mutate(type = fct_relevel(type, "character", "vehicle", "wheels", "glider"))

name_pattern <- "^(\\p{L}| |-|\\d)+"
mk8_equivs   <- read_sheet(sheet_address, 
                           range = "Sheet1!O3:O48",
                           col_types = "c", 
                           col_names = "col") %>% 
  na.omit() %>% 
  pull(col) %>%
  {setNames(
    lapply(
      str_split(str_remove(., name_pattern), " = "), \(x) str_remove(x,"^= ")), 
      str_squish(str_extract(., name_pattern)))} %>%
  stack()

mk8_stats <- mk8_equivs %>%
  inner_join(mk8_kit, by = c("ind"="name")) %>%
  rename(name = values, template = ind)

write_csv(mk8_stats, file = "output/mk8_stats.csv")

random_kit <- \(n = 1){
  replicate(n,
    ungroup(sample_n(group_by(mk8_stats, type), 1)) %>%
      summarize(kit = paste0(name, collapse = "\n"),
              across(where(is.numeric), ~sum(.))), simplify = TRUE
  )
}
random_kit(4)
