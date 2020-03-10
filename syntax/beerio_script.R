library(tidyverse)
library(googlesheets4)

beerio_sheet_raw_new <- read_sheet("https://docs.google.com/spreadsheets/d/1V-7lY3NVqVGGEWL-jwwmjYcpLT8mJMN1MI11y-C3Bl0/", sheet = "Data")
save(beerio_sheet_raw, file = "../data/beerio_sheet_raw.RData")

beerio_sheet_raw_new 
