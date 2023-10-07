library(tidyverse)
library(tesseract)

img_files <- list.files("./data/score_images/", full.names = TRUE)
img_files
eng_engine <- tesseract("eng")
img_file <- magick::image_read(img_files[1])
?magick::ocr
ext_text <- ocr(img_file, engine = eng_engine)
