library(tidyverse)
library(tidytuesdayR)
library(here)

latest <- as.character(tidytuesdayR::last_tuesday())

if(!dir.exists(here(latest))) dir.create(here(latest))

setwd(here(latest))

filenames <- c("penguins.csv", "penguins_raw.csv", "readme.md")

urls <- paste0(
  "https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/",
  latest,
  "/",
  filenames
)

urls %>%
  map( ~ myrmidon::download_file(., dir = latest))
