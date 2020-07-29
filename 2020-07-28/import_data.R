library(here)


penguins.csv <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins.csv'

penguins_raw.csv <- 'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-28/penguins_raw.csv'

download_file(penguins.csv, dir = latest)
download_file(penguins_raw.csv, dir = latest)
