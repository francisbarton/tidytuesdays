
# libraries ---------------------------------------------------------------

library(dplyr, warn.conflicts = FALSE)
library(forcats, warn.conflicts = FALSE)
library(ggplot2)
library(readr)
library(stringr)


# data import -------------------------------------------------------------


firsts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/firsts.csv')
science <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-09/science.csv')


# data fixes --------------------------------------------------------------


links <- science$links %>%
  tail(-1) %>%
  c(., "https://en.wikipedia.org/wiki/Roger_Arliner_Young")

science <- science %>%
  select(-links) %>%
  mutate(links = links)

readr::write_csv(science, "science.csv")

science <- science %>%
  select(-references)

science %>%
  mutate(surname = str_extract(name, "^[^,]*"), .before = "name") %>%
  mutate(firstnames = str_extract(name, "(?<=, ).*"), .before = "name")


firsts <- rename(firsts, "name" = person)

firsts_nas <- firsts %>%
  filter(is.na(name))

na_names <- c("Joe Gans", "Dora Lee Jones", "Martin A. Martin")

firsts_nas <- mutate(firsts_nas, name = na_names)

firsts <- filter(firsts, !is.na(name)) %>%
  bind_rows(firsts_nas) %>%
  arrange(year)

rm(firsts_nas, na_names, links)

# removing this column as too many inaccuracies make it less than helpful
firsts <- firsts %>%
  select(-gender)

firsts <- firsts %>%
  mutate_at(vars(name), ~ str_replace(., "\\[.*$", "")) %>%
  mutate_at(vars(name), ~ str_replace(., "\\((S|s)ee also.*$", "")) %>%
  mutate_at(vars(name), ~ str_replace(., "\\.$", "")) %>%
  mutate_at(vars(category), ~ as_factor(.))


