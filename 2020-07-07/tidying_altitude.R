
# libraries ---------------------------------------------------------------

options(conflicts.policy = list(warn.conflicts = FALSE))

library(tidyverse)
library(here)
library(janitor)

# get data ----------------------------------------------------------------

coffee_ratings <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv")



coffee_with_altitude <- coffee_ratings %>%
  filter(!is.na(altitude)) %>%
  filter(!altitude %in% c("test", "mmm", "huanuco")) %>%
  select(-c(lot_number, ico_number, starts_with("certification"), unit_of_measurement))


# split for altitude tidying ----------------------------------------------

cwa0 <- coffee_ratings %>%
  filter(is.na(altitude)) %>%
  filter(!is.na(farm_name)) %>%
  filter(farm_name %in% coffee_with_altitude$farm_name) %>%
  select(-c(lot_number, ico_number, starts_with("certification"), unit_of_measurement))

fill_ins <- coffee_with_altitude %>%
  filter(farm_name %in% cwa0$farm_name) %>%
  select(farm_name, altitude, region, producer) %>%
  distinct()

cwa0 <- cwa0 %>%
  select(-altitude) %>%
  inner_join(fill_ins)
rm(fill_ins)

cwa1 <- coffee_with_altitude %>%
  filter(str_detect(altitude, "[:digit:]{3,}.*(m{1}$|metros|mts$|m\\.*s\\.*n\\.*m\\.*$|meters$|m[a|o]sl$|dpl$|\\(.*$)"))

cwa2 <- coffee_with_altitude %>%
  filter(str_detect(altitude, "^meters"))

cwa3 <- coffee_with_altitude %>%
  filter(str_detect(altitude, ".*(ft\\.*|pies|psn|')$"))

cwa4 <- coffee_with_altitude %>%
  filter(str_detect(altitude, "^above|^de|^approx\\.*"))

cwa5 <- coffee_with_altitude %>%
  filter(str_detect(altitude, "^[:digit:]{3,4}m?\\s?[:punct:]+\\s?[:digit:]{3,4}$"))

cwa6 <- coffee_with_altitude %>%
  filter(str_detect(altitude, "^[:digit:]{3,4}$"))

cwa7 <- coffee_with_altitude %>%
  filter(str_detect(altitude, "^[:digit:]{3,}[^[:alpha:]-'/]+$")) %>%
  filter(!str_detect(altitude, "^[:digit:]{3,4}$"))

cwa8 <- coffee_with_altitude %>%
  filter(str_detect(company, "^ipanema")) %>%
  filter(str_detect(farm_name, "^rio verde"))


# tidy up altitude column -------------------------------------------------


cwa0 <- cwa0 %>%
  mutate(across(altitude, ~ str_remove(., "\\s?\\+\\+$"))) %>%
  mutate(across(altitude, ~ str_replace(., ".*ft\\.?$", "1400")))

cwa1 <- cwa1 %>%
  mutate(across(altitude, ~ str_remove_all(., "^[^0-9]*|[^0-9]*$|\\s?meters\\s?\\(.*$|\\.00|\\."))) %>%
  mutate(across(altitude, ~ str_replace(., "11000", "1100")))

cwa2 <- cwa2 %>%
  mutate(across(altitude, ~ str_remove_all(., "^[^0-9]*|\\.")))

cwa3 <- cwa3 %>%
  mutate(across(altitude, ~ str_remove(., "[^0-9]*$"))) %>%
  mutate(lower_altitude = round(as.numeric(str_extract(altitude, "^[0-9]+"))/3.3)) %>%
  mutate(upper_altitude = round(as.numeric(str_extract(altitude, "[0-9]+$"))/3.3)) %>%
  select(!starts_with("altitude"))

cwa4 <- cwa4 %>%
  mutate(across(altitude, ~ str_remove_all(., "^[^0-9]*|[^0-9]*$|\\.|'")))

cwa5 <- cwa5 %>%
  filter(producer == "Immaculata John") %>%
  mutate(across(altitude, ~ str_replace(., ".*", "1500-2000"))) %>%
  bind_rows(cwa5 %>%
              filter(!producer == "Immaculata John"))

cwa7 <- cwa7 %>%
  mutate(across(altitude, ~ str_replace(., "800\\+\\+", "800-1200"))) %>%
  mutate(across(altitude, ~ str_replace(., "^1901.*", "1900")))

cwa8 <- cwa8 %>%
  mutate(across(altitude, ~ str_replace(., "^1$", "1200")))


# reconstitute the CWA df -------------------------------------------------


coffee_with_altitude <- bind_rows(cwa0, cwa1, cwa2, cwa4, cwa5, cwa6, cwa7, cwa8) %>%
  mutate(lower_altitude = as.numeric(str_extract(altitude, "^[0-9]+"))) %>%
  mutate(upper_altitude = as.numeric(str_extract(altitude, "[0-9]+$"))) %>%
  mutate(across(c(lower_altitude, upper_altitude), ~ case_when(
    lower_altitude > 2600 ~ round(.x/3),
    lower_altitude <= 2600 ~ .x
  ))) %>%
  select(!starts_with("altitude")) %>%
  bind_rows(cwa3) %>%
  distinct() %>%
  arrange(desc(total_cup_points)) %>%
  filter(total_cup_points > 0) %>%
  mutate(across(country_of_origin, ~ str_replace(., "^Cote.*", "Côte d'Ivoire"))) %>%
  mutate(across(country_of_origin, ~ str_replace(., "^Tanzania.*", "Tanzania"))) %>%
  mutate(continent = case_when(
    str_detect(country_of_origin, "Brazil|Colombia|Ecuador|Peru") ~ "South America",
    str_detect(country_of_origin, "United States|Mexico") ~ "North America",
    str_detect(country_of_origin, "China|India|Indonesia|Japan|Laos|Myanmar|Philippines|Taiwan|Thailand|Vietnam") ~ "Asia",
    str_detect(country_of_origin, "Costa Rica|El Salvador|Guatemala|Haiti|Honduras|Nicaragua|Panama") ~ "Central America",
    str_detect(country_of_origin, "Papua") ~ "Oceania",
    str_detect(country_of_origin, "Burundi|Côte d'Ivoire|Ethiopia|Kenya|Malawi|Mauritius|Rwanda|Tanzania|Uganda|Zambia") ~ "Africa"
  )) %>%
  mutate(across(c(color, species, continent), as_factor)) %>%
  rename(flavour = flavor, colour = color)

rm(cwa0, cwa1, cwa2, cwa3, cwa4, cwa5, cwa6, cwa7, cwa8)

# saveRDS(coffee_with_altitude, here("coffee_with_altitude.Rds"))
