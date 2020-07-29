
# plotting ----------------------------------------------------------------

cwa <- coffee_with_altitude

cwa %>%
  ggplot() +
  aes(x = upper_altitude, y = total_cup_points, colour = continent) +
  geom_point() +
  facet_wrap("species")


cwa %>%
  filter(species == "Arabica") %>%
  filter(continent == "South America") %>%
  ggplot() +
  aes(x = lower_altitude, y = total_cup_points, colour = country_of_origin) +
  geom_point()

cwa %>%
  filter(!is.na(colour)) %>%
  ggplot() +
  aes(x = upper_altitude, y = total_cup_points, colour = colour) +
  geom_point()


flavour_labels <- cwa %>%
  rowwise() %>%
  mutate(mean_altitude = round(mean(lower_altitude, upper_altitude))) %>%
  arrange(desc(flavour)) %>%
  filter(flavour > 8) %>%
  group_by(country_of_origin) %>%
  slice(1) %>%
  ungroup()

cwa %>%
  # filter(continent == "Africa") %>%
  rowwise() %>%
  mutate(mean_altitude = round(mean(lower_altitude, upper_altitude))) %>%
  ggplot() +
  aes(x = mean_altitude, y = flavour, colour = country_of_origin, label = country_of_origin) +
  geom_point() +
  geom_text(data = flavour_labels,
            # mapping = aes(x = mean_altitude, y = flavour, colour = country_of_origin),
             show.legend = FALSE,
            size = 2,
            nudge_x = -500,
            nudge_y = 0.1
             ) +
  facet_wrap("continent")
