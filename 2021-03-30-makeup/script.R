library(tidyverse)
library(tidytext)
library(janitor)
library(waffle)

allShades <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-30/allShades.csv')

# Get frequency count for each name (standardised to lower case)
freq_names <- allShades %>% 
  mutate(name = tolower(name)) %>%
  count(name) %>%
  drop_na() %>%
  arrange(desc(n)) %>%
  mutate(rank = rank(desc(n), ties.method = "first"))

# join frequencies to data 
shades <- left_join(allShades %>% mutate(name = tolower(name)), freq_names, by = c("name"))

# Select 30 largest collections/shared shades within names
shades <- shades %>%
  filter(rank <= 31)

# Get sums for each name
hexsums <- shades %>%
  arrange(lightness) %>%
  group_by(hex) %>%
  summarise(name = first(name)) %>%
  janitor::tabyl(hex, name) %>%
  as_tibble()

# Convert sums for each name by hex to long format
hexsums_long <- hexsums %>%
  pivot_longer(cols = almond:warm) %>%
  filter(!value == 0) %>%
  mutate(name = toupper(name))

# Get position information for facet titles, so they float directly on top when plotted
# (when including offset)
pos_labs <- hexsums_long %>%
  count(name) %>%
  mutate(pos_label = plyr::round_any((n-1)/10, accuracy = 1, f = floor))

#' Plot facetted waffle plot using waffle package (github version)
#' This requires Helvetica Neue fonts to be installed via extrafont
hexsums_long %>%
  ggplot() +
  geom_waffle(aes(fill = hex, values = value), 
              color = "white", size = 1.1, radius = unit(3, "pt"),
              flip = TRUE) +
  facet_wrap(~name, nrow = 3) +
  coord_equal() +
  scale_fill_identity() +
  geom_text(data = pos_labs, aes(x = 0.56, y = pos_label+2.5, label = name), 
            hjust = 0, family = "HelveticaNeue-Medium", colour = "black", size = 3.5) +
  theme_void() +
  ggtitle("Foundation shades for the 30 largest collection names") +
  labs(caption = "\nDatavis by @cjrwebb. Data from ThePudding via #TidyTuesday.") +
  theme(strip.text = element_blank(), plot.margin = unit(c(1,1,1,1), "in"), 
        plot.title = element_text(family = "HelveticaNeue-UltraLight", size = 42),
        plot.caption = element_text(family = "HelveticaNeueLight", size = 11)) 
