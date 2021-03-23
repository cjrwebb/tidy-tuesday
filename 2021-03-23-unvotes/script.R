library(tidyverse)
library(rnaturalearth)
library(patchwork)
library(countrycode)
library(tmap)
library(grid)
library(ggplotify)

unvotes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/unvotes.csv')
roll_calls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/roll_calls.csv')
issues <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-23/issues.csv')

unvotes
roll_calls
issues

# Votes related to children's rights
uncrc <- roll_calls %>%
  filter(unres %in% c("A/RES/72/245", "R/42/101", "R/57/190", "R/58/157", "R/60/231", "R/61/146", "R/62/141", 
                      "R/63/241", "A/RES/72/245")) %>%
  filter(!is.na(short))

crc_votes <- unvotes %>%
  filter(rcid %in% uncrc$rcid) %>%
  filter(rcid == "3194")

world <- ne_countries(returnclass = "sf")

crc_sf <- left_join(world, crc_votes, by = c("iso_a2" = "country_code"))

crc_sf <- crc_sf %>%
  filter(iso_a2 %in% crc_votes$country_code)

crc_map <- crc_sf %>%
  mutate(vote_col = ifelse(vote == "yes", "#3F8BD8", "white")) %>%
  tm_shape(.) +
  tm_borders() +
  tm_fill(col = "vote_col") +
  tm_facets(by = "name") +
  tm_layout(main.title = "The United Nations voted in 1987 to draft the UN Convention on the\nRights of the Child. The CRC was signed in 1989. The US was the only\ncountry to abstain from the vote and, though signing, has never\nratified the CRC.\n",
            frame = FALSE, frame.lwd = NA, panel.label.bg.color = NA,
            panel.label.size = 1, fontfamily = "Georgia", fontface = "bold")

crc_map


  