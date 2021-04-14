# US Post Offices

library(tidyverse)
library(patchwork)

#' "Blevins, Cameron; Helbock, Richard W., 2021, "US Post Offices", https://doi.org/10.7910/DVN/NUKCNA, 
#' Harvard Dataverse, V1, UNF:6:8ROmiI5/4qA8jHrt62PpyA== [fileUNF]"
#' 
#' US Post Offices is a spatial-historical dataset containing records for 166,140 post offices 
#' that operated in the United States between 1639 and 2000. The dataset provides a year-by-year 
#' snapshot of the national postal system over multiple centuries, making it one of the most fine-grained 
#' and expansive datasets currently available for studying the historical geography of the United States
#' 
#' https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-04-13/readme.md

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv')

closures <- post_offices %>%
  select(established, discontinued, duration) %>%
  filter(!is.na(discontinued)) %>%
  filter(!established < 1763) %>% # Some have established dates of 185? Missing code?
  arrange(established, duration) %>%
  mutate(rank_discontinued = row_number(discontinued)) %>%
  filter(!discontinued > 2003) %>% # some very large discontinued numbers
  filter(!duration < 0) # Some negative durations

closure_plot <- closures %>%
  ggplot() +
  geom_segment(aes(x = established, xend = discontinued, y = rank_discontinued, yend = rank_discontinued),
               size = 0.01, alpha = 0.3) +
  annotate("text", x = 2002, y = max(closures$rank_discontinued) + 3e+3, label = "2002") +
  annotate("text", x = 1800, y = -3000, label = "1800") +
  annotate("text", x = 1905, y = 70900, label = "1900", hjust = 0) +
  ggtitle("Post Office Closures in the United States (1764 - 2002)",
          subtitle = "Line end equals closure year. Line start equals year established.") +
  labs(caption = "Data from Blevins & Helbock (2021) via #TidyTuesday. DataVis by @cjrwebb") +
  theme_void() +
  theme(plot.margin = margin(20, 40, 20, 40), plot.title = element_text(hjust = 1, face = "bold"), 
        plot.subtitle = element_text(hjust = 1))

ggsave(plot = closure_plot, filename = "post-office-closures.png", dpi = 700,
       width = 5, height = 8, units = "in", scale = 1.3/1)
