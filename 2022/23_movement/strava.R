setwd("~/R/30DayMapChallenge/2022/23_movement")

library(strava)
library(tidyverse)
library(showtext)

text = "Kranky"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()


data <- process_data("export_91940264/activities")

bg="yellow"
txt_col = "#141414"

map <- data %>% 
  #filter(!(id %in% c(94,370))) %>%
  ggplot(aes(lon, lat, group = id)) +
  geom_path(colour=txt_col,alpha = 0.2) + 
  coord_map(projection = "mercator", 
            clip = "on") +
  labs(title = "Excursions in Ghent by bike",
       caption = "Source: Strava | Graphic: Abhianv Malasi") +
  theme(panel.background = element_rect(fill=bg,color=bg),
        plot.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(size=200, hjust=.5),
        plot.caption = element_text(size=90),
        plot.margin = margin(t=40,b=20),
        legend.position = "none",
        text = element_text(color=txt_col, family=text)) 


ggsave("strava.png", map, width = 15, height = 15, units = "in", dpi = 600)


