setwd("~/R/30DayMapChallenge/2022/6_network")

library(tidyverse)
library(sf)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

road <- read_sf("ne_10m_roads/ne_10m_roads.shp")

bg = "#D6D5A8"
txt_col = "#1B2430"

plot <- road %>% 
  ggplot()+
  geom_sf(color = txt_col,size=.1) +
  labs(title = "Road Network",
       caption = "Source: Natural Earth | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(fill=bg, color = bg),
        panel.background = element_rect(fill=bg, color = bg),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t=20,b=10),
        plot.title = element_text(size=70,hjust=.5,margin = margin(b=20)),
        plot.caption = element_text(size=20),
        text = element_text(family = text, color = txt_col))

ggsave("network.png",plot,width = 8, height = 4, dpi=300)
