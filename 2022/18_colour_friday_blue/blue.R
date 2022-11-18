setwd("~/R/30DayMapChallenge/2022/18_colour_friday_blue")

library(tidyverse)
library(sf)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

df <- st_read("hotosm_bel_waterways_lines_shp/hotosm_bel_waterways_lines.shp") %>%
  filter(!waterway %in% c("fairway","dock"))

bg = "#D3D3D3"
txt_col = "#141414"

plot <- df %>% 
  ggplot()+
  geom_sf(color = "#4A89F3") + 
  labs(title = "Belgian waterways",
       caption = "Source: OpenStreetMap | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=90,face="bold",hjust=.5, margin=margin(t=25,b=20)),
        plot.caption = element_text(size=30,margin=margin(b=10),hjust=.95),
        text = element_text(family = text,color=txt_col))

ggsave("blue.png", plot, width=8, height = 8, dpi = 300)
