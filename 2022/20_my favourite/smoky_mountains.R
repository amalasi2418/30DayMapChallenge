setwd("~/R/30DayMapChallenge/2022/20_my favourite")

library(tidyverse)
library(sf)
library(ggtext)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

trails <- st_read("GRSM_TRAILS/GRSM_TRAILS.shp")
campsite <- st_read("GRSM_HWA_SYSTEMIC_CAMPSITES/GRSM_HWA_SYSTEMIC_CAMPSITES.shp")
GSNP <- st_read("GRSM_BOUNDARY/GRSM_BOUNDARY_POLYGON.shp")

txt_col = "#141414"
bg = "#eeeeee"

plot <- GSNP[1:19,] %>% 
  ggplot()+
  geom_sf(color="#1b270b", fill=NA)+
  geom_sf(data = trails, color="#982614") +
  geom_sf(data = campsite, color="#141414", shape=17) +
  scale_shape_manual(guide="legend")+
  labs(title="My favourite activity in",
       subtitle = "Great Smoky Mountain National Park:<br><span style = 'font-size:60pt;'>camping (campsites as triangles) and hiking (<span style = 'color:#982614;'>trails</span>)</span>",
       #subtitle = "<span style = 'color:#141414;'>Campsites</span> and <span style = 'color:#982614;'>trails</span>",
       caption = "Source: National Park Service | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        plot.subtitle = element_markdown(size=100,hjust=.5, margin=margin(b=40),lineheight = .25),
        plot.title = element_text(size=70,hjust=.5, margin=margin(t=20,b=10)),
        plot.caption = element_text(size=35,margin=margin(t=80),hjust=.95),
        text = element_text(family = text,color=txt_col))

ggsave("smoky_mountains.png", plot, width=8, height = 8, dpi = 500)
