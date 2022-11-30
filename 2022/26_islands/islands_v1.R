setwd("~/R/30DayMapChallenge/2022/26_islands")

#library(raster)
library(tidyverse)
library(sf)
library(ggtext)
library(showtext)

text = "Maven Pro"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()


road <- st_read("lka_rdsl_250k_sdlka/lka_rdsl_250k_sdlka.shp")

rail <- st_read("lka_rlwl_250k_sdlka/lka_rlwl_250k_sdlka.shp")

srilanka <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") %>%
  filter(ADMIN == "Sri Lanka") %>% 
  st_transform(st_crs(rail))

bg = "#CFFDE1"

plot <- srilanka %>% 
  ggplot()+
  geom_sf(fill="#FFBE29")+
  geom_sf(data=road,color="#00534E") +
  geom_sf(data=rail, color="#8D153A") +
  labs(title = "Sri Lanka: <span style = 'color:#8D153A;'>rail</span> and <span style = 'color:#00534E;'>road</span> network",
       caption = "Source: data.humdata.org | Graphic: Abhinav Malasi") +
  theme_void() +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.title = element_markdown(size=150, face = "bold",hjust=.5),
        plot.caption = element_text(size=50,hjust=.5),
        plot.margin = margin(t=20,b=20),
        text = element_text(family = text, color="#000000"))

ggsave("island.png", plot, width=8, height = 8, dpi = 500)
