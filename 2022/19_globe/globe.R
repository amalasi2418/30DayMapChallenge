setwd("~/R/30DayMapChallenge/2022/19_globe")

library(tidyverse)
library(sf)
library(showtext)

text = "Maven Pro"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

crs <- "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=20"

ports <- st_read("wld_trs_ports_wfp/wld_trs_ports_wfp.shp") %>% 
  filter(prttype %in% c("Sea","sea","Sea Port")) %>%
  st_transform(crs = crs)

sphere_graticules <- st_graticule(ndiscr = 10000, margin = 10e-6) %>% 
  st_transform(crs = crs) %>%
  st_convex_hull() %>%
  summarise(geometry = st_union(geometry))


bg = "#eeeeee"
txt_col = "#141414"

plot <- ggplot() +
  geom_sf(data = sphere_graticules, fill = txt_col, size = 0.09) + 
  geom_sf(data = ports,size = 0.002, color = bg,alpha = 0.75)+
  coord_sf(crs = crs) +
  labs(title = "SEA PORTS",
    subtitle = "A dot = a sea port",
    caption = "Source: OpenStreetMap | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(color = bg, fill = bg),
        panel.background = element_rect(color = bg, fill = bg),
        plot.title = element_text(hjust = 0.5, size = 150),
        plot.subtitle = element_text(hjust = 0.5, size=60,margin = margin(t=10)),
        plot.caption = element_text(size= 40),
        plot.margin = margin(t=20,b=10),
        text = element_text(color = txt_col, family = text))

ggsave("sea_ports.png", plot, width=8, height = 8, dpi = 500)


