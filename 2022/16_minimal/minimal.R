setwd("~/R/30DayMapChallenge/2022/16_minimal")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(maps)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

belgium <- ne_countries(scale = "large",type="countries",returnclass = "sf") %>% 
  filter(sovereignt == "Belgium") %>% st_set_crs("WGS84")


brussels <- world.cities %>% filter(country.etc == "Belgium", capital == 1) %>%
  st_as_sf(coords = c("long","lat")) %>% st_set_crs("WGS84")

txt_col = "#141010"
bg = "#EEEEEE"

plot <- belgium %>% ggplot()+ 
  geom_sf(fill=bg, color=txt_col, size=.5)+
  geom_sf(data=brussels,color=txt_col, size=2) +
  geom_sf_label(data=brussels,label ="Brussels", nudge_x = .25,label.size = NA, color=txt_col,fill=NA,family=text,size=12) +
  labs(title = "Belgium", 
       #title = "Shades of Green",
       #subtitle = "Capital: Brussels",
       #caption = "Source: Our World in Data | Graphic: Abhinav Malasi",
       )+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.text = element_text(size=35),
        plot.title = element_text(size=90,face="bold",hjust=.5, margin=margin(t=25,b=20)),
        #plot.subtitle = element_text(size=40,hjust=.5, margin=margin(t=5,b=10)),
        #plot.caption = element_text(size=30,margin=margin(b=10),hjust=.99),
        text = element_text(family = text,color=txt_col))


ggsave("minimal.png", plot, width=7, height = 7, dpi = 300)
  
