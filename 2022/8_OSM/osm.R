setwd("~/R/30DayMapChallenge/2022/8_OSM")

library(osmdata)
library(Rcpp)
library(sf)
library(tidyverse)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()


available_tags("highway")

available_tags("waterway")

available_features()

getbb("Gent, Belgium")

streets <- getbb("Gent, Belgium")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()


small_streets <- getbb("Gent, Belgium")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway")) %>%
  osmdata_sf()


river <- getbb("Gent Belgium")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("river","canal","dock", "weir")) %>%
  osmdata_sf()



small_river <- getbb("Gent Belgium")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = c("soakhole","boatyard","stream")) %>%
  osmdata_sf()


bg = "#141010"
txt_col = "#EEEEEE"

plot <- ggplot() +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = txt_col,#"#ffbe7f",
          size = .4,
          alpha = .7) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = txt_col,#"#ffbe7f",
          size = .2,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "#7fc0ff",#"#ffbe7f",
          size = .8,
          alpha = .5) +
  coord_sf(#xlim = c(3.579762, 3.849325), 
           #ylim = c(50.979542, 51.1088886),
           xlim = c(3.579762, 3.849325), 
           ylim = c(50.979542, 51.188886),
           expand = FALSE) +
  labs(title = "Gent, Belgium", 
       subtitle = "(51.05°N / 3.72°E)",
       caption = "Source: OpenStreetMap | Graphic: Abhinav Malasi")+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(l=30,r=30),
        legend.position = "top",
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.text = element_text(size=35),
        plot.title = element_text(size=90,face="bold",hjust=.5, margin=margin(t=20,b=15)),
        plot.subtitle = element_text(size=40,hjust=.5, margin=margin(b=20)),
        plot.caption = element_text(size=25,margin=margin(t=15,b=15),hjust=.99),
        text = element_text(family = text,color=txt_col)) 


ggsave("gent.png",plot,width=8, height=8, dpi=300)
