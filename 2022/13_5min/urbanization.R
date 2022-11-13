setwd("~/R/30DayMapChallenge/2022/13_5min")

library(tidyverse)
library(sf)
library(viridis)

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

urban <- read_csv("share-of-population-urban.csv") %>% 
  janitor::clean_names() %>% 
  filter(year == 2020)

colnames(urban)[4] <- "percent"

urbanization <- merge(countries %>% select(ADM0_A3), urban %>% select(1,2,4), by.x="ADM0_A3", by.y="code", all.x=TRUE)


urbanization %>% ggplot()+ geom_sf(aes(fill=percent)) +
  scale_fill_viridis()+
  labs(title = "Global Urbanization (2020)",
       caption = "Source: Our World in Data | Graphic: Abhinav Malasi",
       fill = "Urbanization:")+
  theme(panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=30),
        plot.caption = element_text(size=10),
        legend.title = element_text(size=10),
        legend.text = element_text(size=10),
        legend.position = "top")

ggsave("urbanization.png", last_plot(), width=10, height = 8, dpi=300)
