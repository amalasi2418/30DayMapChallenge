setwd("~/R/30DayMapChallenge/2022/2_lines")

library(tidyverse)
library(rnaturalearth)
library(sf)


routes <- read_ods("cliwoc21.ods")
routes <- read_sf("british_shipping_example.csv")

world <- ne_countries(type="countries",scale="large",returnclass = "sf")

routes_coord <- st_as_sf(routes, coords = c("long","lat")) %>% st_set_crs(st_crs(world))

world %>% 
  ggplot()+
  geom_sf() +
  geom_sf(data = routes_coord,aes(group=id,color=nat),size=.05,alpha=.5)+
  labs(title = "European Imperial Expansion",
       subtitle = " The data consists of British, Danish, and Dutch shipping routes between 1750-1850. Each line is a record of a voyage with either civil or military.")

