setwd("~/R/30DayMapChallenge/2022/14_hexagons")

library(tidyverse)
library(sf)
library(raster)
library(showtext)

showtext_auto()
text="Open Sans"

sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

txt="#EEEEEE"
bg = "#858585"
bg="#252525"

pop_den_gemente <- readxl::read_xlsx("Pop_density_en.xlsx",sheet = "2020",skip = 1) %>% 
  janitor::clean_names()

be_shape <- st_read("BELGIUM_-_Municipalities/BELGIUM_-_Municipalities.shp")

be_centroid <- st_centroid(be_shape$geometry)  


df = cbind(be_shape,data.frame(st_coordinates(be_centroid)))

  
pop_den_gemente <- pop_den_gemente %>% 
  mutate(refnis = str_replace(refnis,"12041","12030"))

df1 <-merge(df, pop_den_gemente, by.x="CODE_INS",by.y="refnis",all.x = TRUE)

commune <-df1 %>% ggplot(aes(X, Y))+  
  stat_summary_hex(bins=20,aes(z = population_km2),fun = "sum", colour=bg) + 
  labs(title = "Belgium",
       subtitle = "Total population: 11.5 million\nPopulation density: 375 people per square km",
       caption = "Data: Statbel (2020 census) | Graphic: Abhinav Malasi",
       fill="Population density\n(people per square km)")+
  viridis::scale_fill_viridis(option = "D", 
                              trans = "pseudo_log",  
                              breaks = c(0,25, 250, 2500, 20000),
                              labels = c(0,25, 250, 2500, 20000))+
  guides(fill = guide_colorbar(label.position = "left", 
                               title.hjust = 0.5,label.hjust = 1.5)) +
  theme_void() +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.title = element_text(size=170, face = "bold",margin=margin(t=20,b=10),hjust=.1),
        plot.subtitle = element_text(size=40,face = "bold",lineheight = .35,hjust=.1),
        plot.caption = element_text(size=30,margin = margin(t=10,b=10,r=20)),
        plot.margin = margin(t=20,b=10,20,r=20),
        legend.title = element_text(size=40,lineheight=.25),
        legend.position = c(.2,.2),
        legend.key.width = unit(0.5, "line"),
        legend.text = element_text(size=35),
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        text=element_text(family=text,color=txt))

ggsave("hexagon.png",commune,width = 10,height = 10,units = "in",dpi=320)
