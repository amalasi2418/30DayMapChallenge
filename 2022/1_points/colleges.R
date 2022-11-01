setwd("~/R/30DayMapChallenge/2022/1_points")

library(tidyverse)
library(sf)
library(showtext)
text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

colleges <- read_sf("us-colleges-and-universities.geojson")

bg = "#FFE227"
txt_col = "#121013"

plot <- colleges %>% 
  ggplot()+
  geom_sf(color="#121013",size=.5) + 
  coord_sf(xlim = c(-160,-60),ylim=c(10,70))+
  labs(title="Colleges and Universities of the USA",
       subtitle = "Distribution of colleges and universities in the USA for year 2018-19 shows\nmajority of the post secondary institutions are in Midwest, Southeast,\nand Northeast regions or along the western coastline.",
       caption = "Source: National Center for Education Statistics, US Department of Education | Graphic: Abhinav Malasi") +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size=90,hjust = .5, margin=margin(t=20,b=20)),
        plot.subtitle = element_text(size=45,hjust = .5,lineheight = .3),
        plot.caption = element_text(size=35,margin=margin(b=10)),
        text = element_text(family = text,color = txt_col))


ggsave("colleges.png",plot,width = 8,height = 8,dpi=400)  

