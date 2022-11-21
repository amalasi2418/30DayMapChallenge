setwd("~/R/30DayMapChallenge/2022/21_kontur")

library(tidyverse)
library(sf)
library(showtext)
showtext_auto()
text="Open Sans"

sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

df <- st_read("kontur_population_LU_20220630.gpkg/kontur_population_LU_20220630.gpkg")

txt="#EEEEEE"
bg = "#858585"
bg="#252525"

plot <- df %>% 
  ggplot()+
  geom_sf(aes(fill=population),color=bg) +
  labs(title="Grand Duchy of Luxembourg",
       caption = "Source: Kontur Population Dataset | Graphic: Abhinav Malasi",
       fill="Population density\n(400m H3 Hexagons)") +
  viridis::scale_fill_viridis(option = "D", 
                              trans = "pseudo_log",  
                              breaks = c(1,65, 650, 6500),
                              labels = c(1,65, 650, 6500), na.value="black")+
  guides(fill = guide_colorbar(label.position = "left", 
                               title.hjust = 0.5,label.hjust = 1.5)) +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=100, hjust=0.5,face = "bold",margin=margin(t=20,b=10)),
        #plot.subtitle = element_text(size=40,face = "bold",lineheight = .35),
        plot.caption = element_text(size=30,margin = margin(b=10,r=20)),
        plot.margin = margin(t=20,b=10),
        legend.title = element_text(size=40,lineheight=.25),
        legend.position = c(.95,.8),
        legend.key.width = unit(0.5, "line"),
        legend.text = element_text(size=35),
        legend.background = element_rect(fill=NA,color=NA),
        legend.key = element_rect(fill=bg,color=bg),
        text=element_text(family=text,color=txt))


ggsave("luxembourg.png",plot,width = 8,height = 8,units = "in",dpi=320)
