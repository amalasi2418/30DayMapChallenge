setwd("~/R/30DayMapChallenge/2022/2_lines")

library(tidyverse)
library(sf)
library(showtext)

text = "Old Standard TT"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

zone <- st_read("ne_10m_time_zones/ne_10m_time_zones.shp")
countries <- st_read("ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


zone %>% ggplot()+geom_sf(aes(color=name,fill=name),alpha=.5,show.legend = FALSE)

bg = "black"
txt_col="#F6F6F6"

colors <- colorRampPalette(c("#FBB9C5", "#FDD0B1","#F9EFC7","#C3EDBF","#B8DFE6","#C5BBDE"))(40)
#colors <- rainbow(40)


plot <- zone %>% 
  ggplot()+
  geom_sf(aes(color=name, fill=name),alpha=.85,size=.2,show.legend = FALSE)+
  geom_sf(data=countries,size=.1,fill=NA)+
  labs(title = "TIME ZONES",
       caption = "Source: Natural Earth | Graphic: Abhinav Malasi") +
  scale_fill_manual(values = colors) +
  scale_color_manual(values = colors) +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.title = element_text(hjust=.5, size=80,face = "bold",margin=margin(t=15,b=5)),
        plot.caption = element_text(size=30),
        text = element_text(color=txt_col, family=text))


ggsave("timezone.png",plot,width = 8,height = 4.5,dpi = 400)
