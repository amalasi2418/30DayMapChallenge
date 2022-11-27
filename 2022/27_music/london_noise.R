setwd("~/R/30DayMapChallenge/2022/27_music")

library(tidyverse)
library(sf)
#library(raster)
library(showtext)
showtext_auto()
text = "Montserrat"

sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

txt="#EEEEEE"
#bg = "#858585"
bg="#252525"

london <- st_read("Road_LAeq_16h_London/Road_LAeq_16h_London.shp")

road <- st_read("Road_Lden_London/Road_Lden_London.shp")
rail <- st_read("Rail_Lden_London/Rail_Lden_London.shp")

road$NoiseClass <- factor(road$NoiseClass, levels = c(">=75.0","70.0-74.9","65.0-69.9","60.0-64.9","55.0-59.9"))
rail$NoiseClass <- factor(rail$NoiseClass, levels = c(">=75.0","70.0-74.9","65.0-69.9","60.0-64.9","55.0-59.9"))


noise <- road %>% ggplot()+geom_sf(aes(fill=NoiseClass),size=.1) +
  geom_sf(data=rail,aes(fill=NoiseClass),size=.1) +
  labs(title = "Noise pollution in London",
       subtitle = "from rail and road sources. The data is the 24 hour annual average for 2012.",
       caption = "Source: https://data.london.gov.uk | Graphic: Abhinav Malasi",
       fill = "Noise (in dB)")+
  scale_fill_manual(values = c("#FF0000", "#FF5500" ,"#FFAA00", "#FFFF00" ,"#FFFF80")) +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=120, face = "bold",margin=margin(t=20,b=10),hjust=.5),
        plot.subtitle = element_text(size=60,lineheight = .35,hjust=.5),
        plot.caption = element_text(size=40,margin = margin(b=30,r=20),hjust=.5),
        plot.margin = margin(t=20,b=10),
        legend.title = element_text(size=50,lineheight=.25),
        legend.position = c(.1,.1),
        legend.key.width = unit(0.5, "line"),
        legend.text = element_text(size=50),
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        text=element_text(family=text,color=txt))

ggsave("music.png",noise,width=15, height=15)

