setwd("~/R/30DayMapChallenge/2022/12_scale")

library(tidyverse)
library(sf)
library(rnaturalearth)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

belgium <- ne_countries(scale = "large",type="countries",returnclass = "sf") %>% 
  filter(sovereignt == "Belgium") %>% 
  st_set_crs("WGS84")%>% 
  select(sovereignt) %>% 
  st_coordinates()%>% 
  as.data.frame()

luxembourg <- ne_countries(scale = "large",type="countries",returnclass = "sf") %>% 
  filter(sovereignt == "Luxembourg") %>% 
  st_set_crs("WGS84") %>% 
  select(sovereignt) %>% 
  st_coordinates()%>% 
  as.data.frame()

india <- ne_countries(scale = "large",type="countries",returnclass = "sf") %>% 
  filter(sovereignt == "India") %>% 
  st_set_crs("WGS84") %>% 
  select(sovereignt) %>% 
  st_coordinates()%>% 
  as.data.frame()


usa <- ne_countries(scale = "large",type="countries",returnclass = "sf") %>% 
  filter(sovereignt == "United States of America") %>% 
  st_set_crs("WGS84") %>% 
  select(sovereignt) %>% 
  st_coordinates() %>% 
  as.data.frame()

bg = "#2C3333"
txt_col = "#FFF5E4"
plot <- usa %>% 
  filter(-125<X, X<0,Y>20) %>% 
  ggplot()+geom_polygon(aes(X,Y,group=L2),fill="#FFF5E4",color="#FFF5E4") +
  geom_polygon(data=luxembourg,aes(X-120,Y-15),fill="#850E35",color="#850E35") +
  geom_polygon(data=belgium, aes(X-110,Y-15),fill="#EE6983",color="#EE6983") +
  geom_polygon(data=india, aes(X-170,Y+15,group=L2),fill="#FFC4C4",color="#FFC4C4") +
  xlab("") + ylab("") +
  labs(title = "Scaling the countries I have lived in",
       subtitle = "USA is about 3 times the size of India, India being 107 times that\nof Belgium, and Belgium is 12 times the size of Luxembourg.\nAlaska and other USA territories not included for simplicity.",
       caption = "Graphic: Abhinav Malasi")+
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        axis.text = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t=20,b=20),
        plot.title = element_text(size = 90, face = "bold",hjust = .5, margin = margin(t=20,b=20)),
        plot.subtitle = element_text(size=50,lineheight = .3, hjust = .5,margin = margin(b=20)),
        plot.caption = element_text(size=40,hjust=.95),
        text = element_text(color=txt_col,family=text))

ggsave("scale.png", plot, width=10, height = 10, dpi = 400)


