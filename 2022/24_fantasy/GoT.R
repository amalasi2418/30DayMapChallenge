setwd("~/R/30DayMapChallenge/2022/24_fantasy")

library(tidyverse)
library(sf)
#library(showtext)
#library(ggtext)
library(extrafont)

font_import(paths = "C:/Users/amalasi/AppData/Local/Microsoft/Windows/Fonts")

#loadfonts(device="win")

text = "Game of Thrones"

#text = "Roboto"

windowsFonts(sans=text)
loadfonts(device="win")
loadfonts(device="postscript")



#sysfonts::font_families_google()

#sysfonts::font_add_google(text,text)
#showtext_auto()


continent <- st_read("Westeros_Essos_shp/GoTRelease/continents.shp")
islands <- st_read("Westeros_Essos_shp/GoTRelease/islands.shp")
lakes <- st_read("Westeros_Essos_shp/GoTRelease/lakes.shp")
#landscape <- st_read("Westeros_Essos_shp/GoTRelease/landscape.shp")
#locations <- st_read("Westeros_Essos_shp/GoTRelease/locations.shp")
#areas <- st_read("Westeros_Essos_shp/GoTRelease/officialMapAreas.shp")

nudge_x = 0
nudge_y = 0

political <- st_read("Westeros_Essos_shp/GoTRelease/political.shp") %>% 
  group_by(name,ClaimedBy) %>% 
  mutate(ClaimedBy = str_replace(ClaimedBy,"Night's Watch","The Wall"),
         name = case_when(ClaimedBy == "Wildlings" ~ "Beyond the Wall",
                          TRUE ~ name),
         name = str_replace(name,"Bran's Gift","The Wall"),
         name = str_replace(name,"New Gift","The Wall")) %>%
  summarise(geometry = st_union(geometry)) %>%
  mutate(#nudge_x = c(0,1,0,0,.3,-1,-.3,0,0,0,-.5),
         nudge_x = ifelse(name=="Beyond the Wall",0,nudge_x),
         nudge_x = ifelse(name=="Crownsland",3,nudge_x),
         nudge_x = ifelse(name=="Dorne",0,nudge_x),
         nudge_x = ifelse(name=="Riverlands",0,nudge_x),
         nudge_x = ifelse(name=="Stormlands",.3,nudge_x),
         nudge_x = ifelse(name=="The Iron Islands",-3.5,nudge_x),
         nudge_x = ifelse(name=="The North",-1,nudge_x),
         nudge_x = ifelse(name=="The Reach",0,nudge_x),
         nudge_x = ifelse(name=="The vale",0,nudge_x),
         nudge_x = ifelse(name=="The Wall",0,nudge_x),
         nudge_x = ifelse(name=="The Westerlands",0,nudge_x),
         nudge_y = ifelse(name=="Beyond the Wall",1,nudge_y),
         nudge_y = ifelse(name=="Crownsland",-2,nudge_y),
         nudge_y = ifelse(name=="Dorne",-.3,nudge_y),
         nudge_y = ifelse(name=="Riverlands",-1.5,nudge_y),
         nudge_y = ifelse(name=="Stormlands",0,nudge_y),
         nudge_y = ifelse(name=="The Iron Islands",2,nudge_y),
         nudge_y = ifelse(name=="The North",3,nudge_y),
         nudge_y = ifelse(name=="The Reach",3,nudge_y),
         nudge_y = ifelse(name=="The vale",0,nudge_y),
         nudge_y = ifelse(name=="The Wall",0,nudge_y),
         #nudge_y = c(1,0,-.3,-.3,0,0,1,.5,0,0,0))
         nudge_y = ifelse(name=="The Westerlands",0,nudge_y))
#regions <- st_read("Westeros_Essos_shp/GoTRelease/regions.shp")
rivers <- st_read("Westeros_Essos_shp/GoTRelease/rivers.shp")
roads <- st_read("Westeros_Essos_shp/GoTRelease/roads.shp")
#walls <- st_read("Westeros_Essos_shp/GoTRelease/wall.shp")

bg = "#8FBDD3"

continent %>% filter(name == "Westeros") %>%
  ggplot()+
  geom_sf()+
  geom_sf(data=islands)+
  #geom_sf(data=lakes, fill="blue") +
  geom_sf(data=political, aes(fill=ClaimedBy)) +
  geom_sf(data=rivers, color="blue")+
  geom_sf(data=roads, color="black") +
  geom_sf_label(data=political %>% filter(!ClaimedBy=="The Wall"), aes(label=name),nudge_x = political$nudge_x, nudge_y = political$nudge_y,size=6,label.size = NA, fill=NA, family=text)+
  geom_sf_label(data=political %>% filter(ClaimedBy=="The Wall"), aes(label=name),color="white",size=6,label.size = NA, fill=NA, family=text)+
  labs(title = "GAME OF THRONES\nThe seven kingdoms",
       caption = "Source: www.cartographersguild.com | Inspiration: paulvanderlaken.com\nFont: fontmeme.com | Graphic: Abhinav Malasi") +
  scale_fill_manual(values = c("#D79771","#66806A","#506D84","#B4C6A6","#146356","#EFEAD8","#6B4F4F","#212121","#FFF1AF","#A3DA8D","#FFFBE9"))+
  #geom_sf(data=landscape) +
  #geom_sf(data=locations %>% filter(confirmed==1, size %in% c(3,5))) +
  coord_sf(xlim = c(0,28), ylim = c(-18,50), expand = FALSE) +
  #coord_fixed(ratio=10)+
  theme_void(base_family=text) +
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        legend.position = "none",
        plot.margin = margin(t=20,b=20,l=-200,r=-200),
        plot.title = element_text(hjust=.5,size=30, lineheight = 1.3, margin=margin(t=20)),
        plot.caption = element_text(family = "Arial",size=20, hjust=.5))

ggsave("GOT.png", last_plot(),height=20, width = 15, dpi=500,units = "in")

