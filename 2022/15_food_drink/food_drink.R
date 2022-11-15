setwd("~/R/30DayMapChallenge/2022/15_food_drink")

library(tidyverse)
library(sf)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

meat <- read_csv("per-capita-meat-type.csv") %>% 
  janitor::clean_names() %>% 
  filter(year==2019) %>% na.omit()

colnames(meat)[4:9]<- c("poultry","beef","sheep_goat","pork","others","seafood")

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

meat$max <- apply(meat[,4:9], 1, max)

meat <- meat %>% mutate(pop_meat = case_when(max == poultry ~ "Poultry",
                                     max == beef ~ "Beef",
                                     max == sheep_goat ~ "Sheep & Goat",
                                     max == pork ~ "Pork",
                                     max == others ~ "Others",
                                     TRUE~"Fish & Seafood"))

world_meat <- merge(countries %>% select(ADM0_A3), meat %>% select(1,2,11), by.x="ADM0_A3", by.y="code", all.x=TRUE)

bg = "#141010"
txt_col = "#EEEEEE"

plot <- world_meat %>% 
  ggplot()+
  geom_sf(aes(fill=pop_meat),color=NA,size=.2)+
  geom_sf(data=countries,fill=NA,color=bg,size=.2)+
  scale_fill_manual(values = c("#D72323","#E16428","#4ECCA3","#F1BBD5","#FFE98A","#BE3144"), na.value = "gray50") +
  labs(title = "Popular meat around the world", 
       #title = "Shades of Green",
       #subtitle = "Countries depicted by the shade of green in their flags.",
       caption = "Source: Our World in Data | Graphic: Abhinav Malasi",
       fill="")+
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.text = element_text(size=35),
        plot.title = element_text(size=90,face="bold",hjust=.5, margin=margin(t=25,b=20)),
        plot.subtitle = element_text(size=40,hjust=.5, margin=margin(t=5,b=10)),
        plot.caption = element_text(size=30,margin=margin(b=10),hjust=.99),
        text = element_text(family = text,color=txt_col))


ggsave("food_drink.png", plot, width=10, height = 7, dpi = 300)



