setwd("~/R/30DayMapChallenge/2022/11_colour_friday_red")

library(rvest)
library(tidyverse)
library(sf)
library(showtext)
library(RColorBrewer)
text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

url = "https://en.wikipedia.org/wiki/List_of_countries_by_apple_production"

webpage =  read_html(url)

tbls <- html_nodes(webpage, ".wikitable")

# Shows all tables
tbls
df <- html_table(tbls, fill = TRUE)[[1]] %>% select(2,3) %>% janitor::clean_names()
df1 <- html_table(tbls, fill = TRUE)[[2]] %>% select(2,3) %>% janitor::clean_names()
df2 <- html_table(tbls, fill = TRUE)[[3]] %>% select(2,3) %>% janitor::clean_names()
df3 <- html_table(tbls, fill = TRUE)[[4]] %>% select(2,3) %>% janitor::clean_names()
df4 <- html_table(tbls, fill = TRUE)[[5]] %>% select(2,3) %>% janitor::clean_names()
df5 <- html_table(tbls, fill = TRUE)[[6]] %>% select(2,3) %>% janitor::clean_names()

apples <- rbind(df,df1,df2,df3,df4,df5) %>% 
  mutate(country_region = str_replace(country_region,"United States","United States of America"),
         country_region = str_replace(country_region,"Czech Republic","Czechia"),
         country_region = str_replace(country_region,"Serbia","Republic of Serbia"),
         country_region = str_replace(country_region,"Macedonia","North Macedonia")) %>%
  mutate(x2017 = str_replace_all(x2017,",",""),
         x2017 = as.numeric(x2017))

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

apple_prod <- merge(countries,apples, by.x = "SOVEREIGNT",by.y="country_region", all.x=TRUE)

bg = "#141414"
txt_col <- "#F8FFDB"

plot <- apple_prod %>% 
  ggplot()+
  geom_sf(aes(fill=x2017/1000),color=bg,size=.1)+
  #scale_fill_brewer(palette = "Reds")+
  scale_fill_gradient2(low = "#fbd9d3",high="#ee2400",trans = "pseudo_log",
                       breaks = c(0.4, 400, 40000),
                       labels = c(0.4, 400, 40000))+
  labs(title = "Global apple production",
       caption = "Source: Wikipedia.org | Graphic: Abhinav Malasi",
       fill="Production for 2017\n(in kilo tonnes)")+
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.key.size = unit(.5, "cm"),
        legend.spacing.x = unit(.3, 'cm'),
        plot.margin = margin(t=20,b=20),
        plot.title = element_text(size = 90, face = "bold",hjust = .5, margin = margin(t=5,b=20)),
        plot.caption = element_text(size=30,hjust = .95),
        legend.title = element_text(size=40,lineheight = .25),
        legend.text = element_text(size=35, color=txt_col,margin = margin(t = -10)),
        text = element_text(family = text, color=txt_col))

ggsave("apples.png", plot, width=8, height = 6, dpi = 400)

