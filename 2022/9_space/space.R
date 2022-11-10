setwd("~/R/30DayMapChallenge/2022/New folder")

library(rvest)
library(tidyverse)
library(sf)
library(showtext)
text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

url = "https://en.wikipedia.org/wiki/List_of_impact_craters_on_Earth"

webpage =  read_html(url)

tbls <- html_nodes(webpage, ".wikitable")

# Shows all tables
tbls
df <- html_table(tbls, fill = TRUE)[[1]] %>% select(-c(5,6))
df1 <- html_table(tbls, fill = TRUE)[[2]] %>% select(-5)
df2 <- html_table(tbls, fill = TRUE)[[3]] %>% select(-5)
meteor1 <- rbind(df,df1,df2)

df3 <- html_table(tbls, fill = TRUE)[[4]] %>% select(-5)
#colnames(df3)[4] <- c("Diameter (km)")
df4 <- html_table(tbls, fill = TRUE)[[5]] %>% select(-5)
colnames(df4)[4] <- c("Diameter(km)")
meteor2 <- rbind(df3,df4) 

meteor <- rbind(meteor1, meteor2) %>% janitor::clean_names()

meteor$diameter_km <- gsub("\\[.*","",meteor$diameter_km) %>% as.numeric()

meteor$coordinates <- sapply(strsplit(meteor$coordinates, "/\\s*"), tail, 1)

meteor$coordinates <- gsub("\\s*\\([^\\)]+\\)","",meteor$coordinates)

temp <- str_split(meteor$coordinates,";") %>% 
  unlist() %>% 
  matrix(nrow = 2) %>% 
  t() %>% data.frame() 

temp$X2 <- parse_number(temp$X2)

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

meteor <- cbind(meteor,temp) %>% st_as_sf(coords = c("X2","X1")) %>% st_set_crs(st_crs(countries))

bg = "#141414"
txt_col <- "#F8FFDB"

plot <- countries %>% 
  ggplot()+
  geom_sf(size=.1,color=bg, fill=txt_col)+
  geom_sf(data = meteor,aes(size=diameter_km),color="#FF5F00")+
  labs(title = "Impact craters on Earth",
       subtitle = "The data is for the craters created by the impact of meteors or comets with the earth. 190 such incidents have been identified.\nVredefort crater in South Africa is the oldest and also the biggest crater with a diameter of 160 km. Wabar crater in\nSaudi Arabia is the youngest with a diameter of 0.1 km.",
       caption = "Source: Wikipedia.org | Graphic: Abhinav Malasi",
       size="Diameter (km):")+
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        legend.position = "top",
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.key.size = unit(.75, "cm"),
        legend.spacing.x = unit(.1, 'cm'),
        plot.margin = margin(t=20,b=20),
        plot.title = element_text(size = 90, face = "bold",hjust = .5, margin = margin(b=20)),
        plot.subtitle = element_text(size=50,lineheight = .3, hjust = .5,margin = margin(b=20)),
        plot.caption = element_text(size=30),
        legend.title = element_text(size=40),
        legend.text = element_text(size=35, color=txt_col),
        text = element_text(family = text, color=txt_col))

ggsave("crater.png", plot, width=12, height = 8, dpi = 400)

