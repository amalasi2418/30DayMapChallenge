setwd("~/R/30DayMapChallenge/2022/4_colour_friday_green")

library(tidyverse)
library(sf)
library(showtext)
library(ggtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()
flags <- readxl::read_xlsx("flag_colors.xlsx", sheet = "Green") %>% janitor::clean_names()

flags <- flags %>% 
  mutate(country =  str_replace(country,"Sao Tome and Principe","São Tomé and Principe"),
         country =  str_replace(country,"The Republic of the Congo","Republic of the Congo"),
         country =  str_replace(country,"The Seychelles","Seychelles"),
         country =  str_replace(country,"Cote d'Ivoire","Ivory Coast"),
         country =  str_replace(country,"The United Arab Emirates","United Arab Emirates"),
         country =  str_replace(country,"The Central African Republic","Central African Republic"),
         country =  str_replace(country,"The Comoros","Comoros"),
         country =  str_replace(country,"The Gambia","Gambia"),
         country =  str_replace(country,"The Solomon Islands","Solomon Islands"),
         country =  str_replace(country,"Tanzania","United Republic of Tanzania"))

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

flags_green <- merge(countries,flags,by.x="SOVEREIGNT",by.y="country") %>%
  filter(!is.na(hex_code))

bg = "#313131"

title = "<span style = 'color:#4E5B31;'>S</span><span style = 'color:#009739;'>h</span><span style = 'color:#20603D;'>a</span><span style = 'color:#006A4A;'>d</span><span style = 'color:#006A4E;'>e</span><span style = 'color:#43B02A;'>s</span> <span style = 'color:#477050;'>o</span><span style = 'color:#008C45;'>f</span> <span style = 'color:#009A44;'>G</span><span style = 'color:#165d31;'>r</span><span style = 'color:#009A44;'>e</span><span style = 'color:#007A33;'>e</span><span style = 'color:#046A38;'>n</span>"

plot <- countries %>%
  ggplot() + 
  geom_sf(color=bg,fill = NA,size=.1) + 
  geom_sf(data = flags_green ,
          aes(fill=as.factor(hex_code)),
          color=bg,
          size=.1,
          show.legend = FALSE)+
  scale_fill_manual(values = flags_green$hex_code)+ 
  labs(title = title, 
       #title = "Shades of Green",
       subtitle = "Countries depicted by the shade of green in their flags.",
       caption = "Hex codes: www.flagcolorcodes.com | Graphic: Abhinav Malasi")+
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill="white",color="white"),
        plot.title = element_markdown(size=90,face="bold",hjust=.5, margin=margin(t=25,b=10)),
        plot.subtitle = element_text(size=40,hjust=.5, margin=margin(t=5,b=10)),
        plot.caption = element_text(size=30,margin=margin(b=10)),
        text = element_text(family = text))


ggsave("green.png", plot, width=14, height = 8, dpi = 300)
