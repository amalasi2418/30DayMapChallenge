setwd("~/R/30DayMapChallenge/2022/22_NULL")

library(rvest)
library(tidyverse)
library(sf)
library(showtext)
library(RColorBrewer)
text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

bg = "#EFEFEF"
txt_col <- "#141414"


unpaid <- countries %>% 
  mutate(unpaid = case_when(countries$NAME %in% c("United States of America","Papua New Guinea","Marshall Is.", "Micronesia", "Nauru", "Palau", "Tonga")~1,
                                                  TRUE~0))


plot <- unpaid %>% st_set_crs("van der Grinten") %>%
  ggplot()+
  geom_sf(aes(fill=as.factor(unpaid)),color="#EFEFEF",size=.1)+
  geom_sf_label(aes(label=ifelse(unpaid==1,NAME,NA)),label.size = NA, fill=NA, size=10, family=text) +
  scale_fill_manual(values = c("#F3E0B5","#D2001A"))+
  xlab("") + ylab("")+
  labs(title = "(un)Paid Maternity Leave",
       caption = "Source: Wikipedia.org | Graphic: Abhinav Malasi")+
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none",
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        legend.key.size = unit(.5, "cm"),
        legend.spacing.x = unit(.3, 'cm'),
        plot.margin = margin(t=20,b=20),
        plot.title = element_text(size = 90, face = "bold",hjust = .5, margin = margin(t=5,b=20)),
        plot.caption = element_text(size=25,hjust = .95,margin = margin(t=30)),
        legend.title = element_text(size=40,lineheight = .25),
        legend.text = element_text(size=35, color=txt_col,margin = margin(t = -10)),
        text = element_text(family = text, color=txt_col))

ggsave("maternity.png", plot, width=8, height = 6, dpi = 400)

