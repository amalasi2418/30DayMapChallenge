setwd("~/R/30DayMapChallenge/2022/30_remix")

# eurojam day 19

library(tidyverse)
library(sf)
library(showtext)
library(ggeasy)
library(cowplot)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

countries <- st_read("~/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp") 

countries <- st_transform(countries, "+proj=moll")

countries <- st_transform(countries, "+proj=stere +lat_0=90")
countries <- st_transform(countries, "NAD83")

# "+proj=robin"
# "+proj=moll"
# "+proj=stere +lat_0=-90"
# "+proj=goode"
# "+proj=merc"
# "+proj=laea"
# "+proj=aeqd"
# "+proj=ortho"
#"+proj=vandg4"

projections <- c("+proj=robin",
                  "+proj=moll",
                  "+proj=stere +lat_0=-90",
                  "+proj=goode",
                  "+proj=merc",
                  "+proj=laea",
                  #"+proj=aeqd",
                 "+proj=tissot +lat_1=60 +lat_2=65",
                  "+proj=ortho",
                 "+proj=vandg4")

projections_name <- c("Robinson",
                      "Mollweide",
                      "Stereographic",
                      "Goode Homolosine",
                      "Mercator",
                      "Lambert Azimuthal Equal Area",
                      #"Azimuthal Equidistant",
                      "Tissot",
                      "Orthographic",
                      "van der Grinten IV")

df = data.frame()
plt = vector()

bg="#141414"
fill =  "#eeeeee"
lines = "#CDC7BE"

for(i in 1:length(projections)-1){
  #t = cbind(countries,projections=projections[i])
  #df = rbind(df,t)
 
  plot[i]=countries %>% 
    ggplot() +
    geom_sf(fill=fill,color=bg,size=.1) +
    labs(title = projections_name[i])+
    coord_sf(crs = projections[i],expand = FALSE) +
    theme(plot.background = element_rect(fill=bg,color=bg),
          panel.background = element_rect(fill=bg,color=bg),
          panel.grid = element_line(colour = "#CDC7BE"),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          plot.title = element_text(size=70,hjust=.5),
          text = element_text(family = text, color = fill))
  
  ggsave(paste0("projection_",i,".png"), plot, width=5, height=5)
  
}

i = 9
plot9=countries %>% 
  ggplot() +
  geom_sf(fill=fill,color=bg,size=.1) +
  labs(title = projections_name[i],
       caption = "Source: https://proj.org | Graphic: Abhinav Malasi")+
  coord_sf(crs = projections[i],expand = FALSE) +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_line(colour = "#CDC7BE"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=70,hjust=.5,margin=margin(b=60)),
        plot.caption = element_text(size=30,hjust=.5, margin=margin(t=60)),
        text = element_text(family = text, color = fill))

ggsave(paste0("projection_",i,".png"), plot, width=5, height=5)

