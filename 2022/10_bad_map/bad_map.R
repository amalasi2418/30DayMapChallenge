setwd("~/R/30DayMapChallenge/2022/10_bad_map")

library(tidyverse)
library(sf)
library(showtext)
library(RColorBrewer)
text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

depth_0 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_L_0.shp")
depth_200 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_K_200.shp")
depth_1000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_J_1000.shp")
depth_2000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_I_2000.shp")
depth_3000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_H_3000.shp")
depth_4000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_G_4000.shp")
depth_5000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_F_5000.shp")
depth_6000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_E_6000.shp")
depth_7000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_D_7000.shp")
depth_8000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_C_8000.shp")
depth_9000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_B_9000.shp")
depth_10000 <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_bathymetry_all/ne_10m_bathymetry_A_10000.shp")

depth <- rbind(depth_0,
               depth_200,
               depth_1000,
               depth_2000,
               depth_3000,
               depth_4000,
               depth_5000,
               depth_6000,
               depth_7000,
               depth_8000,
               depth_9000, 
               depth_10000)

countries <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/3_polygons/ne_10m_admin_0_countries/ne_10m_admin_0_countries.shp")


bg = "blue"
plot <- depth %>% 
  ggplot()+
  geom_sf(aes(fill=(depth),
              color=(depth)
              ))+
  geom_sf(data = countries,fill=bg,color=bg)+
  theme_void()+
  scale_fill_gradient2(low = "orange",high="green")+
  scale_color_gradient2(low = "orange",high="green")+
  labs(title = "The Ocean Floor",
       caption = "Source: Natural Earth | Graphic: Abhinav Malasi") +
  theme(panel.grid = element_blank(),
        legend.position = c(.7,.075),
        plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        plot.title = element_text(size=90,color="white",hjust=.5, margin=margin(t=15,b=10)),
        plot.caption = element_text(size=30,color="white"),
        plot.margin = margin(t=15,b=15),
        legend.title = element_text(size=40,color="white",vjust =.75),
        legend.text = element_text(size=35,color="white",vjust =5.5),
        text = element_text(family = text))+
 guides(color = "none",
         fill = guide_colorbar(title = "Depth (in meter)",
                             title.position = "left",
                             label.position = "bottom",
                             barwidth = 15,
                             barheight = 1.25,
                             direction = "horizontal")) 
  
ggsave("bad_map.png", plot, width=14, height = 8, dpi = 400)

