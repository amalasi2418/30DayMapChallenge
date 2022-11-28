setwd("~/R/30DayMapChallenge/2022/28_3D")

library(tidyverse)
library(sf)
library(raster)
library(showtext)
library(rayshader)

showtext_auto()
text="Open Sans"

sysfonts::font_families_google()
sysfonts::font_add_google(text,text)

txt="#EEEEEE"
bg = "#858585"
bg="#252525"

be_shp <- st_read("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/7_raster/TF_POPULATION_GRID_3035_20200101.shp")
pop_den <- read_csv("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/7_raster/TF_POPULATION_GRID_20200101.csv")
pop_den_gemente <- readxl::read_xlsx("C:/Users/amalasi/Documents/R/30DayMapChallenge/2022/7_raster/Pop_density_en.xlsx",sheet = "2020",skip = 1) %>% 
  janitor::clean_names()

#be_communes <- st_read("C:/Users/amalasi/Documents/R/Infographics/Maps/Belgium/communes/communes_L08.shp")

be_communes <- st_read("C:/Users/amalasi/Documents/R/Infographics/Maps/Belgium/sh_statbel_statistical_sectors_20200101.shp/sh_statbel_statistical_sectors_20200101.shp")

# convert coordinate to Lambert system
be_shp1 <- st_transform(be_shp,crs = 31370)

be_pop_den <- merge(be_shp1,pop_den,by.x="grd_newid",by.y="GRD_NEWID")

#########################
# communes

be_commune_den <- merge(be_communes,pop_den_gemente,by.x="CNIS5_2020",by.y="refnis")

commune <- be_commune_den %>% 
  ggplot()+
  geom_sf(aes(fill=population_km2),size=.01,color=NA)+
  #geom_sf_text(x=12000,y=105000,label="Belgium",size=35,hjust=-.18,family=text,fontface="bold",color=txt)+
  #geom_sf_text(x=12000,y=87500,label="Total population: 11.5 million",size=10,hjust=-.13,family=text,fontface="bold",color=txt)+
  labs(title = "Belgium",
       subtitle = "Total population: 11.5 million\nPopulation density: 375 people per square km",
       caption = "Data: Statbel (2020 census) | Graphic: Abhinav Malasi",
       fill="Population density\n(people per square km)")+
  viridis::scale_fill_viridis(option = "D", 
                              #trans = "pseudo_log",  
                              breaks = c(0,25, 250, 2500, 20000),
                              labels = c(0,25, 250, 2500, 20000))+
  guides(fill = guide_colorbar(label.position = "left", 
                               title.hjust = 0.5,label.hjust = 1.5)) +
  theme(plot.background = element_rect(fill=bg,color=bg),
        panel.background = element_rect(fill=bg,color=bg),
        panel.grid = element_blank(),
        plot.title = element_text(size=120, face = "bold",margin=margin(t=20,b=10)),
        plot.subtitle = element_text(size=30,lineheight = .35),
        plot.caption = element_text(size=20,margin = margin(b=10,r=20)),
        plot.margin = margin(t=20,b=10),
        legend.title = element_text(size=30,lineheight=.25),
        legend.position = "none", #c(.2,.2),
        legend.key.width = unit(0.5, "line"),
        legend.text = element_text(size=35),
        legend.background = element_rect(fill=bg,color=bg),
        legend.key = element_rect(fill=bg,color=bg),
        text=element_text(family=text,color=txt))

#ggsave("raster.png",commune,width = 8,height = 8,units = "in",dpi=320)

plot_gg(commune,multicore=TRUE,width=5,height=5,scale=300,windowsize = c(1400, 866), zoom = 0.5, phi = 30, theta = -30)

render_snapshot("belgium_pop_density2", width = 10, height = 10,
                #vignette = FALSE,
                background = bg,dpi=320)
