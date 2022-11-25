setwd("~/R/30DayMapChallenge/2022/25_colour_friday_2_colors")

library(tidyverse)
library(showtext)
library(ggtext)

text = "PT Serif Caption"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)
showtext_auto()

state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

gps_stations <- readxl::read_xlsx('stations.xlsx') %>% select(1:5) %>% 
  janitor::row_to_names(row_number = 1) %>% 
  mutate(application_id = as.numeric(application_id))

us_cities <- read_csv("uscities.csv")

station_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/station_info.csv')


df1 <- merge(state_stations,station_info, by="call_sign", all.x = TRUE)

df <- merge(df1,us_cities,by = "city",all.x=TRUE) %>% 
  filter(status == "LICENSED") %>%
  mutate(channel_1_letter = substr(call_sign,1,1))



df$college <- sapply("College", grepl, df$licensee.y,ignore.case = TRUE)
df$school <- sapply("School", grepl, df$licensee.y,ignore.case = TRUE)
df$university <- sapply("University", grepl, df$licensee.y,ignore.case = TRUE)

df <- df %>% mutate(keyword = case_when(college == TRUE ~ "College",
                                        school == TRUE ~ "School",
                                        university ==  TRUE ~ "University",
                                        TRUE~"other"))

bg = "#032b43"
txt_col = "#eeeeee"


####################################################
df_dual <- merge(df1,us_cities,by = "city",all.x=TRUE) %>% 
  filter(status == "LICENSED") %>%
  mutate(channel_1_letter = substr(call_sign,1,1))


df_dual_sf <- sf::st_as_sf(df_dual %>% na.omit(),coords=c("lng","lat"),crs = "Wgs84")

df_dual_sf %>% 
  ggplot()+
  #geom_point(aes(lng,lat, color = keyword)) + 
  geom_sf(aes(color = channel_1_letter), alpha=.5) + 
  xlab("") + ylab("") +
  labs(title = "Distribution of radio stations starting with <span style = 'color:#d00000;'>K</span> or <span style = 'color:#ffff3f;'>W</span>",
       caption = "Source: Wikipedia | Graphic: Abhinav Malasi",
       color="") +
  scale_color_manual(values = c("#d00000","#ffff3f"))+
  theme(plot.background = element_rect(fill = bg, color = bg),
        panel.background = element_rect(fill = bg, color = bg),
        legend.background = element_rect(fill = bg, color = bg),
        legend.key = element_rect(fill = bg, color = bg),
        legend.text = element_text(size=35),
        #legend.position = c(.9,.9),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t=20, b=20),
        plot.title = element_markdown(size=60, hjust=.5,lineheight = .35),
        plot.caption = element_text(size=35, hjust=.95),
        text = element_text(family = text, color=txt_col))

ggsave("radio_sations_dual.png", last_plot(), width=10, height = 8)
