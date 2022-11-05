setwd("~/R/30DayMapChallenge/2022/5_Ukraine")

library(tidyverse)
library(sf)
library(showtext)

text = "Montserrat"
sysfonts::font_families_google()

sysfonts::font_add_google(text,text)

showtext_auto()

ukraine <- read_sf("ukr_adm_sspe_20221005/ukr_admbnda_adm1_sspe_20221005.shp")

language <- readxl::read_xlsx("19A050501_02.xlsx",skip = 5) %>% 
  na.omit() %>% janitor::clean_names() %>%
  filter(!(language == "Total"))

language$share <- as.numeric(language$share)


popular <- language %>% filter(!is.na(share), !(region == "Ukraina")) %>%
  group_by(region) %>% summarise(across(where(is.numeric),max)) 

popular_lang <- merge(popular,language, by=c("region","share"),all.x = TRUE) %>%
  mutate(region = str_replace(region," oblast",""),
         region = str_replace(region,"m. ",""),
         region = str_replace(region,"Avtonomna Respublika Krym","Autonomous Republic of Crimea"))


ukraine_lang <- merge(ukraine,popular_lang, by.x="ADM1_EN", by.y="region")

bg = "#F8FFDB"
txt_col = "#313131"

plot <- ukraine_lang %>% 
  ggplot()+
  geom_sf(aes(fill=language),color=bg) +
  scale_fill_manual(values = c("#0057b7","#ffd700")) +
  labs(title = "Popular languages in regions of Ukraine",
       caption = "Source: www.ukrcensus.gov.ua, www.data.humdata.org | Graphic: Abhinav Malasi")+
  theme(plot.background = element_rect(fill=bg, color=bg),
        panel.background = element_rect(fill=bg, color=bg),
        panel.grid = element_blank(),
        legend.position = c(.1,.2),
        legend.background = element_rect(fill=NA, color=NA),
        legend.key = element_rect(fill=bg, color=bg),
        legend.title = element_blank(),
        legend.text = element_text(size=30),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(t=20,b=10),
        plot.title = element_text(size=70, face="bold"),
        plot.caption = element_text(size=30),
        text = element_text(color=txt_col))

ggsave("ukraine.png",plot,width = 8, height = 6, dpi=300)
