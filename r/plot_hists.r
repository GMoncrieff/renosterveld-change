library(sf)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(ggpubr)

## change2020.gpkg file contains sensitive data. 
## hence we provide non-spatial data in change2020_nonspatial.csv

renoster <- read_sf("data/change2020.gpkg") %>%
  st_transform(crs = '+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') %>%
  dplyr::select(ID,date_1,date_2,valid) %>%
  filter(valid %in% c(1,2)) %>%
  mutate(area=as.numeric(st_area(.)/10000)) %>%
  filter(area>0) %>%
  mutate(date_1 = as.Date(date_1),
         date_2 = as.Date(date_2)) %>%
  mutate(mid = date_1+ (date_2 - date_1)/2) %>%
  mutate(month = month(mid),year = year(mid)) %>%
  mutate(time = (date_2 - date_1))

reno_sum_year = renoster %>%
  group_by(month,year) %>%
  summarise(lost = sum(area)) %>%
  mutate(month2 = as.Date(paste0("2015-", month,"-01"),"%Y-%m-%d")) %>%
  filter(!is.na(year)) %>%
  ungroup()

reno_count_year = renoster %>%
  group_by(month,year) %>%
  summarise(lost = n()) %>%
  mutate(month2 = as.Date(paste0("2015-", month,"-01"),"%Y-%m-%d")) %>%
  filter(!is.na(year)) %>%
  ungroup()

# figure 3 ------
area <- ggplot(reno_sum_year,aes(x=month2,y=lost)) +
  geom_bar(stat='identity') +
  facet_wrap(~ year, ncol = 2) +
  labs(y = "Hectares lost",
       x = "Month") +
  scale_x_date(date_labels = "%b") +
  theme_pubr()

count <- ggplot(reno_count_year,aes(x=month2,y=lost)) +
  geom_bar(stat='identity') +
  facet_wrap(~ year, ncol = 2) +
  labs(y = "Event count",
       x = "Month") +
  scale_x_date(date_labels = "%b")  +
  theme_pubr()

fig3 <- ggarrange(area, count, ncol = 2, nrow = 1)
ggsave("figures/fig3.png",fig3,height=5,width=9)
ggsave("figures/fig3.pdf",fig3,height=5,width=9)


#figure 4 -------
renoster <- read_sf("data/change2020.gpkg") %>%
  st_transform(crs = '+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') %>%
  dplyr::select(ID,date_1,date_2,valid) %>%
  filter(valid %in% c(0,1,2)) %>%
  mutate(area=as.numeric(st_area(.)/10000)) %>%
  filter(area>0) %>%
  mutate(date_1 = as.Date(date_1),
         date_2 = as.Date(date_2)) %>%
  mutate(mid = date_1+ (date_2 - date_1)/2) %>%
  mutate(month = month(mid),year = year(mid)) %>%
  mutate(time = (date_2 - date_1))
renoster$area <- sort(renoster$area)
renoster$cumulative <- cumsum(renoster$area)

histarea <- ggplot(renoster, aes(x=area)) + 
  geom_histogram() +
  labs(y = "count",
       x = "area (hectares)")+
  theme_pubr()

steparea <- ggplot(renoster, aes(x=area,y=cumulative)) + 
  geom_step()+
  labs(y = "cumulative area",
       x = "area (hectares)")+
  theme_pubr()

fig4 <- ggarrange(histarea, steparea, ncol = 2, nrow = 1)
ggsave("figures/fig4.png",fig4,height=3,width=7)
ggsave("figures/fig4.pdf",fig4,height=3,width=7)


# fig 5 -----
histtime <- ggplot(renoster, aes(x=time)) + 
  geom_histogram(aes(y = stat(count / sum(count))),binwidth = 4) +
  xlim(c(0,60))+
  labs(y = "frequency",
       x = "days") +
  theme_pubr()
ggsave("figures/fig5.png",histtime,height=3,width=3.5)
ggsave("figures/fig5.pdf",fig4,height=3,width=3.5)