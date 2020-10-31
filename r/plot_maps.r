library(tmap)
library(raster)
library(dplyr)
library(sf)
library(elevatr)
library(grid)
library(RColorBrewer)
library(lubridate)

#read data
zaf <- read_sf("data/zaf_smpl.gpkg") %>% st_transform(crs = '+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
ovb <- read_sf("data/overberg_municipality.gpkg")

#region for downloading elevation
ovbbuf <- st_buffer(ovb,0.3)
bb <- st_bbox(ovbbuf)
bbsp <- as(ovbbuf, Class = "Spatial")
ovb <- ovb %>% st_transform(crs = '+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 

#bounding boxes for plots
ovbbuf_utm <- st_buffer(ovb,20000)
bb_utm <- st_bbox(ovbbuf_utm)
bb_map <- st_as_sfc(st_bbox(bb_utm))

#get hillshade
elevation <- get_elev_raster(bbsp, z = 7,clip="bbox")
elevation2 <- elevation
elevation2[elevation2<=0]<-NA
slope = terrain(elevation2, opt='slope')
aspect = terrain(elevation2, opt='aspect')
hill = hillShade(slope, aspect, 40, 270)
hill <- projectRaster(hill,crs='+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs')

#overview map
zafmap <- tm_shape(zaf) + tm_fill()
ovbmap <- tm_shape(bb_map) + tm_borders(lwd = 1.5)
insetmap <- zafmap+ovbmap

#natural and remaining

#natural from:
#http://bgis.sanbi.org/SpatialDataset/Detail/1674
#select BIOREGION_ = East Coast Renosterveld Bioregion

#remaining from:
#http://bgis.sanbi.org/SpatialDataset/Detail/385
#select Transforma = Natural | Degraded

nat <- read_sf("data/overberg_ruens_bioregion.gpkg") %>% st_transform(crs = '+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 
rem <- read_sf("data/ovb_ruens_nat_deg_2009.gpkg") %>% st_transform(crs = '+proj=utm +zone=34 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs') 

#lost fragments data
## change2020.gpkg file contains sensitive data. 
## hence we provide non-spatial data in change2020_nonspatial.csv

lost <- read_sf("data/change2020.gpkg") %>%
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

# figure 1 ----------------------------------------------------------------

ovbnat <- tm_shape(hill,bbox=bb_utm) +
tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = F,alpha=0.5)  +
tm_shape(zaf,bbox=bb_utm) +
tm_fill(col="olivedrab3",alpha=0.3) +
tm_shape(nat,bbox=bb_utm) +
tm_fill(col="blue",alpha=0.3) +
tm_shape(ovb,bbox=bb_utm) +
tm_borders(lwd = 1.5)+
tm_add_legend(
  type = "fill",
  labels = c('Renosterveld extent','Overberg region'),
  col = c('blue',NA),
  alpha=0.5,
  border.lwd = c(NA,1.5),
  border.col='grey40') +
tm_layout(
  legend.position = c("right","bottom")
)

ovbrem <- tm_shape(hill,bbox=bb_utm) +
tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = F,alpha=0.5)  +
tm_shape(zaf,bbox=bb_utm) +
tm_fill(col="olivedrab3",alpha=0.3) +
tm_shape(rem,bbox=bb_utm) +
tm_fill(col="blue",alpha=0.3) +
tm_shape(ovb,bbox=bb_utm) +
tm_borders(lwd = 1.5)  +
tm_scale_bar()

grid.newpage()
pdf(file="figures/fig1.pdf",width=7,height=4)
  pushViewport(viewport(layout=grid.layout(1,2)))
  print(ovbnat, vp=viewport(layout.pos.col = 1))
  print(ovbrem, vp=viewport(layout.pos.col = 2))
  print(insetmap, vp = viewport(0.065, 0.295, width = 0.17, height = 0.17))
dev.off()


lostpl <- lost %>% 
  mutate(area=pmax(0.499,area))

# figure 2 --------------------------------------------------------------
lostplot <- tm_shape(hill,bbox=bb_utm) +
tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE,alpha=0.4)  +
tm_shape(zaf,bbox=bb_utm) +
tm_fill(col="olivedrab3",alpha=0.2) +
tm_shape(ovb,bbox=bb_utm) +
tm_borders(lwd = 1.5)+
tm_shape(lostpl) +
tm_symbols(col = "area",
           size = "area",
           palette = 'YlOrRd',
           border.lwd = 0.5,
           alpha = 0.5,
           style='fixed',
           breaks=c(0,0.5,1,2,5,10,50),
           size.lim = c(0, 10),
           legend.size.show = F,
           legend.col.show = F) +
tm_add_legend("symbol", 
              col=brewer.pal(6, "YlOrRd"),
              border.lwd = 0.5,
              alpha=0.7, 
              size = (pmin(1,c(0.5,1,2,5,10,50)/10))^0.5, 
              labels = as.character(c(0.5,1,2,5,10,50)),
              title = "area lost (hectares)") +
tm_legend(legend.position = c(0.02,-0.02),
          title.size=0.6,
          legend.text.size = 0.4)

#pdf(file="figures/fig2.pdf",width=5,height=4)
#print(lostplot)
#dev.off()

datepl <- lostpl %>% 
  filter(valid!=0)
dateplot <- tm_shape(hill,bbox=bb_utm) +
  tm_raster(palette = gray(0:100 / 100), n = 100, legend.show = FALSE,alpha=0.4)  +
  tm_shape(zaf,bbox=bb_utm) +
  tm_fill(col="olivedrab3",alpha=0.2) +
  tm_shape(ovb,bbox=bb_utm) +
  tm_borders(lwd = 1.5)+
  tm_shape(datepl) +
  tm_symbols(col = "mid",
             size = "area",
             palette = 'RdYlBu',
             border.lwd = 0.5,
             alpha = 0.5,
             size.lim = c(0, 10),
             legend.size.show = F,
             legend.col.show = F) +
  tm_add_legend("symbol", 
                col=brewer.pal(6, "RdYlBu"),
                border.lwd = 0.5,
                alpha=0.7, 
                size = (pmin(1,c(0.5,1,2,5,10,50)/10))^0.5,
                labels = as.character(c("Jan 2016","Aug 2016","Apr 2017","Jan 2018","Aug 2018","Apr 2019")),
                title = "date lost") +
  tm_legend(legend.position = c(0.02,-0.02),
            title.size=0.6,
            legend.text.size = 0.4)

grid.newpage()
pdf(file="figures/fig2.pdf",width=7,height=4)
pushViewport(viewport(layout=grid.layout(1,2)))
print(lostplot, vp=viewport(layout.pos.col = 1))
print(dateplot, vp=viewport(layout.pos.col = 2))
dev.off()
png(file="figures/fig2.png",width=7,height=4)
pushViewport(viewport(layout=grid.layout(1,2)))
print(lostplot, vp=viewport(layout.pos.col = 1))
print(dateplot, vp=viewport(layout.pos.col = 2))
dev.off()
