library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(sf)
library(stringr)
library(ggfx)
library(shadowtext)
library(geomtextpath)

# title font
#sysfonts::font_add_google("Archivo","Archivo", bold.wt=700)
sysfonts::font_add_google("Orbitron","Orbitron")

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Advent Pro","Advent Pro")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()

showtext::showtext_opts(dpi = 300)


dir<-"~/GitHub/30 day map challenge 2022/space"

# get shapefile 
zip_file<-dir %>% 
          list.files(full.names=TRUE) %>%
          str_subset("japan") %>%
          str_subset(".zip$")

unzip(zip_file, exdir=dir)

shp_file<-dir %>%
           list.files(full.names=TRUE) %>%
           str_subset(".shp$") %>%
           str_subset("JPN")

japan_shp<-st_read(shp_file)

# get raster 
files<-list.files(dir, full.name=TRUE) %>%
str_subset('.tif$')

nightlights_raster<-raster(files[3])

# match shapefile and raster coordinate system
japan_shp<-st_transform(seasia_shp1,st_crs(nightlights_raster))

## crop raster to shapefile extent
## mask the cropped raster to the geographic boundaries
## convert to df 
nightlights_cropped<-raster::crop(nightlights_raster, extent(japan_shp%>% as_Spatial()))
nightlights_masked<-raster::mask(nightlights_cropped, japan_shp %>% as_Spatial())
nightlights_masked_df<-as.data.frame(nightlights_masked, xy=TRUE)

# get cities dataset 
cities<-read.csv("jp.csv")

# select 12 most populated cities 
jpn_cities<-cities %>% 
arrange(desc(population)) %>%
slice(1:12)

# get bbox 
jpn_bbox<-st_bbox(japan_shp)


# plot title and subtitle 
title<- "Japan From Space"
subtitle <- "A view of Japan's lights at night with satellite F18 in 2013"

# find bbox diff-will help with placing title and labels 
x_diff<-jpn_bbox[3] - jpn_bbox[1]
y_diff<-jpn_bbox[4] - jpn_bbox[2]

p<-ggplot()+
with_outer_glow(geom_sf(data = japan_shp, color="#0E0E0E", fill=NA, size=.5), colour="#9C9C9C",sigma=30)+
geom_raster(data=nightlights_masked_df, aes(x=x,y=y,fill=F182013.v4c_web.stable_lights.avg_vis))+
scale_fill_viridis_c(option = "inferno", na.value = "transparent")+
guides(fill=ggplot2::guide_colorbar(title="", direction="horizontal", barwidth=19, ticks=FALSE, label=FALSE, barheight=.5))+
geom_text_repel(data = jpn_cities %>% filter(city %in% c("Nagoya","Kyōto","Hiroshima","Fukuoka","Tokyo")), aes(x=lng, y=lat, label=city), color="#9C9C9C", nudge_x=-.095*x_diff, nudge_y=.098*y_diff, segment.linetype=5, segment.size=.1, family="Advent Pro", size=3.5, segment.curvature = -1e-20)+
geom_text_repel(data = jpn_cities %>% filter(city %in% c("Sapporo")), aes(x=lng, y=lat, label=city), color="#9C9C9C",  nudge_x = -.05*x_diff, nudge_y =.05*y_diff, segment.linetype=5, segment.size=.1, family="Advent Pro", size=3.5, segment.curvature = -1e-20)+
geom_text_repel(data = jpn_cities %>% filter(city %in% c("Sendai","Saitama","Kawanakajima")), aes(x=lng, y=lat, label=city), color="#9C9C9C",nudge_x=.09*x_diff, segment.linetype=5, segment.size=.1, family="Advent Pro", size=3.5, segment.curvature = -1e-20)+
geom_text_repel(data = jpn_cities %>% filter(city %in% c("Yokohama")), aes(x=lng , y=lat, label=city), color="#9C9C9C", nudge_x= .1*x_diff, nudge_y=-.08*y_diff,  segment.linetype=5, segment.size=.1,family="Advent Pro", size=3.5, segment.curvature = -1e-20)+
geom_text_repel(data = jpn_cities %>% filter(city %in% c("Kōbe","Ōsaka")), aes(x=lng , y=lat, label=city), color="#9C9C9C", nudge_x= .05*x_diff, nudge_y=-.08*y_diff,  segment.linetype=5, segment.size=.1,family="Advent Pro", size=3.5, segment.curvature = -1e-20)+
geom_shadowtext(aes(x=jpn_bbox[1]+.03*x_diff,y=jpn_bbox[4]-.1*y_diff,label=title), size=10, family="Orbitron", color="#0E0E0E", bg.color= "#9C9C9C", alpha=.7)+
geom_text(aes(x=jpn_bbox[1]+.03*x_diff,y=jpn_bbox[4]-.18*y_diff, label=subtitle), family="Advent Pro", color="#9C9C9C")+
geom_segment(aes(x=jpn_bbox[1]-.15*x_diff, y=jpn_bbox[4]-.28*y_diff, xend=jpn_bbox[1]+.27*x_diff,yend=jpn_bbox[4]-.28*y_diff), linetype="dotted", size=.3, color="#9C9C9C", arrow=arrow(type="closed",length=unit(.015,"npc")))+
geom_text(aes(x=jpn_bbox[1]+.04*x_diff,y=jpn_bbox[4]-.29*y_diff,label="Increasing light emission"),family="Advent Pro", color="#9C9C9C",size=2.5)+
geom_textbox(aes(x=jpn_bbox[1]-.17*x_diff,y=jpn_bbox[2]+.02*y_diff,label="<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Advent Pro\"'> #30DayMapChallenge </span></b><br><b><span style='font-family:\"Advent Pro\"'>Source:</b> Earth Observation Group (lights) and simplemaps (cities)<br><b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Advent Pro\"'> <b>samiaab1990</b></span>"), color="#9C9C9C",size=2, fill=NA, box.colour=NA, width=unit(.28,"npc"))+
theme(plot.margin = margin(l=50, unit="mm"),
      legend.position = c(.05,.75),
      legend.background = element_blank(),
      plot.background = element_rect(fill="#0E0E0E", color=NA),
      panel.background = element_blank(),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank())+
coord_sf(clip="off",
         xlim = c(jpn_bbox[1],150),
         expand=FALSE)

ggsave(plot = p, filename = "space.png", width=8, height=6, units="in", dpi=300, device=ragg::agg_png, bg="#0E0E0E")
