library(dplyr)
library(ggplot2)
library(raster)
library(rgdal)
library(sf)
library(stringr)
library(ggfx)
library(shadowtext)
library(ggsflabel)

# title font
#sysfonts::font_add_google("Archivo","Archivo", bold.wt=700)


# font for the subtitle, caption, etc.
sysfonts::font_add_google("Yanone Kaffeesatz","Yanone Kaffeesatz")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()

showtext::showtext_opts(dpi = 300)

# dir 
dir<-'D:\\space'

zip_files<-dir %>% 
          list.files(full.names=TRUE) %>%
          str_subset("japan") %>%
          str_subset(".zip$")

unzip(zip_files, exdir=dir)

shp_files<-dir %>%
           list.files(full.names=TRUE) %>%
           str_subset(".shp$") %>%
           str_subset("JPN")

japan_shp<-st_read(shp_files)



files<-list.files(dir, full.name=TRUE) %>%
str_subset('.tif$')

nightlights_raster<-raster(files[3])

japan_shp<-st_transform(seasia_shp1,st_crs(nightlights_raster))


nightlights_cropped<-raster::crop(nightlights_raster, extent(japan_shp%>% as_Spatial()))
nightlights_masked<-raster::mask(nightlights_cropped, japan_shp %>% as_Spatial())
nightlights_masked_df<-as.data.frame(nightlights_masked, xy=TRUE)

cities<-read.csv("jp.csv")

jpn_cities<-cities %>% 
arrange(desc(population)) %>%
slice(1:12)

jpn_bbox<-st_bbox(japan_shp)

inferno_colors<-c("#57106e","#bc3754","#f98e09","#fcffa4")
inferno_pal<-colorRampPalette(inferno_colors)

title<- "Japan From Space"
subtitle <- "A view of Japan from space at night"


x_diff<-jpn_bbox[3] - jpn_bbox[1]
y_diff<-jpn_bbox[4] - jpn_bbox[2]

p<-ggplot()+
with_outer_glow(geom_sf(data = japan_shp, color="#0E0E0E", fill=NA, size=.5), colour="#9C9C9C",sigma=30)+
geom_raster(data=nightlights_masked_df, aes(x=x,y=y,fill=F182013.v4c_web.stable_lights.avg_vis))+
scale_fill_viridis_c(option = "inferno", na.value = "transparent")+
geom_text_repel(data = jpn_cities %>% filter(city %in% c("Sapporo","Nagoya","Kyōto","Hiroshima")), aes(x=lng -.2*x_diff, y=lat+.08*y_diff, label=city), color="white", min.segment.length=Inf)+
geom_text_repel(data = jpn_cities %>% filter(!city %in% c("Sapporo","Nagoya","Kyōto","Hiroshima")), aes(x=lng +.3*x_diff, y=lat+.08*y_diff, label=city), color="white", min.segment.length=Inf)+
theme(legend.position = "none",
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
