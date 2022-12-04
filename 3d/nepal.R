library(dplyr)
library(rayshader)
library(stringr)
library(elevatr)
library(sf)
library(rgdal)
library(ggplot2)
library(purrr)
library(raster)
library(ggtext)
library(showtext)
library(magick)
library(janitor)

dir<-"~/GitHub/30 day map challenge 2022/3d/"

# title font
sysfonts::font_add_google("Permanent Marker","Permanent Marker")



# font for the subtitle, caption, etc.
sysfonts::font_add_google("Lora","Lora")
sysfonts::font_add_google("Rubik","Rubik")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

 nepal_shp<-paste0(dir,"nepal") %>%
 list.files(full.names=TRUE) %>%
 str_subset(".shp$") %>%
 st_read() 
 
nepal_raster<-get_elev_raster(locations=nepal_shp, prj = "+proj=longlat +ellps=WGS84 +datum=WGS84", z=9, clip="locations")
 
nepal_raster_to_matrix<- nepal_raster %>% raster_to_matrix()


# with help from: https://spencerschien.info/post/data_viz_how_to/high_quality_rayshader_visuals/
# Dynaimcally set window height and width based on object size
w <- nrow(nepal_raster_to_matrix)
h <- ncol(nepal_raster_to_matrix)

# Scale the dimensions so we can use them as multipliers
wr <- w / max(c(w,h))  
hr <- h / max(c(w,h))

# Limit ratio so that the shorter side is at least .75 of longer side
if (min(c(wr, hr)) < .75) {
  if (wr < .75) {
    wr <- .75
  } else {
    hr <- .75
  }
}

# palette from coolors.com 
pal =  c("#001219","#005f73","#0a9396","#94d2bd","#e9d8a6","#ee9b00")
#pal = met.brewer("Tam")
#not used
#nepal_png<-png::readPNG(paste0(dir,"nepal.png"))

rgl::rgl.clear()

nepal_raster_to_matrix %>%
sphere_shade() %>% 
add_overlay(height_shade(nepal_raster_to_matrix,texture = grDevices::colorRampPalette(pal)(256))) %>% 
add_shadow(ray_shade(nepal_raster_to_matrix,zscale=50),0.3) %>% 

# multiplied width by an additional 1.5 because cutting off in rendering


plot_3d(nepal_raster_to_matrix,
          windowsize = c(800*wr*1.5,800*hr), 
          # not needed since rendering highquality
          #shadowcolor = "#028090",
          solid = FALSE, 
          fov = 0,
          zscale = 50,
          phi = 45,
          zoom = .5, 
          theta = 0) 
# for testing purposes, save the plot as a snapshot rather than 3d image
#render_snapshot("test.png")

# high quality rendering
render_highquality(
  "nepal_3d22.png", 
  parallel = TRUE, 
  samples = 400,
  interactive = FALSE,
  intensity_env = 2,
  width = round(6000 * wr *1.5), 
  height = round(6000 * hr)
)

# 10 highest points
# taken from wikipedia
# initial goal was to use nepal shapefile and plot x + y on map-overlay on rendered image
# used a manual process to plot points since overlay was difficult (plus overlapping points)


elev_dat<-tibble(
  name = c("Mt Everest","Kangchenjunga","Lhotse","Makalu","Cho Oyu", "Dhaulagiri I", "Manaslu", "Annapurna I","Gyachung Kang","Annapurna II"),
  x = c(86.9250, 88.146667, 86.9333, 87.088889,86.660833, 83.4875,84.561944,83.820278, 86.742222, 84.121389),
  y = c(27.9881,27.7025,27.9617,27.889722,28.094167, 28.698333,28.549444,28.596111,28.098056,28.535833),
  elevation = c(8848.86,8586,8516,8463,8201,8167,8163,8091,7952,7937)
) %>%
  mutate(lab = paste0("<b style='font-family:Rubik; font-size:25px; color:#353535'>",name,"</b><br><span style='color:#001A21; font-size:15px; font-family:\"Font Awesome 5 Free Solid\"'>&#xf6fc;</span><b style='font-family:Rubik; font-size:20px; color:#001A21'>", elevation," m"))


# create the title as ggplot() since magick does not allow custom font options
title<-ggplot()+
  geom_text(aes(x=1,y=1,label="Nepal"), family="Permanent Marker", size=40, color="#001A21")+
  theme_void()

ggsave(
  plot = title,
  filename = "title.png",
  width=2000,
  height=500,
  units = "px",
  bg = "transparent",
  dpi=300
)

# create a function to generate labels
# uses elevation_dat and generates a png for each peak
# customizable options for placement of label relative to segment, increasing height of segment

generate_peak_label<-function(mt_name, nudge_x=0, nudge_y=0,y_max=2, width_img=800, height_img=900, x_min=2, x_max=2, y_min=0)
{
  nepal_peak<-ggplot()+
    #geom_sf(data = nepal_shp, fill=NA, color="#353535", linewidth=1)+
    geom_segment(data=elev_dat %>% filter(name==mt_name), aes(x=x, xend=x, y=y, yend=y+nudge_y-.3))+
    geom_richtext(data=elev_dat %>% filter(name==mt_name), aes(x=x+nudge_x,y=y+nudge_y,label=lab),  fill=NA, label.colour=NA, lineheight=.5)+
    theme_void()+
    xlim(elev_dat %>% filter(name==mt_name) %>% pull(x)-x_min,elev_dat %>% filter(name==mt_name) %>% pull(x)+x_max)+
    ylim(elev_dat %>% filter(name==mt_name) %>% pull(y)-y_min,elev_dat %>% filter(name==mt_name) %>% pull(y)+y_max)+
    coord_fixed(clip="off")
  
  ggsave(
    plot = nepal_peak,
    filename = paste0(mt_name,".png"),
    width=width_img,
    height=height_img,
    units = "px",
    bg = "transparent",
    dpi=300
  )
}

# create caption
caption<-ggplot()+
  geom_richtext(aes(x=10,y=1,label="<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Lora\"'> #30DayMapChallenge. </span></b><b style='font-family:\"Lora\"'>Source:</b><span style='font-family:\"Lora\"'> rayshader(@tylermorganwall), elevatr(@jhollist), Wikipedia</span> <b style='font-family:\"Lora\"'>Map made by:</b><span style='font-family:\"Lora\"'> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Lora\"'> samiaab1990</span>"), size=10, color="#353535", fill=NA, label.colour=NA)+
  theme_void()

ggsave(
  plot = caption,
  filename = "caption.png",
  width=8000,
  height=1000,
  units = "px",
  bg = "transparent",
  dpi=300
)

# generate peak label for each peak
generate_peak_label("Mt Everest", nudge_y=2)
generate_peak_label("Kangchenjunga", nudge_y=1.5)
generate_peak_label("Lhotse", nudge_x=.8, nudge_y=1.3)
generate_peak_label("Makalu", nudge_x=.8, nudge_y=.4)
generate_peak_label("Cho Oyu", nudge_x=-.8, nudge_y=.4)
generate_peak_label("Dhaulagiri I", nudge_y=3, y_max=3, height_img = 1000)
generate_peak_label("Manaslu", nudge_y=2)
generate_peak_label("Annapurna I", nudge_y=2.2, y_max=2.2)
generate_peak_label("Gyachung Kang", y_max=2.5, nudge_y=2.5, nudge_x=-3, x_min=6)
generate_peak_label("Annapurna II", y_max=3.5, nudge_y=3.5, height_img=1000)

# read the high quality image
high_quality <- image_read(paste0(dir,"nepal_3d2.png"))

# read the ggplot() generated title and scale 
nepal_title<-image_read("title.png") %>%
  image_scale("4000")

# for all the peak names, read the image and scale width to 900
elev_dat$name %>%
map(function(x) assign(make_clean_names(x), image_read(paste0(x,".png")) %>% image_scale(900), envir=.GlobalEnv))

# read the caption
caption_magick<-image_read("caption.png") %>%
                image_scale("x500")
# start with annotating the high quality rendered image
img<-image_annotate(high_quality, "An elevation map of", font = "Palatino",
               color = "#353535", size = 350,
               location = "+4200+200")


# add the image title 
img_title<-image_composite(img, nepal_title, offset = "+1200+310", gravity="north")

#add the peaks (manually :\)

mt_everest_layer<-image_composite(img_title, mt_everest, offset="+6510+1700")

kangchenjunga_layer<-image_composite(mt_everest_layer, kangchenjunga, offset="+7570+1890")

lhotse_layer<-image_composite(kangchenjunga_layer, lhotse, offset="+6530+1750")

makalu_layer<-image_composite(lhotse_layer, makalu, offset="+6650+1760")

cho_oyu_layer<-image_composite(makalu_layer, cho_oyu, offset="+6280+1650")

dhaulagiri_i_layer<-image_composite(cho_oyu_layer, dhaulagiri_i, offset="+3480+1120")

manaslu_layer<-image_composite(dhaulagiri_i_layer, manaslu, offset="+4430+1360")

annapurna_i_layer<-image_composite(manaslu_layer, annapurna_i, offset="+3790+1290")

gyachung_kang_layer<-image_composite(annapurna_i_layer, gyachung_kang, offset="+6150+1760")

annapurna_ii_layer<-image_composite(gyachung_kang_layer, annapurna_ii, offset="+4060+1150")

annotated_image<-image_composite(annapurna_ii_layer, caption_magick, gravity="south", offset="+0+0")

# write final image
image_write(annotated_image, "nepal_annotated.png")