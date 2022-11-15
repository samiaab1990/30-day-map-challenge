library(ggplot2)
library(dplyr)
library(sf)
library(sp)
library(stringr)
library(tidyr)
library(rgeos)
library(gridExtra)
library(raster)
library(ggtext)
library(ggfx)
library(grid)
# title font
#sysfonts::font_add_google("Pacifico","Pacifico")
sysfonts::font_add(family = "Honeycomb Happiness", regular="~/GitHub/30 day map challenge 2022/fonts/HoneycombHappiness-ywnRm.ttf")


# text for captions, legend, etc.
sysfonts::font_add_google("Ubuntu","Ubuntu")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_opts(dpi = 300)

# get gradient text function 
source("~/GitHub/30 day map challenge 2022/custom_functions/gradient_text_function.R")

dir = "~/GitHub/30 day map challenge 2022/hexagons"

# dc trees file 
csv_file<-dir %>%
list.files() %>%
str_subset(".csv$")

trees<-read.csv(paste0(dir,"/",csv_file))

# dc shape file 
shp_file<-dir %>%
list.files() %>%
str_subset(".shp$")

# transform to 3857 (in meters) rather than lat/long for the hex binning
# https://epsg.io/3857

dc_sf<-st_read(shp_file) %>%
  st_transform(3857)

# make a grid
# divide x and y by 80 bins
dc_grid<-st_make_grid(
  dc_sf,
  n = c(80, 80),
  crs = crs(dc_sf),
  what = "polygons",
  square = FALSE
) %>%
st_intersection(dc_sf) %>% 
st_sf() %>%
mutate(id = row_number())

# find all the cherry blossom trees in the dataset
# using https://trees.dc.gov/apps/DCGIS::dc-cherry-trees-near-me/explore as a reference 
trees_by_cherry<-trees %>%
mutate(cherry_blossom = case_when(
  str_detect(tolower(CMMN_NM),"cherry") ~ "Cherry",
  CMMN_NM %in% c("Purple leaf plum","Crape myrtle","Crabapple") ~ "Cherry",
  TRUE ~ NA_character_))

# filter only cherry blossom trees per above criteria
trees_cherry_dat<-trees_by_cherry %>%
filter(cherry_blossom == "Cherry") 

# convert data frame to sf
# the data frame is in the same coords as the origin dc_shapefile (4326)
# transform to 3857
trees_cherry_sf<-st_as_sf(
    x = trees_cherry_dat,
    coords = c("X","Y"),
    crs = 4326
  ) %>%
  st_transform(st_crs(dc_sf))

# join the trees sf to the grid-this will intersect the points to polygon 
# and enable summarizing by hexagon 
dc_trees_join<-st_join(trees_cherry_sf, dc_grid, join=st_intersects)

# join dc grid by the joined trees
# joined trees-group by ID (cherry blossom is always = Cherry in this case)
# summarise total counts 
# left join because we want all of dc_grid 

trees_grid<-dc_grid %>%
st_join(
dc_trees_join %>%
group_by(id, cherry_blossom) %>%
summarise(counts=n()), left=TRUE) 

# title 
title = "Cherry Blossoms of Washington DC"

# gradient title 
title_gradient<-make_gradient(string_lab=title, start_color="#FDD9E5", end_color = "#FC3084")

# bbox and bbox range
bbox_dc<- st_bbox(dc_sf)
x_diff<-bbox_dc[3]-bbox_dc[1]
y_diff<-bbox_dc[4]-bbox_dc[2]

# hexbin shape 
hexbin<-hexbin::hexcoords(.7, sep=" ")

# create hexbin key-with help from https://stackoverflow.com/questions/69957847/how-do-i-change-the-shape-of-the-legend-key-glyph-to-a-hexagon-in-ggplot2
draw_key_hex <- function (data, params, size) {
  # hexagon vertex coordinates 
  v <- list(x = hexbin$x, 
            y = hexbin$y)
  # hexagon grob
  polygonGrob(v$x, v$y, 
              gp = gpar(col = data$fill,
                        fill = alpha(data$fill, .8),
                        size=.8))
}

# subtitle and caption
subtitle<-paste0("Number of cherry blossom trees per 293 x 365 meters (represented by each hexagon) planted in Washington D.C. Species include: ",paste(unique(trees_cherry_dat$CMMN_NM),collapse=", "),".")
caption<-"<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Ubuntu\"'> #30DayMapChallenge </span></b><br><b><span style='font-family:\"Ubuntu\"'>Source:</b> Open Data DC &<br>DDOT Urban Forestry Division <br><b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Ubuntu\"'> <b>samiaab1990</b></span>"

# find the default ggplot2 breaks for continuous scale
# creating a manual continuous hexbin scale and manually labeling using breaks below 
breaks<-with(trees_grid, labeling::extended(range(counts, na.rm=TRUE)[1], range(counts, na.rm=TRUE)[2], m = 5))
default_breaks<-breaks[2:6]
db_reformatted<-paste(default_breaks, collapse="   ")


p<-ggplot()+
with_shadow(geom_sf(data=dc_sf, fill="#252525", color=NA),sigma=5)+
geom_sf(data=trees_grid, aes(fill=counts), color="#252525", key_glyph=draw_key_hex)+
geom_sf(data=dc_sf, fill=NA, color="#FDD9E5", size=.7)+
scale_fill_gradient(low="#FDD9E5", high="#FC3084", na.value="#494949")+
geom_segment(aes(x=bbox_dc[1]-x_diff*.32, y=bbox_dc[2]+.57*y_diff, xend=bbox_dc[1]-x_diff*.07, yend=bbox_dc[2]+.57*y_diff), color="#FDD9E5", arrow=arrow(type="closed", length=unit(.01,"npc")))+
geom_text(aes(x=bbox_dc[1]-x_diff*.19, y=bbox_dc[2]+.58*y_diff, label=db_reformatted),color="#FDD9E5", family="Ubuntu", size=5.2, fontface="bold")+
geom_textbox(aes(x=bbox_dc[1]-x_diff*.15, y=bbox_dc[2]+.48*y_diff, label=subtitle), color="#FDD9E5", family="Ubunutu", size=5, width=unit(.33, "npc"),fill=NA, box.color=NA)+
geom_textbox(aes(x=bbox_dc[3]-x_diff*.01, y=bbox_dc[2], label=caption), color="#FDD9E5", width=unit(.25,"npc"), fill=NA, box.color=NA, hjust=1, halign=1)+
labs(title = title_gradient)+
guides(fill=ggplot2::guide_legend(title="",  ticks=FALSE, direction="horizontal", label = FALSE))+
theme(#legend.position="none",
      plot.margin = margin(l=10, unit="mm"),
      legend.background = element_blank(),
      legend.box.background = element_blank(),
      legend.position = c(.15,.61),
      legend.key = element_blank(),
      legend.text = element_text(size=15, family="Ubuntu", color="#FDD9E5", face="bold"),
      legend.spacing.x = unit(2,"mm"),
      plot.background=element_rect(fill="#252525", color="#252525"),
      panel.background=element_rect(fill="#252525", color="#252525"),
      plot.title=element_textbox(family="Honeycomb Happiness", size=80, halign=.5, margin=margin(t=5, unit="mm"), width=unit(.9,"npc")),
      panel.grid=element_blank(),
      axis.text=element_blank(),
      axis.ticks=element_blank(),
      axis.title=element_blank())+
coord_sf(clip="off", xlim=c(bbox_dc[1]-x_diff*.36,bbox_dc[3]))

ggsave(plot = p, filename = "cherry_blossom.png", width=15, height=15, units="in", dpi=300, device=ragg::agg_png, bg="#252525")



