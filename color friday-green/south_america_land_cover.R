library(raster)
library(rgdal)
library(cowplot)
library(ragg)
library(broom)
library(dplyr)
library(ggplot2)
library(sp)
library(emojifont)
library(ggtext)
library(shadowtext)
library(grid)
library(gtable)


# title font
sysfonts::font_add_google("Marcellus SC","Marcellus SC")


# font for the subtitle, caption, etc.
sysfonts::font_add_google("Nanum Gothic","Nanum Gothic")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/DataViz/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/DataViz/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# Explore the .tif data 
GDALinfo("SA_lc_Map_41class.tif")


# save as raster
sa_land_cover<-raster("SA_lc_Map_41class.tif")

# get shapefile for basemap and transform to same coordinate system as raster
sa_shapefile<-readOGR("~/GitHub/DataViz/30 day map challenge 2022/color friday-green/stanford-vc965bq8111-shapefile/vc965bq8111.shp")%>%
  spTransform(CRS("+proj=longlat +ellps=clrk66"))

# get data dictionary 
data_dictionary<-read.csv("data_dictionary_csv.csv")

# convert raster and shapefile to data frame 
sa_land_cover_xy<-as.data.frame(sa_land_cover, xy=TRUE)
sa_shapefile_tidy<-broom::tidy(sa_shapefile)


sa_land_cover_xy_forests<-sa_land_cover_xy %>%
left_join(data_dictionary, by=c("SA_lc_Map_41class" = "Value")) %>%
filter(!SA_lc_Map_41class %in% c(0,3,4,5,7,12,14,17,18,19,20,22,25,26,27,28,30,31,33,34,35,37,38,39)) %>%
mutate(Class.Name = case_when(
  Class.Name == "Mixed pine forest with secondary forest and agriculture" ~ "Mixed pine forest with secondary forest\n and agriculture",
  TRUE ~ Class.Name
))


pal<-c("#6BAC8D","#9DFFC8",
       "#C7EA46","#A0CD9C",
       "#B2D3C2","#1cc100",
       "#4D6211","#092901",
       "#39FF14","#40B5AD",
       "#A4B465","#004953",
       "#DFFF00","#17ad76",
       "#c3e796","#245644",
       "#E9FFDB")


background_color = "#D6E7D0"

# tree key symbols 
# with help from: https://stackoverflow.com/questions/72448513/ggplot-legend-with-the-use-of-emojis

draw_key_symbol <- function(data, params, size) {
  data$label <- fontawesome("fa-tree")
  
  shadowtextGrob(data$label, .5, .5, bg.colour = "#33382B",
                 gp = grid::gpar(col = data$fill, 
                                 fill = background_color,
                                 fontfamily = "Font Awesome 5 Free Solid", 
                                 alpha=.8,
                                 size=.8,
                                 fontsize = 50))
}



p<-ggplot()+
geom_polygon(data = sa_shapefile_tidy, aes(x=long, y=lat, group=group), color=NA, fill="#33382B")+
geom_raster(data = sa_land_cover_xy_forests, aes(x=x, y=y, fill=as.factor(Class.Name)), key_glyph=draw_key_symbol)+
geom_polygon(data = sa_shapefile_tidy, aes(x=long, y=lat, group=group), color=background_color, fill=NA, size=1)+
geom_text(aes(x = -10, y=15, label = "Forest Cover\nOf South America"), size=45, family="Marcellus SC", color="#33382B", lineheight=.7, alpha=.8)+
scale_fill_manual(values = pal)+
guides(fill = guide_legend(ncol=2, byrow=TRUE, label.position="right"))+
labs(caption =  "<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Nanum Gothic\"'> #30DayMapChallenge </span></b><b><span style='font-family:\"Nanum Gothic\"'>Source:</b> Bliss, N. 2013. LBA-ECO LC-08 Soil, Vegetation, and Land Cover Maps for Brazil and South America. ORNL DAAC, Oak Ridge, Tennessee, USA. <b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Nanum Gothic\"'> samiaab1990</span>")+
theme(
      plot.margin = margin(0, 330, 0, 0, unit = "mm"),
      plot.background = element_rect(fill=background_color, color=NA),
      panel.background = element_rect(fill=background_color, color=NA),
      panel.border = element_blank(),
      plot.caption = element_textbox(size=20,color="#33382B", hjust=.5, margin=margin(l=310, unit="mm")),
      legend.title = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      legend.position = c(1,.5),
      legend.margin = margin(t=160, l=150, unit="mm"),
      legend.box.just = "center",
      legend.text = element_text(family="Nanum Gothic", size=24, color="#33382B", margin=margin(r=0,unit="mm")),
      legend.key = element_blank(),
      legend.spacing.y = unit(2,"cm"),
      legend.spacing.x = unit(.7,"cm"),
      legend.background = element_blank())+
coord_fixed(xlim = c(-80,-15), ylim = c(-60,20), expand=TRUE, clip="off")

q<-ggplot()+
  geom_polygon(data = sa_shapefile_tidy, aes(x=long, y=lat, group=group), color=NA, fill="#33382B")+
  geom_polygon(data = sa_shapefile_tidy, aes(x=long, y=lat, group=group), color=background_color, fill=NA, size=1)+
  geom_segment(aes(x=-32, xend=-32, y=-55, yend=10), size=1, color='#33382B')+
  geom_segment(aes(x=-32, xend=-40, y=-55, yend=-55), size=1, color="#33382B")+
  geom_segment(aes(x=-32, xend=-40, y=10, yend=10), size=1, color="#33382B")+
  geom_text(aes(x=-8, y=((-55+10)/2), label = "Basemap"), color="#33382B", size=8, family="Nanum Gothic")+
  theme(
    plot.background = element_rect(fill=background_color, color=NA),
    panel.background = element_rect(fill=background_color, color=NA),
    panel.border = element_blank(),
    legend.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank())+
  coord_fixed(clip="off")

# convert legend to grob and put in plot
q_sub<- ggplotGrob(q)

legend_plot <- grobTree(q_sub, vp=viewport(width=unit(5.5,"in"), x=0.6, y=0.6,
                                           height=unit(5.5,"in")))
# draw main plot and small plot
r<-ggdraw() +
  draw_plot(p) +
  draw_plot(legend_plot, x = .4, y = .5, width = .3, height = .3)


# comment out-for testing only
#ggsave(plot = inset, filename = "legend.png", width = 5, height = 5, units="in", dpi=300, ragg::agg_png, bg = background_color)
ggsave(plot = r, filename = "sa_land_cover.png", width=30, height=20, units="in", dpi=300, device=ragg::agg_png, bg=background_color)
