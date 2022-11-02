library(ggplot2)
library(sf)
library(dplyr)
library(cowplot)
library(geomtextpath)
library(emojifont)
library(ggtext)


# title font
sysfonts::font_add_google("Teko","Teko")


# font for the subtitle, caption, etc.
sysfonts::font_add_google("Encode Sans Condensed","Encode Sans Condensed")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/DataViz/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/DataViz/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


## shapefiles - used for testing and guidance in creating nl_line dataframe
nl_shapefile<-st_read("~/GitHub/DataViz/30 day map challenge 2022/lines/Netherlands/NLD_adm1.shp") 
nl_shapefile_agg<-st_combine(nl_shapefile) %>% st_make_valid() %>% st_union()

## this is for dividing by regions - ignore if not using 
nl_bike_shapefile<-nl_shapefile %>% st_make_valid() %>% st_join(netherlands_bike) %>% as.data.frame()
nl_bike_dat<-as.data.frame(netherlands_bike)

nl_bike_shapefile_sf<-nl_bike_shapefile %>% select(-geometry) %>% merge(nl_bike_dat) %>% st_as_sf()
nl_bike_shapefile_sf_split<-nl_bike_shapefile_sf %>% split(.$NAME_1)

nl_line<-data.frame(
  x = c( 3.1,4, 4.4, 4.6, 4.9, 6),
  y = c( 51.5,52, 52.25, 53.05, 53.55, 53.65)
)

## create color gradient 
pal<-colorRampPalette(c("#2B005C","#1CB5E0"))

title<-"Cycle Paths of Netherlands"

title_no_space<-str_remove_all(title," ")

pal_n<-pal(nchar(title)+1)

make_title<-""

for(i in 1:nchar(title))
{
  j = i 
  if(substr(title,i,i) !=" ")
  {
    make_title<-paste0(make_title,"<span style='font-family:Teko;color:",pal_n[j],";'>",substr(title,i,i),"</span>")
  } else
  {
    make_title<-paste0(make_title," ")
    j = i - 1
  }
  
}

## commented out-alternative version of map
# pal_shapefile<-pal(12)
# 
# pal_shapefile_light<-pal_shapefile %>%
# map(function(x) colorRampPalette(colors = c(x,"#FFFFFF"))(10)[3]) %>%
# unlist()

map_color<-colorRampPalette(colors=c(pal_n[as.integer(nchar(title)/2)],"#FFFFFF"))(10)[2]

p<-ggplot()+
geom_sf(data = netherlands_bike, color=map_color, alpha=.8, size=1)+
#geom_sf(data=nl_shapefile_agg, fill=pal_n[as.integer(length(pal_n)/2)], alpha=.9)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Zeeland"]], color=pal_shapefile_light[1], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Noord-Brabant"]], color=pal_shapefile_light[2], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Limburg"]], color=pal_shapefile_light[3], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Zuid-Holland"]], color=pal_shapefile_light[4], alpha=.8,size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Utrecht"]], color=pal_shapefile_light[5],alpha=.8,size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Gelderland"]], color=pal_shapefile_light[6], alpha=.8,size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Noord-Holland"]], color=pal_shapefile_light[7], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Flevoland"]], color=pal_shapefile_light[8], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Overijssel"]], color=pal_shapefile_light[9], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["IJsselmeer"]], color=pal_shapefile_light[10], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Friesland"]], color=pal_shapefile_light[10], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Drenthe"]], color=pal_shapefile_light[11], alpha=.8, size=1)+
# geom_sf(data = nl_bike_shapefile_sf_split[["Groningen"]], color=pal_shapefile_light[12], alpha=.8, size=1)+
geomtextpath::geom_textpath(data = nl_line, aes(x=x-.3, y=y), label=make_title, family = "Teko", size=55, linecolor=NA, show.legend=FALSE,  text_smoothing=50, vjust=.5, hjust=-.5, rich=TRUE)+
geom_richtext(data = nl_line, aes(x=nl_line[nrow(nl_line),"x"]+.9,y=nl_line[nrow(nl_line),"y"]+.05), label="<span style='font-family:\"Font Awesome 5 Free Solid\"'> &#xf206; </span>",family='Font Awesome 5 Free Solid', size=55, fill=NA, label.color=NA, color=pal_n[length(pal_n)])+
labs(
  caption= "<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span> #30DayMapChallenge</b><br><b>Source:</b> ©OpenStreetMap contributors via GeoFabrik <br><b>Map made by:</b> Samia B <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990")+
theme(
  panel.border = element_blank(),
  plot.caption = element_markdown(size=25, family="Encode Sans Condensed", hjust=.5, color=pal_n[as.integer(nchar(title)/2)], lineheight=.3, margin=margin(t=20, unit="pt")),
  panel.background = element_blank(),
  plot.background = element_rect(fill="#FFFFFF", color=NA),
  panel.grid = element_blank(),
  axis.ticks =element_blank(),
  axis.text= element_blank(),
  axis.title = element_blank()
)+
coord_sf(xlim=c(2.5,8.1), ylim=c(50.6,54), expand=FALSE)

save_plot("cycleway_test222.png", plot=p, dpi=300, base_width=28, base_height=28, units="in")
