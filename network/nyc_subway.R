library(ggplot2)
library(dplyr)
library(sf)
library(rmapshaper)
library(tidyr)
library(stringr)
library(ggforce)
library(broom)
library(ggbump)
library(ggtext)

# not used 
# title font
#sysfonts::font_add_google("Archivo","Archivo", bold.wt=700)


# font for the subtitle, caption, etc.
sysfonts::font_add_google("Yanone Kaffeesatz","Yanone Kaffeesatz")
sysfonts::font_add_google("Nunito Sans","Nunito Sans")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

wd = "~/GitHub/30 day map challenge 2022/network/"

# nyc colors http://web.mta.info/developers/developer-data-terms.html#data
nyc_subway_colors <- read.csv(paste0(wd,"colors.txt"))
nyc_subway_colors_cleaned <- nyc_subway_colors[4:13,] %>%
                     select(X, X.1) %>%
                     rename(name = X,
                            hex = X.1) %>%
                     mutate(hex = paste0("#", hex)) %>%
                     separate_rows(name) 

# unzip- only do once
unzip(paste0(wd,"Borough Boundaries.zip"), exdir = paste(wd,"borough shapefiles"))
unzip(paste0(wd,"Subway Lines.zip"), exdir = paste(wd,"subway lines shapefiles"))
unzip(paste0(wd,"Subway Stations.zip"), exdir = paste(wd,"subway stations shapefiles"))

# unziping each dir in respective directory and extracting shapefiles
borough_shp_dir = list.dirs(wd) %>%
  str_subset("borough shapefiles")

subway_shp_dir = list.dirs(wd) %>%
  str_subset("subway lines")

stations_shp_dir = list.dirs(wd) %>%
  str_subset("subway stations")

borough_shapefile = list.files(borough_shp_dir, full.names=TRUE) %>%
str_subset(".shp$")

subway_shapefile = list.files(subway_shp_dir, full.names=TRUE) %>%
  str_subset(".shp$")

stations_shapefile = list.files(stations_shp_dir, full.names=TRUE) %>%
  str_subset(".shp$")

# read shapefiles 
nyc_boroughs<-st_read(borough_shapefile) %>%
  st_transform("+proj=longlat +datum=WGS84")

# For the boroughs shapefile
# Simplify shape
# Find centroid (for labels)

nyc_boroughs_simplified<-nyc_boroughs %>%
  ms_simplify(keep=.002, keep_shapes=TRUE)  %>%
  as_Spatial() %>%
  tidy()

nyc_boroughs_centroids<-nyc_boroughs %>%
  st_centroid() %>% 
  as_Spatial() %>%
  as.data.frame() %>%
  rename (x = coords.x1,
          y = coords.x2)

# For subway lines shapefile
# Analyze subway length
subway_lines_shapefile <- st_read(subway_shapefile) %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  left_join(nyc_subway_colors_cleaned) 


 subway_lines_sum<-subway_lines_shapefile %>%
   separate_rows(name) %>%
   group_by(name, hex) %>%
   summarise(shape_len = sum(shape_len)) %>%
   ungroup() %>%
   mutate(pct = round((shape_len/sum(shape_len))*100,2)) %>%
   as.data.frame() %>%
   select(-geometry)
  

# For subway stations shapefile
# Analyze No. of stations
subway_stations_shapefile<- st_read(stations_shapefile)%>%
  st_transform("+proj=longlat +datum=WGS84") 

subway_stops_summary<-subway_stations_shapefile %>%
  mutate(line_name = str_remove_all(line,"-[:digit:] Express")) %>%
  separate_rows(line_name) %>%
  left_join(nyc_subway_colors_cleaned, by = c("line_name"="name")) %>% 
  mutate(hex = ifelse(line_name=="W",nyc_subway_colors_cleaned %>% filter(name=="N") %>% pull(hex),hex)) %>% 
  group_by(line_name, hex) %>%
  summarise(counts = n()) %>%
  as_tibble() %>%
  select(-geometry) %>%
  ungroup() 


# For ranking 
subway_endpoint<-subway_lines_shapefile %>%
  separate_rows(name) %>% 
  group_by(name) %>%
  st_cast("POINT") 

subway_lines_point_coords<-subway_endpoint %>% 
                              st_coordinates() %>%
                              as_tibble() %>% 
                              mutate(name = subway_endpoint$name) %>%
                              merge(subway_lines_sum) %>%
                              group_by(name) %>%
                              filter(X == max(X, na.rm=TRUE)) %>%
                              distinct(.keep_all=TRUE)

bbox_nyc<-st_bbox(nyc_boroughs)


rank_df = tibble(
  yend = normalize(rank(subway_lines_sum$pct), method = "range", range = c(bbox_nyc[2]+.07, bbox_nyc[4]-.08)),
  xaxis_start = bbox_nyc[3]+.40,
  name = subway_stations_point_coords$name,
  pct = subway_lines_sum$pct,
  color = subway_stations_point_coords$hex
) %>% 
merge(subway_lines_point_coords) 

# to move letters side by side if tie occurs 
# with help from https://stackoverflow.com/questions/43196718/increment-by-one-to-each-duplicate-value

rank_df<-rank_df %>%
group_by(yend) %>%
mutate(letter_start = xaxis_start + (row_number()-1)*.01)

# repeat same for 2nd ranking 
rank_df2 = tibble(
  x = rank_df$xaxis_start+.01,
  y = rank_df$yend,
  yend = normalize(rank(subway_stations_count$counts), method = "range", range = c(bbox_nyc[2]+.07, bbox_nyc[4]-.08)),
  xend = rank_df$xaxis_start +.015,
  counts = subway_stations_count$counts,
  name = subway_stations_count$line_name,
  color = rank_df$color)

rank_df2<-rank_df2 %>%
  group_by(yend) %>%
  mutate(letter_start = x + (row_number()-1)*.01)

# create box in the background to reduce extension of first ranking 
box = tibble(
  xmin = first(rank_df$xaxis_start),
  xmax = first(rank_df$xaxis_start) + .02,
  ymin = bbox_nyc[2]+.07,
  ymax = bbox_nyc[4]-.08
)

# not used - coloring title by subway line 
# title = "SUBWAY LINES OF NYC"
# make_title =""
# 
# for(i in 1:nchar(title))
# {
#   if(substr(title,i,i) %in% rank_df$name)
#   {
#     make_title = paste0(make_title, "<span style='color:", 
#            rank_df %>% filter(name == substr(title,i,i)) %>% pull(color),"'>", substr(title,i,i),"</span>")
#     
#   } else if(substr(title,i,i)==" ")
#   {
#     make_title = paste0(make_title, " ")
#   }
#   else
#   {
#     make_title = paste0(make_title, "<span style='color:#FFFFFF'>",substr(title,i,i),"</span>")
#   }
#   
# }

p<-ggplot()+
geom_shape(data = nyc_boroughs_simplified,  aes(x=long, y=lat, group=group), color='#FFFFFF', fill='#BEBEBE', radius=unit(.2,'cm'), size=1)+
geom_text(data = nyc_boroughs_centroids %>% filter(boro_name == "Queens"), aes(x = x, y = y+.03, label=boro_name), size=18, fontface="bold", color="#FFFFFF", alpha=.6, family="Yanone Kaffeesatz")+
geom_text(data = nyc_boroughs_centroids %>% filter(boro_name == "Brooklyn"), aes(x = x, y = y-.005, label=boro_name), size=18, alpha=.6, fontface="bold", color="#FFFFFF", family="Yanone Kaffeesatz")+
geom_text(data = nyc_boroughs_centroids %>% filter(boro_name == "Bronx"), aes(x = x, y = y-.005, label=boro_name), size=18, alpha=.6, fontface="bold", color="#FFFFFF", family="Yanone Kaffeesatz")+
geom_text(data = nyc_boroughs_centroids %>% filter(boro_name == "Manhattan"), aes(x = x-.07, y=y+.018, label=boro_name), size=10, fontface="bold", color="#FFFFFF", family="Yanone Kaffeesatz", alpha=.6)+
geom_segment(aes(x=-73.97, y=40.78, xend=-74, yend=40.79), linetype="dotted", color="#FFFFFF")+
geom_text(data = nyc_boroughs_centroids %>% filter(boro_name == "Staten Island"), aes(x = x+.12, y=y-.02, label=boro_name), size=10, fontface="bold", color="#FFFFFF", family="Yanone Kaffeesatz", alpha=.6)+
geom_segment(aes(x=-74.08, y=40.56, xend=-74.1, yend=40.56), linetype="dotted", color="#FFFFFF")+
geom_sf(data = subway_lines_shapefile, size=6, color="#FFFFFF", alpha=.4)+
geom_sf(data = subway_lines_shapefile, aes(color=hex), size=2)+
geom_sf(data = subway_stations_shapefile, color="black", shape=21, fill="white", size=2)+
with_outer_glow(geom_sigmoid(data = rank_df, 
  aes(x = X+.0015, y = Y, xend = xaxis_start, yend = yend, group = name, color = color), 
               alpha = .4, smooth=15, size = 1), colour="white", sigma=10)+
geom_rect(data = box, aes(xmin = xmin-.15, xmax = xmax+.03, ymin=ymin, ymax=ymax), fill="#135568")+
with_outer_glow(geom_sigmoid(data = rank_df2, 
                               aes(x = x-.16, y = y, xend = xend, yend = yend, group = name, color = color), 
                               alpha = .4, smooth=15, size = 1), colour="white", sigma=10)+
geom_shadowtext(data = rank_df, aes(x=letter_start-.128-.025, y=yend, label=name, bg.colour=color), color="#FFFFFF", size=5)+
geom_shadowtext(data = rank_df2, aes(x=letter_start+.01, y=yend, label=name, bg.colour=color), color="#FFFFFF", size=5)+
scale_color_identity()+
geom_text(aes(x = first(rank_df$letter_start-.128-.025), y=first(rank_df$yend+.20), label = "Ranking of Line\nBy Length*"), color="#FFFFFF", size=5, family="Nunito Sans", fontface="bold")+
geom_text(aes(x = first(rank_df$letter_start-.128-.025), y=first(rank_df$yend-.1), label = "*Length was determined by calculating\nshape length provided in the city\n subway lines open dataset"), color="#FFFFFF", size=2.5, fontface="bold", family="Nunito Sans")+
geom_text(aes(x = first(rank_df2$letter_start+.01), y=first(rank_df$yend+.20), label = "Ranking of Line\nBy No. of Stations"), color="#FFFFFF", size=5, family="Nunito Sans", fontface="bold")+
geom_text(aes(x = first(rank_df$letter_start-.126), y=first(rank_df$yend+.26), label = "SUBWAY LINES OF NYC"), size=22, family="Yanone Kaffeesatz", fontface="bold", color="#FFFFFF")+
geom_richtext(
  aes(x = first(rank_df$letter_start-.128-.215), y=first(rank_df$yend-.109), 
  label = "<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Nunito Sans\"'> #30DayMapChallenge </span></b><br><b><span style='font-family:\"Nunito Sans\"'>Source:</b> NYC Open Data/Metropolitan Transit Authority (MTA)<br><b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Nunito Sans\"'> samiaab1990</span>"), 
  color="#FFFFFF", size=2, fill=NA, label.color=NA)+
theme(
  plot.margin = margin(r=260, unit="mm"),
  plot.background = element_rect(fill="#135568",color="#135568"),
  panel.background = element_rect(fill="#135568", color="#135568"),
  panel.border = element_blank(),
  panel.grid = element_blank(),
  axis.text = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank()
)+
coord_sf(xlim=c(-74.1, bbox_nyc[3]), ylim=c(40.57,bbox_nyc[4]), clip="off")

ggsave(plot = p, filename = "nyc_subway.png", width=20, height=12, units="in", dpi=200, device=ragg::agg_png, bg = "#135568")


