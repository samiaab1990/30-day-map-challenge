library(dplyr)
library(ggplot2)
library(stringr)
library(purrr)
library(sf)
library(BBmisc)
library(grid) 
library(ggtext)

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Roboto","Roboto")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()

showtext::showtext_opts(dpi = 300)

dir<-"~/GitHub/30 day map challenge 2022/a bad map"

# get shapefiles
zip_files<-dir %>% 
  list.files(full.names=TRUE) %>%
  str_subset(".zip$")

for(i in 1:length(zip_files))
{
  file = unzip(zip_files[i], list=TRUE) %>%
  filter(str_detect(Name,".shp$")) %>%
  pull(Name) 
  
  unzip(zipfile = zip_files[i], exdir=dir)
  
  name = str_remove_all(file,".shp")
  assign(name, st_read(file), envir=.GlobalEnv)
}

centroids<-list(IRL_adm0, ISL_adm0) %>%
map(st_centroid) %>%
bind_rows() %>%
as_Spatial() %>%
as.data.frame() %>%
rename(x = coords.x1,
       y = coords.x2)

expand.grid(c("ICELAND","IRELAND"), c("x","y")) %>%
rename(a = Var1, b=Var2) %>% 
pmap(function(a,b) assign(paste0(a,b),centroids %>% filter(NAME_ISO==a) %>% pull(b), envir=.GlobalEnv))

pal<-colorRampPalette(c("#F9D423","#FF4E50"))
colors<-pal(length(3:18))


names<-tibble(a = letters[4:17], b = colors[2:(length(colors)-1)]) %>%
pmap(function(a,b) paste0("<span style='color:#FFFFFF'>I</span><span style='color:",b,"'>",a,"</span>","<span style='color:#FFFFFF'>eland</span>"))

names<-append(names,paste0("<span style='color:",colors[1],"'>Iceland</span>"), after=0)
names<-append(names,paste0("<span style='color:",colors[length(colors)],"'>Ireland</span>"),after=length(names))
names_unlist<-unlist(names)

names_path<-tibble(
names = names_unlist,
x = normalize(3:18, method="range", range=c(ICELANDx,IRELANDx)),
y = normalize(3:18, method="range", range=c(ICELANDy,IRELANDy))
)

names_path<-names_path %>%
mutate(y = case_when(
  str_detect(names,"Iceland") ~ y+.01,
  str_detect(names,"Ireland") ~ y-.01,
  TRUE ~ y
))

#thanks https://stackoverflow.com/questions/30136725/plot-background-colour-in-gradient
g <- rasterGrob(c("#FF4E50","#F9D423"), width=unit(1,"npc"), height = unit(1,"npc"), 
                interpolate = TRUE) 


p<-ggplot()+
annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf,ymax=Inf)+
geom_sf(data=IRL_adm0, color=NA, fill="white")+
geom_sf(data=ISL_adm0, color=NA,fill="white")+
geom_point(data = centroids, aes(x=x,y=y))+
geom_segment(data = centroids, aes(x=ICELANDx,y=ICELANDy,xend=IRELANDx,yend=IRELANDy), linetype="dotted",size=.8, color="black")+
geom_richtext(data = names_path,aes(x=x,y=y,label=names), fill="black", size=5, family="Roboto")+
geom_textbox(aes(x=-23,y=61,label=paste0("Distance Between <b>",names[1],"</b> and <b>", names[length(names)],"</b>")), size=9,lineheight=.8, halign=.5, width=unit(.5,"npc"), box.color=NA, fill=NA, color="white",family="Roboto")+
geom_textbox(aes(x=-22.9,y=60,label="The distance between Iceland and Ireland is <b>14 letters</b>."), width=unit(.4,"npc"), size=5, fill=NA, box.color=NA,family="Roboto")+
geom_textbox(aes(x=-24.3,y=52,label="<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Roboto\"'> #30DayMapChallenge </span></b><br><b><span style='font-family:\"Roboto\"'>Source:</b> @UndeniablyAlex via Twitter <br>found on @TerribleMaps and Reddit<br><b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Roboto\"'> samiaab1990</span>"), color="black", size=2.5, width=unit(.35,"npc"), fill=NA, box.color=NA)+
theme(
  plot.background = element_blank(),
  panel.background = element_blank(),
  panel.grid=element_blank(),
  axis.text=element_blank(),
  axis.title=element_blank(),
  axis.ticks=element_blank())+coord_sf(clip="off")

ggsave(plot = p, filename = "a_bad_map_test.png", width=14, height=10, units="in", dpi=300, device=ragg::agg_png, bg="transparent")
