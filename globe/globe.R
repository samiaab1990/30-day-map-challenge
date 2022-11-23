library(ggplot2)
library(sf)
library(dplyr)
library(ggtext)
library(magick)
library(purrr)

# title font
sysfonts::font_add_google("Francois One","Francois One")


# font for the subtitle, caption, etc.
sysfonts::font_add_google("Roboto","Roboto")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# doing both sf and data frame version to compare 
world_map<-rworldmap::getMap()

 world_map_df<-world_map %>%
 broom::tidy() %>%
 left_join(world_map@data, by=c("id"="ADMIN")) %>%
 select(long,lat,order,hole,piece,group,id,ISO3) %>%
 rename(Code = ISO3) %>%
 mutate(Code = as.character(Code))

world_map_sf<-world_map_df %>%
  st_as_sf(coords = c("long", "lat"), crs = st_crs(world_map)) %>%
  group_by(group) %>%
  summarise(geometry = st_combine(geometry)) %>%
  st_cast("POLYGON") %>%
left_join(world_map_df)


global_health<-read.csv("global-health.csv")

global_health<-global_health %>%
rename(indicator = Indicator.Current.health.expenditure..CHE..as.percentage.of.gross.domestic.product..GDP.....) %>%
filter(Year==2019) 

 world_map_health<-world_map_df %>%
 left_join(global_health)

# joining on ISO3 (renamed to code above)
world_map_health_sf<-world_map_sf %>%
  left_join(global_health)

# with help from 
## https://egallic.fr/en/maps-with-r/ - for the coord_map() way to do this and animation
## https://gist.github.com/fzenoni/ef23faf6d1ada5e4a91c9ef23b0ba2c1 - for the sf way to do this
## https://gist.github.com/rafapereirabr/26965dd851debad32ad2e659024ba451 - for the sf way to do this
## https://stackoverflow.com/questions/70756215/plot-geodata-on-the-globe-perspective-in-r - for the sf way to do this

# create an animate map function which will rotate along long

animate_map<-function(x){
  
# Define crs string = in the function, x (long) is the increment 
crs_string <- paste0("+proj=ortho +lat_0=20 +lon_0=",x)

# background for the globe 
background <- st_point(x = c(0,0)) %>%
  st_buffer(dist = 6371000) %>%
  st_sfc(crs = crs_string) %>%
  st_make_valid()


# make health sf dataframe valid geometry only
world_map_health_sf$geometry<- world_map_health_sf$geometry %>% 
  st_make_valid()


# find polygons on world map that intersect with the circular background
world_map_health_sf_w_background<-st_intersection(world_map_health_sf,background %>% st_transform(4326) %>% st_make_valid()) %>% # select visible area only
  st_make_valid() %>% 
  st_transform(crs = crs_string) 

## IGNORE THIS-WILL COME UP LATER IN CODE-SEE ERROR HANDLING IN FOR LOOP-----------------------
# Part 2: A check of illegal geometries and skipping illegal ones
## globe contains 'illegal' points at certain longitudes
## if error occurs, use a projection that's +1 from the longitude that's throwing off error 
# check_error<-try(print(ggplot()+
#                      geom_sf(data = background, color="#282828", fill="#000C20", linewidth=.8)))
# 
# check_error2<-try(print(ggplot()+
#                           geom_sf(data=world_map_health_sf_w_background, aes(fill=indicator), color="#000C20", linewidth=.08)))
# 
# 
#   while ("try-error" %in% c(class(check_error), class(check_error2)))
#   {
#     x = x + 1
#   
#     crs_string = paste0("+proj=ortho +lat_0=20 +lon_0=",x)
#   
#     check_error<-try(print(ggplot()+
#                            geom_sf(data = background %>% st_transform(crs_string), color="#282828", fill="#000C20", linewidth=.8)))
#   
#     check_error2<-try(print(ggplot()+
#                             geom_sf(data=world_map_health_sf_w_background %>% st_transform(crs_string), aes(fill=indicator), color="#000C20", linewidth=.08)))
# }
# 
# # recreate background and health_indicator polygons-may be the same of no illegal polygons or changed if illegal polygons
# background <- st_point(x = c(0,0)) %>%
#   st_buffer(dist = 6371000) %>%
#   st_sfc(crs = crs_string) %>%
#   st_make_valid()
# 
# world_map_health_sf_w_background<-st_intersection(world_map_health_sf,background %>% st_transform(4326) %>% st_make_valid()) %>% # select visible area only
#   st_make_valid() %>% 
#   st_transform(crs = crs_string) 
# 
#-------------------------------------------------------------------------------------------------------------------------------------

# caption for ggplot2
caption = "<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Roboto\"'> #30DayMapChallenge </span></b><br><b><span style='font-family:\"Roboto\"'>Source:</b> World Health Organization via Our World in Data <br><b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Roboto\"'> samiaab1990</span>"

# ggplot2 graph
q<-ggplot()+
  geom_sf(data = background, color="#282828", fill="#000C20", linewidth=.8)+
  geom_sf(data = world_map_health_sf_w_background, aes(fill=indicator), color="#000C20", linewidth=.08)+
   #geom_sf(data=st_graticule(background, lon = seq(-180,180,by=10), lat=seq(-60,60,by=10)), color="grey", linewidth=.08)+
   # scale_y_continuous(breaks = seq(-60,60,by=10)) +
   # scale_x_continuous(breaks = seq(-180,180,by=10)) + 
   scale_fill_gradient(low="#57ebde",high="#aefb2a", breaks=c(0,5,10,15,20,25),labels=c(0,5,10,15,20,25),
                       limits=c(0,25))+
   guides(fill=ggplot2::guide_colorbar(title="Expenditure as share of GDP (%)", title.hjust=.5, direction="horizontal", barwidth=8, ticks=FALSE,  barheight=.5, title.position="top"))+
   labs(title = "Global Healthcare Expenditure 2019",
        subtitle = "Percent of national gross domestic product (GDP) used for healthcare in each country in 2019",
        caption = caption)+
   theme(legend.position = "bottom",
         legend.justification = "center",
         legend.title = element_text(size=6.5, color="#CDCDCD", family="Roboto", face="bold"),
         legend.background = element_blank(),
         legend.text = element_text(color="#CDCDCD", family="Roboto", size=6.5),
         plot.background = element_rect(fill="#000918",color ="#000918"),
         panel.background = element_rect(fill="#000918", color="#000918"),
         plot.title = element_text(color="#CDCDCD", size=20, hjust=.5, family="Francois One", margin=margin(b=0,unit="mm")),
         plot.subtitle = element_textbox(width=unit(1.1,"npc"), family="Roboto", color="#CDCDCD", size=8,  hjust=.5, margin=margin(b=5,unit="mm")),
         plot.caption = element_textbox(width=unit(.5,"npc"), color="#CDCDCD", size=4.5, hjust=.5, halign=.5),
         panel.grid = element_blank(),
         axis.ticks = element_blank(),
         axis.text = element_blank(),
         axis.title = element_blank())+
   #coord_map("ortho", orientation=c(20, x, 0))
   coord_sf(crs=crs_string)

return(q)
}

  # for loop to run animations-should generate total 361 images
  
   for(i in -180:180)
   {
     # in the function, the for loop must continue even if an error happens along the way
     # the error is due to 'illegal geometries' in the orthographic projection at certain longs
     # start with defining a skip variable and setting it to FALSE
    skip_to_next <- FALSE
    
    # creates the png device = will occur for error or not
    ragg::agg_png(filename = paste0("images/globe_png",sprintf("%03d",i),".png"), res=300, width=5, height=5, units="in", background="#000918")
     
    # try catch = error check
    # skip to next turns to TRUE if an error results 
    tryCatch(print(animate_map(i)), error=function(e) { skip_to_next <<- TRUE})
    
    dev.off()
    
    #if skip to next is true = go next
    if(skip_to_next) { next }
      
   }

   
 dir<-"~/GitHub/30 day map challenge 2022/globe"
 img_files<-paste0(dir,"/images") %>%
  list.files(full.names=TRUE) %>%
  file.info() %>%
  tibble::rownames_to_column("filename") %>%
  filter(size!=10018) %>%
  arrange(mtime) %>%
  pull(filename)

# create gif
gifski::gifski(img_files, gif_file = "globe.gif", width = 1500, height = 1500, delay = .1)

# used for testing the main plot
#ggsave(plot = q, filename = "globe.png", width=5, height=5, units="in", dpi=300, device=ragg::agg_png)