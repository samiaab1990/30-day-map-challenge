library(ggplot2)
library(dplyr)
library(purrr)
library(tidyr)
library(sf)
library(httr)
library(stringr)
library(ggpattern)
library(grid)
library(cowplot)
library(ggstar)



dir<-"~/GitHub/30 day map challenge 2022/food-drink"

# font for the title 
sysfonts::font_add_google("Viga","Viga")

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Titillium Web","Titillium Web")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)


shp<-dir %>%
list.files() %>%
str_subset(".shp$") 

sf_counties<-st_read(paste0(dir,"/",shp))
          
# manually collected 
bay_area_cities<-tibble(
      city = c("San Francisco", "Berkeley", "San Jose", "Oakland", "Fremont", "Santa Rosa",
          "Hayward", "Sunnyvale", "Santa Clara", "Vallejo", "Concord", "Fairfield","Richmond","Napa",
          "Antioch","San Mateo", "Daly City", "Vacaville", "San Leandro", "Livermore", "San Ramon","Redwood City",
          "Mountain View","Milipitas", "Pleasanton","Alameda","Pittsburg","Dublin","Union City","Walnut Creek","Palo Alto","South San Francisco",
          "San Rafael", "Sausalito"),
        x = c(-122.431297, -122.272781, -121.893028, -122.271111,-121.988571,-122.720306,
              -122.080795,-122.036346, -121.955238, -122.256638, -122.043686, -122.054169,
              -122.347748,  -122.286865, -121.805832, -122.313057, -122.470207,-121.987747,
              -122.156830,-121.768005, -121.973496106, -122.236115, -122.083855,-121.899574,-121.871496514,-122.241638,
              -121.884681,-121.935791,-122.02583,-122.065186,-122.143936, -122.433014,-122.534752,
              -122.498055),
        y = c(37.773972, 37.871666, 37.335480, 37.804363, 37.548271, 38.444660,37.668819, 
              37.368832, 37.354107, 38.104088, 37.989128,  38.257778,37.935757, 38.297539,38.005001,
              37.554169,37.687923,38.356579,37.725685,37.681873,37.774663568, 37.487846,
              37.386051,37.432335, 37.65749737,37.765205,38.027976,37.702152,37.58694,37.910076,37.468319,37.662937,
              37.986076,37.865894))

# turn cities into sf object 
bay_area_cities_sf<-st_as_sf(
  x = bay_area_cities,
  coords = c("x","y"),
  crs = st_crs(sf_counties)
) %>% 
st_join(sf_counties)

bay_area_cities_query<-paste0(bay_area_cities$city, ", CA")

bay_area_counties<-sf_counties$county

# Begin yelp query 
# with help from https://billpetti.github.io/2017-12-23-use-yelp-api-r-rstats/
client_id <- Sys.getenv("client_id")
api_key <- Sys.getenv("client_secret")

# next page of results - continue until ~ 150-200
offset<-c(1,51,101,151)

yelp_call<-function(region,offset)
{
yelp <- "https://api.yelp.com"
term <- "Restaurant"
location <- region 
categories <- NULL
radius <- 40000
offset <- offset
limit <-50
url <- modify_url(yelp, path = c("v3", "businesses", "search"),
                  query = list(term = term, 
                               location = location, 
                               limit = limit,
                               radius = radius,
                               offset = offset))
res <- GET(url, add_headers('Authorization' = paste("bearer", api_key)))

results <- content(res)
return(results)
}

results<-expand.grid(bay_area_cities_query, offset) %>%
as_tibble() %>%
mutate(Var1 = as.character(Var1)) %>%
rename(region = Var1,
       offset = Var2)

yelp_call_cities<-results %>%
pmap(yelp_call)

parse_fn<-function(restaurant)
{
  tibble <- tibble(
    id = restaurant$id,
    alias = restaurant$alias,
    name = restaurant$name,
    is_closed = restaurant$is_closed,
    review_count = restaurant$review_count,
    categories = restaurant$categories[[1]]$alias,
    categories_title = restaurant$categories[[1]]$title,
    rating = restaurant$rating,
    lat = restaurant$coordinates$latitude,
    long = restaurant$coordinates$longitude,
    address = restaurant$address1,
    city = restaurant$location$city,
    zip_code = restaurant$location$zip_code,
    country = restaurant$location$country,
    state = restaurant$location$state)
    
  
  return(tibble)
}


full_tibble<-yelp_call_cities %>%
map(function(x) x$businesses %>% map(parse_fn)) %>%
bind_rows()

# turn restaurants into sf object
# remove duplicates 
full_tibble_sf<-st_as_sf(
  x = full_tibble,
  coords = c("long","lat"),
  crs = st_crs(sf_counties)
) %>%
distinct(.keep_all=TRUE)

# merge with counties 
full_tibble_w_counties<-st_join(full_tibble_sf, sf_counties, left=TRUE) %>% 
mutate(rating_categ = case_when(
       rating >=4 & rating <4.5 ~ "4 Stars",
       rating >=4.5 ~ "4.5 Stars and above")) %>%
  filter(!is.na(county))


# get # of most popular restaurants by county 
sum<-full_tibble_w_counties%>%
filter(rating >=4.5 ) %>%
  group_by(county,categories_title) %>%
  summarise(counts=n()) %>%
  arrange(desc(counts)) %>%
  slice(1) %>%
  rename(most_rated = categories_title)

# data frame version 
full_tibble_w_counties_df<-full_tibble_w_counties %>%
  as_Spatial() %>%
  as_tibble() %>%
  merge(sum %>% select(-geometry))

# simplify county shapefile (lots of tiny islands)
sf_simplify<-st_simplify(sf_counties, preserveTopology = FALSE, dTolerance = 1000)

# data frame version of simplified shapefile 
sf_tidy<-sf_simplify%>%
as_Spatial() %>% 
broom::tidy(region="county") 

# create a labels dat for labels-find centroid and merge sum 
labels<-sf_tidy %>%
group_by(id) %>%
summarise(long = mean(long, na.rm=TRUE),
          lat = mean(lat, na.rm=TRUE)) %>%
merge(sum %>% select(county, most_rated), by.x="id", by.y="county") %>%
select(-geometry) %>%
mutate(labs = paste0(id,": ",most_rated))

  

 bbox_sf<-st_bbox(sf_counties) 
 
 # need diff for positioning 
 x_diff = bbox_sf[3]-bbox_sf[1]
 y_diff = bbox_sf[4]-bbox_sf[2]
 
 
 subtitle<-"Restaurants rated 4.5 stars or higher on Yelp, with the <b style='color:#FFE800'>most popular category</b> amongst 4.5 stars or higher ratings highlighted on the map and listed next to each Bay Area county. Data comes from Yelp Fusion API that returned approximately 4,500 restaurants across the Bay Area within 40,000 meters of 32 of the most populated cities."
 caption<-"<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span><span style='font-family:\"Titillium Web\"'> #30DayMapChallenge </span></b><br><b><span style='font-family:\"Titillium Web\"'>Source:</b> <span style='font-family:\"Font Awesome 5 Brands Regular\"'>&#xf1e9;</span>Yelp Fusion API<br><b>Map made by:</b> Samia B</span> <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-family:\"Titillium Web\"'> samiaab1990</span>"
 
 # create manual legend 
 breaks<-with(full_tibble_w_counties_df %>% filter(rating>=4.5), labeling::extended(range(rating, na.rm=TRUE)[1], range(rating, na.rm=TRUE)[2], m = 5))
 
 legend_df<-tibble( x = breaks,
         y = rep(1,length(breaks)))

# main plot 
p<-ggplot()+
ggpattern::geom_rect_pattern(aes(xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf), pattern="gradient", pattern_fill2="#c81d77", pattern_fill="#6710c2")+
geom_text(aes(x=9,y=12,label="Bay Area's\nFavorite Cuisines"), size=23, color="white", family="Viga", lineheight=.8)+
geom_textbox(aes(x=9,y=9.5, label=subtitle), size=8, family="Titillium Web", color="white", fill=NA, width=unit(.4,"npc"),box.color=NA, halign=.5)+
geom_textbox(aes(x=9,y=5.7), label=caption, size=4.5, color="white", fill=NA, width=unit(.3,"npc"), box.color=NA, halign=.5)+
  theme(plot.margin = margin(0,0,0,0),
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
      axis.ticks=element_blank(),
      axis.text=element_blank(),
      axis.title = element_blank())+
      coord_fixed(xlim=c(0,18), ylim=c(0,18),clip="off")

# individual county plots function
county_plots<-function(x){
  p<-ggplot()+
     geom_sf(data=sf_simplify %>% filter(county==x), linewidth=2, color="white", fill="#F52013", alpha=.03)+
     geom_star(data=full_tibble_w_counties_df %>% filter(county==x, rating>=4.5), aes(x=coords.x1, y=coords.x2, size=rating),color="white", fill="#FFE800", alpha=.1)+
    geom_star(data=full_tibble_w_counties_df %>% filter(county==x, rating>=4.5, categories_title==most_rated), aes(x=coords.x1, y=coords.x2, size=rating),color="white", fill="#FFE800", alpha=.8)+ 
    scale_size(range=c(4,8))+
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title = element_blank())+
  coord_sf(clip="off")
  
  return(print(p))
}
  
q<-bay_area_counties %>%
map(county_plots) %>%
setNames(bay_area_counties)

# need to add labels-requires individual positioning 
 q[["Solano"]]<-q[["Solano"]] + geom_text(data=labels %>% filter(id=="Solano"), aes(x=long,y=lat-.1*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["Marin"]]<-q[["Marin"]] + geom_text(data=labels %>% filter(id=="Marin"), aes(x=long,y=lat-.11*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["Sonoma"]]<-q[["Sonoma"]] + geom_text(data=labels %>% filter(id=="Sonoma"), aes(x=long-.01*x_diff,y=lat-.191*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["San Francisco"]]<-q[["San Francisco"]] + geom_text(data=labels %>% filter(id=="San Francisco"), aes(x=long,y=lat-.04*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["San Mateo"]]<-q[["San Mateo"]] + geom_text(data=labels %>% filter(id=="San Mateo"), aes(x=long,y=lat-.2*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["Napa"]]<-q[["Napa"]] + geom_text(data=labels %>% filter(id=="Napa"), aes(x=long,y=lat-.2*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["Santa Clara"]]<-q[["Santa Clara"]] + geom_text(data=labels %>% filter(id=="Santa Clara"), aes(x=long,y=lat-.2*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["Contra Costa"]]<-q[["Contra Costa"]] + geom_text(data=labels %>% filter(id=="Contra Costa"), aes(x=long,y=lat-.15*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")
 q[["Alameda"]]<-q[["Alameda"]] + geom_text(data=labels %>% filter(id=="Alameda"), aes(x=long+.1*x_diff,y=lat-.16*y_diff,label=labs), color="white", fontface="bold", size=10, family="Titillium Web")

# function to make grob for positioning on main plot
make_county_grob<-function(x){
sub<- ggplotGrob(q[[x]])

miniplot <- grobTree(sub, vp=viewport(width=unit(4.7,"in"), x=0.6, y=0.6,
                                           height=unit(4.7,"in")))
return(miniplot)
}

miniplot_grobs<-bay_area_counties %>% 
map(make_county_grob) %>%
setNames(bay_area_counties)

# star legend 
legend_star<-ggplot()+
  geom_star(data = legend_df, aes(x=x,y=y,size=x), color="white",fill="#FFE800")+
  scale_size(range=c(4,8))+
  labs(x = "Rating")+
  theme(legend.position = "none",
        plot.background = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(color="white", size=15, family="Titillium Web", face="bold"),
        axis.title.x = element_text(color="white", size=18, family="Titillium Web"),
        axis.ticks=element_blank(),
        axis.title.y = element_blank())+
  coord_fixed(clip="off")
  
legend_star_grob<-grobTree(ggplotGrob(legend_star), vp=viewport(width=unit(5,"in"), x=0.6, y=0.6,
                                       height=unit(5,"in")))
# draw main plot, small plot and legend 
r<-ggdraw() +
  draw_plot(p) +
  draw_plot(miniplot_grobs[["Sonoma"]], x = -.04, y = .7, width = .3, height = .3)+
  draw_plot(miniplot_grobs[["Marin"]], x=-.04,y=.48, width=.3, height=.3)+
  draw_plot(miniplot_grobs[["San Francisco"]], x=-.04,y=.23, width=.3, height=.3)+
  draw_plot(miniplot_grobs[["San Mateo"]],x=-.04, y=-.04, width=.3, height=.3)+
  draw_plot(miniplot_grobs[["Napa"]],x=.3,y=.7,width=.3,height=.3)+
  draw_plot(miniplot_grobs[["Santa Clara"]], x=.35, y=-.057, width=.3, height=.3)+
  draw_plot(miniplot_grobs[["Solano"]], x=.65,y=.7, width=.3, height=.3)+
  draw_plot(miniplot_grobs[["Contra Costa"]], x=.65, y=.48, width=.3, height=.3)+
  draw_plot(miniplot_grobs[["Alameda"]], x=.65, y=.25, width=.3, height=.3)+
  draw_plot(legend_star_grob, x=.32, y=.22, width=.3, height=.3)

ggsave(plot = r, filename = "food_drink.png", width=18, height=18, units="in", dpi=300, device=ragg::agg_png)

