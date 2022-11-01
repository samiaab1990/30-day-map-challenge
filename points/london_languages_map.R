library(ggplot2)
library(sf)
library(rgdal)
library(dplyr)
library(broom)
library(ggfx)
library(ggtext)


# title font
sysfonts::font_add_google("Saira Condensed","Saira Condensed", regular.wt = 800, bold.wt=900)

# font for the subtitle, caption, etc.
sysfonts::font_add_google("Roboto Condensed","Roboto Condensed")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/DataViz/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/DataViz/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()


# shapefile for london using OA (finest) output
london_shapefile<-readOGR("~/GitHub/DataViz/30 day map challenge 2022/points/statistical-gis-boundaries-london/ESRI/OA_2011_London_gen_MHW.shp") %>%
spTransform(CRS("+proj=longlat +datum=WGS84"))

# london borough shapefile (for outline)
london_borough<-readOGR("~/GitHub/DataViz/30 day map challenge 2022/points/statistical-gis-boundaries-london/ESRI/London_Borough_Excluding_MHW.shp") %>%
  spTransform(CRS("+proj=longlat +datum=WGS84")) 

# condense london borough shapefile to london area total 
london_total<-raster::aggregate(london_borough)

london_total_tidy<-broom::tidy(london_total)

# languages data from UK census

languages<-readr::read_csv("~/GitHub/DataViz/30 day map challenge 2022/points/bulk (1).csv")

# language by borough, region, local authority (to select top 10 languages)
languages_borough<-readxl::read_xlsx("~/GitHub/DataViz/30 day map challenge 2022/points/main-language-spoken-borough-census.xlsx", sheet=2)

colnames(languages_borough)[1]<-"main languages"

# Find top 1- languages in London
top_languages_london<-languages_borough %>%
slice(5:ncol(.)) %>%
select(`main languages`,London) %>%
mutate(London = as.numeric(London),
       `main languages` = str_trim(str_remove_all(`main languages`, ".*:"))) %>%
arrange(desc(London)) %>%
slice(1:10) %>% 
pull(`main languages`)

# UK Census languages summary 
languages_summary<-languages %>%
pivot_longer(cols = 5:ncol(.), names_to="Language", values_to = "Population") %>%
select(-c("date","Rural Urban")) %>%
mutate(Language = str_trim(str_remove_all(Language,"^[^:]*:|; measures: Value")),
       Language = str_trim(str_remove_all(Language, ".*:")),
       pop_for_map = as.integer(Population/5)) %>%
filter(Language %in% top_languages_london) 

# Languages shapefile (of OA)- merge with london shapefile 
languages_shapefile<-sp::merge(london_shapefile, languages_summary, by.x="OA11CD", by.y="geography code",  duplicateGeoms = TRUE)

# Split by language 
languages_split <- languages_shapefile %>% split(.$Language)

# Use dotsInPolys function on each split SpatialPolygonsDataFrame to generate dots 
points<-map(languages_split, function(x) maptools::dotsInPolys(x, x$pop_for_map, f = "random")) 

# Convert SpatialPolygonsDataFrame to data frame and bind rows 
points_on_map<-points %>%
  map_df(as.data.frame, .id="Language") %>%
  bind_rows() 

# Palette generated from https://medialab.github.io/iwanthue/ for distinct colors 
pal<-c("#ca7b3e",
       "#9771da",
       "#cdb037",
       "#6d93d7",
       "#80ad49",
       "#c971bd",
       "#64b170",
       "#d95a8b",
       "#42c8b4",
       "#d46a62")

# create plot 
ragg::agg_png(filename = "languages.png", width=25, height=24.5, units="in", res=300)               
language_map<-ggplot()+ 
  with_outer_glow(geom_polygon(data = london_total_tidy, aes(x=long,y=lat), fill="#141414", color="#B2B2B2"), colour="#ECECEC", sigma=20)+
  geom_point(data = points_on_map, aes(x = x, y=y, color=Language), size=.02)+
  scale_color_manual(values = pal)+
  labs(title = "Languages of London",
       subtitle = "Top 10 main languages spoken by persons aged 3 and above in London, United Kingdom, after English. Each dot on the map represents <b>5 people</b> per <b>London Output Area</b>, the smallest census geographic unit.",
       caption= "<b><span style='font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span> #30DayMapChallenge</b><br><b>Source:</b> 2011 Census Population and Household Estimates for England and Wales, Office of National Statistics<br><b>Map made by:</b> Samia B <span style='font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span> samiaab1990")+
 guides(color = guide_legend(nrow=1,override.aes = list(size=12, colour="#B2B2B2", pch=21, fill=pal)))+
 theme(
 plot.title = element_text(size=500, family="Saira Condensed", hjust=.5, color="#ECECEC", margin = margin(t=20, unit="pt")),
 plot.subtitle = element_textbox(size=110, family="Roboto Condensed", width = unit(.68,"npc"), hjust=.5, color="#ECECEC", lineheight=.2, margin = margin(t=10, unit="pt")),
 plot.caption = element_markdown(size=70, family="Roboto Condensed", hjust=.5, color="#ECECEC", lineheight=.3, margin=margin(t=30, unit="pt")),
 plot.background = element_rect(fill="#191919"),
 legend.title = element_blank(),
 legend.background = element_blank(),
 legend.position = "bottom",
 legend.text = element_markdown(size=90, color="#B2B2B2", family="Roboto Condensed", lineheight=.3),
 legend.margin = margin(b=50, unit="pt"),
 legend.spacing.x = unit(.2, units="cm"),
 legend.box.background = element_blank(),
 legend.key=element_blank(),
 panel.background = element_blank(),
 panel.grid = element_blank(),
 axis.text = element_blank(),
 axis.title = element_blank(),
 axis.ticks = element_blank()
 )

language_map
dev.off()

