library(dplyr)
library(ggplot2)
library(sf)
library(purrr)
library(cowplot)
library(ggfx)
library(gtable)
library(grid)
library(ragg)
library(ggtext)

# title font
sysfonts::font_add_google("Playfair Display","Playfair Display", bold.wt=700)


# font for the subtitle, caption, etc.
sysfonts::font_add_google("Ubuntu","Ubuntu")

# font awesome for caption
sysfonts::font_add(family = "Font Awesome 5 Brands Regular", regular= "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Brands-Regular-400.otf")
sysfonts::font_add(family = "Font Awesome 5 Free Solid", regular = "~/GitHub/30 day map challenge 2022/fonts/Font Awesome 5 Free-Solid-900.otf")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 300)

# data
broadband_health = read.csv("c2hgis_county.csv")
broadband_health_states = read.csv("c2hgis_state.csv")

broadband_health_summary<-broadband_health %>%
dplyr::select(geography_id, geography_desc, geom, centroid, pctpopwbbacc, pcp_per_capita)

median_broadband<- broadband_health_summary %>% 
                   summarise(median_broadband_access = median(pctpopwbbacc, na.rm=TRUE)) %>%
                   pull()

median_pcp<- broadband_health_summary %>%
             summarise(median_pcp_per_capita = median(pcp_per_capita, na.rm=TRUE)) %>%
             pull()

tertiles_broadband_access<-quantile(broadband_health_summary$pctpopwbbacc, na.rm=TRUE, probs = seq(0,1,(1/3)))
tertiles_pcp_capita<-quantile(broadband_health_summary$pcp_per_capita, na.rm=TRUE, probs = seq(0,1,(1/3)))

# specify background color for NA value
background_color = "#CDCDCD"

broadband_health_summary_levels<-broadband_health_summary %>%
mutate( 
  levels_broadband_access = case_when(
    pctpopwbbacc >= tertiles_broadband_access[1] & pctpopwbbacc <= tertiles_broadband_access[2] ~ "LOW",
    pctpopwbbacc > tertiles_broadband_access[2] & pctpopwbbacc <= tertiles_broadband_access[3] ~ "MED",
    pctpopwbbacc > tertiles_broadband_access[3] & pctpopwbbacc <= tertiles_broadband_access[4] ~ "HIGH",
    TRUE ~ NA_character_
  ),
  
  levels_pcp_access = case_when(
    pcp_per_capita >= tertiles_pcp_capita[1] & pcp_per_capita <= tertiles_pcp_capita[2] ~ "LOW",
    pcp_per_capita > tertiles_pcp_capita[2] & pcp_per_capita <= tertiles_pcp_capita[3] ~ "MED",
    pcp_per_capita > tertiles_pcp_capita[3] & pcp_per_capita <= tertiles_pcp_capita[4] ~ "HIGH",
    TRUE ~ NA_character_
  ),
  
  levels_total = case_when(
    levels_broadband_access == "LOW" & levels_pcp_access == "LOW" ~ 1,
    levels_broadband_access ==  "LOW" & levels_pcp_access == "MED" ~ 2,
    levels_broadband_access == "LOW"  & levels_pcp_access == "HIGH" ~ 3,
    levels_broadband_access == "MED" & levels_pcp_access == "LOW" ~ 4,
    levels_broadband_access == "MED" & levels_pcp_access == "MED" ~ 5,
    levels_broadband_access == "MED" & levels_pcp_access == "HIGH" ~ 6,
    levels_broadband_access == "HIGH" & levels_pcp_access == "LOW" ~ 7,
    levels_broadband_access == "HIGH"  & levels_pcp_access == "MED" ~ 8,
    levels_broadband_access == "HIGH" & levels_pcp_access == "HIGH" ~ 9
  ),
  
  color_pal = case_when(
    levels_total == 1 ~ "#fff979",
    levels_total == 2 ~ "#c7fa85",
    levels_total == 3 ~ "#08fcad",
    levels_total == 4 ~ "#f3a28b",
    levels_total == 5 ~ "#bea399",
    levels_total == 6 ~ "#08a4c7",
    levels_total == 7 ~ "#dd00ad",
    levels_total == 8 ~ "#ac00bd",
    levels_total == 9 ~ "#0700f6",
    TRUE ~ background_color 
  )
  )


broadband_map<-broadband_health_summary_levels %>% 
      dplyr::select(-c(centroid)) %>%
      sf::st_as_sf(wkt = "geom") %>%
      st_set_crs(4326) %>% 
      shift_geometry()

states_boundary<-broadband_health_states %>%
                 dplyr::select(-c(centroid)) %>%
                 sf::st_as_sf(wkt = "geom") %>%
                 st_set_crs(4326) %>%
                 shift_geometry()

tile_grid<-expand.grid(x=c("LOW","MED","HIGH"), y=c("LOW","MED","HIGH")) %>%
as.data.frame() %>%
mutate(levels = case_when(
  y == "LOW" & x == "LOW" ~ 1,
  y ==  "LOW" & x == "MED" ~ 2,
  y == "LOW"  & x == "HIGH" ~ 3,
  y == "MED" & x == "LOW" ~ 4,
  y == "MED" & x == "MED" ~ 5,
  y == "MED" & x == "HIGH" ~ 6,
  y == "HIGH" & x == "LOW" ~ 7,
  y == "HIGH"  & x == "MED" ~ 8,
  y == "HIGH" & x == "HIGH" ~ 9
),
color_pal = case_when(
  levels == 1 ~ "#fff979",
  levels == 2 ~ "#c7fa85",
  levels == 3 ~ "#08fcad",
  levels == 4 ~ "#f3a28b",
  levels == 5 ~ "#bea399",
  levels == 6 ~ "#08a4c7",
  levels == 7 ~ "#dd00ad",
  levels == 8 ~ "#ac00bd",
  levels == 9 ~ "#0700f6"
))

# For subtitle 
low_counts_pcp_internet<-broadband_health_summary_levels %>%
filter(levels_broadband_access == "LOW", levels_pcp_access == "LOW") %>% 
summarise(counts = n()) %>%
pull()

low_counts_pcp<-broadband_health_summary_levels %>% 
filter(levels_pcp_access == "LOW") %>%
summarise(counts = n()) %>%
pull()

comma_sep<-scales::label_comma()

#Footnote - did not include in w/o footnote version
#footnote = "Data on broadband access and primary care physicians per capita comes from The Federal Communications Commission and Robert Wood Johnson Foundation County \nHealth Rankings & Roadmap respectively. Broadband access was expressed as a percentage of population with broadband access and PCPs per capita were expressed as the \nnumber of primary care physicians per population for each county.'Low', 'medium' and 'high' levels were determined by whether the statistic for each county fell in \nthe lower, middle or upper third of all counties nationwide."

# main plot
p<-ggplot()+
geom_sf(data = broadband_map, aes(fill = color_pal), color="#5F5F5F")+
with_shadow(geom_sf(data = states_boundary, color="#E2E2E2", size=1, fill=NA), colour="#222222", sigma=20)+
scale_fill_identity()+
labs(title = "Broadband Internet Access and \nPrimary Care Availability",
     subtitle = paste0("As telehealth and mHealth are being explored as prospective solutions to the primary care healthcare professional shortage in the United States, <b>", round((low_counts_pcp_internet/low_counts_pcp)*100,1),"%</b> of US counties that have lower primary care physicians per capita relative to two thirds of the country also have the lowest population with access to broadband internet. The map below looks at the percentage of population with access to broadband internet alongside the number of primary care physicians per capita for each county in 50 states and the District of Columbia."),
     caption = "<b><span style='font-size:25px;font-family:\"Font Awesome 5 Free Solid\"'>&#xf279;</span> <span style='font-size:25px;font-family:Ubuntu;'>#30DayMapChallenge</span> <span style='font-size:25px'>&#9670;</span> <span style='font-size:25px;font-family:Ubuntu;'>Source:</span><span style='font-size:25px;font-family:Ubuntu'> FCC and Robert Wood Johnson Foundation</span> <span style='font-size:25px'>&#9670;</span> <span style='font-family:Ubuntu;font-size:25px;'>Map made by: Samia B</span> <span style='font-size:30px;font-family: \"Font Awesome 5 Brands Regular\"'>&#xf09b;</span><span style='font-size:25px;font-family:Ubuntu'> samiaab1990</span></b>"
     )+
theme(
  plot.background = element_rect(fill = background_color, color=NA),
  plot.title = element_text(family="Playfair Display", size=110, color="#777777", hjust=.5, lineheight=.7, margin = margin(t=10)),
  plot.subtitle = element_textbox(width = unit(.9,"npc"), color="#777777", family = "Ubuntu",size=30, hjust=.55,  margin = margin(t=35, b=30), lineheight=1),
  plot.caption = element_textbox(color="#777777", hjust=.5, lineheight=.7, margin = margin(t=80)),
  panel.border = element_rect(color = background_color, fill=NA),
  panel.background = element_rect(fill = background_color, color=NA),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  panel.grid = element_blank()
)+
  coord_sf(clip = "off")

# legend
q<- ggplot()+
  geom_tile(data = tile_grid, aes(x=x, y=y, fill=color_pal))+
  annotate(geom = "segment", x=.3, y=.3, xend=1, yend=.3, color="#777777", size=1)+
  annotate(geom = "segment", x=.3, y=.3, xend=.3, yend=1, color="#777777", size=1)+
  annotate(geom = "segment", x=.3, y=.3, xend=.2, yend=.2, color="#777777", size=1)+
  annotate(geom = "text", x = .2-.5, y = .2-.5, label = "Low broadband access\nLow PCP per capita", angle=315, color="#555555", size=8, family="Ubuntu", lineheight=.9, fontface="bold")+
  annotate(geom = "segment", x=3, y=.3, xend=3.7, yend=.3, color="#777777", size=1)+
  annotate(geom = "segment", x=3.7, y=.3, xend=3.7, yend=1, color="#777777", size=1)+
  annotate(geom = "segment", x=3.7, y=.3, xend=3.8, yend=.2, color="#777777", size=1)+
  annotate(geom = "text", x = 3.8+2.7, y = .2-2.1, label = "Low broadband access\nHigh PCP per capita", color="#555555", size=8, family="Ubuntu", lineheight=.9, angle=315, fontface="bold")+
  annotate(geom = "segment", x = .3, y=3, xend=.3, yend=3.7, color="#777777", size=1)+
  annotate(geom = "segment", x = .3, y=3.7, xend=1, yend=3.7, color="#777777", size=1)+
  annotate(geom = "segment", x = .3, y=3.7, xend=.2, yend=3.8, color="#777777", size=1)+
  annotate(geom = "text", x = .2-2, y=3.7+2.8, label = "High broadband access\nLow PCP per capita", color="#555555", size=8, family="Ubuntu", lineheight=.9, angle=315, fontface="bold")+
  annotate(geom = "segment", x = 3, y = 3.7, xend=3.7, yend=3.7, color="#777777", size=1)+
  annotate(geom = "segment", x = 3.7, y=3.7, xend = 3.7, yend = 3, color="#777777", size=1)+
  annotate(geom = "segment", x = 3.7, y=3.7, xend=3.8, yend=3.8, color="#777777", size=1)+
  annotate(geom = "text", x = 3.8 +.7, y=3.8+.3, label = "High broadband access\nHigh PCP per capita", color="#555555", size=8, family="Ubuntu", lineheight=.9, angle=315, fontface="bold")+
  scale_fill_identity()+
  coord_equal(clip="off")+
  theme(plot.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

# convert legend to grob and put in plot
q_sub<- ggplotGrob(q)

legend_plot <- grobTree(q_sub, vp=viewport(width=unit(4,"in"), x=0.6, y=0.6,
                                  height=unit(4,"in"),
                                  angle=45))
# draw main plot and small plot
r<-ggdraw() +
  draw_plot(p) +
  draw_plot(legend_plot, x = .5, y = -.05, width = .3, height = .3)
  
# add footnote 
#r<-add_sub(plot = r, label=footnote, fontfamily="Ubuntu", size=20, color="#777777", vpadding = grid::unit(1,"lines"))
#r<-ggdraw(r) 

# comment out-for testing only
ggsave(plot = legend_plot, filename = "legend.png", width = 5, height = 5, units="in", dpi=300, ragg::agg_png, bg = background_color)
ggsave(plot = r, filename = "broadband_internet_final_with_caption_test.png", width=26, height=21, units="in", dpi=300, device=ragg::agg_png, bg=background_color)
