# World Map to show production
# PBH March 2023



# World Map using ggplot
# https://www.riinu.me/2022/02/world-map-ggplot2/
library(ggthemes)

world_map = map_data("world") %>% 
  filter(! long > 180)

countries = world_map %>% 
  distinct(region) %>% 
  rowid_to_column()

countries_latLong <- world_map %>%
  filter(is.na(subregion)|region=="Japan") %>%
  group_by(region) %>%
  summarise(long=mean(long,na.rm=T),
            lat=mean(lat,na.rm=T)) %>% ungroup()

f.fig.map <- function(stats_df, includeChina=T,
                      ev=T,lab="Light Duty EV",yt=2035,
                      prod=T,
                      scenario=""){
  
  unit_label <- paste0(lab," Production ",yt)
  
  if (prod==F){
    unit_label <- paste0(lab," Sales ",yt)
    stats_df$p <- stats_df$s # sales instead
  }
  
  
  
  if(ev==F){
    stats_df$p <- stats_df$MWh # Show Battery capacity instead
    unit_label <- paste0(lab," Battery Production ",yt," [MWh]")
  }
  
  
  stats_df %>% 
    left_join(ag) %>% rename(regMonet=region) %>% 
    filter(c!="China"|includeChina) %>% 
    full_join(countries,by=c("region_map"="region")) %>% 
    ggplot(aes(fill = p, map_id = region_map)) +
    geom_map(map = world_map, col="black") +
    expand_limits(x = world_map$long, y = world_map$lat) +
    scale_fill_distiller(direction = 1, trans="sqrt",
                         labels=function(x) format(x, big.mark = " ", scientific = FALSE))+
    labs(fill=unit_label,
         title = scenario)+
    # coord_map("moll") +
    theme_map(22)
 
}

# Map with MONET Regions ----

# ag %>% rename(regMonet=region) %>% 
#   full_join(countries,by=c("region_map"="region")) %>% 
#   mutate(regMonet=factor(regMonet,levels=level_region)) %>% 
#   ggplot(aes(fill=as.factor(regMonet),map_id = region_map)) +
#   geom_map(map = world_map, col="black") +
#   expand_limits(x = world_map$long, y = world_map$lat) +
#   labs(title = "MONET 65 Countries/Regions",fill="")+
#   scale_fill_viridis_d(direction = -1,na.value = "lightgrey",
#                        labels =c(level_region,'Countries aggregated \n into "Rest of Region ..."'))+
#   # coord_map("moll") +
#   theme_map(22)
# 
# f.fig.save("Figures/Map_MONET.png")

