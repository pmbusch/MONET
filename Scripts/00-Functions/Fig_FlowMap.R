# World Map of Flows (battery or EV) amon countries
# PBH March 2023

# Source: https://personal.tcu.edu/kylewalker/interactive-flow-visualization-in-r.html
library(geosphere)
library(leaflet)

# Define a function to select the first half of rows from a data frame
select_half_rows <- function(df,firstHalf=T) {
  n_row <- nrow(df)
  if (n_row<3){
    df
  }
  else{
    n_mid <- floor(n_row/2)+1
    n_mid2 <- n_mid
    
    # check if LINE changes from hemisphere:
    # criteria: contains a point above long 160 and below -160
    max_lon <- max(df[,1]);min_lon <- min(df[,1]);
    
    if(max_lon>160 & min_lon < -160){
      n_mid <- which.max(df[,1]*sign(df[1,1]))
      n_mid2 <- n_mid+1
    }
    
    if (firstHalf==T){
      df[1:n_mid,]
    }
    else {
      df[n_mid2:n_row,]
    }}
}


f.fig.flowMap <- function(data.supply,ev=T){
  
  scale_factor <- 1e2
  lab <- " EV"
  
  # EV or battery capacity
  if(ev==F){
    data.supply$qty <- data.supply$mWh
    scale_factor <- 1e3
    lab <- " EV Battery MWh"
  }
  
  
  # Add coordinates to data
  df3 <- data.supply %>%
    left_join(ag, by=c("origin"="c")) %>% rename(regMonet=region) %>%
    left_join(countries_latLong,by=c("region_map"="region")) %>% rename(reg=region_map) %>%
    left_join(ag, by=c("destination"="c")) %>% rename(regMonet2=region) %>%
    left_join(countries_latLong,by=c("region_map"="region")) %>%
    filter(!is.na(lat.x)) %>%
    filter(!is.na(lat.y)) %>%
    arrange(desc(qty))
  
  # Create flows between countries
  flows <- gcIntermediate(dplyr::select(df3,long.x,lat.x),
                          dplyr::select(df3,long.y,lat.y),
                          sp = TRUE, addStartEnd = TRUE)
  
  flows$qty <- df3$qty
  flows$origin <- df3$origin
  flows$destination <- df3$destination
  
  hover <- paste0(flows$origin, " to ",
                  flows$destination, ': ',
                  as.character(round(flows$qty/1e3,0)),"K",lab)
  
  # Transformation
  flows$qty <- sqrt(flows$qty)
  
  # Split poly lines into two segments
    flows_start <- flows
  flows_end <- flows
  for (i in 1:length(flows)){
    # get inside each element manually to modify the coords
    segments <- flows@lines[[i]]@Lines[[1]]@coords
    
    # split flows
    flows_start@lines[[i]]@Lines[[1]]@coords <- select_half_rows(segments,firstHalf = T)
    flows_end@lines[[i]]@Lines[[1]]@coords <- select_half_rows(segments,firstHalf = F)
  }
  
  # Figure
  m <- leaflet() %>%
    addProviderTiles('CartoDB.Positron') %>%
    addPolylines(data = flows_start, weight = ~qty/scale_factor, label = hover,
                 group = ~origin, color = "green") %>%
    addPolylines(data = flows_end, weight = ~qty/scale_factor, label = hover,
                 group = ~origin, color = "red") %>%
    addLegend(color=c("green","red"), labels=c("Origin","Destination")) %>%
    clearBounds()
  
  return(m)
  
}



# EoF