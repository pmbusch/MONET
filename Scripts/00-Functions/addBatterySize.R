# Function to add battery size
# PBH and FP March 2023


batsize <- read.csv("Inputs/batsize.csv") %>% rename(region=Region)
batsize <- batsize %>% rename(segment=Monet_Segment) %>% 
  mutate(segment=str_replace(segment,"S","s"))

ag <- read.csv("Inputs/ag.csv") 
batsize_region <- batsize
batsize <- ag %>% left_join(batsize)


f.addBatterysize <- function(fromto, sales,ds, segm){
  
  #Adding domestic exports (de= to calculate the bat-cap requirements)
  diag(sales)<-ds
  de<-melt(sales)
  # fromto$destination==de$variable # equal
  fromto$de<-de$value
  

  # calculate bat size
  fromto <- fromto %>% 
  left_join(filter(batsize,segment==segm), # filter by segment
            by=c("destination"="c"),
            relationship = "many-to-many") %>%  
  mutate(MWh=kwh_veh*de/1000) # kWh to MWh
  
  return(fromto)
}

# Function to add battery size requeriments to the stats data.frame
# Important: Battery requirements is based only on Domestic Exports
# It separetes into DS and DE based on country origin-dest information
f.addBatterysizeStats <- function(df_stats,fromto){
  
  # get MWh from fromto table, we want the origin of EV
  x <- fromto %>% 
    mutate(ds=if_else(origin==destination,"MWh.ds","MWh.de")) %>% #domestic supply?
    group_by(ds,origin) %>% 
    summarise(MWh=sum(MWh,na.rm=T)) %>% ungroup() %>% 
    pivot_wider(names_from = ds, values_from = MWh) %>% 
    mutate(MWh=MWh.ds+MWh.de)
  
  # fix strings to join only
  df_stats <- df_stats %>% 
    mutate(origin=c %>% str_replace_all("\\/"," "))
  
  df_stats <- left_join(df_stats,x)
  df_stats$origin <- NULL
  
  return(df_stats)
  
}
