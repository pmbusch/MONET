# Calculate battery requirements based only on the demand, for comparison
# PBH July 2023

# Future EV Sales ---------

## Sales
EV_sales <- read.csv("Inputs/ICCT_MONET_2035.csv") %>% 
  mutate(c=str_replace_all(c,"\\/"," ")) %>% arrange(c)
EV_sales <- EV_sales %>% filter(Scenario=="Ambitious")

## Market Segments
sales_share <- read.csv("Inputs/segment_sales_share - MONET.csv")
names(sales_share)[2:5] <- paste0("s",1:4)
sales_share <- sales_share %>% pivot_longer(c(-c), names_to = "segment", values_to = "share_s") %>% 
  mutate(c=str_replace_all(c,"\\/"," "))



sales <- EV_sales %>% left_join(sales_share) %>% 
  mutate(sales=sales*share_s)


# Battery Requirements ------
batsize <- read.csv("Inputs/batsize.csv") %>% rename(region=Region)
batsize <- batsize %>% rename(segment=Monet_Segment) %>% 
  mutate(segment=str_replace(segment,"S","s")) 
ag <- read.csv("Inputs/ag.csv") 
batsize <- ag %>% left_join(batsize) %>%mutate(c=str_replace_all(c,"\\/"," "))


# Combine both -----------

demand_bat <- sales %>% left_join(batsize)
demand_bat <- demand_bat %>% 
  mutate(MWh=sales*kwh_veh/1e3)



# EoF