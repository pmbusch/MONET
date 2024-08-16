# MONET Example - Default Inputs and Demand Scenarios
# High and Low Demand Scenarios Example
# PBH August 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load default Inputs --------

## Sales
EV_sales <- read.csv("Inputs/ICCT_MONET_2035.csv") %>% 
  mutate(c=str_replace_all(c,"\\/"," ")) %>% arrange(c)
EV_sales <- EV_sales %>% filter(Scenario=="Ambitious")

## Market Segments
sales_share <- read.csv("Inputs/segment_sales_share - MONET.csv")
names(sales_share)[2:5] <- paste0("s",1:4)
sales_share <- sales_share %>% pivot_longer(c(-c), names_to = "Segment", values_to = "share_s") %>% 
  mutate(c=str_replace_all(c,"\\/"," "))


## Battery size - already loaded through functions
head(batsize)

## Trade Ratios
supply.stats_seg <- read.csv("Inputs/supplystats_seg.csv")
supply.stats_seg <- supply.stats_seg %>% arrange(c)
# fix iso
ag_aux <- ag %>% mutate(c=c %>% str_replace_all("\\/"," "))
supply.stats_seg <- supply.stats_seg %>% dplyr::select(-iso) %>% 
  left_join(dplyr::select(ag_aux,c,iso))
rm(ag_aux)

sales_ratio <- c()
sales_ratio[[1]] <- read.csv("Inputs/sales_share_s1.csv")
sales_ratio[[2]] <- read.csv("Inputs/sales_share_s2.csv")
sales_ratio[[3]] <- read.csv("Inputs/sales_share_s3.csv")
sales_ratio[[4]] <- read.csv("Inputs/sales_share_s4.csv")



# Scenario High Demand in Global South
# DIRECT USER INPUT TO MODIFY 
#################
region_change <- tibble(
  MONET_Region=c("Middle East/Africa","South America","South Asia","Europe",
                 "North America","Greater China","Japan/Korea"),
  EV_high=c(0.3,0.3,0.1,0.1,0.1,0.1,0.1), # relative change wrt defaul
  EV_low=c(0,0,0,-0.2,-0.2,-0.2,-0.2))
#################

sales_high <- EV_sales %>% left_join(region_change) %>% 
  mutate(sales=sales*(1+EV_high))

sales_low <- EV_sales %>% left_join(region_change) %>% 
  mutate(sales=sales*(1+EV_low))


# Production Allocation using trade ratios
result <- f.ProdAllocationSegment(sales_high,sales_share,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply

# Save data
url_save <- "Results/Demand Scenarios/High/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)


# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/HighDemand_EV.png")
# MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/HighDemand_MWh.png")

# Low scenario --------
# Production Allocation using trade ratios
result <- f.ProdAllocationSegment(sales_low,sales_share,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply

# Save data
url_save <- "Results/Demand Scenarios/Low/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)


# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/LowDemand_EV.png")
# MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/LowDemand_MWh.png")

# EoF