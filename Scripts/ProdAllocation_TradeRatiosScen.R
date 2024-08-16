# MONET Example - Default Inputs and Trade Ratios Scenarios
# Higher Regional Domestic Supply and Global Free Trade Scenario
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


# Higher Regional Domestic Supply ---------------

## Trade Ratios
url_read <- "Inputs/Trade Ratios Scenarios/Region Higher Domestic Supply/%s.csv"
supply.stats_seg <- read.csv(sprintf(url_read,"supplystats_seg"))
supply.stats_seg <- supply.stats_seg %>% arrange(c)
# fix iso
ag_aux <- ag %>% mutate(c=c %>% str_replace_all("\\/"," "))
supply.stats_seg <- supply.stats_seg %>% dplyr::select(-iso) %>% 
  left_join(dplyr::select(ag_aux,c,iso))
rm(ag_aux)

sales_ratio <- c()
sales_ratio[[1]] <- read.csv(sprintf(url_read,"sales_share_s1"))
sales_ratio[[2]] <- read.csv(sprintf(url_read,"sales_share_s2"))
sales_ratio[[3]] <- read.csv(sprintf(url_read,"sales_share_s3"))
sales_ratio[[4]] <- read.csv(sprintf(url_read,"sales_share_s4"))

# Production Allocation using trade ratios
result <- f.ProdAllocationSegment(EV_sales,sales_share,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply

# Save data
url_save <- "Results/Trade Ratios Scenarios/Regional Higher Domestic Supply/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)

# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/DSTradeRatios_EV.png")
# MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/DSTradeRatios_MWh.png")

# Global Free Trade ---------------

## Trade Ratios
url_read <- "Inputs/Trade Ratios Scenarios/Global Free Trade/%s.csv"
supply.stats_seg <- read.csv(sprintf(url_read,"supplystats_seg"))
supply.stats_seg <- supply.stats_seg %>% arrange(c)
# fix iso
ag_aux <- ag %>% mutate(c=c %>% str_replace_all("\\/"," "))
supply.stats_seg <- supply.stats_seg %>% dplyr::select(-iso) %>% 
  left_join(dplyr::select(ag_aux,c,iso))
rm(ag_aux)

sales_ratio <- c()
sales_ratio[[1]] <- read.csv(sprintf(url_read,"sales_share_s1"))
sales_ratio[[2]] <- read.csv(sprintf(url_read,"sales_share_s2"))
sales_ratio[[3]] <- read.csv(sprintf(url_read,"sales_share_s3"))
sales_ratio[[4]] <- read.csv(sprintf(url_read,"sales_share_s4"))

# Production Allocation using trade ratios
result <- f.ProdAllocationSegment(EV_sales,sales_share,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply

# Save data
url_save <- "Results/Trade Ratios Scenarios/Global Free Trade/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)

# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/FreeTrade_EV.png")
# MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/FreeTrade_MWh.png")

# EoF