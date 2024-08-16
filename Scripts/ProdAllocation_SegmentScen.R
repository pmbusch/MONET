# MONET Example - Default Inputs and Market segment Scenarios
# Market Segments Scenarios Example
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



# High Market segment 3 and 4 Scenarios --------

# DIRECT USER INPUT TO MODIFY 
#################
# Half of share of S1 goes to S3, and half of S2 to S3
sales_share_s3_s4 <- sales_share %>% 
  pivot_wider(names_from = Segment, values_from = share_s) %>% 
  mutate(s3=s3+s1/2,
         s1=s1/2,
         s4=s4+s2/2,
         s2=s2/2) %>% 
  pivot_longer(c(-c), names_to = "Segment", values_to = "share_s")
#################

# Production Allocation using trade ratios
result <- f.ProdAllocationSegment(EV_sales,sales_share_s3_s4,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply

# Save data
url_save <- "Results/MarketSegment Scenarios/s3s4/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)

# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.43,source_pos = 0.5,
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/Segment34_EV.png")
# MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/Segment34_MWh.png")


# High Market segment 1 and 2 Scenarios --------

# DIRECT USER INPUT TO MODIFY 
#################
# Half of share of S3 goes to S1, and half of S4 to S2
sales_share_s1_s2 <- sales_share %>% 
  pivot_wider(names_from = Segment, values_from = share_s) %>% 
  mutate(s1=s1+s3/2,
         s3=s3/2,
         s2=s2+s4/2,
         s4=s4/2) %>% 
  pivot_longer(c(-c), names_to = "Segment", values_to = "share_s")
#################

# Production Allocation using trade ratios
result <- f.ProdAllocationSegment(EV_sales,sales_share_s1_s2,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply

# Save data
url_save <- "Results/MarketSegment Scenarios/s1s2/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)


# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.43,source_pos = 0.5,
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/Segment12_EV.png")
# MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/Segment12_MWh.png")


# EoF