# MONET Example Baseline - All Default Inputs
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


# Production Allocation using trade ratios ----------
result <- f.ProdAllocationSegment(EV_sales,sales_share,sales_ratio,supply.stats_seg)

ftstats <- result$ftstats
future.supply <- result$future.supply



# Save data -----
url_save <- "Results/%s.csv"
write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
rm(url_save)


## Figures ----

# North America EV
p <- f.fig.sankigenerator(future.supply,1,"North America",
                          mtr=1,colorFlow = "Segment")
p

# North america Battery MWh
p <- f.fig.sankigenerator(future.supply,1,"North America",
                          mtr=2,colorFlow = "Segment")
p

# Japan/Korea EVs
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45, # adjust position manually for exporting
                          mtr=1,colorFlow = "Segment")
p
export(p,"Figures/Sankey/Base_EV.png")


# Japan/Korea MWh
p <- f.fig.sankigenerator(future.supply,1,"Japan/Korea",
                          target_pos = 0.45,source_pos = 0.5,
                          mtr=2,colorFlow = "Segment")
p
export(p,"Figures/Sankey/Base_MWh.png")


# China EVs
p <- f.fig.sankigenerator(future.supply,1,"Greater China",
                          mtr=1,colorFlow = "Segment")
p

# Europe
p <- f.fig.sankigenerator(future.supply,2,
                          c("Germany","Spain","France","Belgium","Italy"),
                          mtr=1,colorFlow = "none")
p




# EoF