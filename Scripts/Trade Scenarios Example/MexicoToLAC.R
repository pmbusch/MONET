## Production Allocation for Scenario  Mexico takes over South America
## PBH July 2023


# Load Baseline Trade Ratios -------
source("Scripts/00-Libraries.R", encoding = "UTF-8")

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


# Sales Share Modification - Global Free Trade -----

# DIRECT USER INPUT TO MODIFY 
#################
# allocate 50% of imports outside of south america to Mexico, except for Brazil and Argentina?
fs_allocation <- 0.5
region_destiny <- "South America"
country_orig <- "Mexico"
sh_increase <- 1.5 # relative increase in factor
#################

for (i in 1:4){
  
  sales.share.matrix <- sales_ratio[[i]]
  sup_stats <- filter(supply.stats_seg,seg==paste0("s",i))
  
  # indexes
  regs <- sup_stats %>% left_join(ag) %>% pull(region)
  index_dest <- which(regs %in% c(region_destiny))
  index_orig <- which(sup_stats$c %in% c(country_orig))
  # sup_stats$c[index_dest]
  
  # increase DS of Mexico
  sup_stats$s.ds <- diag(as.matrix(sales.share.matrix))
  ds <- sup_stats$s.ds[index_orig]
  sh_factor <- if (ds*sh_increase>0.95) 0.95/ds else sh_increase # 95% is limit
  new_ds <- ds*sh_factor
  
  sales.share.matrix <- as.matrix(sales.share.matrix)
  diag(sales.share.matrix) <- 0
  sales.share.matrix <- as.data.frame(sales.share.matrix)
  
  # update share of imports to Mex
  tot_share <- sum(sales.share.matrix[,index_orig]) #original sum of imports
  scale_factor <- (1-new_ds)/tot_share
  sales.share.matrix[,index_orig] <- sales.share.matrix[,index_orig]*scale_factor
  
  
  # for each region of destination, do loop
  for (j in index_dest){
    # tot exports outside region
    # j=4 # Chile
    tot_exports <- sum(sales.share.matrix[-index_dest,j])*fs_allocation
    
    # reduce
    sales.share.matrix[-index_dest,j] <- sales.share.matrix[-index_dest,j]*(1-fs_allocation)
    # add to from country of origin (Mexico) to that country
    sales.share.matrix[index_orig,j] <- tot_exports+sales.share.matrix[index_orig,j]
  }
  
  # save new share of Domestic supply for algorithm of future production
  sup_stats$s.ds[index_orig] <- new_ds 
  colSums(sales.share.matrix)+sup_stats$s.ds # add 1
  
  # store diagonal
  sales.share.matrix <- as.matrix(sales.share.matrix)
  diag(sales.share.matrix) <- sup_stats$s.ds
  sales.share.matrix <- as.data.frame(sales.share.matrix)
  
  sup_stats <- sup_stats %>% dplyr::select(c,seg,s.ds) %>% rename(new_sds=s.ds)
  supply.stats_seg <- supply.stats_seg %>% left_join(sup_stats) %>% 
    mutate(s.ds=if_else(seg==paste0("s",i),new_sds,s.ds)) %>%  # change only current segment
    dplyr::select(-new_sds)
  
  rm(regs,index_dest,index_orig,tot_exports,ds,new_ds,sh_factor,tot_share,scale_factor)
  

  # save results
  sales.share.matrix <- as.matrix(sales.share.matrix)
  sales_ratio[[i]] <- sales.share.matrix
}

# save results to load later
url_save <- "Inputs/Trade Ratios Scenarios/Mexico to LAC/%s.csv"
write.csv(sales_ratio[[1]],sprintf(url_save,"sales_share_s1"),row.names = F)
write.csv(sales_ratio[[2]],sprintf(url_save,"sales_share_s2"),row.names = F)
write.csv(sales_ratio[[3]],sprintf(url_save,"sales_share_s3"),row.names = F)
write.csv(sales_ratio[[4]],sprintf(url_save,"sales_share_s4"),row.names = F)
write.csv(supply.stats_seg,sprintf(url_save,"supplystats_seg"),row.names = F)

# Figure to check
# need to pass the trade ratios matrices to a fromto table format
fromto <- f.tensor.fromto(sales_ratio)
f.fig.tradeRatiosSeg(fromto)
ggsave("Figures/Trade Ratios Scenarios/MexicoToLAC.png", 
       dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)


# EoF