## Production Allocation for Scenario  Higher Free Global Trade
## PBH March 2023



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
# new Foreign supply quota to allocate 
fs_allocation <- 0.5
top_count <- 12
#################

for (i in 1:4){
  
  sales.share.matrix <- sales_ratio[[i]]
  sup_stats <- filter(supply.stats_seg,seg==paste0("s",i))
  sup_stats$s.ds <- diag(as.matrix(sales.share.matrix))
  
# Use shares of Domestic exports, not considering Domestic supply to allocate 
# Allocation based on LDV Exports shares
  ldv_prod <- read.csv("Inputs/LDV_export_share.csv")
  
  # Top 12 producers and their share
  ldv_prod <- ldv_prod %>% top_n(top_count) %>% mutate(s.p=perc/sum(perc)) %>% 
    mutate(country=country %>% str_replace_all(" ","."))
  ldv_prod
  sum(ldv_prod$s.p) 
  
  # Plant seeds - assign new FS supply to other countries
  prod_seeds <- data.frame(country=names(sales.share.matrix)) %>% 
    left_join(ldv_prod) %>% replace_na(list(s.p=0)) %>% pull(s.p)
  
  sales.share.matrix <- as.matrix(sales.share.matrix)
  
  # add new seeds and reduce current FS proportionally
  s.fs <- 1-sup_stats$s.ds # Share of imports for in country sales 
  
  # prod seeds per country FS share (cross-product)
  prod_seeds_c <- prod_seeds %*% t(s.fs)
  
  diag(sales.share.matrix) <- 0
  sales.share.matrix <- sales.share.matrix*(1-fs_allocation)
  sales.share.matrix <- sales.share.matrix+prod_seeds_c*fs_allocation
  
  # remove diagonal and add to DS
  sales.share.matrix <- as.matrix(sales.share.matrix)
  sup_stats$s.ds <- sup_stats$s.ds+diag(sales.share.matrix)
  diag(sales.share.matrix) <- 0
  sales.share.matrix <- as.data.frame(sales.share.matrix)
  
  colSums(sales.share.matrix)+sup_stats$s.ds # add 1
  
  rm(s.fs,ldv_prod,prod_seeds_c,prod_seeds)

  # save results
  sales.share.matrix <- as.matrix(sales.share.matrix)
  diag(sales.share.matrix) <- sup_stats$s.ds # store diagonal in maitrx
  sales_ratio[[i]] <- sales.share.matrix
  
  # save new share of Domestic supply for algorithm of future production
  sup_stats <- sup_stats %>% dplyr::select(c,seg,s.ds) %>% rename(new_sds=s.ds)
  supply.stats_seg <- supply.stats_seg %>% left_join(sup_stats) %>% 
    mutate(s.ds=if_else(seg==paste0("s",i),new_sds,s.ds)) %>%  # change only current segment
    dplyr::select(-new_sds)
  
}


# save results to load later
url_save <- "Inputs/Trade Ratios Scenarios/Global Free Trade/%s.csv"
write.csv(sales_ratio[[1]],sprintf(url_save,"sales_share_s1"),row.names = F)
write.csv(sales_ratio[[2]],sprintf(url_save,"sales_share_s2"),row.names = F)
write.csv(sales_ratio[[3]],sprintf(url_save,"sales_share_s3"),row.names = F)
write.csv(sales_ratio[[4]],sprintf(url_save,"sales_share_s4"),row.names = F)
write.csv(supply.stats_seg,sprintf(url_save,"supplystats_seg"),row.names = F)


# Figure to check
# need to pass the trade ratios matrices to a fromto table format
fromto <- f.tensor.fromto(sales_ratio)
f.fig.tradeRatiosSeg(fromto)
ggsave("Figures/Trade Ratios Scenarios/FreeTrade.png", 
       dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)


# EoF