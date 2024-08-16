## Production Allocation for Scenario Higher DOMESTIC SUPPLY
## PBH March 2023
# Create trade scenarios increasing domestic supply (diagonal)

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


# Sales Share Modification - Increase Domestic Supply -----

# DIRECT USER INPUT TO MODIFY 
#################
#  Relative increase of DS
inc_ds <- 0.4
max_ds <- 0.95 # max increase
#################

for (i in 1:4){

  sales.share.matrix <- as.matrix(sales_ratio[[i]])
  sup_stats <- filter(supply.stats_seg,seg==paste0("s",i))
  ds <- sup_stats$s.ds
  
  diag(sales.share.matrix) <- 0
  
  # Note: need to allocate correctly both of these matrix
  colSums(sales.share.matrix)+ds
  
  max_ds <- ifelse(max_ds>ds,max_ds,ds) # allow max at current limit
  new_ds <-ds*(1+inc_ds) 
  new_ds <- ifelse(new_ds>max_ds,max_ds,new_ds) # limit DS to allowed limite
  
  # get total increase in DS
  increase_ds <- new_ds- ds
  
  # Allocate increase in DS by reducing share of foreign supply proportionally
  # 1. We need to scale the Foreign Supply to 100%
  x <- sweep(sales.share.matrix,2,colSums(sales.share.matrix),"/")
  # replace NaN to zero for countries with 100% DS
  x[] <- apply(x, 2, function(x) replace(x, is.nan(x), 0))
  
  colSums(x) # now share is 100%
  
  # 2. Now we can assign the increase of DS proportionally
  y <- sweep(x,2,increase_ds,"*")
  
  # 3. Substract to each country the proportional decrease in FS
  sales.share.matrix <- sales.share.matrix-y
  colSums(sales.share.matrix)+new_ds # add 1

  
  
  # save results
  diag(sales.share.matrix) <- new_ds
  sales_ratio[[i]] <- sales.share.matrix
  
  # save new share of Domestic supply for algorithm of future production
  sup_stats$s.ds <- new_ds 
  sup_stats <- sup_stats %>% dplyr::select(c,seg,s.ds) %>% rename(new_sds=s.ds)
  supply.stats_seg <- supply.stats_seg %>% left_join(sup_stats) %>% 
    mutate(s.ds=if_else(seg==paste0("s",i),new_sds,s.ds)) %>%  # change only current segment
    dplyr::select(-new_sds)
  
  

  rm(sup_stats,new_ds,ds,sales.share.matrix)
}

# save results to load later
url_save <- "Inputs/Trade Ratios Scenarios/Higher Domestic Supply/%s.csv"
write.csv(sales_ratio[[1]],sprintf(url_save,"sales_share_s1"),row.names = F)
write.csv(sales_ratio[[2]],sprintf(url_save,"sales_share_s2"),row.names = F)
write.csv(sales_ratio[[3]],sprintf(url_save,"sales_share_s3"),row.names = F)
write.csv(sales_ratio[[4]],sprintf(url_save,"sales_share_s4"),row.names = F)
write.csv(supply.stats_seg,sprintf(url_save,"supplystats_seg"),row.names = F)


# Figure to check
# need to pass the trade ratios matrices to a fromto table format
fromto <- f.tensor.fromto(sales_ratio)
f.fig.tradeRatiosSeg(fromto)
ggsave("Figures/Trade Ratios Scenarios/HighDS.png", 
       dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)


# EoF