## Production Allocation for Higher DOMESTIC SUPPLY with USMCA and EU Free trade
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



# Sales Share Modification - Increase Domestic Supply -----


# Let's increase Domestic Supply Share by 20% ------
# DIRECT USER INPUT TO MODIFY 
#################
inc_ds <- 0.4
max_ds <- 0.95
#################

for (i in 1:4){
  
  sales.share.matrix <- as.matrix(sales_ratio[[i]])
  sup_stats <- filter(supply.stats_seg,seg==paste0("s",i))
  
  ds <- diag(sales.share.matrix)
  diag(sales.share.matrix) <- 0
  
  # increase DS for every country 
  max_ds <- ifelse(max_ds>ds,max_ds,ds) # allow max at current limit
  new_ds <-ds*(1+inc_ds) 
  new_ds <- ifelse(new_ds>max_ds,max_ds,new_ds) # limit DS to allowed limit
  
  # get total increase in DS
  increase_ds <- new_ds- ds
  
  # Allocate Increase in DS among regional partners
  
  ## USMCA ----
  # Get index for UMSCA
  index_usmca <- which(sup_stats$c %in% c("United States","Mexico","Canada"))
  
  # Get current shares of Foreign supply from Mex-CA to US 
  usmca_matrix <- sales.share.matrix[index_usmca,index_usmca]
  diag(usmca_matrix) <- ds[index_usmca]
  usmca_matrix    
  
  # Get total share by each country by relative between Foreign Supply and Domestic Supply
  colSums_matrix <- ifelse(colSums(usmca_matrix)==0,1,colSums(usmca_matrix)) # avoid dividing by zero
  usmca_share <- sweep(usmca_matrix,2,colSums_matrix,"/")
    # Distribute this new increase in Mexico and Canada and US
  usmca_share # Column sums to one, represents share of increase in DS that each country gets
  
  # get increase in USMCA
  inc_usmca <- increase_ds[index_usmca]
  
  # Allocate increase in each country by the regions
  # Key: calculate increase but add after re-balance
  new_shares_usmca <- usmca_matrix+sweep(usmca_share,2,inc_usmca,"*")
  
  
  # Check new total supply, if bigger than 100 need to be reduced
  only_usmca <- colSums(new_shares_usmca)>1
  if(any(only_usmca)){ # re-scale to 100% if this sum bigger than 100
    scale_usmca <-colSums(new_shares_usmca)
    scale_usmca <- ifelse(scale_usmca>1,scale_usmca,1)
    new_shares_usmca <- sweep(new_shares_usmca,2,scale_usmca,"/")
    index_usmca_above <- index_usmca[only_usmca] # need to consider that UMSCA becomes the only source of cars
    rm(scale_usmca)
  }
  
  # don't reduce FS of countries in the region during the balance process, keep as before
  sales.share.matrix[index_usmca,index_usmca] <- 0

  
  ## EU ------
  # Get index
  index_eu <- which(sup_stats$c %in% 
                      c("Austria","Belgium","Czechia","Denmark","Finland","France",
                        "Germany","Greece","Hungary","Ireland","Italy","Netherlands",
                        "Poland","Portugal",
                        # "Rest of EU", # not really included in the EU
                        "Romania","Spain","Sweden"))
  
  # Get current shares of Foreign supply from Mex-CA to US 
  eu_matrix <- sales.share.matrix[index_eu,index_eu]
  diag(eu_matrix) <- ds[index_eu]
  # eu_matrix    
  
  # Get total share by each country by relative between Foreign Supply and Domestic Supply
  colSums_matrix <- ifelse(colSums(eu_matrix)==0,1,colSums(eu_matrix)) # avoid dividing by zero
  eu_share <- sweep(eu_matrix,2,colSums_matrix,"/")
  # Distribute this new increase in Mexico and Canada and US
  # eu_share # Column sums to one, represents share of increase in DS that each country gets
  # colSums(eu_share)
  
  # get increase
  inc_eu <- increase_ds[index_eu]
  
  # Allocate increase in each country by the regions
  # Key: calculate increase but add after re-balance
  new_shares_eu <- eu_matrix+sweep(eu_share,2,inc_eu,"*")
  
  
  # Check new total supply, if bigger than 100 need to be reduced
  only_eu <- colSums(new_shares_eu)>1
  if(any(only_eu)){ # re-scale to 100% if this sum bigger than 100
    scale_eu <-colSums(new_shares_eu)
    scale_eu <- ifelse(scale_eu>1,scale_eu,1)
    new_shares_eu <- sweep(new_shares_eu,2,scale_eu,"/")
    index_eu_above <- index_eu[only_eu] # need to consider that UMSCA becomes the only source of cars
    rm(scale_eu)
  }
  
  # don't reduce FS of countries in the region during the balance process, keep as before
  sales.share.matrix[index_eu,index_eu] <- 0
  
  # SAME AS IncreaseDomesticSupply.R -----
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
  
  # correct negatives for special case of UMSCA=100%
  if(any(only_usmca)){
    # Replace negative values with zeros 
    sales.share.matrix[,index_usmca_above][ sales.share.matrix[,index_usmca_above]<0] <- 0
  }
  
  if(any(only_eu)){
    # Replace negative values with zeros 
    sales.share.matrix[,index_eu_above][ sales.share.matrix[,index_eu_above]<0] <- 0
  }
  
  
  # add FS of UMSCA and EU 
  sales.share.matrix[index_usmca,index_usmca] <- new_shares_usmca
  diag(sales.share.matrix) <- 0
  new_ds[index_usmca] <- diag(as.matrix(new_shares_usmca))
  # EU
  sales.share.matrix[index_eu,index_eu] <- new_shares_eu
  diag(sales.share.matrix) <- 0
  new_ds[index_eu] <- diag(as.matrix(new_shares_eu))
  
  colSums(sales.share.matrix)+new_ds # add 1
  
  # new shares of USMCA
  usmca_matrix # old
  sales.share.matrix[index_usmca,index_usmca] # new
  new_ds[index_usmca]
  
  # no negative sales ratios?
  apply(sales.share.matrix,2,min)
  
  rm(new_shares_usmca, index_usmca, only_usmca,usmca_matrix,usmca_share,
     new_shares_eu, index_eu, only_eu,eu_matrix,eu_share)
  
  # save results
  diag(sales.share.matrix) <- new_ds # store diagonal in maitrx
  sales_ratio[[i]] <- sales.share.matrix
  
  # save new share of Domestic supply for algorithm of future production
  sup_stats$s.ds <- new_ds 
  sup_stats <- sup_stats %>% dplyr::select(c,seg,s.ds) %>% rename(new_sds=s.ds)
  supply.stats_seg <- supply.stats_seg %>% left_join(sup_stats) %>% 
    mutate(s.ds=if_else(seg==paste0("s",i),new_sds,s.ds)) %>%  # change only current segment
    dplyr::select(-new_sds)
  
}


# save results to load later
url_save <- "Inputs/Trade Ratios Scenarios/Region Higher Domestic Supply/%s.csv"
write.csv(sales_ratio[[1]],sprintf(url_save,"sales_share_s1"),row.names = F)
write.csv(sales_ratio[[2]],sprintf(url_save,"sales_share_s2"),row.names = F)
write.csv(sales_ratio[[3]],sprintf(url_save,"sales_share_s3"),row.names = F)
write.csv(sales_ratio[[4]],sprintf(url_save,"sales_share_s4"),row.names = F)
write.csv(supply.stats_seg,sprintf(url_save,"supplystats_seg"),row.names = F)


# Figure to check
# need to pass the trade ratios matrices to a fromto table format
fromto <- f.tensor.fromto(sales_ratio)
f.fig.tradeRatiosSeg(fromto)
ggsave("Figures/Trade Ratios Scenarios/HighDSRegion.png", 
       dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)


# EoF