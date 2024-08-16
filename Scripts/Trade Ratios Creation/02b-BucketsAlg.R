## Gil proposed Method: Iterative allocation algorithm based on buckets to empty and fill
# Use balanced trade flows to get total flows, then use shares of production and sales
# to iteratively allocated into each segment
# B: Bucket Algorithm
## PBH May 2023

# load data -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# part A
sales <- read.csv("Inputs/sales_segment.csv")
prod <- read.csv("Inputs/prod_segment.csv")
sales_share <- read.csv("Inputs/segment_sales_share.csv")
share_prod <- read.csv("Inputs/segment_prod_share.csv")


# Trade ratios
supply.stats <- read.csv("Inputs/Intermediate Trade Ratios/supply.stats.csv")
trade <- read.csv("Inputs/Intermediate Trade Ratios/balanced.trade.matrix.csv")
diag(trade) <- supply.stats$ds
sales.share.matrix <- read.csv("Inputs/Intermediate Trade Ratios/sales_share.csv")
colSums(sales.share.matrix)+supply.stats$s.ds
trade_ratios <- sales.share.matrix
diag(trade_ratios) <- supply.stats$s.ds
colSums(trade_ratios)

# Algorithm ------

# compare totals 
sum(trade)/1e6 # 379 M
sum(sales[,2:5])/1e6 # 369M
sum(prod[,2:5])/1e6 # 371M

colSums(trade) # sales
rowSums(trade) # prod

# mistmatch prod and sales
# a <- sales %>% pivot_longer(c(-c), names_to = "key", values_to = "value") %>% group_by(key) %>% summarise(x=sum(value))
# b <- prod %>% pivot_longer(c(-c), names_to = "key", values_to = "value") %>% group_by(key) %>% summarise(x=sum(value))

# Match total sales and prod by trimming excess in each category
# p_corr <- a$x/b$x # p correction
# s_corr <- b$x/a$x # s correction
# p_corr <- ifelse(p_corr>1,1,p_corr)
# s_corr <- ifelse(s_corr>1,1,s_corr)
# 
# stock[,2:5] <- round(stock[,2:5]*p_corr)
# target[,2:5] <- round(target[,2:5]*s_corr)


# Bucket Method
# Function gets fromto table, sales and productionby semgment to allocate
# Important: order of fromto table is used for the algorithm
f.bucket <- function(b.fromto,b.sales,b.prod,
                     b.salesShare,b.prodShare, fromtoAll,
                     showFig=F){
  
  fromto_seg <- b.fromto
  # start at zero every flow
  fromto_seg$s4 <-fromto_seg$s3 <- fromto_seg$s2 <- fromto_seg$s1 <- 0
  
  # Intermediate Variables
  target <- b.sales
  stock <- b.prod
  
  # convert to double
  target[2:5] <- lapply(target[2:5], as.numeric)
  stock[2:5] <- lapply(stock[2:5], as.numeric)
  
  
  ## for Loop 
  for (i in 1:nrow(b.fromto)){

    # Flow data
    flow <- b.fromto[i,]$qty
    flow_rem <- flow # remaining flow to allocate
    orig <- b.fromto[i,]$origin
    dest <- b.fromto[i,]$destination
    
    if(showFig){
      cat(i,": ", orig," to ",dest, "\n")
    }
    
    i_orig <- which(b.prod$c==orig)
    i_dest <- which(b.sales$c==dest)
    
    if (identical(i_orig, integer(0))) { # Country does not produce, skip it
      next
    } 
    
    if (identical(i_dest, integer(0))) { # No sales data for rest of south asia ??
      next
    } 
    
    # Sales Data
    sales_sh <- b.salesShare %>% filter(c==dest)
    # target_i <- target %>% filter(c==dest)
    
    # Prod Data
    prod_sh <- b.prodShare %>% filter(c==orig)
    # stock_i <- stock %>% filter(c==orig)
    
    # Allocate from S4 to S1 - note [[]] returns a scalar instead of a tibble
    # s=5
    # while (flow_rem>0){
    for (s in 5:2){
      missing = 0
      alloc_amount <- flow*sales_sh[[1,s]]
      
      # CHECK IF SALES ARE NEEDED
      if(alloc_amount >  target[[i_dest,s]]){
        missing_sales = as.numeric(alloc_amount - target[[i_dest,s]])
        alloc_amount = target[[i_dest,s]]
        
        # update sales share for next iteration
        rem_sales <- sum(target[i_dest,2:5])
        if (rem_sales<=0) {next} # break
        if (rem_stock<missing_sales){
          missing_sales=rem_sales
        }
        sales_sh[1,2:5] <- sales_sh[1,2:5] * ifelse(target[i_dest,2:5]>0,1,0) # dont count values with 0
        if (sum(sales_sh[1,2:5])==0) {next} # break - error condition
        sales_sh[1,2:5] <- sales_sh[1,2:5]/sum(sales_sh[1,2:5]) # rebalance
      }
      
      # CHECK AVAILABLE STOCK
      if (alloc_amount > stock[[i_orig,s]]) { # check if allocation is not possible
        missing = as.numeric(alloc_amount - stock[[i_orig,s]])
        alloc_amount = stock[[i_orig,s]]
      } 
      # update target and stock
      target[i_dest,s] =  target[i_dest,s] - alloc_amount
      stock[i_orig,s] =  stock[i_orig,s] - alloc_amount
      
      flow_rem = flow_rem - alloc_amount
      
      # Update trade flows
      fromto_seg[fromto_seg$origin==orig & fromto_seg$destination==dest,s+4] <- alloc_amount
      
      
      if (missing>0) { # allocate missing based on other proportions with available stock
        
        # check if stock is enough
        rem_stock <- sum(stock[i_orig,2:5])
        if (rem_stock<=0) {next} # break
        if (rem_stock<missing){
          missing=rem_stock
        }
        
        miss_share <- prod_sh
        miss_share[1,2:5] <- miss_share[1,2:5] * ifelse(stock[i_orig,2:5]>0,1,0)
        miss_share$c <- NULL   
        miss_share <- as.vector(miss_share) %>% unname() %>% unlist()
        miss_share <- miss_share/sum(miss_share) # rebalance
        miss_share <- miss_share*missing
        
        # update target and stock
        target[i_dest,2:5] =  ifelse(target[i_dest,2:5] - miss_share>0,
                                     target[i_dest,2:5] - miss_share,
                                     0) # border condition
        stock[i_orig,2:5] =  stock[i_orig,2:5] - miss_share
        
        # Update trade flows
        aux <- fromto_seg[fromto_seg$origin==orig & fromto_seg$destination==dest,6:9]
        fromto_seg[fromto_seg$origin==orig & fromto_seg$destination==dest,6:9] <- miss_share+aux
      } 
      
      # Next Segment
      # s = s-1
      # s = if (s==1) 5 else s
      
    }
  }
  
  # create a complete from to, adding zeros in no flow places
  fromto_aux <- fromtoAll %>% left_join(fromto_seg)
  fromto_aux <- fromto_aux %>% 
    mutate(across(c(s1, s2, s3, s4), ~ifelse(is.na(.), 0, .))) %>% 
    mutate(across(c(s1, s2, s3, s4), ~ifelse(.<0, 0, .)))
  
  
  # PERFORMANCE ANALYSIS
  # Check remaining stocks
  cat("Remaining stocks unallocated \n")
  miss_s=cbind(stock %>% pivot_longer(c(2:5), names_to = "key", values_to = "value") %>% 
          group_by(key) %>% summarise(max=max(value)/1e6,min=min(value/1e6),sum=sum(value)/1e6),
        total=apply(b.prod[,2:5],2,sum)/1e6) %>% 
    mutate(Rel_Error=sum/total*100)
  print(miss_s)
  # Check remaining targets
  cat("Targets not met \n")
  miss_t=cbind(target %>% pivot_longer(c(2:5), names_to = "key", values_to = "value") %>% 
          group_by(key) %>% summarise(max=max(value)/1e6,min=min(value)/1e6,sum=sum(value)/1e6),
        total=apply(b.sales[,2:5],2,sum)/1e6) %>% 
    mutate(Rel_Error=sum/total*100)
  print(miss_t)
  # Trade flows unfinished
  cat("Trade flows total missing \n")
  miss <- fromto_seg %>% mutate(qty_seg=s1+s2+s3+s4,
                                    diff=(qty-qty_seg)/1e6,
                                    relative_diff=diff*1e6/qty*100) %>% 
    pull(diff) %>% sum()
  cat(miss," Million vehicles \n")
  cat("In relative terms: ",miss/(sum(fromtoAll$qty)/1e6)*100,"%")
  
  
  if (showFig){
    p <- fromto_seg %>% mutate(qty_seg=s1+s2+s3+s4,
                              diff=(qty-qty_seg),diff1=diff,
                              relative_diff=diff*1e6/qty*100) %>% 
      arrange(desc(diff)) %>% head(20) %>% 
      mutate(x=paste0(origin," to ",destination)) %>%
      dplyr::select(x,s1,s2,s3,s4,diff,diff1) %>% 
      pivot_longer(c(s1,s2,s3,s4,diff), names_to = "key", values_to = "value") %>% 
      mutate(value=value/1e6) %>% 
      ggplot(aes(reorder(x,diff1),value,fill=key))+
      geom_col()+
      coord_flip()+
      labs(x="",y="Top 20 Trade unallocated flows (in millions)",fill="Segment")+
      theme(legend.position = "bottom")
    print(p)
  }
  
  
  # return list
  x <- list(fromto_seg=fromto_aux, stock=stock,target=target)
  return(x)
}

f.formatResults <- function(fromTo){
  return(fromTo %>% 
    dplyr::select(-qty) %>% 
    pivot_longer(c(s1,s2,s3,s4), names_to = "segment", values_to = "qty"))
  
}


# RUNs -----

## Largest to Smallest -----

fromto <- f.fromto.table(trade,supply.stats$ds)
fromto1 <- fromto %>% filter(qty>0) # dont allocate 0 flows
# SORT
fromto1 <- fromto1 %>% arrange(desc(qty)) %>% 
  filter(qty>10) # allocate flows at least of 10
head(fromto1)
# 121 US-Japan

res <- f.bucket(fromto1,sales,prod,sales_share,share_prod,fromto,showFig=T)
fromto_seg_res <- res$fromto_seg
target_res <- res$target
stock_res <- res$stock

fromto_aux <- f.formatResults(fromto_seg_res)

write.csv(fromto_aux,"Inputs/Intermediate Trade Ratios/fromto_seg_bucket_LtoS.csv",
          row.names = F)

# EoF