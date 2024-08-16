# Function to allocate future production by segment


# All SEGMENTS balancing

f.ProdAllocationSegment <- function(total_sales,segm_share,trade_ratios,supStats){
  
  # results storage
  ftstats <- c()
  future.supply <- c()
  
  # Loop through segments
  i=1
  for(segm in paste0("s",1:4)){
    cat("Segment ",segm,"\n",sep="")
    
    # EV sales
    sales_seg <- segm_share %>%
      filter(Segment==segm) %>% right_join(total_sales) %>% 
      mutate(sales=sales*share_s) %>% arrange(c) %>% pull(sales) 
    
    # Trade ratios
    trade_ratios_seg <- as.matrix(trade_ratios[[i]])
    diagonal <- diag(trade_ratios_seg)
    diag(trade_ratios_seg) <- 0
    trade_ratios_seg <- as.data.frame(trade_ratios_seg)
    
    colSums(trade_ratios_seg)+diagonal
    
    supStats <- filter(supply.stats_seg,seg==segm) %>% arrange(c)
    supStats$s.ds <- diagonal 
    
    # Allocate Production
    prod <- f.futureProduction(demandvec=sales_seg, # IMPORTANT: Order of countries must be the same!!! 
                               stats_supply = supStats,
                               salesRatios=trade_ratios_seg)
    fbt <- prod$trade
    ftstats_aux <- prod$stats
    fs.bt <- prod$de
    rm(prod)
    
    #Producing a Future from to 
    future.supply_aux <- f.fromto.table(fbt,ftstats_aux$ds)
    
    
    # Add battery requiements
    future.supply1 <- f.addBatterysize(future.supply_aux,
                                       fs.bt, # sales matrix of Domestic Exports
                                       ftstats_aux$ds, # domestic supply
                                       segm) 
    ftstats_aux <- f.addBatterysizeStats(ftstats_aux,future.supply1)
    
    
    # add segment
    future.supply1$seg <- segm
    ftstats_aux$seg <- segm
    
    # store
    ftstats <- rbind(ftstats,ftstats_aux)
    future.supply <- rbind(future.supply,future.supply1)
    
    i=i+1 # add to counter
    rm(ftstats_aux,future.supply_aux) #clean WS
  }
  
  return(list(ftstats=ftstats,future.supply=future.supply))
}

