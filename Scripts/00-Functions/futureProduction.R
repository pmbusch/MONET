# FORECASTING FUNCTION - Create production based on sales vector and trade ratios
# FP 2023


# Function input:
# - demandvec: Vector of future sales for each country
# -stast_supply: Data.frame with stats for each country: s,p,e,i,...
# -salesRatio: Matrix in ratios format coming from trade balanced matrix
f.futureProduction <- function(demandvec, stats_supply, salesRatios){
  
  #Calculating 2050 ds given s
  ftstats<-data.frame(c=stats_supply$c,
    # iso=stats_supply$iso,
                      s=demandvec)
  
  # ftstats <- ftstats %>% f.addCountries() # add c
  
  #1
  ftstats$ds<-demandvec*stats_supply$s.ds       
  
  #2 multiply matrix above by the future sales to get future FS, then apply 
  # below code to get total imports and then calculate re-exports.
  fs.bt<-sweep(salesRatios, 2, demandvec, "*")
  
  #Calculating P by adding Domestic Supply + Domestic Exports: p=ds+de
  ftstats$de<-rowSums(fs.bt)
  ftstats$p<- ftstats$ds+ftstats$de     #p=ds+de
  # ftstats$fs<-as.vector(colSums(fs.bt))
  
  #  Calculating I and E to balance
  ftstats$i<- as.vector(colSums(sweep(fs.bt, 2, stats_supply$fs.i, "*")))
  # ftstats$re<-as.vector(colSums(sweep(fs.bt, 2, supply.stats$fs.i, "*")-fs.bt))
  ftstats$e<- ftstats$p+ftstats$i-ftstats$s 
  
  ftstats$e <- ifelse(ftstats$e<0,0,ftstats$e)
  
  #Producing the unified future matrix
  fbt<-as.data.frame(sweep(fs.bt, 2, stats_supply$fs.i, "*"))
  fbt<-f.balanceMatrix(fbt,ftstats$e,ftstats$i)
  
  for(i in 1:nrow(ftstats)){
    if (ftstats$p[i]>0 && ftstats$p[i]>ftstats$e[i]) {
      ftstats$ds[i]=ftstats$p[i]-ftstats$e[i]
      ftstats$re[i]=0
      ftstats$fs[i]=ftstats$i[i]
      ftstats$de[i]=ftstats$e[i]
    } else {
      ftstats$ds[i]=ftstats$p[i]*(ftstats$s[i]/(ftstats$p[i]+ftstats$i[i]))
      ftstats$re[i]=ftstats$i[i]*(ftstats$e[i]/(ftstats$p[i]+ftstats$i[i]))
      ftstats$fs[i]=ftstats$i[i]*(ftstats$s[i]/(ftstats$p[i]+ftstats$i[i]))
      ftstats$de[i]=ftstats$p[i]*(ftstats$e[i]/(ftstats$p[i]+ftstats$i[i]))
    }
  }  
  
  # return list
  fut_Prod <- list(stats=ftstats, trade=fbt,de=fs.bt)
  return(fut_Prod)
}
