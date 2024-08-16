# Proportional Allocation Method
# Based on proportional allocation principle
## PBH May 2023


# load data -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")


# part A
sales <- read.csv("Inputs/sales_segment.csv")
prod <- read.csv("Inputs/prod_segment.csv")
sales_share <- read.csv("Inputs/segment_sales_share.csv")
share_prod <- read.csv("Inputs/segment_prod_share.csv")
fromto <- read.csv("Inputs/Intermediate Trade Ratios/global.fromto.csv")

# Trade ratios
supply.stats <- read.csv("Inputs/Intermediate Trade Ratios/supply.stats.csv")
trade <- read.csv("Inputs/Intermediate Trade Ratios/balanced.trade.matrix.csv")


# Flows disag. -----
# dissagregate flows into 4 segments using proportional allocation alg.

# complete prod data
prod <- sales %>% dplyr::select(c) %>% left_join(prod) %>% 
  replace(is.na(.), 0) %>% .[,2:5] %>% as.matrix()
sales <- as.matrix(sales[,2:5])


x <- array(rep(1, 65*65*4), dim=c(65, 65, 4))
for (i in 1:4){
  (corr_factor <- sum(sales[,i])/sum(prod[,i]))
  prod[,i] <- prod[,i]*corr_factor
  
  # Proportional allocation
  x[,,i] <- (prod[,i] %*% t(sales[,i]))/sum(prod[,i])
}
sum(prod)/1e6;sum(sales)/1e6
sum(x)/1e6

# convert X to shares
fromto_seg <- c()
for (i in 1:4){
  fromto_seg_aux <- f.fromto.table2(x[,,i]) %>% mutate(seg=paste0("s",i))
  fromto_seg <- rbind(fromto_seg,fromto_seg_aux)
}
rm(i,fromto_seg_aux)

fromto_seg <- fromto_seg %>% group_by(origin,destination) %>% 
  mutate(perc=qty/sum(qty)) %>% ungroup() %>% 
  mutate(perc=if_else(is.nan(perc),0,perc),
         qty=NULL)

# Balance according to trade -------------
sum(fromto$qty)/1e6
head(fromto)
fromto$iso.x <- fromto$iso.y <- fromto$de <- fromto$batt.size <- fromto$mWh <- NULL
fromto$r.origin <- fromto$r.destination <- NULL
fromto <- fromto %>% 
  mutate(origin=origin %>% str_replace_all("\\/"," "),
         destination=destination %>% str_replace_all("\\/"," "))

fromto_seg <- fromto_seg %>% left_join(fromto, by=c("origin","destination"))
fromto_seg <- fromto_seg %>% mutate(qty=qty*perc,perc=NULL)
sum(fromto_seg$qty)/1e6

write.csv(fromto_seg,"Inputs/Intermediate Trade Ratios/fromto_seg_Prop.csv",
          row.names = F)

# EoF