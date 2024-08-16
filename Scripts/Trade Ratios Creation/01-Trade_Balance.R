## Create ratios matrix based on Trade balanced Matrix
## PBH and FP March 2023
# Balance the 2-D trade flows using production data

# load libraries ----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data-----


# PROPIETARY DATA - NOT INCLUDED FOR PUBLIC USE
load("lastbaldat.RData")


ag <- read.csv("Inputs/ag.csv") # restore ag 

# trade <- smooth.list[[7]] # 2019
# 2016-2019
trade4 <- smooth.list[[4]]
trade1 <- smooth.list[[5]]
trade2 <- smooth.list[[6]]
trade3 <- smooth.list[[7]]
trade <- trade1+trade2+trade3+trade4
rm(trade1,trade2,trade3,trade4)


# Balance Method -----

y <- c(2016,2017,2018,2019) # 4 years to consolidate data
# y <- 2013:2019

# Filter for the selected year
s <- supply %>%
  filter(year %in% y & type=="sales") %>%
  group_by(highlight) %>%
  summarise(s=sum(units)) %>%
  ungroup() %>% 
  rename(c=highlight) %>% 
  f.addISO()

p <- rbind(supply %>%
            filter(year %in% y & type=="prod") %>%
            group_by(highlight) %>%
            summarise(p=sum(units)),
          supply %>%
            filter(year %in% y & type=="sales") %>%
            group_by(highlight) %>%
            summarise(p=0)
) %>% 
  distinct(highlight, .keep_all = TRUE) %>%
  arrange(highlight) %>%
  rename(c=highlight) %>% 
  f.addISO()

# Creating a full supply and trade stats df 

tstats <- p %>% left_join(s) %>% 
  mutate(i=0,
         e=0,
         ds=0,
         re=0,
         fs=0,
         de=0) %>% 
  arrange(c)


# Calculate correction factor
cf <- as.numeric(sum(tstats$s)/sum(tstats$p))

# Scale production units to match sales units
tstats$p <- tstats$p* cf

# Calculate the sum of every row in the trade matrix
tstats$eTrade <- rowSums(trade)
tstats$iTrade <- colSums(trade)


# Calculate unbalance between trade exports and imports
tstats$unBalance <- tstats$p+tstats$iTrade-tstats$s-tstats$eTrade
# Make corrections based on proportion between E and I
tstats$prop <- tstats$eTrade/(tstats$eTrade+tstats$iTrade)

# calculate I and E
tstats$i <- tstats$iTrade-tstats$unBalance*(1-tstats$prop)
tstats$e <- tstats$eTrade+tstats$unBalance*tstats$prop

# negative exports are zero (case of Philippine)
tstats$i <- ifelse(tstats$e<0,abs(tstats$e),0)+tstats$i
tstats$e <- ifelse(tstats$e<0, 0,tstats$e)

# Check balance
sum(tstats$p+tstats$i) 
sum(tstats$s+tstats$e)


#USING RAS TO BALANCE -----
names(trade)==tstats$c
bt <- f.balanceMatrix(trade,tstats$e,tstats$i)

#Calculating all of the Trade Stats: tstats -----

for(i in 1:nrow(tstats)){
  # case Production positive, and P>E
  if (tstats$p[i]>0 && tstats$p[i]>tstats$e[i]) {
    tstats$ds[i]=tstats$p[i]-tstats$e[i]
    tstats$re[i]=0
    tstats$fs[i]=tstats$i[i]
    tstats$de[i]=tstats$e[i]
  } else { # Proportional allocation - Problem is here RE-EXPORTS ARE NOT COUNTED IN THE SUPPLY STATS, BUT ON THE TRADE 
    tstats$ds[i]=tstats$p[i]*(tstats$s[i]/(tstats$p[i]+tstats$i[i]))
    tstats$re[i]=tstats$i[i]*(tstats$e[i]/(tstats$p[i]+tstats$i[i]))
    tstats$fs[i]=tstats$i[i]*(tstats$s[i]/(tstats$p[i]+tstats$i[i]))
    tstats$de[i]=tstats$p[i]*(tstats$e[i]/(tstats$p[i]+tstats$i[i]))
  }
}

# Check balances
index <- 4 #Austria
index=12 # Czechia
# index=61 # US

tstats[index,]
(tstats[index,]$p+tstats[index,]$i)/1e6 # P+I
(tstats[index,]$s+tstats[index,]$e)/1e6 # S+E
# Sales
(tstats[index,]$ds+tstats[index,]$fs)/1e6 # DS+FS
tstats[index,]$s/1e6
# Prod
(tstats[index,]$ds+tstats[index,]$de)/1e6 # DS+FS
tstats[index,]$p/1e6


rowSums(bt)[index]/1e6
tstats$ds[index]/1e6
tstats$p[index]/1e6
tstats$re[index]/1e6

#Proportions to Calculate Production given Sales-----
#Share of Sales represented by the domestic supply
tstats$s.ds<-tstats$ds/tstats$s    
#Foreign supply correction factor: Share of imports for in country sales 
tstats$i.fs<-ifelse(tstats$i==0,0,tstats$fs/tstats$i)   
#Re-converting to Imports given Foreign Supply
tstats$fs.i<-ifelse(tstats$fs==0,0,tstats$i/tstats$fs)  


# 2016-2019 LDV Trade ratios -----

supply.stats <- tstats

# Sales Share Matrix -----
# Creating a share of sales matrix using ONLY Foreign Supply as a share of Sales
ss.bt<-as.matrix(bt) #Copying balanced trade matrix

#Foreign Supply Matrix (Removing Imports meant for Re-export)
ss.bt<- sweep(ss.bt, 2, supply.stats$i.fs, "*") 

de.bt<- f.balanceMatrix(ss.bt,tstats$de,tstats$fs)

sales.share.matrix <- f.SalesShare(de.bt,tstats$ds)

# Fromto for final trade volumes            
global.supply.volumes<- f.fromto.table(bt,tstats$ds) 


# add diagonal to bt
bt %>% as.matrix() %>% as.data.frame() %>% names()
bt1 <- as.matrix(bt) 
diag(bt1) <- tstats$ds
bt <- as.data.frame(bt1)

# save data
write.csv(bt,"Inputs/Intermediate Trade Ratios/balanced.trade.matrix.csv",row.names = F)
write.csv(tstats,"Inputs/Intermediate Trade Ratios/supply.stats.csv",row.names = F)
write.csv(sales.share.matrix,"Inputs/Intermediate Trade Ratios/sales_share.csv",
          row.names = F)
write.csv(global.supply.volumes,"Inputs/Intermediate Trade Ratios/global.fromto.csv",row.names = F)



# Figure to visualize ratios of sales 
f.fig.tradeRatios(sales.share.matrix)

## With reduced number of countries ----
bt_flows <- as.matrix(de.bt)
diag(bt_flows) <- tstats$ds

flows <- f.fromto.table2(bt_flows)
flows <- flows %>% 
  mutate(origin=if_else(origin %in% selected_countries,origin,
                        paste0("Rest of ",r.origin)),
         destination=if_else(destination %in% selected_countries,destination,
                             paste0("Rest of ",r.destination))) %>% 
  group_by(r.origin,origin,r.destination,destination) %>% 
  reframe(qty=sum(qty)) %>% ungroup()
# create matrix
flows <-flows %>% arrange(destination) %>% arrange(origin) %>% f.trade.matrix()

# Create sales share
sales.share.matrix <- f.SalesShare(flows)
colSums(sales.share.matrix)


f.fig.tradeRatios(sales.share.matrix,fewCountries = 1)

# even fewer countries ---------
bt_flows <- as.matrix(de.bt)
diag(bt_flows) <- tstats$ds

flows <- f.fromto.table2(bt_flows)
flows <- flows %>% 
  mutate(origin=if_else(origin=="Taiwan","China",origin),
         destination=if_else(destination=="Taiwan","China",destination)) %>% 
  mutate(origin=if_else(origin %in% selected_countries_few,origin,
                        paste0("Rest of ",r.origin)),
         destination=if_else(destination %in% selected_countries_few,destination,
                             paste0("Rest of ",r.destination))) %>% 
  group_by(r.origin,origin,r.destination,destination) %>% 
  reframe(qty=sum(qty)) %>% ungroup()
# create matrix
flows <-flows %>% arrange(destination) %>% arrange(origin) %>% f.trade.matrix()

# Create sales share
sales.share.matrix <- f.SalesShare(flows)
colSums(sales.share.matrix)


f.fig.tradeRatios(sales.share.matrix,fewCountries = 2)

# EoF