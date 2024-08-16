# Cross Validation of LDV Ratios
# Run Prod. Allocation for each year based on other years LDV ratios. Using observed sales
# May 2023
# PBH

# load libraries ----
source("Scripts/00-Libraries.R", encoding = "UTF-8")


# PROPIETARY DATA - NOT INCLUDED FOR PUBLIC USE
load("lastbaldat.RData")


# Balance all years for trade ratios -------------

ag <- read.csv("Inputs/ag.csv") # restore ag 

balanced.smooth.list <- list()
tstats.list <- list()

# i=1 # 2013 to 2021
for (i in 1:length(smooth.list)){
  
  cat("Year ",2012+i,"\n")
  
  trade <- smooth.list[[i]]
  
  y <- 2012+i
  
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
  
  # negative exports are zero
  tstats$i <- ifelse(tstats$e<0,abs(tstats$e),0)+tstats$i
  tstats$e <- ifelse(tstats$e<0, 0,tstats$e)
  
  # Check balance
  sum(tstats$p+tstats$i) 
  sum(tstats$s+tstats$e)
  
  #USING RAS TO BALANCE 
  names(trade)==tstats$c
  bt <- f.balanceMatrix(trade,tstats$e,tstats$i)

  #Calculating all of the Trade Stats: tstats 
  
  for(j in 1:nrow(tstats)){
    # case Production positive, and P>E
    if (tstats$p[j]>0 && tstats$p[j]>tstats$e[j]) {
      tstats$ds[j]=tstats$p[j]-tstats$e[j]
      tstats$re[j]=0
      tstats$fs[j]=tstats$i[j]
      tstats$de[j]=tstats$e[j]
    } else { # Proportional allocation - Problem is here RE-EXPORTS ARE NOT COUNTED IN THE SUPPLY STATS, BUT ON THE TRADE 
      tstats$ds[j]=tstats$p[j]*(tstats$s[j]/(tstats$p[j]+tstats$i[j]))
      tstats$re[j]=tstats$i[j]*(tstats$e[j]/(tstats$p[j]+tstats$i[j]))
      tstats$fs[j]=tstats$i[j]*(tstats$s[j]/(tstats$p[j]+tstats$i[j]))
      tstats$de[j]=tstats$p[j]*(tstats$e[j]/(tstats$p[j]+tstats$i[j]))
    }
  }
  rm(j)
 
  
  #Share of Sales represented by the domestic supply
  tstats$s.ds<-tstats$ds/tstats$s    
  #Foreign supply correction factor: Share of imports for in country sales 
  tstats$i.fs<-ifelse(tstats$i==0,0,tstats$fs/tstats$i)   
  #Re-converting to Imports given Foreign Supply
  tstats$fs.i<-ifelse(tstats$fs==0,0,tstats$i/tstats$fs)  
  
  tstats$yr <- 2012+i
  
  # save results
  balanced.smooth.list[[i]] = bt
  tstats.list[[i]] = tstats
  
  rm(bt,tstats)
  
}
rm(i)

# LOOP TO GENERATE DATA -----
# only run it to re-generate all the production allocation data

# Loop for each year
for (i in 1:length(balanced.smooth.list)){

  cat("Year: ",2012+i,"\n")
  
  #  balanced matrices for each year
  s1_supply.stats <- tstats.list[[i]]
  s1_supply.stats$s.ds <- s1_supply.stats$ds/(s1_supply.stats$s)
  
  # add missing stats
  s1_supply.stats$fs.i<-ifelse(s1_supply.stats$fs==0,0,s1_supply.stats$i/s1_supply.stats$fs)
  s1_supply.stats$i.fs<-ifelse(s1_supply.stats$i==0,0,s1_supply.stats$fs/s1_supply.stats$i)

  
  # Create Trade Ratios  
  ss.bt<-as.matrix(balanced.smooth.list[[i]]) #Copying balanced trade matrix
  #Foreign Supply Matrix (Removing Imports meant for Re-export)
  ss.bt<- sweep(ss.bt, 2, s1_supply.stats$i.fs, "*") 
  de.bt<- f.balanceMatrix(ss.bt,s1_supply.stats$de,s1_supply.stats$fs)
  #Share of Sales represented by the column values Fs  
  ss.bt<- sweep(de.bt, 2, s1_supply.stats$s, "/")
  colSums(ss.bt) # DOES NOT SUM 1, as it does not count domestic supply
  colSums(ss.bt)+s1_supply.stats$s.ds
  s1_sales.share.matrix <- ss.bt; rm(ss.bt) #
  names(s1_sales.share.matrix) <- names(s1_sales.share.matrix) %>% 
    str_replace_all(" ",".") %>% str_replace_all("\\/",".")
  
  # Allocate production for future years, based on trade ratios (i) and sales(j)
  for (j in 1:length(tstats.list)){
    
    url_save <- paste0("Inputs/Cross_Validation/",2012+i,"_",2012+j,"%s.csv")
    
    cat("Sales Year: ",2012+j,"\n")
    # run allocation
    salesvec <- tstats.list[[j]]$s  
    supply.stats <- tstats.list[[j]]
    
    # Add missing stats
    supply.stats$s.ds <- supply.stats$ds/supply.stats$s
    supply.stats$fs.i<-ifelse(supply.stats$fs==0,0,supply.stats$i/supply.stats$fs)
    
    # Production Allocation
    # Allocate Production
    s1 <- f.futureProduction(demandvec=salesvec, # IMPORTANT: Order of countries must be the same!!! 
                             stats_supply=supply.stats, 
                             salesRatios=s1_sales.share.matrix)
    fbt <- s1$trade
    ftstats <- s1$stats
    fs.bt <- s1$de
    rm(s1)
    
    #Producing a Future from to 
    future.supply <- f.fromto.table(fbt,ftstats$ds)
    
    
    # Save data -----
    write.csv(ftstats,sprintf(url_save,"supplystats"),row.names = F)
    write.csv(future.supply,sprintf(url_save,"globalFromTo"),row.names = F)
    
    }
 rm(j) 
}
rm(i)


# Load prod. allocation data --------

# Function to read results
f.read <- function(file_path,scenario_string){
  x <- read.csv(file_path)
  x$scenario <- scenario_string
  return(x)
}

url_file <- list.files(path = "Inputs/Cross_Validation")

# From to tables
files_supply <- url_file[grepl("globalFromTo", url_file)]
res <- lapply(files_supply, function(x) f.read(paste0("Inputs/Cross_Validation/",x),x))
future.supply <- do.call("rbind",res)

# supply stats
files_supply <- url_file[grepl("supplystats", url_file)]
res <- lapply(files_supply, function(x) f.read(paste0("Inputs/Cross_Validation/",x),x))
supply.stats <- do.call("rbind",res)

rm(res,url_file,files_supply)

# data wrangling
library(stringr)
supply.stats <- supply.stats %>% 
  mutate(ldv_year=substr(scenario, 1,4) %>% as.numeric(),
         sales_year=substr(scenario, 6,9) %>% as.numeric())
future.supply <- future.supply %>%   
  mutate(year=substr(scenario, 1,4) %>% as.numeric(),
         sales_year=substr(scenario, 6,9) %>% as.numeric())

# Cross Validation Analysis -----

# Example
# Real Production: 2019
(a <- tstats.list[[7]]$p)
# Allocated Production of 2019 using 2018 Ratios
(b <- supply.stats %>% filter(ldv_year==2018,sales_year==2019) %>% .$p)
sum(a)/1e6;sum(b)/1e6
(a-b)/1e6
sum(abs(a-b))/1e6 # 3.02 M unallocated
3.023/89.64*100/2 # divided by two to avoid double counting

# Real Production
supply.stats_real <- do.call("rbind",tstats.list)
supply.stats_real <- supply.stats_real %>% 
  dplyr::select(c,p,yr) %>% 
  rename(sales_year=yr) 

# join to prod.allocation data
supply.stats <- supply.stats %>% rename(p_allocated=p)

supply.comparison <- supply.stats %>% left_join(supply.stats_real,by=c("c","sales_year"))

# Get cross-year LDV errors
supply.comparison <- supply.comparison %>% 
  mutate(p_diff=p_allocated-p)

# total prod by year
total_prod <- supply.stats_real %>% group_by(sales_year) %>% 
  summarise(total_p=sum(p)/1e6) %>% ungroup()

supply.comparison %>% 
  group_by(ldv_year,sales_year) %>% 
  summarise(x=sum(abs(p_diff)/1e6/2)) %>%  # divide by 2 to avoid double counting
  left_join(total_prod) %>%
  mutate(x=x/total_p) %>%  dplyr::select(-total_p) %>% # percentage error
  pivot_wider(names_from = ldv_year, values_from = x)
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)


# what about SQ-10 error?
n=round(65*0.1,0) # quantile elements
supply.comparison %>% 
  mutate(p_rel=if_else(p>10,abs(p_diff)/p,0)) %>% 
  group_by(ldv_year,sales_year) %>%
  top_n(n,p_rel) %>%
  # top_n(n,abs(p_diff)) %>% 
  reframe(x=sum(abs(p_diff)/1e6/2),total_p=sum(p)/1e6) %>%  # divide by 2 to avoid double counting
  mutate(x=x/total_p) %>%  dplyr::select(-total_p) %>% # percentage error
  pivot_wider(names_from = ldv_year, values_from = x)
.Last.value %>% write.table("clipboard", sep="\t",row.names = F)



# Countries with biggest misallocation
# Note that misallocation of production is not evenly distributed!


total_prod <- supply.stats_real %>% group_by(c,sales_year) %>% 
  summarise(total_p=sum(p)/1e6) %>% ungroup()


top_c <- supply.comparison %>% 
  group_by(c) %>% 
  summarise(p_diff=mean(abs(p_diff)/1e6)) %>% ungroup() %>% 
  arrange(desc(p_diff)) %>% top_n(10) %>% pull(c)


supply.comparison %>% 
  filter(c %in% top_c) %>% 
  filter(sales_year==2019) %>% 
  mutate(x=p_diff/1e6) %>% 
  left_join(total_prod) %>%
  # mutate(x=x/total_p) %>% 
  ggplot(aes(c,x,fill=factor(ldv_year)))+
  geom_bar(position=position_dodge2(reverse = T),stat = "identity",width = 0.6)+
  # geom_text(aes(y=p*1.05+x_lim/50,label=labelK),size=3,
  #           position = position_dodge2(width = .9,reverse=T))+
  coord_flip(expand=F)+
  # scale_y_continuous(labels = scales::percent)+
  scale_fill_viridis_d()+
  labs(x="",fill="LDV Ratios Year",
       # y=paste0("Relative error in 2019 Production Allocation")
       y=paste0("Million units of Production Misallocated for 2019 Production")
       )+
  theme_bw(20)+
  theme(text=element_text(size=20),
        panel.grid.major = element_blank())


# EoF