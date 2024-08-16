# Multidimensional
# Part C balance all using a multidimensional RAS method
# PBH October 2023


# load libraries ----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# LOAD DATA -----

# Marginals: S and P -----
sales <- read.csv("Inputs/sales_segment.csv")
prod <- read.csv("Inputs/prod_segment.csv")


# Adjustment of marginals sales and prod.
s <- as.matrix(sales[,2:5])
sum(s)/1e6
# s[s==0]=0.01
p <- sales %>% dplyr::select(c) %>% left_join(prod) %>% 
  replace(is.na(.), 0) %>% .[,2:5] %>% as.matrix()
sum(p)/1e6
# p[p==0]=0.01
# they dont match, so assume Sales as true for every segment
for (i in 1:4){ # CORRECT FOR EVERY SEGMENT
  (corr_factor <- sum(s[,i])/sum(p[,i]))
  p[,i] <- p[,i]*corr_factor
}
rm(corr_factor,i)

## Marginals: trade balance -----
trade <- as.matrix(read.csv("Inputs/Intermediate Trade Ratios/balanced.trade.matrix.csv"))
supply.stats <- read.csv("Inputs/Intermediate Trade Ratios/supply.stats.csv")

# diag(trade)== supply.stats$ds
diag(trade) <- supply.stats$ds
# trade[trade==0] <- 0.01

sum(trade)/1e6
# correct trade totals
corr_factor <- sum(s)/sum(trade)
trade <- trade*corr_factor
sum(trade);sum(s);sum(p)


# matrix balancing function - 100 times iteration ----
f.multiRAS <- function(X, minIter=100){
  
  tol = 1
  # mdif= max(abs(te-rowSums(X)),abs(ti-colSums(X)))
  mdif=10000
  # flag= FALSE
  count=0
  
  x=X
  
  # compute norm
  
  # while(mdif>tol){
  while(count<minIter){
    x_old=x
    # x=x_old
    count=count+1
    
    p_x <- apply(x, MARGIN = c(1,3), FUN = sum)  # remove 2 dim - prod
    R = p/p_x
    R[is.nan(R)] <- 0
    R[is.infinite(R)] <- 0
    x = sweep(x,c(1,3),R,"*")
    x_p=x
    
    trade_x <- apply(x, MARGIN = c(1,2), FUN = sum)  # remove 3 dim - segms
    Z = trade/trade_x
    Z[is.nan(Z)] <- 0
    Z[is.infinite(Z)] <- 0
    x = sweep(x, c(1,2), Z, "*")
    x_t=x
    
    s_x <- apply(x, MARGIN = c(2,3), FUN = sum)  # remove 1 dim - sales
    C = s/s_x
    C[is.nan(C)] <- 0
    C[is.infinite(C)] <- 0
    x = sweep(x, c(2,3), C, "*")
    x_s=x
    
    x-x_old
    # prev_mdif = mdif
    # mdif= max(abs(te-rowSums(X)),abs(ti-colSums(X)))
    # mdif=norm(x-x_old)
    
    # if (prev_mdif * 0.95 < mdif && mdif < prev_mdif * 1.05 && count>minIter) {
    #   flag = TRUE
    #   break
    # }
  }
  
  # return avg of 3 last runs, so last balanced matters less
  x = (x_s+x_p+x_s)/3
  
  return(x)
}


# Compute average difference between matrices ---------

# Load initial tensors to balance
propAll <- read.csv("Inputs/Intermediate Trade Ratios/fromto_seg_Prop.csv") %>% 
  rename(segment=seg)
bucket <- read.csv("Inputs/Intermediate Trade Ratios/fromto_seg_Bucket_LtoS.csv")


# convert to Array
x_bucket <- array(rep(1, 65*65*4), dim=c(65, 65, 4))
for (i in 1:4){ x_bucket[,,i] <- bucket %>% filter(segment==paste0("s",i)) %>% f.trade.matrix() %>% as.matrix() }
x_prop <- array(rep(1, 65*65*4), dim=c(65, 65, 4))
for (i in 1:4){ x_prop[,,i] <- propAll %>% filter(segment==paste0("s",i)) %>% f.trade.matrix() %>% as.matrix() }

# save initials
x_bucket_initial=x_bucket
x_prop_initial=x_prop

# Balance
x_bucket <- f.multiRAS(x_bucket,minIter = 100)
x_prop <- f.multiRAS(x_prop,minIter = 100)
x_mix <- (x_prop+x_bucket)/2 # simple average

# Mean relative absolute error
f.MRAE <- function(a,b){
  tot <- (sum(a)+sum(b))/2
  diff <- abs(a-b)
  return(sum(diff)/tot/2)
}

f.MRAE(x_bucket,x_prop)

# Compare all resulting matrices among themselves
objects <- list(x_bucket,x_prop,x_mix)
num_objects <- length(objects)
comparison_matrix <- matrix(0, nrow = num_objects, ncol = num_objects)
for (i in 1:num_objects) {
  for (j in 1:num_objects) {
    comparison_matrix[i, j] <- f.MRAE(objects[[i]], objects[[j]])
  }
}
colnames(comparison_matrix) <- c("Buckets","Proportional","Mix")
rownames(comparison_matrix) <- colnames(comparison_matrix)
comparison_matrix

# Marginal diagnostics
f.diagnostics <- function(X){
  # note: sum(p) = sum(s) = sum(trade)
  cat(" Absolute difference in relative terms in production: ")
  x1=(sum(abs(apply(p,1,sum)-apply(X,MARGIN = 1, FUN = sum)))/sum(p))
  print(x1)
  cat("\n Absolute difference in relative terms in sales: ")
  x2=(sum(abs(apply(s,1,sum)-apply(X,MARGIN = 2, FUN = sum)))/sum(p))
  print(x2)
  cat("\n Absolute difference in relative terms in segments: ")
  x3=(sum(abs(apply(s,2,sum)-apply(X,MARGIN = 3, FUN = sum)))/sum(p))
  print(x3)
  return(c(x1,x2,x3))
}

f.diagnostics(x_prop)
f.diagnostics(x_bucket)
f.diagnostics(x_mix)


# marginal diagnostic as 2D level, just reduce one dimension
f.diagnostics.2D <- function(X){
  # note: sum(p) = sum(s) = sum(trade)
  cat(" Absolute difference in relative terms in production-segment: ")
  x1=(sum(abs(p-apply(X,MARGIN = c(1,3), FUN = sum)))/sum(p))
  print(x1)
  cat("\n Absolute difference in relative terms in sales-segment: ")
  x2=(sum(abs(s-apply(X,MARGIN = c(2,3), FUN = sum)))/sum(p))
  print(x2)
  cat("\n Absolute difference in relative terms in trade: ")
  x3=(sum(abs(trade-apply(X,MARGIN = c(1,2), FUN = sum)))/sum(trade))
  print(x3)
  return(c(x1,x2,x3))
}

f.diagnostics.2D(x_prop)
f.diagnostics.2D(x_bucket)
f.diagnostics.2D(x_mix)


# Diagnostics based on super quantile: average difference of upper quantile
f.diag.SuperQuantile <- function(X,quantile=0.1){
  # note: sum(p) = sum(s) = sum(trade)
  cat("Absolute difference in relative terms of the worst quantile: ",round(quantile*100,0),"%")
  cat("\n Production: ")
  n=round(65*quantile,0) # quantile elements
  abs_diff = abs(apply(p,1,sum)-apply(X,MARGIN = 1, FUN = sum)) # DIFFERENCE
  top_i = order(abs_diff/apply(p,1,sum), decreasing = TRUE)[1:n] # get worst indices - note: need to take worst of error/p to be in relative terms, and to avoid picking worst countries every time
  x1=(sum(abs_diff[top_i])/sum(apply(p,1,sum)[top_i])) # get average ABS of worsts
  print(x1)
  
  cat("\n Sales: ")
  abs_diff = abs(apply(s,1,sum)-apply(X,MARGIN = 2, FUN = sum)) #
  top_i = order(abs_diff/apply(s,1,sum), decreasing = TRUE)[1:n]
  x2=(sum(abs_diff[top_i])/sum(apply(s,1,sum)[top_i])) 
  print(x2)
  
  cat("\n Segments: ")
  n = round(4*quantile,0)
  n = if (n==0) 1 else n
  abs_diff = abs(apply(s,2,sum)-apply(X,MARGIN = 3, FUN = sum)) #
  top_i = order(abs_diff/apply(s,2,sum), decreasing = TRUE)[1:n]
  x3=(sum(abs_diff[top_i])/sum(apply(s,2,sum)[top_i])) 
  print(x3)
  return(c(x1,x2,x3))
}

f.diag.SuperQuantile(x_prop,0.1)
f.diag.SuperQuantile(x_bucket,0.1)
f.diag.SuperQuantile(x_mix,0.1)


# Collpase just one dimension
f.diag.SuperQuantile.2D <- function(X,quantile=0.1){
  # note: sum(p) = sum(s) = sum(trade)
  cat("Absolute difference in relative terms of the worst quantile: ",round(quantile*100,0),"%")
  cat("\n Production-Segment: ")
  n=round(65*4*quantile,0) # quantile elements
  abs_diff = abs(p-apply(X,MARGIN = c(1,3), FUN = sum)) # DIFFERENCE
  top_i = order(abs_diff/p, decreasing = TRUE)[1:n] # get worst indices - note: need to take worst of error/p to be in relative terms, and to avoid picking worst countries every time
  x1=(sum(abs_diff[top_i])/sum(p[top_i])) # get average ABS of worsts
  print(x1)
  
  cat("\n Sales: ")
  abs_diff = abs(s-apply(X,MARGIN = c(2,3), FUN = sum)) #
  top_i = order(abs_diff/s, decreasing = TRUE)[1:n]
  x2=(sum(abs_diff[top_i])/sum(s[top_i])) 
  print(x2)
  
  cat("\n Trade: ")
  n = round(65*65*quantile,0)
  n = if (n==0) 1 else n
  abs_diff = abs(trade-apply(X,MARGIN = c(1,2), FUN = sum)) #
  top_i = order(abs_diff/trade, decreasing = TRUE)[1:n]
  x3=(sum(abs_diff[top_i])/sum(trade[top_i])) 
  print(x3)
  return(c(x1,x2,x3))
}

f.diag.SuperQuantile.2D(x_bucket,0.1)
f.diag.SuperQuantile.2D(x_prop,0.1)
f.diag.SuperQuantile.2D(x_mix,0.1)



# Number of missing trade flows
sum(trade>10) # 2400 of 4225 possible
f.missSeeds <- function(X, range=10){
  total=sum(trade>range)
  cat("\n Total trade flows in T: ")
  print(total)
  aux=apply(X, MARGIN = c(1,2), FUN = sum) # sum over segment dimension
  x1=sum(aux>range)
  cat("\n Total trade flows in X: ")
  print(x1)
  cat("\n Completeness in relative terms: ")
  print(x1/total)
  return(c(x1,x1/total))
}
f.missSeeds(x_prop)
f.missSeeds(x_bucket)
f.missSeeds(x_mix)


# Diagonistcs figures ------

f.fig.scatter <- function(X,dim="Sales"){
  
  if(dim=="Sales"){
    mar <- 2
    lab_plot <- "Sales"
    df <- sales %>% 
      mutate(s=X1+X2+X3+X4,c1=c)
    apply_pred <- apply(X,MARGIN = mar, FUN = sum)
  }
  if(dim=="Prod"){
    mar <- 1
    lab_plot <- "Production"
    df <- sales %>% dplyr::select(c) %>% left_join(prod) %>% 
      mutate(s=X1+X2+X3+X4,c1=c)
    df[is.na(df)]=0
    apply_pred <- apply(X,MARGIN = mar, FUN = sum)
  }
  if(dim=="TradeFlow"){
    mar <- c(1,2)
    lab_plot <- "Trade Flow"
    tr <- as.data.frame(trade)
    rownames(tr) <- colnames(tr)
    df <- tr %>% rownames_to_column() %>% 
      pivot_longer(c(-rowname), names_to = "key", values_to = "s") %>% 
      mutate(c1=paste0(rowname,"-to-",key),
             c=rowname %>% str_replace_all("\\."," "))
    apply_pred <- apply(X,MARGIN = mar, FUN = sum) %>% as.data.frame() %>% 
      rowid_to_column() %>% 
      pivot_longer(c(-rowid), names_to = "key", values_to = "x") %>% pull(x)
  }

  # plot
  p1 <- df %>% 
  mutate(c=c %>% str_replace("Europe Central","Europe/Central") %>% 
           str_replace("East Africa","East/Africa") %>% 
           str_replace("Asia Oceania","Asia/Oceania")) %>% 
  left_join(ag) %>% 
  mutate(s_pred=apply_pred,
         diff=abs(s-s_pred)/s) %>%
  mutate(label_c=if_else(diff>0.1,c1,"")) %>% 
  ggplot(aes(s,s_pred))+
  geom_point(aes(col=region),size=2)+
  geom_text_repel(aes(label=label_c))+
  geom_abline(intercept = 0, slope=1, linetype="dashed")+
  scale_x_log10()+scale_y_log10()+
  labs(x=paste0("Actual ",lab_plot),y=paste0("Predicted ",lab_plot),col="",
       caption="Countries labels are shown when relative error is above 10%.")+
  theme(legend.position = c(0.8,0.3))
  
  return(p1)
}
# Sales
f.fig.scatter(x_bucket)
f.fig.scatter(x_prop)
f.fig.scatter(x_mix)
# Prod
f.fig.scatter(x_bucket,"Prod")
f.fig.scatter(x_prop,"Prod")
f.fig.scatter(x_mix,"Prod")

# Trade Flow
f.fig.scatter(x_trade,"TradeFlow")
f.fig.scatter(x_prop,"TradeFlow")


# 10 worst allocations
f.fig.bar <- function(X,dim="Sales",relative=F){
  
  if(dim=="Sales"){
    mar <- 2
    lab_plot <- "Sales"
    df <- sales %>% mutate(s=X1+X2+X3+X4,c1=c)
    apply_pred <- apply(X,MARGIN = mar, FUN = sum)
  }
  
  if(dim=="Prod"){
    mar <- 1
    lab_plot <- "Production"
    df <- sales %>% dplyr::select(c) %>% left_join(prod) %>% 
      mutate(s=X1+X2+X3+X4,c1=c)
    df[is.na(df)]=0
    apply_pred <- apply(X,MARGIN = mar, FUN = sum)
  }
  
  if(dim=="TradeFlow"){
    mar <- c(1,2)
    lab_plot <- "Trade Flow"
    tr <- as.data.frame(trade)
    rownames(tr) <- colnames(tr)
    df <- tr %>% rownames_to_column() %>% 
      pivot_longer(c(-rowname), names_to = "key", values_to = "s") %>% 
      mutate(c1=paste0(rowname,"-to-",key),
             c=rowname %>% str_replace_all("\\."," "))
    apply_pred <- apply(X,MARGIN = mar, FUN = sum) %>% as.data.frame() %>% 
      rowid_to_column() %>% 
      pivot_longer(c(-rowid), names_to = "key", values_to = "x") %>% pull(x)
  }
  
  # plot
  df <- df %>% 
    mutate(c=c %>% str_replace("Europe Central","Europe/Central") %>% 
             str_replace("East Africa","East/Africa")) %>% 
    left_join(ag) %>% 
    mutate(s_pred=apply_pred,
           diff=(s-s_pred))
  lab_x="Units"
  if(relative){
    df <- df %>% mutate(diff=diff/s) # relative
    lab_x="Relative diff "
  }
  p1 <- df %>% 
   arrange(desc(abs(diff))) %>% head(10) %>% 
    ggplot(aes(reorder(c1,diff),diff))+
    geom_col(aes(fill=region))+
    coord_flip()+
    # scale_y_log10()+
    labs(x="",title=paste0("Actual - Prediction: ",lab_plot),y=lab_x,fill="")
  
  return(p1)
}
# Sales
f.fig.bar(x_bucket,relative=F)
f.fig.bar(x_prop,relative=F)
f.fig.bar(x_mix,relative=F)
# Prod
f.fig.bar(x_bucket,"Prod")
f.fig.bar(x_prop,"Prod")
f.fig.bar(x_mix,"Prod")
# Trade Flow
f.fig.bar(x_bucket,"TradeFlow")
f.fig.bar(x_prop,"TradeFlow")
f.fig.bar(x_mix,"TradeFlow")


# China to China
trade[10,10]/1e6
sum(x_bucket[10,10,]/1e6)
sum(x_prop[10,10,]/1e6)


# Density of trade differences
f.fig.dens.trade <- function(X){
  
  mar <- c(1,2)
  lab_plot <- "Trade Flow"
  tr <- as.data.frame(trade)
  rownames(tr) <- colnames(tr)
  df <- tr %>% rownames_to_column() %>% 
      pivot_longer(c(-rowname), names_to = "key", values_to = "s") %>% 
      mutate(c1=paste0(rowname,"-to-",key),
             c=rowname %>% str_replace_all("\\."," "))
  apply_pred <- apply(X,MARGIN = mar, FUN = sum) %>% as.data.frame() %>% 
      rowid_to_column() %>% 
      pivot_longer(c(-rowid), names_to = "key", values_to = "x") %>% pull(x)
  

  # plot
  p1 <- df %>% 
    mutate(c=c %>% str_replace("Europe Central","Europe/Central") %>% 
             str_replace("East Africa","East/Africa") %>% 
             str_replace("Asia Oceania","Asia/Oceania")) %>% 
    left_join(ag) %>% 
    mutate(s_pred=apply_pred,
           diff=(s-s_pred),
           missing=if_else(s>0 & s_pred==0,"Missing flow","Flow")) %>%
    filter(s>10) %>% # 2400 flows with more than 10
    # filter(abs(diff)>10) %>% 
    ggplot(aes(diff,fill=missing))+
    # geom_histogram()+
    geom_density(aes(y = ..count..),alpha=.5)+
    scale_x_log10(limits=c(2,1e6))+
    ylim(0,320)+ # naybe remove, just to make scales comparable
    labs(x=paste0("Actual - Predicted:",lab_plot),fill="")
  
  return(p1)
}
f.fig.dens.trade(x_bucket)
f.fig.dens.trade(x_prop)
f.fig.dens.trade(x_mix)

# Mix curve diagnostics ----
df_mix <- c()
for (i in seq(0,1,0.01)){
  mix_new <- x_bucket*i+x_prop*(1-i)
  df_mix <- rbind(df_mix,cbind(
  tibble(prop_bucket=i),
  t(f.diagnostics(mix_new)),
  t(f.diag.SuperQuantile(mix_new,0.1)),
  t(f.missSeeds(mix_new))))
}
names(df_mix) <- c("share_bucket","Avg. MRAE Sales","Avg. MRAE Prod.","Avg. MRAE Trade",
                   "SQ-10 Sales","SQ-10 Prod.","SQ-10 Trade","n_flows","perc_flows")
df_mix
data_fig <- df_mix %>% 
  pivot_longer(c(-share_bucket,-n_flows,-perc_flows), names_to = "key", values_to = "value")

ggplot(data_fig,aes(value,perc_flows,col=share_bucket))+
  geom_point(size=0.1)+
  geom_point(data=filter(data_fig,share_bucket==0.49),col="red",size=0.1)+
  facet_wrap(~key,scales = "free_x")+
  # scale_color_continuous(  low = "darkblue",high = "darkred",mid = "grey")+
  scale_color_viridis_c(labels=scales::label_percent())+
  scale_x_continuous(labels = scales::percent_format())+
  scale_y_continuous(labels = scales::percent_format())+
  labs(x="Error %",y="Flows retained %",col="Proportion of Bucket in Mix")+
  theme_bw(8)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "bottom")
  # geom_text_repel(aes(label=share_bucket))

ggsave("Figures/Proportion_Analysis.png",
       units = "cm",dpi = 500,
       width = 18.4,height = 10)


f.xfromTo <- function(X){
  S1 <- X[,,1] %>% as.matrix() %>% f.fromto.table2() %>% 
    mutate(segment="s1")
  S2 <- X[,,2] %>% as.matrix() %>% f.fromto.table2() %>% 
    mutate(segment="s2")
  S3 <- X[,,3] %>% as.matrix() %>% f.fromto.table2() %>% 
    mutate(segment="s3")
  S4 <- X[,,4] %>% as.matrix() %>% f.fromto.table2() %>% 
    mutate(segment="s4")
  fromto_ALL<- rbind(S1,S2,S3,S4)
  return(fromto_ALL)
}


bucket <- f.xfromTo(x_bucket)
propAll <- f.xfromTo(x_prop)
mix <- f.xfromTo(x_mix)

figname <- "Figures/salesRatios_%s.png"

f.fig.tradeRatiosSeg(bucket,T)
ggsave(sprintf(figname,"bucket"), dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)
f.fig.tradeRatiosSeg(propAll,T)
ggsave(sprintf(figname,"prop"), dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)
f.fig.tradeRatiosSeg(mix,T)
ggsave(sprintf(figname,"mix"), dpi=600,units="mm",width = 1586/3.7795275591, height = 1200/3.7795275591)



# CREATE SUPPLY STATS SEG -----

# Get share statistics for each segment, based on balancing process
fromto_aux <- mix # SELECTED
supply.stats_seg <- data.frame()

for (se in paste0("s",1:4)){

  f <- fromto_aux %>% filter(segment==se)
  
  tr <- f.trade.matrix(f)
  
  aux <- data.frame(
    c=names(tr), # country
    s=colSums(tr), # sales
    p=rowSums(tr),
    ds=diag(as.matrix(tr)),
    s.ds=diag(as.matrix(tr))/colSums(tr),
    fs=colSums(tr)-diag(as.matrix(tr)),
    fs.i=1, # Trade data is already without exports balancing, so lets check if this approach work to balance future production
    seg=se)
  
  supply.stats_seg <- rbind(supply.stats_seg,aux)
}

supply.stats_seg <- supply.stats_seg %>% 
  mutate(s.ds=if_else(is.nan(s.ds),0,s.ds))
rownames(supply.stats_seg) <- NULL
supply.stats_seg <- supply.stats_seg %>% f.addISO()


write.csv(supply.stats_seg,"Inputs/supplystats_seg.csv",
          row.names = F)

# SAVE DATA -------
# selected. mix proportional allocation and buckets


for (segm in paste0("s",1:4)){
  
  # filter by segment
  trade_seg <- fromto_aux %>% filter(segment==segm) %>% 
    arrange(destination) %>% arrange(origin) %>% f.trade.matrix()
  sup_stats_seg <- supply.stats_seg %>% filter(seg==segm)
  
  # Create sales share
  sales.share.matrix_seg<- f.SalesShare(trade_seg)
  
  colSums(sales.share.matrix_seg)
  ds <- diag(as.matrix(sales.share.matrix_seg))
  # diag(sales.share.matrix_seg) <- 0
  
  # Save data
  write.csv(sales.share.matrix_seg,
            paste0("Inputs/sales_share_",segm,".csv"),row.names = F)
}


# EoF