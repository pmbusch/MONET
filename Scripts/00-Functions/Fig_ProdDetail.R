## Figure: Bar Plot to show Production destiny for a specific country
#  Purpose: Story Making
## PBH
## March 2023

# Bar Plot showing for a specic country, the top 10 destiny of vehicles
# If EV=F, show battery capacity
f.fig.ProdDestiny <- function(fromto, country="Mexico",
                              ev=T,lab="Light Duty EV",
                              scenario=""){
  
  unit_label <- paste0(" - ",lab," 2035 [units]")
  if(ev==F){
    fromto$qty <- fromto$mWh # Show Battery capacity instead
    unit_label <- paste0(" - ",lab," Battery Supply 2035 [MWh]")
  }
  
  x <-fromto %>% filter(origin==country)
  total_prod <- sum(x$qty,na.rm=T)
  x <- x %>% top_n(10,qty) # top 10 countries
  ommited <- (total_prod-sum(x$qty,na.rm=T)) %>% round(0)
  
  x_lim <- max(x$qty)*1.1
  
  
  x %>%
    mutate(labelK=case_when(
      qty>1e6 ~ paste0(round(qty/1e6,1),"M"),
      qty>1e3 ~ paste0(round(qty/1e3,0),"K"),
      T ~ paste0(round(qty,0)," "))) %>% 
    mutate(r.destination=factor(r.destination,levels=level_region)) %>% 
    ggplot(aes(reorder(destination,qty),qty,fill=r.destination))+
    geom_bar(position="dodge",stat = "identity")+
    geom_text(aes(y=qty*1.05+x_lim/50,label=labelK),size=16*5/14 * 0.8)+
    coord_flip(expand=F)+
    scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                       limits=c(0,x_lim))+
    scale_fill_viridis_d()+
    labs(x="",fill="",
         caption=paste0(scenario,". Total production exported to other countries not shown: ",
                        format(ommited,big.mark=",")),
         y=paste0("Exports from: ",country,unit_label))+
    theme(legend.position = c(0.8,0.3),
          axis.text.y = element_text(size=16))
}


# Bar Plot showing for a specific country, the top 10 origin of vehicles
f.fig.SalesOrigin <- function(fromto, country="Mexico",
                              ev=T,lab="Light Duty EV",
                              scenario=""){
  
  unit_label <- paste0(" - ",lab," 2035 [units]")
  if(ev==F){
    fromto$qty <- fromto$mWh # Show Battery capacity instead
    unit_label <- paste0(" - ",lab," Battery Supply 2035 [MWh]")
  }
  
  
  x <-fromto %>% filter(destination==country)
  total_sales <- sum(x$qty,na.rm=T)
  x <- x %>% top_n(10,qty) # top 10 countries
  ommited <- (total_sales-sum(x$qty,na.rm=T)) %>% round(0)
  
  x_lim <- max(x$qty)*1.1
  
  x %>%
    mutate(labelK=case_when(
      qty>1e6 ~ paste0(round(qty/1e6,1),"M"),
      qty>1e3 ~ paste0(round(qty/1e3,0),"K"),
      T ~ paste0(round(qty,0)," "))) %>% 
    mutate(r.origin=factor(r.origin,levels=level_region)) %>% 
    ggplot(aes(reorder(origin,qty),qty,fill=r.origin))+
    geom_bar(position="dodge",stat = "identity")+
    geom_text(aes(y=qty*1.05+x_lim/50,label=labelK),size=16*5/14 * 0.8)+
    coord_flip(expand=F)+
    scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                       limits=c(0,x_lim))+
    scale_fill_viridis_d()+
    labs(x="",fill="",
         caption=paste0(scenario,". Total imports from other countries not shown: ",
                        format(ommited,big.mark=",")),
         y=paste0("Imports to: ",country,unit_label))+
    theme(legend.position = c(0.8,0.3),
          axis.text.y = element_text(size=16))
}

