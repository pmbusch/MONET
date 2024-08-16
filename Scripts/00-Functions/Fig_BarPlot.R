## Figure: Bar Plot to show Production or Sales as share
## PBH
## March 2023

# Bar Plot showing country level sales or Production, disaggregated
# Sales should be called s and production p, country labels is c
f.fig.barSales <- function(dataStats, xlab="Light Duty EV",
                           includeChina=T,yt=2035,
                           scenario=""){
  
  x <-dataStats %>% filter(c!="China"|includeChina)
  x_lim <- max(x$s)*1.1
  
  xlab <- paste0(xlab," Domestic Sales ",yt)
  
  x %>%
    pivot_longer(c(fs,ds), names_to = "key", values_to = "value") %>% 
    mutate(labelK=case_when(
      s>1e6 ~ paste0(round(s/1e6,1),"M"),
      s>1e3 ~ paste0(round(s/1e3,0),"K"),
      T ~ paste0(round(s,0)," "))) %>% 
    mutate(key=if_else(key=="ds","Domestic Supply","Foreign Supply")) %>%
    ggplot(aes(reorder(c,s),value,fill=key))+
    geom_bar(position="stack",stat = "identity")+
    geom_text(aes(y=s*1.05+x_lim/50,label=labelK),size=14*5/14 * 0.8)+
    coord_flip(expand=F)+
    scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                       limits=c(0,x_lim))+
    labs(x="",y=xlab,fill="",
         caption=scenario)+
    theme(legend.position = c(0.8,0.1),
          axis.text.y = element_text(size=14))
}

# Bar sales by segment
f.fig.barSales_Seg <- function(dataStats, xlab="Light Duty EV",
                              includeChina=T,ev=T,yt=2035,
                              scenario=""){
  
  xlab <- paste0(xlab," Domestic Sales ",yt)
  x <-dataStats %>% filter(c!="China"|includeChina)
  
  
  x_lim <- x %>% group_by(c) %>% reframe(s=sum(s)) %>% ungroup() %>% 
    pull(s) %>% max()*1.1
  
  x_lab <- x %>% 
    group_by(c) %>% summarise(s=sum(s)) %>% ungroup() %>%
    mutate(s_total=s) %>% 
    mutate(labelK=case_when(
      s>1e6 ~ paste0(round(s/1e6,1),"M"),
      s>1e3 ~ paste0(round(s/1e3,0),"K"),
      T ~ paste0(round(s,0)," ")))
  
  x %>%
    group_by(c) %>% mutate(s_total=sum(s)) %>% ungroup() %>% 
    ggplot(aes(reorder(c,s_total),s))+
    geom_bar(aes(fill=fct_rev(seg)), position="stack",stat = "identity")+
    geom_text(data=x_lab,aes(y=s*1.05+x_lim/50,label=labelK),size=14*5/14 * 0.8)+
    coord_flip(expand=F)+
    scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                       limits=c(0,x_lim))+
    scale_fill_manual(values=c("#3288bd", "#99d594", "#e6f598", "#fee08b"))+
    guides(fill = guide_legend(reverse = T,nrow = 1)) +
    labs(x="",y=xlab,fill="Segment",
         caption=scenario)+
    theme(legend.position = c(0.7,0.2),
          axis.text.y = element_text(size=16),
          legend.text = element_text((size=20)))
}


f.fig.barProd <- function(dataStats, xlab="Light Duty EV",
                          includeChina=T,ev=T,yt=2035,
                          scenario=""){
  
  xlab <- paste0(xlab," Production ",yt)
  
  if(ev==F){
    # change variables to re-use code easily
    dataStats$p <- dataStats$MWh
    dataStats$de <- dataStats$MWh.de
    dataStats$ds <- dataStats$MWh.ds
    xlab <- paste0(xlab," Battery Supply Requirement ",yt," [MWh] (for EV production)")
  }
  
  x <-dataStats %>% filter(c!="China"|includeChina)
  x_lim <- max(x$p)*1.1
  
  x %>%
    filter(p>0) %>% 
    pivot_longer(c(de,ds), names_to = "key", values_to = "value") %>% 
    mutate(labelK=case_when(
      p>1e6 ~ paste0(round(p/1e6,1),"M"),
      p>1e3 ~ paste0(round(p/1e3,0),"K"),
      T ~ paste0(round(p,0)," "))) %>% 
    mutate(key=if_else(key=="ds","Domestic Supply","Production for Exports")) %>% 
    ggplot(aes(reorder(c,p),value,fill=key))+
    geom_bar(position="stack",stat = "identity")+
    geom_text(aes(y=p*1.05+x_lim/50,label=labelK),size=16*5/14 * 0.8)+
    coord_flip(expand=F)+
    scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                       limits=c(0,x_lim))+
    labs(x="",y=xlab,fill="",
         caption=scenario)+
    theme(legend.position = c(0.7,0.2),
          axis.text.y = element_text(size=16),
          legend.text = element_text((size=20)))
}

# Bar prod by segment
f.fig.barProd_Seg <- function(dataStats, xlab="Light Duty EV",
                          includeChina=T,ev=T,yt=2035,
                          scenario=""){
  
  xlab <- paste0(xlab," Production ",yt)
  
  if(ev==F){
    # change variables to re-use code easily
    dataStats$p <- dataStats$MWh
    dataStats$de <- dataStats$MWh.de
    dataStats$ds <- dataStats$MWh.ds
    # xlab <- paste0(xlab," Battery Supply Requirement 2035 [MWh] (for EV production)")
    xlab <- paste0("Battery Supply Requirement ",yt," [MWh] (for EV production)")
  }
  
  x <-dataStats %>% filter(c!="China"|includeChina)
  
  
  x_lim <- x %>% group_by(c) %>% reframe(p=sum(p)) %>% ungroup() %>% 
    pull(p) %>% max()*1.1
  
  x_lab <- x %>% 
    filter(p>0) %>% 
    group_by(c) %>% summarise(p=sum(p)) %>% ungroup() %>%
    mutate(p_total=p) %>% 
    mutate(labelK=case_when(
    p>1e6 ~ paste0(round(p/1e6,1),"M"),
    p>1e3 ~ paste0(round(p/1e3,0),"K"),
    T ~ paste0(round(p,0)," ")))
  
  x %>%
    filter(p>0) %>% 
    group_by(c) %>% mutate(p_total=sum(p)) %>% ungroup() %>% 
    ggplot(aes(reorder(c,p_total),p))+
    geom_bar(aes(fill=fct_rev(seg)), position="stack",stat = "identity")+
    geom_text(data=x_lab,aes(y=p*1.05+x_lim/50,label=labelK),size=16*5/14 * 0.8)+
    coord_flip(expand=F)+
    scale_y_continuous(labels = label_comma(drop0trailing = TRUE),
                       limits=c(0,x_lim))+
    scale_fill_manual(values=c("#3288bd", "#99d594", "#e6f598", "#fee08b"))+
    guides(fill = guide_legend(reverse = T,nrow = 1)) +
    labs(x="",y=xlab,fill="Segment",
         caption=scenario)+
    theme(legend.position = c(0.7,0.2),
          axis.text.y = element_text(size=16),
          legend.text = element_text((size=20)))
}


# EoF