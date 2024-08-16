# Function for Heatmap of trade ratios
# PBH 2023


# reduce number of countries
selected_countries <- c("Germany","France","Turkey","Czechia","United Kingdom",
                        "Russia","Switzerland","Belgium","Sweden","Slovakia",
                        "Morocco","Spain","China","Japan","South Korea","Taiwan",
                        "United States","Mexico","Canada",
                        "South Africa","Algeria","Iran","Egypt",
                        "Argentina","Brazil","Venezuela","Colombia",
                        "Thailand","Pakistan","India","Indonesia","Vietnam")


selected_countries_few <- c("Germany","Spain","Turkey","Czechia","United Kingdom",
                        "Russia","Sweden","Slovakia",
                        "Morocco","China","Japan","South Korea",
                        "United States","Mexico","Canada",
                        "South Africa","Iran",
                        "Argentina","Brazil",
                        "Thailand","India")

# Inputs
# -Trade Matrix shares
# - Diagonal: domestic supply
# - Data to map country names
# - Figure is for share of sales or production sales
# - Include countries with zero production
f.fig.tradeRatios <- function(tradeMatrix,ag_countries=ag,ds=0,
                              sales=T,incProdZero=T,
                              fewCountries=0,
                              scenario=""){
  
  ratios <- tradeMatrix
  if (length(ds)!=1) { # diag is vector
    diag(ratios) = ds
  }

  
  # make sure names match
  colnames(ratios) <- colnames(ratios) %>% str_replace_all(" ",".") %>% str_replace_all("\\/",".")
  
    if(sales==F){
    # NaN occurs due to countries without production, replace as zero
    ratios[is.na(ratios)] <- 0
    }
  
  ratios$country_source <- names(ratios)
  ratios <- ratios %>% 
    pivot_longer(c(-country_source), names_to = "country_target", values_to = "value")
  
  ratios <- ratios %>% 
    # mutate(interval=case_when(value<0.01 ~ "0%",value<0.2 ~ "1-20%",value<0.4 ~ "20-40%",value<0.6 ~ "40-60%",value<0.8 ~ "60-80%",T ~ "80-100%") %>% as.factor()) %>%
    # mutate(interval=case_when(value<0.01 ~ "0%",value<0.13 ~ "1-13%",value<0.25 ~ "13-25%",value<0.38 ~ "25-38%",value<0.5 ~ "38-50%",value<0.63 ~ "50-63%",value<0.75 ~ "63-75%",value<0.88 ~ "75-88%",T ~ "88-100%") %>% as.factor()) %>%
    mutate(interval=case_when(
      value<0.01 ~ "0%",
      value<0.1 ~ "1-10%",
      value<0.2 ~ "10-20%",
      value<0.3 ~ "20-30%",
      value<0.4 ~ "30-40%",
      value<0.5 ~ "40-50%",
      value<0.6 ~ "50-60%",
      value<0.7 ~ "60-70%",
      value<0.8 ~ "70-80%",
      value<0.9 ~ "80-90%",
      T ~ "90-100%") %>% as.factor())
  
  # order by region  and then production
  region_order <- c("Europe","North America","South America","Japan/Korea",
                    "Greater China","South Asia","Middle East/Africa")
  ag_aux <- ag_countries %>% mutate(c=c %>% str_replace_all(" ",".") %>% str_replace_all("\\/","."))
  ag_aux <- rbind(ag_aux,
                  c("Europe","Rest.of.Europe","xx","xx"),
                  c("South Asia","Rest.of.South.Asia","xx","xx"))
  
  country_order <- ratios %>% 
    left_join(ag_aux,by=c("country_source"="c")) %>% 
    group_by(region,country_source) %>% 
    summarise(value=sum(value)) %>% ungroup() %>% 
    mutate(last=str_detect(country_source,"Rest.of")) %>% # rest at the end of regions
    mutate(region=factor(region,levels=region_order)) %>% 
    arrange(region,last,desc(value)) %>% 
    pull(country_source)
  
  
  countries_prod <- ratios %>%  filter(value>0.01|incProdZero) %>% 
    pull(country_source) %>% unique()
  countries_prod <- country_order[country_order %in% countries_prod]
  
  text_caption <- paste0(ifelse(sales,"Vertical","Horizontal")," sum = 100%")
  
  ratios_reg <- ratios %>% 
    left_join(ag_aux,by=c("country_source"="c")) %>% rename(r.source=region) %>% 
    left_join(ag_aux,by=c("country_target"="c")) %>% rename(r.target=region) %>% 
    mutate(region=if_else(r.source==r.target,r.source,"")) %>% 
    filter(region!="")

  new_scale <- function(new_aes) {
    structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
  }
  
  ggplot_add.new_aes <- function(object, plot, object_name) {
    plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
    plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
    plot$labels <- bump_aes(plot$labels, new_aes = object)
    plot
  }
  
  # limits to draw rectangles
  if (fewCountries==1){
    box_x <- c(0.5,12.5,15.5,20.5,24.5,30.5,36.5)
    box_y <- c(0.5,6.5,12.5,16.5,21.5,24.5,36.5)
    text_x <- c(11.5,12,22,21,27,32.5)
    text_y <- c(1,7,13,21,21,26)
  } else if(fewCountries==2){
    box_x <- c(0.5,9.5,12.5,15.5,18.5,21.5,25.5)
    box_y <- c(0.5,4.5,7.5,10.5,13.5,16.5,25.5)
    text_x <- c(10.2,9,16.6,18.5,20,20.2)
    text_y <- c(1,4,11,13,13,22)
  }
  else {
    box_x <- c(0.5,26.5,29.5,38.5,42.5,53.5,65.5)
    box_y <- c(0.5,12.5,23.5,27.5,36.5,39.5,65.5)
    text_x <- c(23,29.5,32.5,41.5,44.5,57)
    text_y <- c(3,14,29,29,41,41)
  }

  size_text <- if (fewCountries==2) 16 else 10
  
  ratios %>% 
    filter(value>0.01) %>% 
    ggplot(aes(country_target,country_source,fill=interval))+
    # boxes
    geom_rect(xmin=box_x[1],xmax=box_x[2],ymin=box_y[6],ymax=box_y[7],linewidth=1,col="brown",fill="white")+
    geom_rect(xmin=box_x[2],xmax=box_x[3],ymin=box_y[5],ymax=box_y[6],linewidth=1,col="brown",fill="white")+
    geom_rect(xmin=box_x[3],xmax=box_x[4],ymin=box_y[4],ymax=box_y[5],linewidth=1,col="brown",fill="white")+
    geom_rect(xmin=box_x[4],xmax=box_x[5],ymin=box_y[3],ymax=box_y[4],linewidth=1,col="brown",fill="white")+
    geom_rect(xmin=box_x[5],xmax=box_x[6],ymin=box_y[2],ymax=box_y[3],linewidth=1,col="brown",fill="white")+
    geom_rect(xmin=box_x[6],xmax=box_x[7],ymin=box_y[1],ymax=box_y[2],linewidth=1,col="brown",fill="white")+
    geom_tile(color="black")+
    # geom_tile(data = filter(ratios,value>0.01),color="black")+
    scale_fill_brewer(palette ="RdYlGn", type="seq", direction=-1, breaks=levels(ratios$interval),
                      labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    geom_text(x=text_x[1],y=text_y[6],label="Europe",col="brown")+
    geom_text(x=text_x[2],y=text_y[5],label="North America",col="brown")+
    geom_text(x=text_x[3],y=text_y[4],label="South America",col="brown")+
    geom_text(x=text_x[4],y=text_y[3],label="Asia",col="brown")+
    geom_text(x=text_x[5],y=text_y[2],label="South Asia",col="brown")+
    geom_text(x=text_x[6],y=text_y[1],label="Middle East/Africa",col="brown")+
    labs(x="Country Target",y="Source",fill="% Origin",
         caption=paste0(text_caption,". ",scenario))+
    xlim(country_order)+ylim(rev(countries_prod))+
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=size_text),
          axis.text.y = element_text(size=size_text))
    # new_scale("fill") +
    # geom_tile(data=ratios_reg,aes(fill=region),alpha=0.5)+
    # scale_fill_manual("Region",values=c("#b3e2cd","#fdcdac","#cbd5e8","#f4cae4",
                                        # "#e6f5c9","#fff2ae","#f1e2cc"))
  }

# FIG RATIOS SEGMENT -------------

# generates figure starting from fromto_aux
f.fig.tradeRatiosSeg <- function(fromTo,fewCountries=0,getData=F){
  
  high_c <- if(fewCountries==1) selected_countries_few else selected_countries
  
  ratios <- list()
  i=1
  for (segm in paste0("s",1:4)){
    
    # filter by segment
    trade_seg <- fromTo %>% filter(segment==segm)
    
    if (fewCountries==1){
      trade_seg <- trade_seg %>% 
        mutate(origin=if_else(origin=="Taiwan","China",origin),
               destination=if_else(destination=="Taiwan","China",destination))
    }
    
    # reduce number of countries
    trade_seg <- trade_seg %>% 
      mutate(origin=if_else(origin %in% high_c,origin,
                            paste0("Rest of ",r.origin)),
             destination=if_else(destination %in% high_c,destination,
                                 paste0("Rest of ",r.destination))) %>% 
      group_by(r.origin,origin,r.destination,destination,segment) %>% 
      reframe(qty=sum(qty)) %>% ungroup()
    
    # create matrix
    trade_seg <-trade_seg %>% 
      arrange(destination) %>% arrange(origin) %>% f.trade.matrix()
    
    # Create sales share
    sales.share.matrix_seg <- f.SalesShare(trade_seg)
    
    
    sales.share.matrix_seg <- f.fromto.table(sales.share.matrix_seg,
                                             diag(as.matrix(sales.share.matrix_seg)),
                                             ag_input = F)
    # fix region error
    sales.share.matrix_seg <- sales.share.matrix_seg %>% 
      mutate(r.origin=if_else(is.na(r.origin),str_remove(origin,"Rest of "),r.origin),
             r.destination=if_else(is.na(r.destination),str_remove(destination,"Rest of "),r.destination))
    
    sales.share.matrix_seg$seg <- segm
    
    ratios[[i]] <- sales.share.matrix_seg
    i=i+1
    
  }
  
  ratios <- do.call(rbind, ratios)
  
  ## Play with data
  
  # order of segment by square
  # |S1|S2
  # |S3|S4
  
  # ratios %>% group_by(seg,destination) %>% reframe(x=sum(qty)) %>% 
  # pivot_wider(names_from = seg, values_from = x) %>% view()
  
  ratios <- ratios %>% 
    rename(value=qty) %>% 
    mutate(interval=case_when(
      value<0.01 ~ "0-1%",
      value<0.2 ~ "1-20%",
      value<0.4 ~ "20-40%",
      value<0.6 ~ "40-60%",
      value<0.8 ~ "60-80%",
      T ~ "80-100%") %>% as.factor()) %>% 
    mutate(interval=case_when(
      value<0.01 ~ "0%",
      value<0.13 ~ "1-13%",
      value<0.25 ~ "13-25%",
      value<0.38 ~ "25-38%",
      value<0.5 ~ "38-50%",
      value<0.63 ~ "50-63%",
      value<0.75 ~ "63-75%",
      value<0.88 ~ "75-88%",
      T ~ "88-100%") %>% as.factor()) %>% 
    mutate(interval=case_when(
      value<0.01 ~ "0%",
      value<0.1 ~ "1-10%",
      value<0.2 ~ "10-20%",
      value<0.3 ~ "20-30%",
      value<0.4 ~ "30-40%",
      value<0.5 ~ "40-50%",
      value<0.6 ~ "50-60%",
      value<0.7 ~ "60-70%",
      value<0.8 ~ "70-80%",
      value<0.9 ~ "80-90%",
      T ~ "90-100%") %>% as.factor())
  
  # level_origin <- levels(ratios$interval)
  level_origin <- c("1-10%","10-20%","20-30%","30-40%","40-50%",
                    "50-60%","60-70%","70-80%","80-90%","90-100%")
  
  
  # order by region  and then production
  
  ag_aux <- ag %>% mutate(c=c  %>% str_replace_all("\\/"," "))
  region_order <- c("Europe","North America","South America","Japan/Korea",
                    "Greater China","South Asia","Middle East/Africa")
  
  country_order <- ratios %>% 
    # left_join(ag_aux,by=c("origin"="c")) %>% 
    rename(region=r.origin) %>% 
    group_by(region,origin) %>% 
    summarise(value=sum(value)) %>% ungroup() %>% 
    mutate(last=str_detect(origin,"Rest of")) %>% # rest at the end of regions
    mutate(region=factor(region,levels=region_order)) %>% 
    arrange(region,last,desc(value)) %>% 
    # filter(value>0.01) %>% 
    # top_n(15,value) %>% 
    pull(origin)
  
  
  text_caption <-"Vertical sum = 100% for each Market Segment"
  
  ratios_reg <- ratios %>% 
    # left_join(ag_aux,by=c("origin"="c")) %>% rename(r.source=region) %>% 
    # left_join(ag_aux,by=c("destination"="c")) %>% rename(r.target=region) %>% 
    rename(r.source=r.origin) %>% rename(r.target=r.destination) %>% 
    mutate(region=if_else(r.source==r.target,r.source,"")) %>% 
    filter(region!="")
  
  new_scale <- function(new_aes) {
    structure(ggplot2::standardise_aes_names(new_aes), class = "new_aes")
  }
  
  ggplot_add.new_aes <- function(object, plot, object_name) {
    plot$layers <- lapply(plot$layers, bump_aes, new_aes = object)
    plot$scales$scales <- lapply(plot$scales$scales, bump_aes, new_aes = object)
    plot$labels <- bump_aes(plot$labels, new_aes = object)
    plot
  }
  
  
  # test_countries <- country_order[c(1:10,43,44)]
  countr <- unique(ratios$origin)
  data_fig <- ratios %>% 
    filter(origin %in% country_order) %>%
    filter(destination %in% country_order) %>%
    filter(value>0.01) %>% dplyr::select(-value) %>%
    pivot_wider(names_from = seg, values_from = interval) %>% 
    rename(y=origin,x=destination)
  
  # add missing countries again - only for origin
  miss_c <- setdiff(countr,unique(data_fig$x))
  if (length(miss_c)!=0){
    miss_c <- tibble(r.origin=NA,r.destination=NA,s1=NA,s2=NA,s3=NA,s4=NA,y=miss_c,x=miss_c)
    data_fig <- rbind(data_fig,miss_c)
  }
  
  # limits to draw rectangles
  if (fewCountries==1){
    box_x <- c(0.5,9.5,12.5,15.5,18.5,21.5,25.5)
    box_y <- c(0.5,4.5,7.5,10.5,13.5,16.5,25.5)
    text_x <- c(10.2,9,16.7,18.5,20,20)
    text_y <- c(1,4,11,13,13,22)
  } else {
    box_x <- c(0.5,12.5,15.5,20.5,24.5,30.5,36.5)
    box_y <- c(0.5,6.5,12.5,16.5,21.5,24.5,36.5)
    text_x <- c(11.5,12,22.2,21.2,26,32.5)
    text_y <- c(1,6,13,21,21,26)
  }
  
  size_text <- if (fewCountries==1) 19 else 15
  size <- length(unique(data_fig$x))
  x_seg <- size+1.5
  y_seg <- if (fewCountries==1) 4 else 10
  
  
  p <- ggplot(data_fig,aes(x,y))+
    # boxes first for order
    geom_rect(xmin=box_x[1],xmax=box_x[2],ymin=box_y[6],ymax=box_y[7],linewidth=1,col="brown",fill="white")+
    geom_text(x=text_x[1],y=text_y[6],label="Europe",col="brown")+
    geom_rect(xmin=box_x[2],xmax=box_x[3],ymin=box_y[5],ymax=box_y[6],linewidth=1,col="brown",fill="white")+
    geom_text(x=text_x[2],y=text_y[5],label="North America",col="brown")+
    geom_rect(xmin=box_x[3],xmax=box_x[4],ymin=box_y[4],ymax=box_y[5],linewidth=1,col="brown",fill="white")+
    geom_text(x=text_x[3],y=text_y[4],label="South America",col="brown")+
    geom_rect(xmin=box_x[4],xmax=box_x[5],ymin=box_y[3],ymax=box_y[4],linewidth=1,col="brown",fill="white")+
    geom_text(x=text_x[4],y=text_y[3],label="Asia",col="brown")+
    geom_rect(xmin=box_x[5],xmax=box_x[6],ymin=box_y[2],ymax=box_y[3],linewidth=1,col="brown",fill="white")+
    geom_text(x=text_x[5],y=text_y[2],label="South Asia",col="brown")+
    geom_rect(xmin=box_x[6],xmax=box_x[7],ymin=box_y[1],ymax=box_y[2],linewidth=1,col="brown",fill="white")+
    geom_text(x=text_x[6],y=text_y[1],label="Middle East/Africa",col="brown")+
    # add each tile in their own segment
    geom_tile(color = ifelse(is.na(data_fig$s1), "darkgrey", "black"),
              aes(fill = s1),width = 0.45, height = 0.45, 
              position = position_nudge(x = -0.225, y = 0.225)) +
    geom_tile(color = ifelse(is.na(data_fig$s2), "darkgrey", "black"),
              aes(fill = s2),width = 0.45, height = 0.45, 
              position = position_nudge(x = 0.225, y = 0.225)) +
    geom_tile(color = ifelse(is.na(data_fig$s3), "darkgrey", "black"),
              aes(fill = s3),width = 0.45, height = 0.45, 
              position = position_nudge(x = -0.225, y = -0.225)) +
    geom_tile(color = ifelse(is.na(data_fig$s4), "darkgrey", "black"),
              aes(fill = s4),width = 0.45, height = 0.45, 
              position = position_nudge(x = 0.225, y = -0.225)) +
    # re add for borders
    geom_tile(color = ifelse(is.na(data_fig$s1), alpha("#FF0000", 0), "black"),
              aes(fill = s1),width = 0.45, height = 0.45, 
              position = position_nudge(x = -0.225, y = 0.225)) +
    geom_tile(color = ifelse(is.na(data_fig$s2), alpha("#FF0000", 0), "black"),
              aes(fill = s2),width = 0.45, height = 0.45, 
              position = position_nudge(x = 0.225, y = 0.225)) +
    geom_tile(color = ifelse(is.na(data_fig$s3), alpha("#FF0000", 0), "black"),
              aes(fill = s3),width = 0.45, height = 0.45, 
              position = position_nudge(x = -0.225, y = -0.225)) +
    geom_tile(color = ifelse(is.na(data_fig$s4), alpha("#FF0000", 0), "black"),
              aes(fill = s4),width = 0.45, height = 0.45, 
              position = position_nudge(x = 0.225, y = -0.225)) +
    scale_fill_brewer(palette ="RdYlGn", type="seq", direction=-1, breaks=level_origin,limits=level_origin,
                      labels=function(x) format(x, big.mark = " ", scientific = FALSE)) +
    # scale_fill_gradient(colors = brewer.pal(9, "YlGnBu"), 
    #                      breaks = level_origin,
    #                      labels = function(x) format(x, big.mark = " ", scientific = FALSE))+
    # fake to expand scale
    annotate("rect", xmin = x_seg +3.5, xmax = x_seg + 3.5, ymin = y_seg + 3.5, ymax = 3.5, color = "white", fill = "white") +
    # manual legend for segments
    annotate("rect", xmin = x_seg - 0.5, xmax = x_seg + 1, ymin = y_seg + 1, ymax = y_seg + 3, color = "black", fill = "white") +
    annotate("text", x = x_seg+0.25, y =y_seg + 2, label = "S1", vjust = 0.5, hjust = 0.5,size=7) +
    annotate("rect", xmin = x_seg + 1, xmax = x_seg + 2.5, ymin = y_seg + 1, ymax = y_seg + 3, color = "black", fill = "white") +
    annotate("text", x = x_seg + 1.75, y = y_seg + 2, label = "S2", vjust = 0.5, hjust = 0.5,size=7) +
    annotate("rect", xmin = x_seg - 0.5, xmax = x_seg + 1, ymin = y_seg-1, ymax = y_seg + 1, color = "black", fill = "white") +
    annotate("text", x = x_seg+0.25, y = y_seg, label = "S3", vjust = 0.5, hjust = 0.5,size=7) +
    annotate("rect", xmin = x_seg + 1, xmax = x_seg + 2.5, ymin = y_seg-1, ymax = y_seg + 1, color = "black", fill = "white") +
    annotate("text", x = x_seg + 1.75, y = y_seg , label = "S4", vjust = 0.5, hjust = 0.5,size=7)+
    annotate("text", x = x_seg+1, y = y_seg + 4, label = "Segment", vjust = 0.5, hjust = 0.5,size=7)+
    labs(x="Country Target",y="Source",fill="% Origin",
         caption=paste0(text_caption,"."))+
    xlim(country_order)+ylim(rev(country_order))+
    # add vertical lines (box frame) manually
    annotate(geom = "segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf, color = "black") +
    annotate(geom = "segment", x = size+0.5, xend = size+0.5, y = -Inf, yend = Inf, color = "black") +
    annotate(geom = "segment", x = -Inf, xend = size+0.5, y = -Inf, yend = -Inf, color = "black") +
    annotate(geom = "segment", x = -Inf, xend = size+0.5, y = Inf, yend = Inf, color = "black") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1,size=size_text),
          axis.text.y = element_text(size=size_text),
          legend.position = c(0.95,0.7),
          panel.border = element_blank())
  
  if(getData){
    return(list(data_fig=data_fig,country_order=country_order))
  } else{
    return(p)
  }
}
