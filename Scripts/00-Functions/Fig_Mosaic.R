# Figure- Mosaic Chart
# https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
# PBH March 2023

f.fig.mosaic <- function(dataStats,xlab="Light Duty EV",
                         includeChina=T,ev=T,yt=2035,
                         prodB=T,
                         scenario=""){
  
  xlab <- paste0(xlab," Production ",yt)
  
  if(prodB==F){
    xlab <- paste0(xlab," Sales ",yt)
    dataStats$p <- dataStats$s # sales instead
  }
  
  
  if(ev==F){
    # change variables to re-use code easily
    dataStats$p <- dataStats$MWh
    xlab <- paste0(xlab," Battery Supply Requirement ",yt," [MWh] (for EV production)")
  }
  
  
  prod <- dataStats %>% left_join(ag) %>% 
    dplyr::select(c,region,p) %>% filter(p>0)  %>% 
  filter(c!="China"|includeChina)
  
  # create stats for display
  prod <- prod %>% group_by(region) %>% 
    mutate(p_total = sum(p),
           p_prop = p/sum(p)) %>%
    ungroup() %>% 
    mutate(p_share=p/sum(p)) %>% 
    mutate(p_label=if_else(p_share>0.01,paste0(c,": ",format(round(p/1e6,1),big.mark=","),"M"),""))
  
  
  reg_order <- prod %>% group_by(region) %>% summarise(p=sum(p)) %>% 
    arrange(desc(p)) %>% pull(region)
  prod <- prod %>% mutate(region=factor(region,levels=reg_order))
  c_order <- prod %>% group_by(region,c) %>% summarise(p=sum(p)) %>% 
    arrange(desc(p)) %>% pull(c)
  prod <- prod %>% mutate(c=factor(c,levels=c_order))
  
  ggplot(prod,
         aes(x = region, y = p_prop, width = p_total, fill = c)) +
    geom_bar(stat = "identity", position = "fill", colour = "black") +
    geom_text(aes(label = p_label), position = position_stack(vjust = 0.5),size=20*5/14 * 0.8) + # if labels are desired
    facet_grid(~region, scales = "free_x", space = "free_x") +
    # scale_fill_brewer(palette = "RdYlGn") +
    # scale_fill_viridis_d(direction=-1)+
    labs(title=xlab,caption=scenario)+
    theme_void(20)+
    theme(legend.position="none",
          panel.spacing.x = unit(0, "npc")) # if no spacing preferred between bars
}

