## Figure: Sales vs Production
## PBH
## February 2023

# Scatter Plot showing country level sales vs Production
# Sales should be called s and production p, country labels is c
f.fig.prodSales <- function(dataStats, incZeroProd=T, 
                            year=2035,
                            lab="Light Duty EV",
                            scenario=""){
  dataStats %>%
  filter(p>0|incZeroProd) %>%
    # filter(Flag_Code!="") %>% 
    ggplot(aes(s,p))+
    # geom_point(size=6,shape=16)+
    # geom_point(aes(size=ds))+
    geom_point(size=2)+
    # geom_flag(aes(country=Flag_Code))+
    # scale_country() +
    geom_text_repel(aes(label=c),size=20*5/14 * 0.8)+
    geom_abline(slope=1, intercept = 0,linetype="dashed")+
    scale_x_log10(labels = label_comma(drop0trailing = TRUE))+
    scale_y_log10(labels = label_comma(drop0trailing = TRUE))+
    labs(caption=paste0("Note: Log base 10 scale. ",scenario),
         x=paste0(lab," Domestic Sales ",year),
         y=paste0(lab," Production ",year))+
    theme(legend.position = "none")
}

# EoF