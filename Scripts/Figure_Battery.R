# Function of battery size
# PBH  August 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")



batsize_region %>% 
  mutate(segment=segment %>% str_replace("s","Segment ") %>% 
           factor(levels=rev(c("Segment 1","Segment 2","Segment 3","Segment 4")))) %>% 
  mutate(region=factor(region,levels=rev(level_region))) %>% 
  ggplot(aes(region,kwh_veh,fill=segment))+
  geom_col(position = "dodge")+
  coord_flip(expand = F,ylim = c(0,100))+
  guides(fill= guide_legend(reverse = TRUE))+
  scale_fill_viridis_d(option = "D")+
  labs(x="",y="Battery capacity [kWh per BEV]",fill="")+
  theme_bw(8)+
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())

ggsave("Figures/batsize.png",dpi=600,
       units = "cm",width = 8.2,height=6)
