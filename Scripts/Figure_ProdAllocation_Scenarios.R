# MONET Analysis of the results of the different production allocation scenarios
# PBH August 2024

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# NOTE: Need to run the codes to generate the results a priori

# custom function to extract only results for production and battery, for South Korea and Japan
f.get.prod <- function(url_location){
  supply.stat <- read.csv(paste0(url_location,"/supplystats.csv")) %>% 
    filter(c %in% c("South Korea","Japan")) %>% 
    dplyr::select(c,seg,p,MWh,MWh.ds,MWh.de) %>% 
    mutate(folder=url_location)
  return(supply.stat)
}

# Load all results ----------
folders <- c("Results",
             "Results/Demand Scenarios/Low","Results/Demand Scenarios/High",
             "Results/Trade Ratios Scenarios/Regional Higher Domestic Supply",
             "Results/Trade Ratios Scenarios/Global Free Trade",
             "Results/MarketSegment Scenarios/s1s2",
             "Results/MarketSegment Scenarios/s3s4",
             "Results/Battery Scenarios/Low","Results/Battery Scenarios/High")
names <- c("Baseline",
           "Low EV Sales","High EV Sales",
           "High Region Domestic Supply","Global Free Trade",
           "High Segment 1 and 2","High Segment 3 and 4",
           "Low Battery Capacity","High Battery Capacity")

# Load all together and join them
df <- lapply(folders, f.get.prod)
df <- do.call(rbind,df) %>% 
  left_join(tibble(folder=folders,name=names)) %>% 
  mutate(name=factor(name,levels=rev(names)))

# Figure ----------

# By Market Segment
df %>% 
  mutate(seg=factor(seg,levels=paste0("s",4:1))) %>% 
  mutate(Gwh=MWh/1e3) %>% 
  ggplot(aes(name,Gwh))+
  geom_col(aes(fill=seg))+
  facet_wrap(~c)+
  coord_flip()+
  scale_fill_manual(values=(c("#2E4053E6", "#3498DBE6", "#E67E22E6", "#27AE60E6")))+ #90% alpha
  labs(x="",y="Battery Requirements [GWh]",fill="Market Segment")+
  theme_bw(8)+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        panel.grid = element_blank())

ggsave("Figures/BatScenarios_Seg.png",dpi=600,units = "cm",
       width = 11,height = 8.4)
  
# By Domestic need and for Exports
df %>% 
  rename("Domestic Supply"=MWh.ds,"Exports"=MWh.de) %>% 
  pivot_longer(c(`Domestic Supply`,Exports), names_to = "key", values_to = "value") %>% 
  mutate(Gwh=value/1e3) %>%
  mutate(key=factor(key,levels=rev(c("Domestic Supply","Exports")))) %>% 
  ggplot(aes(name,Gwh))+
  geom_col(aes(fill=key))+
  facet_wrap(~c)+
  coord_flip()+
  labs(x="",y="Battery Requirements [GWh]",fill="Battery Destination")+
  scale_fill_manual(values = c("Domestic Supply" = "#1f77b4", "Exports" = "#ff7f0e"))+
  theme_bw(8)+
  guides(fill= guide_legend(reverse = TRUE))+
  theme(legend.position = "bottom",
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm'),
        panel.grid = element_blank())

ggsave("Figures/BatScenarios.png",dpi=600,units = "cm",
       width = 11,height = 8.4)
