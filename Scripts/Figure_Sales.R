# Figure for forecasted 2035 sales 
# PBH December 2023


# Load data -----
source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Sales ------------

# Future EV Sales - BEV only
EV_sales <- read.csv("Inputs/ICCT_MONET_2035.csv")
EV_sales <- EV_sales %>% filter(Scenario=="Ambitious")
sum(EV_sales$sales)/1e6 # 94.5 M


# Figure 2035 Sales -----
head(EV_sales)

# create stats for display
white_c <- c("DEU","KOR","AUS","PAK","REU","RUS","FRA","GBR",
             "ESP","IRN","CHN","CAN","MEX","USA")

data_fig <- EV_sales %>% 
  left_join(ag) %>% 
  group_by(region) %>% 
  mutate(s_total = sum(sales)/1e6,
         s_prop = sales/sum(sales)) %>%
  ungroup() %>% 
  mutate(region=paste0(region," ",format(round(s_total,1),big.mark=","),"M")) %>% 
  mutate(s_share=sales/sum(sales)) %>% 
  mutate(s_label=if_else(s_share>0.017,paste0(iso,":",format(round(sales/1e6,1),big.mark=","),"M"),"")) %>% 
  mutate(s_label2=if_else(s_share>0.007 & s_share<0.017,paste0(iso,":",format(round(sales/1e6,1),big.mark=","),"M"),"")) %>% 
  mutate(s_label_white=if_else(iso %in% white_c & s_share>0.017,paste0(iso,":",format(round(sales/1e6,1),big.mark=","),"M"),"")) %>% 
  mutate(s_label_white2=if_else(iso %in% white_c & s_share>0.007 & s_share<0.017,paste0(iso,":",format(round(sales/1e6,1),big.mark=","),"M"),""))



reg_order <- data_fig %>% group_by(region) %>% summarise(sales=sum(sales)) %>% 
  arrange(desc(sales)) %>% pull(region)
data_fig <- data_fig %>% mutate(region=factor(region,levels=reg_order))
c_order <- data_fig %>% group_by(region,c) %>% summarise(sales=sum(sales)) %>% 
  arrange((sales)) %>% pull(c)
data_fig <- data_fig %>% mutate(c=factor(c,levels=c_order))


ggplot(data_fig,
       aes(y = region, x = s_prop, width = s_total, fill = c)) +
  geom_bar(stat = "identity", position = "fill", colour = "black") +
  geom_text(aes(label = s_label), position = position_stack(vjust = 0.5),size=8*5/14 * 0.8) + # if labels are desired
  geom_text(aes(label = s_label2), position = position_stack(vjust = 0.5),size=6*5/14 * 0.8) + # if labels are desired
  # white fonts
  geom_text(col="white",aes(label = s_label_white), position = position_stack(vjust = 0.5),size=8*5/14 * 0.8) + # if labels are desired
  geom_text(col="white",aes(label = s_label_white2), position = position_stack(vjust = 0.5),size=6*5/14 * 0.8) + # if labels are desired
  facet_grid(region~., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = c("China" = "#FF0000", "United States" = "#3C3B6E", "Germany" = "#000000",
                               "France" = "#0055A4", "United Kingdom" = "#00247D", "Italy" = "#008C45",
                               "Spain" = "#AA151B", "Canada" = "#FF0000", "Mexico" = "#006847",
                               "India" = "#FF9933", "Thailand" = "#A51931", "Brazil" = "#009739",
                               "Venezuela" = "#FFD700", "Japan" = "#BC002D", "South Korea" = "#003478", 
                               "Poland" = "#D73B3E", "Rest of EU" = "#003399", "Russia" = "#005BBB",
                               "Saudi Arabia" = "#006C35", "Pakistan" = "#00401A", 
                               "Iran" = "#DA0000", "Indonesia" = "#D10000",
                               "Australia" = "#002868","Rest of Middle East/Africa" = "#F4A300", 
                               "Rest of South America" = "#8E44AD", "Rest of South Asia/Oceania" = "#F39C12",
                               "Malaysia" = "#FFCC00",
                               "Other" = "#808080"))+
  theme_void(8)+
  theme(legend.position="none",
        strip.placement = "outside",
        panel.spacing.y = unit(0, "npc")) # if no spacing preferred between bars


ggsave("Figures/2035EVDemand.png",dpi=600,units = "cm",
       width = 18.4,height = 14)


# EoF