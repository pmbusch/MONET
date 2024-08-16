# Share figure for sales and production by Segment
## PBH June 2023

source("Scripts/00-Libraries.R", encoding = "UTF-8")

# Load data -----
sales <- read.csv("Inputs/segment_sales_share - MONET.csv")
names(sales)[2:5] <- paste0("S",1:4)

ag <- read.csv("Inputs/ag.csv") 
ag <- ag %>% mutate(c=str_replace_all(c,"\\/"," "))

# Fix names
sales <- sales %>% mutate(c=c %>% str_replace_all("\\/"," "))

# Prepare data ----
sales <- sales %>% pivot_longer(c(-c), names_to = "Segment", values_to = "share_s")

# Labels
df_fig <- sales %>% 
  mutate(s_text=if_else(share_s>0.105,paste0(round(share_s*100,0),"%"),""))

df_fig <- df_fig %>% 
  mutate(seg=Segment,
         Segment=case_when(
           seg=="S1" ~ "S1 - Small (< 4.1m)",
           seg=="S2" ~ "S2 - Compact (4.1 - 4.6m)",
           seg=="S3" ~ "S3 - Mid (4.6 - 4.8m)",
           seg=="S4" ~ "S4 - Large (> 4.8m)",
           T ~"na"),
         Segment=case_when(
           seg=="S1" ~ "S1 - Mini & Small",
           seg=="S2" ~ "S2 - Compact",
           seg=="S3" ~ "S3 - Mid",
           seg=="S4" ~ "S4 - Large & X-Large",
           T ~"na"))

df_fig$Segment %>% table()

df_fig <- df_fig %>% left_join(ag) %>% 
  mutate(region=factor(region,levels=level_region))

# order by S1 share
df_s1 <- df_fig %>% filter(seg=="S1") %>% rename(share_s1=share_s) %>% dplyr::select(c,share_s1)
df_fig <- df_fig %>% left_join(df_s1)

# Figure -----


p <- df_fig %>% 
  ggplot(aes(reorder(c,share_s1),share_s,fill=fct_rev(Segment),label=s_text))+
  geom_col() +
  # geom_text(position = position_stack(vjust = 0.5),size=font_size*5/14 * 0.8)+
  ggforce::facet_col(facets = vars(region), 
                     scales = "free_y", 
                     space = "free") +
  coord_flip(expand = F)+
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=c("#2E4053E6", "#3498DBE6", "#E67E22E6", "#27AE60E6"))+ #90% alpha
  labs(x="",y="Sales Share %",fill="Segment")+
  guides(fill = guide_legend(reverse = TRUE,nrow=2)) +
  theme_bw(7)+
  theme(legend.position = "bottom",
        # ,text = element_text(family = "serif") 
        axis.text.y = element_text(size=3),
        strip.text = element_text(margin = margin(t = 2, b = 2)),
        legend.key.height= unit(0.25, 'cm'),
        legend.key.width= unit(0.25, 'cm')
        )
p

ggsave("Figures/prop_sales.png",
       units="cm",dpi=600,
       height = 12.4,width = 12.4)



# EoF