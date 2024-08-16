# Creating a function to filter the from to table given prompts
# FP 2023

f.fig.sankigenerator <- function(fromto,region.type=0,
                                 location="",mtr=0,
                                 lab="Light Duty EV",
                                 year=2035,
                                 colorFlow="None",
                                 source_pos=0,target_pos=0, # optional
                                 scenario="",title="XYZ",
                                 lower_limit_qty=10,
                                 y_anot=c(0.9,0.1,0.9,0.1)){
  
  
  if (mtr==0){
    mtr <- as.numeric(readline(prompt = "Indicate if you'd like to visualize units(1), battery capacity requirements(2) or minerals (3)"))
  }
  

  #Changes the names of the columns in fromto
  # fromto <- fromto %>% rename(r.source=r.origin, c.source=origin,
  #                             r.target=r.destination,c.target=destination)
  
  unit_label <- paste0(" ",lab," units")
  if (mtr == 2) {
    #Change qty variable to battery
    fromto$qty <- fromto$MWh
    unit_label <- paste0(" ",lab," Battery MWh")
    lab <- paste0(lab," Battery MWh")
  } 
  
  if(mtr == 3){
    #Change qty variable to mineral
    fromto$qty <- fromto$mineral_tons
    unit_label <- paste0(" ",lab," Mineral tons")
    lab <- paste0(lab," Mineral tons")
  }
  
  # Prompt for a type of location
  if (region.type==0){
    region.type<- as.numeric(readline(prompt = "Select indicate if you'd like to explore a Region(1) or a Country(2) "))
  } 
  
  count_title=0 # used later
  
  if (region.type==1) print(unique(ag$region))

    if (location[1]==""){
      location<- readline(prompt = "please copy and paste the location you're interested in and hit enter ")
    }
    #################  EXTRACTS THE APPROPRIATE DATA TO GENERATE A REGIONAL SANKI   #################
    
  # Country filter - Idea change region name to selected countries, rest of the code should work
  if (region.type==2){
    fromto <- fromto %>% 
      mutate(r.origin=if_else(origin %in% location,origin,r.origin),
             r.destination=if_else(destination %in% location,destination,r.destination))
    
    # If title is equal to a region name, aggregate all countries to the region
    if(title %in% unique(fromto$r.origin)){
      fromto <- fromto %>% 
        mutate(r.origin=if_else(r.origin==title,paste0("Rest of ",title),r.origin),
               r.destination=if_else(r.destination==title,paste0("Rest of ",title),r.destination))
      count_title=1
    }
    
    }
    
    if(colorFlow=="None"){
      fromto$seg <- "All"
    }
    if (colorFlow == "Mineral"){ 
      fromto$seg <- fromto$Mineral # Need to use fromto data with Mineral detail 
    }
    if (colorFlow == "Chemistry"){
      fromto$seg <- fromto$chemistry
    }
  
    
    # Show Sankey by World regions
    if(region.type==3){
      df <- fromto %>% 
        mutate(source=r.origin,
               target=r.destination) %>% 
        group_by(source,target,seg) %>% 
        summarise(qty=sum(qty,na.rm=T)) %>% ungroup() %>% 
        arrange(desc(qty)) %>% 
        arrange(seg)
    } else {
      
      # create countries, rest as regions
      df <- fromto %>% 
        mutate(order_source=if_else(r.origin %in% location|str_detect(r.origin,title),0,1),
               order_target=if_else(r.destination %in% location|str_detect(r.destination,title),1,0)) %>% 
        mutate(source=if_else(r.origin %in% location,origin,r.origin),
               target=if_else(r.destination %in% location,destination,r.destination)) %>% 
        filter(r.origin %in% location|r.destination %in% location|
                 str_detect(r.origin,title)|str_detect(r.destination,title)) %>% # rest of Zone
        filter(qty>lower_limit_qty) %>%  # at least certain flow
        # group_by(order_source,order_target,source,target) %>% 
        group_by(order_source,order_target,source,target,seg) %>% 
        summarise(qty=sum(qty,na.rm=T)) %>% ungroup() %>% 
        arrange(desc(qty)) %>% 
        arrange(desc(order_target)) %>% 
        arrange((order_source)) %>% 
        arrange(seg)  # order flows by segments
      
    }
  
  
    # Total Origin by country
    df_orig <- df %>% 
      group_by(order_source,source) %>% 
      summarise(qty=sum(qty,na.rm=T)) %>% ungroup() %>% 
      mutate(labelK=case_when(
        qty>1e6 ~ paste0(round(qty/1e6,2),"M"),
        qty>1e3 ~ paste0(round(qty/1e3,0),"K"),
        T ~ paste0(round(qty,0),""))) %>% 
      mutate(label=paste0(source," (",labelK,")")) %>% 
      mutate(label_c=source) %>% # label without number
      mutate(source=paste0("s.",source)) %>% rename(name=source) %>% 
      arrange(desc(qty)) %>% arrange(order_source) %>% 
      mutate(order_source=NULL)
    
    # needs to order again  
    order_source <- df_orig$name
    
    # Total target by country
    df_target <- df %>% 
      group_by(order_target,target) %>% 
      summarise(qty=sum(qty,na.rm=T)) %>% ungroup() %>% 
      mutate(labelK=case_when(
        qty>1e6 ~ paste0(round(qty/1e6,2),"M"),
        qty>1e3 ~ paste0(round(qty/1e3,0),"K"),
        T ~ paste0(round(qty,0),""))) %>% 
      mutate(label=paste0(target," (",labelK,")")) %>% 
      mutate(label_c=target) %>%  # label without number
      mutate(target=paste0("t.",target)) %>% rename(name=target) %>% 
      arrange(desc(qty)) %>% arrange(desc(order_target)) %>% 
      mutate(order_target=NULL)
    order_target <- df_target$name
    
    df_label <- rbind(df_orig,df_target); rm(df_orig,df_target);
    
    
    df <- df %>% 
      mutate(labelK=case_when(
        qty>1e6 ~ paste0(round(qty/1e6,2),"M"),
        qty>1e3 ~ paste0(round(qty/1e3,0),"K"),
        T ~ paste0(round(qty,0),""))) %>% 
      mutate(source=paste0("s.",source),
             target=paste0("t.",target))
    # mutate(source=paste0("s.",source,"-",seg),
    #        target=paste0("t.",target,"-",seg))
    
    # df is LINKS
    
    # create NODES
    nodes <- data.frame(name=c(order_source,order_target)) %>% 
      rownames_to_column()
    
    # convert linkes to numbers (integers)
    links <- df %>% left_join(nodes,by=c("source"="name")) %>% 
      rename(source_name=source) %>% rename(source=rowname) %>% 
      left_join(nodes,by=c("target"="name")) %>% 
      rename(target_name=target) %>% rename(target=rowname)
    
    nodes$rowname <- NULL; links$source_name <- NULL; links$target_name <- NULL
    links <- as.data.frame(links)
    links <- links %>% relocate(qty,.after = target)
    links$qty <- as.numeric(links$qty)
    links$source <- as.integer(links$source)-1
    links$target <- as.integer(links$target)-1
    
    # Add new name with total to nodes
    nodes <- nodes %>% left_join(df_label)
    
    # Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
    # head(links)
    # head(nodes)
    
    
    # create a color palette with n colors, where n is the number of unique values in s.index
    n_country <- nodes$name %>% str_remove_all("s\\.|t\\.") %>% unique()
    colors <- brewer.pal(length(n_country), "Paired")
    if (length(n_country)>12){
      colors <- rep(colors,ceiling(length(n_country)/length(colors)))
      colors <- colors[1:length(n_country)]
    }
    colors <- data.frame(name_country=n_country,n.color=colors)
    
    # add color
    nodes <- nodes %>% 
      mutate(name_country=name %>% str_remove_all("s\\.|t\\.")) %>% 
      left_join(colors)
    nodes$name_country <- NULL
    
    # Colors for segment and for legend
    if(colorFlow=="Segment"){
      links <- links %>% 
        left_join(data.frame(
          seg=c("s4","s3","s2","s1"),
          # colors=c("#3288bd", "#99d594", "#e6f598", "#fee08b")))
          # colors=c("#2E4053", "#E67E22", "#3498DB", "#27AE60")))
          colors=c("#2E4053E6", "#3498DBE6", "#E67E22E6", "#27AE60E6")))

      leg_color <- "<span style='font-weight:bold;color:#27AE60'>Segment 1</span><br><span style='font-weight:bold;color:#E67E22'>Segment 2</span><br><span style='font-weight:bold;color:#3498DB'>Segment 3</span><br><span style='font-weight:bold;color:#2E4053'>Segment 4</span>"
    } 
    else if(colorFlow=="Mineral") {
      links <- links %>% 
        left_join(data.frame(
          seg=c("Lithium","Cobalt","Nickel"),
          colors=c("#FFC0CB","#0047AB","#8C8C8C")))
      
      leg_color <- "<span style='font-weight:bold;color:#0047AB'>Cobalt </span><br><span style='font-weight:bold;color:#FFC0CB'>Lithium </span><br><span style='font-weight:bold;color:#8C8C8C'>Nickel </span>"
    }
    else if(colorFlow=="Chemistry"){
      links <- links %>% 
        left_join(data.frame(
          seg=c("LFP", "LMO", "NCA","NMC", "NMC 111", "NMC 532", "NMC 622", "NMC 811", "Other"),
          colors=c("#8B4513", "#800080", "#0047AB","#FF4500", "#008000", "#800000", "#800080", "#FFA500", "#D3D3D3")))
      
      leg_color <- "<span style='font-weight:bold;color:#8B4513'>LFP </span><br><span style='font-weight:bold;color:#800080'>LMO </span><br><span style='font-weight:bold;color:#0047AB'>NCA </span><br><span style='font-weight:bold;color:#FF4500'>NMC </span><br><span style='font-weight:bold;color:#008000'>NMC 111 </span><br><span style='font-weight:bold;color:#800000'>NMC 532 </span><br><span style='font-weight:bold;color:#800080'>NMC 622 </span><br><span style='font-weight:bold;color:#FFA500'>NMC 811 </span><br><span style='font-weight:bold;color:#D3D3D3'>Other </span>"
      
      }
    else {
      links$colors <- "#D3D3D3"
    }
    
    hover_lab <- if (colorFlow=="None") "Segment" else colorFlow
    
    # get order
  
      # source countries
    world_reg <- ag$region %>% unique()
    s.n_country <- nodes %>% filter(str_detect(name,"s\\.")) %>% pull(name)
    s.n_country_reg <- s.n_country %>% str_remove_all("s\\.|t\\.") %>% 
      tibble(x=.) %>% filter(x %in% world_reg) %>% nrow() # number of regs
    s.n_country <- length( s.n_country)
    s.n_country_no_reg <- s.n_country-s.n_country_reg
    
    # target countries
    t.n_country <- nodes %>% filter(str_detect(name,"t\\.")) %>% pull(name)
    t.n_country_reg <- t.n_country %>% str_remove_all("s\\.|t\\.") %>% 
      tibble(x=.) %>% filter(x %in% world_reg) %>% nrow() # number of regs
    t.n_country <- length( t.n_country)
    t.n_country_no_reg <- t.n_country-t.n_country_reg
    
    # Position and order
    # adjust countries in region of interest - equal column separation
    x_pos <- c(rep(0.1+0.2,s.n_country_no_reg),rep(0.1,s.n_country_reg), # source
               rep(0.9-0.2,t.n_country_no_reg),rep(0.9,t.n_country_reg)) # target
    
    # % based on qty
    s.qty_perc <- nodes %>% 
      filter(str_detect(name,"s\\.")) %>% 
      mutate(qty_perc=qty/sum(qty)) %>% pull(qty_perc)
    t.qty_perc <- nodes %>% 
      filter(str_detect(name,"t\\.")) %>% 
      mutate(qty_perc=qty/sum(qty)) %>% pull(qty_perc)
    
    # need to account for separation
    sep_node <- 0.033 # node relative size in separation
    # n_country*sep_node # pixels lost in separation only
    
    # modify position if required
    if (source_pos==0){
      # Key idea: Get total share of flows, plus the first one for position issues 
      y_source_pos <- (sum(s.qty_perc[(s.n_country_no_reg+1):s.n_country])+
                         0.8*max(s.qty_perc[1:s.n_country_reg]))* # add first position
        # n_country*sep_node+n_country_no_reg*sep_node # remove space and add corresponding to number of countries
        (1-s.n_country*sep_node)+ # get relative proportion without separation
        (s.n_country_no_reg-1)*sep_node #add separation
       
      } else {
      y_source_pos <- source_pos
    }
    
    if (target_pos==0){
      y_target_pos <- sum(t.qty_perc[(1):(t.n_country_reg)])*
        (1-t.n_country*sep_node)+
        t.n_country_reg*sep_node
    } else {
      y_target_pos <- target_pos
    }
    
    # print to know
    cat("Taget pos: ",y_target_pos)
    cat("Source pos: ",y_source_pos)
    
    # adjust vertically based on their relative size
    y_pos <- c(rep(y_source_pos,s.n_country_no_reg),rep(0.01,s.n_country_reg), # source
               rep(0.01,t.n_country_no_reg),rep(y_target_pos,t.n_country_reg)) # target
    
    
    # https://plotly.com/r/sankey-diagram/
    p <- plot_ly(type = "sankey", valuesuffix = unit_label,
                 # orientation = "h", 
                 arrangement="snap",
                 # scale = 83.8 * 17780.0, # dpi=300, for width 3.3in
                 # arrangement="freeform",
                 node = list(label = nodes$label,
                             # text = c(nodes$labelK),
                             customdata=nodes$label_c,
                             textfont = list(size = 18, color = "black"), # not serif is 12
                             color = c(nodes$n.color),
                             x=x_pos,
                             y=y_pos,
                             pad = 24.5 # 0.03 perc
                 ),
                 link = list(
                   source = c(links$source),
                   target = c(links$target),
                   value =  c(links$qty),
                   color = c(links$colors),
                   customdata = paste0(links$seg,"<br>Flow: ",links$labelK),
                   hovertemplate = paste0(hover_lab,": %{customdata}<br>Source: %{source.customdata}<br>Target: %{target.customdata}")
                 ),
    )
    
    # Add title and annotations
    title <- if (title=="XYZ") location else title
    p <- p %>% 
      layout(title=paste0("<b>",title," ",lab," ",year," flows. \n",scenario,"</b>"),
             font = list(size = 24,family="serif"),
             # font = list(size = 18),
             showlegend=T,
             margin=list(t=80),
             legend = list(
               orientation = "h",
               x = 0.5, y = -0.2,
               bgcolor = "white",
               bordercolor = "black",
               borderwidth = 1
             )) %>% 
      add_annotations(
        x = c(-0.04,0.12,0.8,1.01),
        y = y_anot,
        # xshift = c(25, -25),
        text = c("<b>Imports</b>", "<b>Production</b>","<b>Sales</b>","<b>Exports</b>"),
        font = list(size = 27), # not serif is 20
        showarrow = FALSE,
        textangle = c(0, 0)
      )
    
    if(colorFlow %in% c("Segment","Mineral","Chemistry")){
      # Add Legend Manually
      p <- p %>% 
        add_annotations(
          x = 0.1,
          y = 0.3,
          xref = "paper",
          yref = "paper",
          text = leg_color,
          showarrow = FALSE,
          align = "left",
          font = list(size = 24) # not serif is 18
        )
    }
    
    
    return(p)
    
}


# Function to save multiple versions of a Sankey - With battery and segment color
f.save.sankey <- function(name="",
                          fromto,region.type=0,
                           location="",
                           lab="Light Duty EV",
                           year=2035,
                           source_pos=0,target_pos=0, # optional
                           scenario="",title="XYZ"){
  
  # save to other folder - replace pattern T1/T1... with T1/sankey/T1...
  fig_name_aux <- gsub("(T\\d+)/T\\d", paste("\\1/sankey/\\1", sep = ""), fig_name, ignore.case = TRUE)
  fig_name_aux <- fig_name_aux %>% str_replace("T2b\\/","T2b/sankey/") # 2b special case
  
  # # Vehicles no Segment
  p <- f.fig.sankigenerator(fromto,region.type,location,
                            mtr=1,colorFlow = "None", # Only parameters to change
                            lab=lab,scenario=scenario,
                            title=title,
                            source_pos=source_pos,target_pos=target_pos)
  export(p,sprintf(fig_name_aux,paste0("sankey_",name)))
  
  # Vehicles Segment
  # p <- f.fig.sankigenerator(fromto,region.type,location,
  #                           mtr=1,colorFlow = "Segment, # Only parameters to change
  #                           lab=lab,scenario=scenario,
  #                           title=title,
  #                           source_pos=source_pos,target_pos=target_pos)
  # export(p,sprintf(fig_name_aux,paste0("sankey_",name,"_seg")))
  
  # if (file.exists("ldv_run")){
  #   if (!ldv_run){
  #     # Battery no Segment
  #     p <- f.fig.sankigenerator(fromto,region.type,location,
  #                               mtr=2,colorFlow = "None, # Only parameters to change
  #                               lab=lab,scenario=scenario,
  #                               title=title,
  #                               source_pos=source_pos,target_pos=target_pos)
  #     export(p,sprintf(fig_name_aux,paste0("sankey_",name,"_bat")))
  #   
  #     # Battery Segment
  #     p <- f.fig.sankigenerator(fromto,region.type,location,
  #                               mtr=2,colorFlow = "Segment, # Only parameters to change
  #                               lab=lab,scenario=scenario,
  #                               title=title,
  #                               source_pos=source_pos,target_pos=target_pos)
  #     export(p,sprintf(fig_name_aux,paste0("sankey_",name,"_bat_seg")))
  #   }
  # }
}


