#HEAT MAP FUNCTION
# FP 2023

f.fig.heatmap <- function(fromto,transf=0){
  
  hm.name <- deparse(substitute(fromto))  
  maxval<- as.character(formatC(floor(max(abs(fromto$qty))), format = "d", big.mark = ","))
  fromto$qty <- if (transf == 0) fromto$qty else sqrt(fromto$qty)
  
  # Plot the heatmap
  hm.plot<-ggplot(fromto, aes(x = destination , y = origin, fill = abs(qty))) +
    geom_tile() +
    scale_fill_gradient(low = "white", high = "red", 
                        guide = "colorbar", 
                        breaks = c(0, max(abs(fromto$qty))), 
                        labels = c("0", maxval)) +
    labs(x = "Destination", y = "Origin", title = hm.name, 
         fill = "Resid") +
    theme(axis.text.y = element_text(size = 7), 
          axis.text.x = element_text(size = 8,angle = 90,hjust = 1, vjust = 0))
  
  return(hm.plot)
}
