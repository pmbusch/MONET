# Generating from->to table of anything (Volumes, Residuals, Etc)
# FP 2023

ag <- read.csv("Inputs/ag.csv")
level_region <- c("North America","South America","Europe","Middle East/Africa",
                   "South Asia","Greater China","Japan/Korea")


f.fromto.table <- function(matrix,mdiag=0,ag_input=T){
  
  # with correct column names
  ag_aux <- ag %>% mutate(c=c %>% str_replace_all("\\/"," "))
  ag_aux$region_map <- NULL
  ag_aux$iso <- NULL
  # 
  m.name <- deparse(substitute(matrix))
  fromto.m<-matrix
 diag(fromto.m)<-mdiag
 
  colnames(fromto.m)<- gsub("\\.", " ", colnames(fromto.m))
  if (ag_input){
    fromto.m$origin<-ag_aux$c
  } else{
    fromto.m$origin<-colnames(fromto.m)
  }

  
  fromto <- melt(fromto.m,id.vars = "origin")
  # fromto <- melt(fromto.m)
  colnames(fromto)<-c("origin","destination","qty")
  
  # add region for origin and destination
  fromto <- fromto %>% left_join(ag_aux,by=c("origin"="c")) %>% 
    rename(r.origin=region)
  fromto <- fromto %>% left_join(ag_aux,by=c("destination"="c")) %>% 
    rename(r.destination=region)
  
  fromto<-fromto %>% 
    relocate(r.origin) %>% 
    relocate(r.destination,.before = destination)

  return(fromto)
  
}

f.fromto.table2 <- function(matrix,ag_input=T){
  
  # with correct column names
  ag_aux <- ag %>% mutate(c=c %>% str_replace_all("\\/"," "))
  ag_aux$region_map <- NULL
  ag_aux$iso <- NULL
  
  m.name <- deparse(substitute(matrix))
  fromto.m<-matrix

  rownames(fromto.m) <- ag_aux$c
  colnames(fromto.m) <- ag_aux$c
  
  
  fromto <- melt(fromto.m,id.vars = "origin")
  colnames(fromto)<-c("origin","destination","qty")
  
  # add region for origin and destination
  fromto <- fromto %>% left_join(ag_aux,by=c("origin"="c")) %>% 
    rename(r.origin=region)
  fromto <- fromto %>% left_join(ag_aux,by=c("destination"="c")) %>% 
    rename(r.destination=region)
  
  fromto<-fromto %>% 
    relocate(r.origin) %>% 
    relocate(r.destination,.before = destination)
  
  return(fromto)
  
}



# Function to aggregate countries into smaller regions
# countries must be column "c" with regions"
f.ag.countries <- function(df,var,limit=100e3){
  
  df %>% 
    mutate(c_old=c) %>% 
    group_by(c_old) %>% # to sum by segment
    mutate(c=case_when(
    sum({{var}})<limit ~ paste0("Rest of ",region),
    T ~ c)) %>% ungroup() %>% 
    return()
  
}

# Function to generate matrix from "fromto" object
f.trade.matrix <- function(fromto){
  
  fromto <- fromto %>% arrange(origin) %>% arrange(destination) %>% dplyr::select(origin,destination,qty)
  
  mat <- fromto %>% pivot_wider(names_from = destination, values_from = qty,values_fill = 0)
  
  # correct column order?
  # names(mat)==ag$highlight
  # mat$origin == ag$highlight
  
  mat$origin  <-  NULL
  
  return(mat)
  
}

# Function to convert a tensor object (list of 4 matrix) into a fromto table
f.tensor.fromto <- function(tensor){
  # convert each matrix to a dataframe in format of fromto
  list_df <- lapply(sales_ratio, f.fromto.table2)

  # combine them and add a column with segment name
  combined_df <- do.call(rbind, lapply(seq_along(list_df), function(i) {
    transform(list_df[[i]], segment = paste0("s", i))
  }))
  
  return(combined_df)
}

