# Function to Add iso code or vice-versa
# PBH July 2023

# ag must be previosuly loaded

# Must have column c as country
f.addISO <- function(x){
  x <- x %>% left_join(dplyr::select(ag,c,iso)) %>% 
    dplyr::select(iso,c, everything()) # move to first
  return(x)
}

# Add countries based on ISO
f.addCountries <- function(x){
  x <- x %>% left_join(dplyr::select(ag,c,iso)) %>% 
    dplyr::select(iso,c, everything()) # move to first
  return(x)
}

# EoF