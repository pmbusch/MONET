# Function to create a share of sales matrix using Foreign Supply and DS as share of sales


f.SalesShare <- function(matrix_bt,diag=0){
  
  # ADD diag or DS
  if (length(diag)!=1) { # diag is vector
    diag(matrix_bt) = diag
  }
  
  # NORMALIZE COLUMN TO 1
  shares <- sweep(matrix_bt, 2, colSums(matrix_bt), "/")
  shares <- shares %>% mutate(across(everything(), ~ifelse(is.nan(.), 0, .)))
  names(shares) <-  names(shares) %>% str_replace_all(" ",".") %>% str_replace_all("\\/",".")
  
  return(shares)
}
