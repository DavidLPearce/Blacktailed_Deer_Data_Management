# Alleles have the same name for two columns this is a function 
# to add suffix to alleles, e.g C273 C273 > C273.1 C273.2

fix_alleles <- function(colnms) {
  ave(seq_along(colnms), colnms, FUN = function(x) {
    if (length(x) == 1) return("") 
    paste0(".", seq_along(x))       # add suffixes
  }) -> suffixes
  
  paste0(colnms, suffixes)
}

# Example
# names(data_gen) <- fix_alleles(names(data_gen))