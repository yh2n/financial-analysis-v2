# Baskets Reader


# Update log  ----------------------------------
# v1   Initial version
# v2   renaming following tidyverse style guide
# v3   add abs() when normalizing weights to adapt for shorting situation


# Configuration --------------------------------


# Core -----------------------------------------
read_baskets <- function(path, weighted, normalize=TRUE) {
  # Read baskets from csv path.
  #
  # Args:
  #   path: (str) the path to basket csv file.
  #   weighted: (bool) whether the basket file is weighted. If FALSE will be assigned equal weights.
  #   normalize: (bool) only applicable when weighted=TRUE. Normalize the weight to sum up to 1.
  #
  # Returns:
  #   a list of 2 lists: [lists of baskets,  lists of weights].
  bskt_table <- read.table(path, header=T, sep=",", stringsAsFactors=F, check.names=F)
  bskt_list <- list()
  wts_list <- list()
  if(weighted) {
    for(i in 1:NCOL(bskt_table)) {
      if(i%%2==0) next
      bskt_n_wt <- na.omit(bskt_table[,i:(i+1)])
      bskt_list[[(i+1)/2]] <- bskt_n_wt[,1]
      wts_list[[(i+1)/2]] <- bskt_n_wt[,2]
      if(normalize)
        wts_list[[(i+1)/2]] <- wts_list[[(i+1)/2]] / sum(abs(wts_list[[(i+1)/2]]))
      names(wts_list[[(i+1)/2]]) <- bskt_list[[(i+1)/2]]
      names(bskt_list)[(i+1)/2] <- colnames(bskt_n_wt)[1]
    }
  } else {
    for(i in 1:NCOL(bskt_table)) {
      bsk_i <- bskt_table[,i]
      bskt_list[[i]] <- bsk_i[bsk_i!=""]
      wts_list[[i]] <- rep(1/length(bskt_list[[i]]), length(bskt_list[[i]]))
      names(wts_list[[i]]) <- bskt_list[[i]]
    }
    names(bskt_list) <- colnames(bskt_table)
  }
  return(list(bskt_list, wts_list))
}

