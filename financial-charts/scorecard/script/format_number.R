# Formatting numbers

### update log ############################
# v1    wrap into utility functions
# v2    imporv speed by using sprintf+round instead of formatC;
#       add vector input support
# v3    add toDollar; drop call to round() (have no idea why I used round() here)
# v4    add thousand separator support in toDollar
# v5    add option "plus_sign"
# v6    add "plus_sign" for toDecimalPlaces

toPercent <- function(x, digits=2, plus_sign=FALSE) {
  res <- sapply(x, function(i){
    paste0(sprintf(paste0("%.",digits,"f"), i*100), "%")
  })
  if(plus_sign) {
    to_add <- which(!(substr(res, 1, 1) %in% c("-", "N")))
    res[to_add] <- paste0("+", res[to_add])
  }
  return(res)
}

toDecimalPlaces <- function(x, digits=2, plus_sign=FALSE) {
  res <- sapply(x, function(i){
    sprintf(paste0("%.",digits,"f"), i)
  })
  if(plus_sign) {
    to_add <- which(!(substr(res, 1, 1) %in% c("-", "N")))
    res[to_add] <- paste0("+", res[to_add])
  }
  return(res)
}

toDollar <- function(x, digits=2, thousand_sep=FALSE) {
  if(thousand_sep) {
    sapply(x, function(i){
      if(!is.na(i)) {
        return(paste0("$", format(round(i, digits=digits), nsmall=digits, big.mark=",")))
      } else {
        return("NA")
      }
    })  
  } else {
    sapply(x, function(i){
      if(!is.na(i)) {
        return(paste0("$", sprintf(paste0("%.",digits,"f"), i)))
      } else {
        return("NA")
      }
    }) 
  }
}
# # Benchmarking
# library(microbenchmark)
# xx <- rnorm(1000)
# f1 <- function() sapply(xx, function(x) paste0(formatC(x*100, format="f", digits=2), "%"))
# f2 <- function() sapply(xx, function(x) paste0(format(round(x*100, digits=2), nsmall=2), "%"))
# f3 <- function() sapply(xx, function(x) paste0(sprintf(paste0("%.",2,"f"), x*100), "%"))
# 
# microbenchmark(f1(),f2(),f3(), times=100)
# 
# # Unit: milliseconds
# # expr       min        lq      mean    median        uq      max neval
# # f1() 21.280958 22.020437 23.178913 22.555626 23.993773 43.16342   100
# # f2() 16.056139 16.496952 17.741470 16.841299 17.200025 74.64498   100
# # f3()  5.168704  5.394559  5.756271  5.508183  5.697864 24.42902   100