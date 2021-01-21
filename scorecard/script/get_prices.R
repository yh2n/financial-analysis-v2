# Download prices from source

# Update log  ----------------------------------
# 101  provide options for datasource; save into .csv as need
# v2   fixed using kensho as source by calling python script; 
#      fixed start dates of some tickers from yahoo; output tickers in alphabetical order
# v3   added option for sorting tickers, default to be unsorted
# v3.1 fix 'MB'
# v3.2 changed Kensho index type from Posixct to Date; fixed path to .py file;
# v3.3 allow string input for start & end date 
# v3.4 fix downloading single ticker with Kensho
# v3.5 fix start dates of some tickers from yahoo;
# v3.6 drop version number in file name; change inline printout info a little bit;
# v3.7 fix bug related to quantmod update
# v4.0 add get_all_prices(); add argument "types" in get_prices; 
#      change get_prices() to call get_all_prices(); change argument name "basket" to "tickers";
#      change argument name "outfile" to "outfiles";
#      remove google as source(stopped providing data in March, 2018)

library(quantmod)


# Get prices from source and aggregate by type ------------------------------------------
get_prices <- function(tickers, start, end, types="Ad", datasource="Y", outfiles=NULL, sort_tks=FALSE, py_path="./script/") {
  if(!all(types %in% c("Op", "Hi", "Lo", "Cl", "Vo", "Ad"))) {
    stop('"types" of price must be chosen from "Op", "Hi", "Lo", "Cl", "Vo" and "Ad" ')
  }
  if(!is.null(outfiles) & length(types) != length(outfiles)) {
    stop("length of types and outfiles must be the same")
  }
  # download from source
  price_list <- get_all_prices(tickers, start, end, datasource, sort_tks=sort_tks, py_path=py_path)
  
  # aggregate by price type
  price_list_by_type <- lapply(types, function(type) {
    prices <- do.call(cbind, lapply(price_list, FUN=get(type)))
    colnames(prices) <- names(price_list)
    
    # prices validation and cleaning 
    # NA handling: fill gaps with first available price from future
    cat("Cleaning", type, "prices ... ")
    for(tk in tickers) {
      # find first and last trading day
      first_day <- min(which(!is.na(prices[, tk])))
      last_day <- max(which(!is.na(prices[, tk])))

      # print the gap
      if(anyNA(prices[first_day:last_day, tk])) {
        cat("\nGaps found and filled in ", tk, " ", type, " prices : \n")
        print(paste(index(prices[which(is.na(prices[first_day:last_day, tk]))+first_day-1, tk])))
        # fill gap with the most recent price available
        prices[first_day:last_day, tk] <- na.locf(prices[first_day:last_day, tk], fromLast=FALSE)
      }
    }
    return(prices)
  })
  names(price_list_by_type) <- types
  
  # if outfile specified, save into file
  if(!is.null(outfiles)) {
    sapply(seq(price_list_by_type), function(i) {
      write.zoo(price_list_by_type[[i]], file=outfiles[i], sep=",")
      cat("\nData (", types[i], ") cleaned and saved in ", '"', outfiles[i], '"')
    })
  } else {
    cat("Done.\n")
  }
  cat("\n")
  
  if(length(types)==1) return(price_list_by_type[[1]])
  return(price_list_by_type)
}


# get OHLC data for tickers --------------------------------------------------------
get_all_prices <- function(tickers, start, end, datasource="Y", out_path=NULL, sort_tks=FALSE, py_path="./script/") {
  if(sort_tks) tickers <- sort(tickers)
  
  # download from source 
  if(datasource == "Y") {
    cat("Downloading from Yahoo ... ")
    # inception dates(for fixing yahoo data)
    inception_dates <- c(
      DOMO = "2018-06-29",
      PS = "2018-05-17",
      SMAR = "2018-04-27",
      TWLO = "2016-06-23",
      ZUO = "2018-04-12",
      MB = "2015-06-22",
      GDDY = "2015-04-16",
      HDP = "2014-12-15",
      SHOP = "2015-05-22",
      TEAM = "2015-12-16",
      PD = "2019-04-12",
      U = "2020-09-18"
    )
    # download prices into list
    price_list <- lapply(tickers, function(tk) {
      price <- getSymbols(tk, src="yahoo", from=start, to=end, auto.assign=FALSE)
      if(tk %in% names(inception_dates)) price <- price[paste0(inception_dates[tk], "/"), ]
      if(length(price) == 0) stop(tk, " has no data during selected period")
      return(price)
    })
    names(price_list) <- tickers
  } else {
    stop('Currently datasource must be "Y"')
  }
  cat("Download finished. \n")
  
  # if outfile specified, save into file 
  if(!is.null(out_path)) {
    sapply(tickers, function(tk) {
      file_name <- paste("prc", tk, start, end, datasource, "all", sep="_")
      write.zoo(price_list[[tk]], file=paste0(out_path, file_name, ".csv"), sep=",")
    })
    cat("\nOHLC Data saved in ", '"', out_path, '"', "\n")
  }
  
  return(price_list)
}

# 
# # get high prices (will be merged into get_prices in future)
# get_high_prices <- function(tickers, start, end, datasource="Y", outfile=NULL, sort_tks=FALSE, py_path="./script/") {
#   # download prices from source
#   price_list <- get_all_prices(tickers, start, end, datasource, out_path=NULL, sort_tks=sort_tks, py_path=py_path)
#   prices <- do.call(cbind, lapply(price_list, FUN=Hi))
#   colnames(prices) <- names(price_list)
#   
#   # prices validation and cleaning -------------------------------------------------------
#   # NA handling: fill gaps with first available price from future
#   cat("Cleaning...")
#   for(tk in tickers) {
#     # find first and last trading day
#     first_day <- min(which(!is.na(prices[, tk])))
#     last_day <- max(which(!is.na(prices[, tk])))
#     
#     # print the gap
#     if(anyNA(prices[first_day:last_day, tk])) {
#       cat("\nGaps found and filled in ", tk, " : \n")
#       print(paste(index(prices[which(is.na(prices[first_day:last_day, tk]))+first_day-1, tk])))
#       # fill gap with the most recent price available
#       prices[first_day:last_day, tk] <- na.locf(prices[first_day:last_day, tk], fromLast=FALSE)
#     }
#   }
#   
#   # if outfile specified, save into file --------------------------------------------------
#   if(!is.null(outfile)) {
#     write.zoo(prices, file=outfile, sep=",")
#     cat("\nData cleaned and saved in ", '"', outfile, '"')
#   } else {
#     cat("Done.\n")
#   }
#   return(prices)
# }
