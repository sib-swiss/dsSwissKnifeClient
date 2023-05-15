dssParseDictionary <- function(dict, primary_col = 1, syn_col = 2){
  file <- read.csv(dict)
  temp <- split(file, file[[as.numeric(primary_col)]])
  dict <- lapply(temp, function(x) unlist(strsplit(x[[syn_col]], ",")))
  unlist_dict <- unlist(dict)
  err <- unlist_dict[duplicated(unlist_dict)]
  if(length(err) > 0 ) {
    stop("There are duplicated synonymous ID/s: ", paste(err, collapse = ","))
  }
  dict
}
