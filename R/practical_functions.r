#' From binomial to numerical
#'
#' @param dat dataset
#' @return A new dataset. Convert every binomial column in the dataset as numerical to avoid errors
#' @export

all_binom2num <- function(data) {
  
  for(h in 1:ncol(data))
  {
    print(names(data)[h])
    tab1 <- table(data[,h])
    if(length(tab1)==2)
    {
      nam <- names(tab1)
      data[,h] <- as.numeric(as.character(data[,h]))
      ifelse(data[,h]==nam[1],0,1)
    }
  }
  
  return(data)
}