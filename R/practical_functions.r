#' From binomial to numerical
#'
#' @param dat dataset
#' @return A new dataset. Convert every binomial column in the dataset as numerical to avoid errors
#' @export

all_binom2num <- function(data) {
    
    for (h in 1:ncol(data)) {
        print(names(data)[h])
        tab1 <- table(data[, h])
        if (length(tab1) == 2) {
            
            nam <- names(tab1)
            
            if (!(nam[1] %in% c(0, 1) & nam[2] %in% c(0, 1))) {
                first <- as.character(data[1, h])
                if (is.na(as.numeric(first))) {
                  data[, h] <- as.numeric(as.factor(data[, h])) - 1
                } else {
                  data[, h] <- as.numeric(as.character(data[, h])) - 1
                }
                ifelse(data[, h] == nam[1], 0, 1)
            }
        }
    }
    
    return(data)
}

#' Detect the format of the file
#'
#' @param pathfile a string with the name and format of the file
#' @return the format of the file
#' @export

which.format <- function(pathfile) {
    lenpath <- nchar(pathfile)
    point <- gregexpr("\\.", pathfile)
    point.pos <- point[[1]][length(point[[1]])]
    format <- substr(pathfile, point.pos + 1, lenpath)
    return(format)
}
