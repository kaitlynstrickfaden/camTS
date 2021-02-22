

#' Matches observations of two datasets by datetime
#'
#' Binds all columns from the second dataset to the first dataset by finding the closest Datetime observations.
#'
#' @import dplyr
#' @import lubridate
#' @param dataset1 a data frame object containing a column called "Datetime".
#' @param dataset2 another data frame containing a column called "Datetime". Some data from this dataset may get lost.
#' @return A new data frame with dataset1 and dataset2 merged together.
#' @export


match_data <- function(dataset1, dataset2) {

  
  nearest <- function(d1, d2, ends=c(-Inf,Inf)) {
    #
    # Both `d1` and `d2` must be vectors of numbers in ascending order.
    #
    glb <- function(u, v) {
      n <- length(v)
      z <- c(v, u)
      j <- i <- order(z)
      j[j > n] <- -1
      k <- cummax(j)
      return(k[i > n])
    }
    y <- c(ends[1], d2, ends[2])
    
    i.lower <- glb(d1, y)
    i.upper <- length(y) + 1 - rev(glb(rev(-d1), rev(-y)))
    y.lower <- y[i.lower]
    y.upper <- y[i.upper]
    lower.nearest <- d1 - y.lower < y.upper - d1
    i <- ifelse(lower.nearest, i.lower, i.upper) - 1
    i[i < 1 | i > length(d2)] <- NA
    return(i)
  }
  
  
  ##### 
  
  
  d1 <- as.numeric(dataset1$Datetime, format = "%Y-%m-%d %H:%M:%OS")
  colnames(dataset1)[which(colnames(dataset1) == "Datetime")] <- "Datetime_d1"
  
  d2 <- as.numeric(dataset2$Datetime, format = "%Y-%m-%d %H:%M:%OS")
  colnames(dataset2)[which(colnames(dataset2) == "Datetime")] <- "Datetime_d2"
  dataset2$Index <- c(1:nrow(dataset2))
  
    
  dataset1$Index <- nearest(d1, d2) # returns a vector of indices
    
  return(left_join(dataset1, dataset2, by = c("Index")))
    
  
}



  