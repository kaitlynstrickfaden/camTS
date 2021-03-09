

#' Matches observations of two datasets by datetime
#'
#' Binds all columns from the second dataset to the first dataset by finding the closest Datetime observations.
#'
#' @import dplyr
#' @import lubridate
#' @param dataset a data frame object containing a column called "Datetime". Datetime column must contain POSIXct date-time objects.
#' @param imagedata a data frame containing image metadata. Must have a column called "Datetime" containing POSIXct date-time objects and a column called "TriggerMode" with camera trigger mode as character objects (see TS_extract_meta.R).
#' @return A new data frame with dataset and imagedata merged together.
#' @export


TS_match_data <- function(dataset, imagedata) {

  
  nearest <- function(d1, d2, ends=c(-Inf,Inf)) {
    #
    # Both `d1` and `d2` must be vectors of numbers in ascending order.
    # Returns a vector of indices
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
  
  timelapse <- imagedata[imagedata$TriggerMode != "M",]
  motion <- imagedata[imagedata$TriggerMode == "M",]
  
  d <- as.numeric(dataset$Datetime, format = "%Y-%m-%d %H:%M:%OS")
  dataset$Index2 <- c(1:nrow(dataset))
  
  tl <- as.numeric(timelapse$Datetime, format = "%Y-%m-%d %H:%M:%OS")
  colnames(timelapse)[which(colnames(timelapse) == "Datetime")] <- "Image_Datetime"
  timelapse$Index1 <- c(1:nrow(timelapse))
  
  mt <- as.numeric(motion$Datetime, format = "%Y-%m-%d %H:%M:%OS")
  colnames(motion)[which(colnames(motion) == "Datetime")] <- "Image_Datetime"
  
  dataset$Index1 <- nearest(d, tl) 
  motion$Index2 <- nearest(mt, d)
    
  timelapse_join <- inner_join(dataset, timelapse, by = c("Index1"))
  motion_join <- inner_join(dataset, motion, by = c("Index2"))
  
  joined <- rbind(timelapse_join, motion_join)
  joined <- select(joined, !c("Index1", "Index2"))
  joined <- arrange(joined, "Image_Datetime")
  
  return(joined)
    
  
}















  