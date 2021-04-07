

#' Image file check
#'
#' Checks if a file is an image file.
#'
#' @import tidyverse
#' @param f a vector containing file paths to images
#' @return a vector of Trues and Falses indicating if the files are image files
#' @export



TS_check_image <- function(f) {
  
  paths <- c("jpg", "jpeg", "png", "tif", "tiff", "gif")
  checked <- c()
  
  for (i in seq_along(f)) {
    
    f1 <- stringr::str_split(tolower(f[i]), "\\.")
    f2 <- f1[[1]][length(f1[[1]])]
    checked <- append(checked, f2 %in% paths)
    
  }

  return(checked)
  
}

