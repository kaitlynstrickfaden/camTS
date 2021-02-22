

#' Extract metadata from images
#'
#' Extracts metadata from images.
#'
#' @import exiftoolr
#' @param ims a vector containing file paths to images.
#' @param tags a vector containing the data columns you want to extract. To see the list of available tags, go to https://exiftool.org/TagNames/
#' @param install Default is FALSE. if TRUE, installs Exiftool
#' @return A new data frame with dataset1 and dataset2 merged together.
#' @export


extract_meta <- function(ims, tags = c("FileName", "UserLabel", "DatetimeOriginal", "TriggerMode", "MoonPhase", "AmbientTemperatureFahrenheit", "AmbientTemperature", "SceneCaptureType"), install = F) {
  
  source("R/TS_check_image.R")
  
  if (install == TRUE) {
    install_exiftool()
  }
  
  im <- ims[image_check(ims) == TRUE]
  
  if (length(ims) != length(im)) {
    warning("Some of your input files were not images. Only metadata from the image files was extracted.")
  }
  
  exif_read(im, pipeline = "csv", tags = tags)
  
}

