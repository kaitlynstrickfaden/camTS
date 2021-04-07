

#' Extract metadata from images
#'
#' Extracts metadata from images.
#'
#' @import exiftoolr
#' @import lubridate
#' @import tidyverse
#' @param ims a vector containing file paths to images.
#' @param tags a vector containing the data columns you want to extract. To see the list of available tags, go to https://exiftool.org/TagNames/
#' @return a data frame of image metadata.
#' @export


TS_extract_meta <- function(ims, tags = c("FileName", "UserLabel", "DatetimeOriginal", "TriggerMode", "MoonPhase", "AmbientTemperatureFahrenheit", "AmbientTemperature", "SceneCaptureType")) {
  
  im <- ims[TS_check_image(ims) == TRUE]
  
  if (length(ims) != length(im)) {
    warning("Some of your input files were not images. Only metadata from the image files were extracted.")
  }
  
  m <- exiftoolr::exif_read(im, pipeline = "csv", tags = tags)
  
  colnames(m)[which(colnames(m) == "DateTimeOriginal")] <- "Datetime"
  m$Datetime <- lubridate::ymd_hms(m$Datetime)
  
  
  if ("MoonPhase" %in% colnames(m)) {
    m$MoonPhase <- dplyr::case_when(
        m$MoonPhase == 0 ~ "New",
        m$MoonPhase == 1 ~ "New Crescent",
        m$MoonPhase == 2 ~ "First Quarter",
        m$MoonPhase == 3 ~ "Waxing Gibbous",
        m$MoonPhase == 4 ~ "Full",
        m$MoonPhase == 5 ~ "Waning Gibbous",
        m$MoonPhase == 6 ~ "Last Quarter",
        TRUE ~ "Old Crescent"
      )
    
  }
  
  
  if ("SceneCaptureType" %in% colnames(m)) {
    m$SceneCaptureType = dplyr::case_when(
        m$SceneCaptureType == 0 ~ "Standard",
        m$SceneCaptureType == 1 ~ "Landscape",
        m$SceneCaptureType == 2 ~ "Portrait",
        m$SceneCaptureType == 3 ~ "Night",
        TRUE ~ "Other"
      )
    
  }
  
  return(m)
  
}



