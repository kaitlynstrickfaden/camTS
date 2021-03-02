

#' Extract metadata from images
#'
#' Extracts metadata from images.
#'
#' @import exiftoolr
#' @import lubridate
#' @import tidyverse
#' @param ims a vector containing file paths to images.
#' @param tags a vector containing the data columns you want to extract. To see the list of available tags, go to https://exiftool.org/TagNames/
#' @return A new data frame with dataset1 and dataset2 merged together.
#' @export


TS_extract_meta <- function(ims, tags = c("FileName", "UserLabel", "DatetimeOriginal", "TriggerMode", "MoonPhase", "AmbientTemperatureFahrenheit", "AmbientTemperature", "SceneCaptureType")) {
  
  im <- ims[TS_check_image(ims) == TRUE]
  
  if (length(ims) != length(im)) {
    warning("Some of your input files were not images. Only metadata from the image files was extracted.")
  }
  
  m <- exif_read(im, pipeline = "csv", tags = tags)
  
  colnames(m)[which(colnames(m) == "DateTimeOriginal")] <- "Datetime"
  m$Datetime <- ymd_hms(m$Datetime)
  
  
  if ("MoonPhase" %in% colnames(m)) {
    m <- mutate(m, MoonPhase = case_when(
        MoonPhase == 0 ~ "New",
        MoonPhase == 1 ~ "New Crescent",
        MoonPhase == 2 ~ "First Quarter",
        MoonPhase == 3 ~ "Waxing Gibbous",
        MoonPhase == 4 ~ "Full",
        MoonPhase == 5 ~ "Waning Gibbous",
        MoonPhase == 6 ~ "Last Quarter",
        TRUE ~ "Old Crescent"
        
      ))
  }
  
  
  if ("SceneCaptureType" %in% colnames(m)) {
    m <- mutate(m, SceneCaptureType = case_when(
        SceneCaptureType == 0 ~ "Standard",
        SceneCaptureType == 1 ~ "Landscape",
        SceneCaptureType == 2 ~ "Portrait",
        SceneCaptureType == 3 ~ "Night",
        TRUE ~ "Other"
        
      ))
  }
  
  return(m)
  
}



