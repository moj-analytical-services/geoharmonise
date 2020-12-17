#' List ONS area code lists
#'
#' This function generates a list of dates that relate to ONS area code lists.
#' ONS regularly update lists of geographical area codes. This allows you to see the date of each updated list for a particular area type.
#' For the area type given in 'area_type', the function will return a list of dates which relate to each updated list of codes in the ONS geographical data portal.
#' You should select the date that relates to your data and use that date in the geocheck() function call.
#'
#'
#' @param area_type Type of area (eg. "Local Authority", "Region")
#' @param date The most recent date you would like a list for. This will return only those files which pre-date the date parameter.
#' If left blank, the function will return all valid file dates. This should be in the format eg."31-Dec-19"
#' @return A vector of dates
#' @export

ONS_geolist <- function(area_type, date = NULL) {
  
  # Add error if date is not in correct format
  
  if(is.null(date)){} 
  else{
  if(is.na(as.Date(date, "%d-%b-%y"))| nchar(date) > 9) {
    stop("Date is not in the expected format DD-MMM-YY")
  }}

  # Change parameter names

  input_date <- date
  input_area <- area_type

  # Extracting csv into data frame

  df <- read.csv("https://raw.githubusercontent.com/moj-analytical-services/harmonisation-reference/main/reference_files/geography/config/ONS_area_URLs.csv",
                 stringsAsFactors = FALSE)

  # Check that area type is valid

  if (area_type %in% unique(df$area_type)) {

    df_areafilter <- dplyr::filter(df, area_type == input_area)

  } else {

    stop(paste("Invalid value for area_type. Valid values are:\n\t",paste(unique(df$area_type),collapse = "\n\t")))

  }

  # If no date entered, then return all valid dates for specified area type

  if (is.null(date)){

    return(dplyr::select(df_areafilter,date))

    # Else show only dates before specified date

  } else {

    df_datefilter <- dplyr::filter(df_areafilter,
                              as.Date(date, format = "%d-%b-%y") <= as.Date(input_date, format = "%d-%b-%y"))

    if (nrow(df_datefilter) < 1) {

      stop("No data available before this date.")

    } else {

      return(dplyr::select(df_datefilter,date))

    }

  }

}
