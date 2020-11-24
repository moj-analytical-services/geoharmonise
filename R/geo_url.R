# Function to extract URL from list of ONS files
#
# Toussaint Gervais, July 2020

geo_url <- function(input_date, input_area){

  # Extracting csv into data frame

  df <- read.csv("https://raw.githubusercontent.com/moj-analytical-services/harmonisation-reference/main/reference_files/geography/config/ONS_area_URLs.csv",
                 stringsAsFactors = FALSE)

  # Select URL and return

  return(as.character(dplyr::select(dplyr::filter(df,
                                      as.Date(date, format = "%d-%b-%y") == as.Date(input_date, format = "%d-%b-%y") &
                                      area_type == input_area),
                                      url)))
}
