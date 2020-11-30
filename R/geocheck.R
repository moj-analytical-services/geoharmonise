#' Compare a set of area names to an ONS-maintained list
#'
#' This function compares a list of geographical area names to an ONS area code list.
#' It takes as an input a list of names, the area type (eg. "Local Authority" or "Region"), and the date of the ONS list you wish to compare to.
#' To obtain a list of valid dates, use the function ONS_geolist()
#'
#' @param names A vector of names to be checked
#' @param area_type Type of area (eg. "Local Authority", "Region")
#' @param ONS_filedate A date corresponding to the ONS code list you wish to compare to. Run the ONS_geolist() function to obtain a list of valid dates.
#' This should be in the format eg. "31-Dec-19".
#' @return A list containing three elements: \cr \cr
#' \strong{matches:} A data frame of area names which match with the ONS list (includes Exact and Partial matches) \cr
#' \strong{unmatched_names:} A vector of names taken from the "names" input parameter which could not be matched to an ONS name \cr
#' \strong{unmatched_ONS:} A vector of names taken from the requested ONS list which could not be matced to a name from the vector specified in the "names" input parameter
#' @export
#' @importFrom magrittr "%>%"

geocheck <- function(names, area_type, ONS_filedate) {

  if(is.na(as.Date(ONS_filedate, "%d-%b-%y"))) {
    stop("Date is not in the expected format DD-MMM-YY")
    } 
  
  #Return an error if date is not in expected list
  if(ONS_filedate %in% ONS_geolist(area_type)$date == FALSE) {
    stop(paste(ONS_filedate, "is not a recognised date for area type:", area_type))
    } 

  namespec <- deparse(substitute(names))

  # Get data from ONS Geo portal and check for errors

  ONS_data <- httr::GET(url = geo_url(ONS_filedate, area_type)) # to get the initial object using the  API

  if (ONS_data$status_code == 200) {
    print("ONS area list downloaded OK...")
  } else if (ONS_data$status_code == 404) {
    stop("ONS area list not found. May be invalid date or area type.")
  }

  # Remove unwanted fields and retain only England and Wales data

  ONS_data <- httr::content(ONS_data, as = "parsed") %>%
    jsonlite::fromJSON() %>%
    .$features %>%
    .$attributes

  EW <- stringr::str_which(ONS_data[, 1], "E|W")

  ONS_data <- ONS_data[EW, 1:2]

  names <- stringr::str_trim(names, side = "both")


  ## Check spelling of area names

  # Custom dictionary - created dynamically every time no need to update

  file.create("dictionary.dic")
  file.create("dictionary.aff")

  ONS_names <- ONS_data[, 2]

  if (area_type == "Region") {
    ONS_names <- c(ONS_names, "Wales")
  }

  area_dictionary <- hunspell::dictionary("dictionary.dic", add_words = ONS_names)

  # Spell check

  checked <- hunspell::hunspell_check(names, dict = area_dictionary)

  matched <- unique(names[checked])
  unmatched <- unique(names[!checked])

  ONS_unused <- ONS_names[!ONS_names %in% matched]

  unused_dictionary <- hunspell::dictionary("dictionary.dic", add_words = ONS_unused)

  suggested <- hunspell::hunspell_suggest(unmatched, dict = unused_dictionary)
  suggested <- sapply(suggested, "[", 1)

  suggestions <- as.data.frame(cbind(unmatched, suggested))
  suggestions <- suggestions %>% dplyr::filter(!is.na(suggested))
  unmatched <- unmatched[is.na(suggested)]

  ONS_unused <- ONS_unused[!ONS_unused %in% suggested]

  # Assemble results into outputs

  matched <- as.data.frame(matched)
  if (nrow(suggestions) == 0) {
    partial <- cbind(suggestions, data.frame("Match" = character(0)))
  } else {
    partial <- as.data.frame(cbind(suggestions, "partial"))
  }
  if (nrow(matched) == 0) {
    exact <- cbind(matched, matched, data.frame("Match" = character(0)))
  } else {
    exact <- cbind(matched, matched, "Exact")
  }

  names(partial) <- c(namespec, names(ONS_data)[2], "Match")
  names(exact) <- c(namespec, names(ONS_data)[2], "Match")

  matched <- as.data.frame(rbind(exact, partial))

  unmatched_data <- unmatched
  unmatched_ONS <- ONS_unused

  file.remove("dictionary.dic")
  file.remove("dictionary.aff")

  return(list("matches" = matched, "unmatched_names" = unmatched_data,
              "unmatched_ONS" = unmatched_ONS))
}
