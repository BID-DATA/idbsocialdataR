#' Get available data about selected indicator from the IADB resource.
#' @param indicator Selected indicator
#' @param categories Optional categories (area, sex, quintile, education_level, age, ethnicity)
#' @param countries Optional. countries (alpha-3 country code)
#' @param yearstart Optional. Start of the year range
#' @param yearend Optional. End of the year range
#' @param latest Optional. Latest data point available
#' @return A data frame with selected indicators
#' @import httr
#' @import stringr
#' @import jsonlite
#' @importFrom jsonlite flatten
#' @import dplyr
#' @importFrom tidyr gather
#' @importFrom dplyr left_join
#' @export
#' @examples
#' query_indicator_soda(indicator="pobreza", categories=c('sex','age'), countries='MEX')
#' query_indicator_soda(indicator="tasa_ocupacion", categories=c('sex','ethnicity'))


query_indicator_soda <- function(indicator, categories=NULL, countries='All', yearstart='All', yearend='All', latest=FALSE){
  #argument "indicator" is missing, with no default
  if(is.na(indicator))   stop('argument "indicator" is missing, with no default')

  base_url <- "https://mydata.iadb.org/resource/q8e9-eb82.json"
  query <- paste0("SELECT * WHERE indicator='", indicator, "'")

  # If multiple countries are specified, include all of them in the query
  if(any(countries != 'All')){
    countries_query <- paste0("isoalpha3 IN ('", paste(countries, collapse = "','"), "')")
    query <- paste0(query, " AND ", countries_query)
  }

  # List of all possible categories
  all_categories <- c('area', 'sex', 'quintile', 'education_level', 'age', 'ethnicity')

  # If no categories are specified, assume 'Total' for all categories
  if(is.null(categories)){
    for (category in all_categories){
      query <- paste0(query, " AND ", category, "='Total'")
    }
  }
  else {
    # If specific categories are specified, assume 'Total' for unspecified categories
    for (category in all_categories){
      if(!(category %in% categories)){
        query <- paste0(query, " AND ", category, "='Total'")
      }
    }
  }

  if(yearstart!='All')   query <- paste0(query, " AND year>=", yearstart)
  if(yearend!='All')   query <- paste0(query, " AND year<=", yearend)
  if(latest!=FALSE) {
    # Assuming "latest" means the maximum year in the data
    query <- paste0(query, " AND year=(SELECT MAX(year) WHERE indicator='", indicator, "')")
  }

  response <- httr::GET(base_url, query = list(`$query` = query))

  # error handling
  if(httr::http_error(response)){
    stop("HTTP request failed.")
  }

  raw_json <- httr::content(response, as = "text")
  data <- jsonlite::fromJSON(raw_json, flatten = TRUE) %>%
    as_tibble()

  # query dictionary
  dictionary <- query_dictionary_soda()

  # left join dictionary
  data <- data %>%
    left_join(dictionary, by = "indicator")

  return(data)
}




