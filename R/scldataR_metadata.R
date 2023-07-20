#' Get countries
#' @return A data frame with countries
#' @import dplyr
#' @import stringr
#' @import readr
#' @import jsonlite
#' @importFrom tidyr gather
#' @export
#' @examples
#' get_countries()

get_countries <- function(){

  urls <- iadburls()
  url <- urls$matedata_url
  url <- str_c(url, 'countries')

  countries <- jsonlite::fromJSON(url) %>%
    as_tibble()

  return(countries)
}


#' Get sources
#' @return A data frame with countries
#' @import dplyr
#' @import stringr
#' @import readr
#' @import jsonlite
#' @importFrom tidyr gather
#' @export
#' @examples
#' get_sources()

get_sources <- function(){

  urls <- iadburls()
  url <- urls$matedata_url
  url <- str_c(url, 'sources')

  sources <- jsonlite::fromJSON(url) %>%
    as_tibble()

  return(sources)
}


#' Get themes
#' @return A data frame with countries
#' @import dplyr
#' @import stringr
#' @import readr
#' @import jsonlite
#' @importFrom tidyr gather
#' @export
#' @examples
#' get_themes()

get_themes <- function(){

  urls <- iadburls()
  url <- urls$matedata_url
  url <- str_c(url, 'themes')

  themes <- jsonlite::fromJSON(url) %>%
    as_tibble()

  return(themes)
}



#' Query dictionary
#' @param indicator Selected indicator
#' @param collection Optional categories ()
#' @param resource Optional.
#' @param theme_en Optional.
#' @param theme_es Optional.
#' @return A data frame with dictionary
#' @import dplyr
#' @import stringr
#' @import readr
#' @import jsonlite
#' @importFrom tidyr gather
#' @export
#' @examples
#' query_dictionary()
#' query_dictionary(indicator="pobreza")
#' query_dictionary(collection="Household Socio-Economic Surveys")

query_dictionary <- function(indicator='All',collection='All',resource='All'){

  urls <- iadburls()
  url <- urls$matedata_url
  url <- str_c(url, 'dictionary?')

  if(indicator!='All')   url <- str_c(url,"&indicator=",indicator)
  if(collection!='All')   url <- str_c(url,"&collection=",collection)
  if(resource!='All')   url <- str_c(url,"&resource=",resource)
  # ToDo(rsanchezavalos): Add theme
  # if(theme_en!='All')   url <- str_c(url,"&theme_en=",theme_en)
  # if(theme_es!='All')   url <- str_c(url,"&theme_es=",theme_es)

  dictionary <- jsonlite::fromJSON(url) %>%
    as_tibble()

  return(dictionary)
}

#' Get available indicators from dictionary.
#' @param indicator Selected indicator
#' @param collection Optional categories ()
#' @param resource Optional.
#' @param theme_en Optional.
#' @param theme_es Optional.
#' @return A data frame with dictionary
#' @import dplyr
#' @import stringr
#' @import readr
#' @import jsonlite
#' @importFrom jsonlite flatten
#' @importFrom tidyr gather
#' @export
#' @examples
#' query_dictionary()
#' query_dictionary(indicator="pobreza")
#' query_dictionary(collection="Household Socio-Economic Surveys")

query_dictionary_soda <- function(indicator='All', collection='All', resource='All') {

  base_url <- "https://mydata.iadb.org/resource/5bbz-ibhf.json"
  query <- "SELECT *"

  # Add conditions to the query based on the provided parameters
  conditions <- list()
  if(indicator != 'All') conditions <- c(conditions, paste0("indicator='", indicator, "'"))
  if(collection != 'All') conditions <- c(conditions, paste0("collection='", collection, "'"))
  if(resource != 'All') conditions <- c(conditions, paste0("resource='", resource, "'"))

  # If there are any conditions, add them to the query
  if(length(conditions) > 0){
    query <- paste0(query, " WHERE ", paste(conditions, collapse=" AND "))
  }

  response <- httr::GET(base_url, query = list(`$query` = query))

  # error handling
  if(httr::http_error(response)){
    message("HTTP request failed with status ", httr::http_status(response)$message)
    stop("Please check your API endpoint or your network connection.")
  }

  raw_json <- httr::content(response, as = "text")
  data <- jsonlite::fromJSON(raw_json, flatten = TRUE) %>%
    as_tibble()

  return(data)
}


#' Get Countries
#'
#' This function retrieves country data from a CSV file stored in a GitHub repository.
#' The CSV file contains information related to countries.
#'
#' @return A data frame containing country data.
#' @examples
#' \dontrun{
#' country_data <- get_countries_soda()
#' }
#' @export
#' @importFrom utils read.csv
get_countries_soda <- function() {

  url <- "https://raw.githubusercontent.com/BID-DATA/idbsocialdataR/main/data/countries.csv"

  countries <- utils::read.csv(url)

  return(countries)
}

