#' Download Microbial Data from HOMD
#'
#' This function downloads microbial data from the Human Oral Microbiome Database (HOMD).
#' It optionally filters the data by genus.
#'
#' @param genus Optional. A string specifying the genus to filter by (e.g., "Prevotella").
#' @return A data frame with HOMD species data, including taxonomic and reference sequence information.
#' @details This function uses the HOMD API to retrieve microbial data. If the `genus` parameter
#' is specified, it filters the results to include only species from that genus.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @export
download_homd_data <- function(genus = NULL) {
  # Base URL for the HOMD API
  base_url <- "https://www.homd.org/api/v1/species"

  # Modify URL if filtering by genus
  if (!is.null(genus)) {
    url <- paste0(base_url, "?genus=", genus)
  } else {
    url <- base_url
  }

  # Fetch data from HOMD
  response <- httr::GET(url)

  # Check for successful response
  if (httr::status_code(response) != 200) {
    stop("Failed to retrieve data from HOMD. Please check the URL or try again later.")
  }

  # Parse the JSON response into a data frame
  data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)

  # Validate the structure of the response
  if (is.null(data) || !is.data.frame(data)) {
    stop("Unexpected response format from HOMD.")
  }

  return(data)
}

