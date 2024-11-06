#' Download microbial species data from the Human Oral Microbiome Database (HOMD)
#'4th function
#'still working on
#'this is not working and the test doesnt work
#'
#'
#' @param genus Optional filter by genus (e.g., "Prevotella").
#' @return A data frame with HOMD species data.
download_homd_data <- function(genus = NULL) {
  library(httr)
  library(jsonlite)

  url <- "https://www.homd.org/genome/blast_sserver?type=refseq"
  if (!is.null(genus)) {
    url <- paste0(url, "?genus=", genus)
  }

  response <- GET(url)
  if (status_code(response) != 200) {
    stop("Failed to retrieve data from HOMD.")
  }

  data <- fromJSON(content(response, "text"), flatten = TRUE)
  return(data)
}
