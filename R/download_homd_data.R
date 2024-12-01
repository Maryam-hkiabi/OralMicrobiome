#' Download and Enrich Microbial Data from Multiple Sources
#'
#' This function downloads microbial data from the Human Oral Microbiome Database (HOMD),
#' enriches it with data from NCBI using `rentrez`, and integrates with `HMP16SData`.
#' Users can also provide their own data if desired.
#'
#' @param genus Optional. A string specifying the genus to filter by (e.g., "Prevotella").
#' @param custom_data Optional. A data frame supplied by the user as an alternative data source.
#' @param enrich_ncbi Logical. If `TRUE`, fetch additional data from NCBI. Default is `FALSE`.
#' @param include_hmp Logical. If `TRUE`, integrates HMP16SData. Default is `FALSE`.
#' @return A list with up to three elements:
#' - `homd_data`: A data frame with HOMD species data, if available.
#' - `ncbi_data`: A data frame with enriched NCBI data, if requested.
#' - `hmp_data`: A data frame with HMP16SData, if requested.
#' @details This function first attempts to fetch data from HOMD. If unavailable,
#' it allows users to supply their own data or use NCBI Taxonomy and HMP16SData as alternative sources.
#' @importFrom httr GET content
#' @importFrom jsonlite fromJSON
#' @import rentrez
#' @import ExperimentHub
#' @import SummarizedExperiment
#' @export
download_homd_data <- function(genus = NULL, custom_data = NULL, enrich_ncbi = FALSE, include_hmp = FALSE) {
  # Attempt to fetch data from HOMD
  homd_data <- NULL
  tryCatch({
    # Base URL for the HOMD API
    base_url <- "https://www.homd.org/api/v1/species"
    if (!is.null(genus)) {
      base_url <- paste0(base_url, "?genus=", genus)
    }

    # Fetch data from HOMD
    response <- httr::GET(base_url)

    if (httr::status_code(response) == 200) {
      homd_data <- jsonlite::fromJSON(httr::content(response, "text"), flatten = TRUE)
    } else {
      warning("HOMD API is unavailable. HOMD data will not be included.")
    }
  }, error = function(e) {
    warning("HOMD API request failed: ", e$message)
  })

  # Use custom data if provided
  if (!is.null(custom_data)) {
    homd_data <- custom_data
    message("Using user-supplied custom data.")
  }

  # Filter HOMD data by genus, if applicable
  if (!is.null(homd_data) && !is.null(genus)) {
    homd_data <- homd_data[homd_data$genus == genus, , drop = FALSE]
    if (nrow(homd_data) == 0) {
      warning("No data found for the specified genus in HOMD or custom data.")
    }
  }

  # Enrich with NCBI data if requested
  ncbi_data <- NULL
  if (enrich_ncbi && !is.null(homd_data)) {
    if ("species" %in% colnames(homd_data)) {
      ncbi_data <- lapply(homd_data$species, function(species) {
        query <- paste(species, "[ORGN]")
        search_results <- rentrez::entrez_search(db = "taxonomy", term = query, retmax = 1)
        if (length(search_results$ids) > 0) {
          summary <- rentrez::entrez_summary(db = "taxonomy", id = search_results$ids[1])
          return(data.frame(
            species = species,
            tax_id = summary$uid,
            lineage = summary$lineage
          ))
        } else {
          return(data.frame(
            species = species,
            tax_id = NA,
            lineage = NA
          ))
        }
      })
      ncbi_data <- do.call(rbind, ncbi_data)
    } else {
      warning("Species column not found in data. Skipping NCBI enrichment.")
    }
  }

  # Include HMP16SData if requested
  hmp_data <- NULL
  if (include_hmp) {
    tryCatch({
      hub <- ExperimentHub::ExperimentHub()
      hmp_dataset <- hub[["EH2786"]]  # Use mock ID or real HMP16SData dataset
      hmp_data <- as.data.frame(SummarizedExperiment::assay(hmp_dataset))  # Convert assay to data.frame
    }, error = function(e) {
      warning("Failed to load HMP16SData: ", e$message)
    })
  }

  # Return results as a list
  return(list(
    homd_data = homd_data,
    ncbi_data = ncbi_data,
    hmp_data = hmp_data
  ))
}
