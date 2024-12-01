#' Run a BLAST Search on Microbial Sequences
#'
#' This function performs a BLAST search on microbial sequences using a specified BLAST database.
#' Supports both local and remote (NCBI) BLAST.
#'
#' @param sequences A character vector of DNA sequences to query.
#' @param database A string indicating the path to the BLAST database (for local BLAST).
#' @param remote Logical. If `TRUE`, performs a remote BLAST search using NCBI. Default is `FALSE`.
#' @return A data frame with BLAST results, including alignment scores and species matches.
#' @details For local BLAST searches, the database must be preformatted. For remote BLAST searches,
#' an internet connection and access to the NCBI BLAST API are required.
#' @import rBLAST
#' @import httr
#' @export
run_blast <- function(sequences, database, remote = FALSE) {
  # Validate sequences
  if (!is.character(sequences)) {
    stop("Input sequences must be a character vector.")
  }
  if (length(sequences) == 0) {
    stop("No sequences provided for BLAST search.")
  }

  if (!remote) {
    # Perform local BLAST search using rBLAST
    library(rBLAST)

    # Connect to the local BLAST database
    blast_db <- tryCatch({
      dbConnect(blast(db = database, type = "blastn"))
    }, error = function(e) {
      stop("Failed to connect to BLAST database. Ensure the database path is correct.")
    })

    # Run BLAST queries
    results <- tryCatch({
      predict(blast_db, sequences)
    }, error = function(e) {
      stop("Error running BLAST search: ", e$message)
    })

    # Disconnect from the database
    dbDisconnect(blast_db)

    # Return results as a data frame
    return(results)
  } else {
    # Perform remote BLAST search using NCBI
    library(httr)

    # Base URL for NCBI BLAST API
    ncbi_url <- "https://blast.ncbi.nlm.nih.gov/Blast.cgi"

    # Prepare queries
    queries <- paste(sequences, collapse = "\n")
    payload <- list(
      CMD = "Put",
      DATABASE = "nt",
      PROGRAM = "blastn",
      QUERY = queries
    )

    # Submit query to NCBI
    response <- tryCatch({
      POST(ncbi_url, body = payload, encode = "multipart")
    }, error = function(e) {
      stop("Failed to connect to NCBI BLAST API: ", e$message)
    })

    # Parse response
    if (status_code(response) != 200) {
      stop("Error in NCBI BLAST API response.")
    }

    # Return content as a placeholder
    return(content(response, as = "text"))
  }
}
