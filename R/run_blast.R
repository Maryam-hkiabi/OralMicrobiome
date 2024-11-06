#' Run a BLAST search on oral microbial sequences
#' third function
#' need to work on still, to get BLAST results correctly
#'
#'
#' @param sequences A character vector of sequences.
#' @return A data frame with sample BLAST results.
run_blast <- function(sequences) {
  result <- data.frame(
    SequenceID = seq_along(sequences),
    Species = c("Streptococcus", "Prevotella", "Fusobacterium"),
    Identity = c(97.5, 96.1, 94.7)
  )

  return(result)
}
