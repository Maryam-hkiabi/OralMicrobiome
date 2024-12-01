test_that("download_homd_data enriches with NCBI data", {
  # Mock HOMD data
  mock_homd_data <- data.frame(
    species = c("Prevotella melaninogenica", "Streptococcus mitis"),
    genus = c("Prevotella", "Streptococcus")
  )

  # Mock NCBI responses
  testthat::with_mocked_bindings(
    `httr::GET` = function(...) {
      structure(list(status_code = 200, content = jsonlite::toJSON(mock_homd_data, auto_unbox = TRUE)),
                class = "response")
    },
    `rentrez::entrez_search` = function(...) list(ids = "12345"),
    `rentrez::entrez_summary` = function(...) list(uid = "12345", lineage = "Bacteria; Firmicutes"),
    {
      result <- download_homd_data(enrich_ncbi = TRUE)

      # Validate NCBI data
      expect_type(result, "list")
      expect_s3_class(result$ncbi_data, "data.frame")
      expect_equal(colnames(result$ncbi_data), c("species", "tax_id", "lineage"))
      expect_equal(result$ncbi_data$lineage[1], "Bacteria; Firmicutes")
    }
  )
})

test_that("download_homd_data integrates HMP16SData", {
  library(SummarizedExperiment)

  testthat::with_mocked_bindings(
    `ExperimentHub::ExperimentHub` = function(...) {
      mocked_assay <- matrix(c(1, 2, 3, 4), nrow = 2, dimnames = list(c("Species1", "Species2"), c("Sample1", "Sample2")))
      mocked_metadata <- list(description = "Mocked HMP16SData")
      structure(
        list(assay = function(...) mocked_assay, metadata = mocked_metadata),
        class = "SummarizedExperiment"
      )
    },
    {
      result <- download_homd_data(include_hmp = TRUE)

      # Validate HMP16SData
      expect_type(result, "list")
      expect_s3_class(result$hmp_data, "data.frame")
      expect_true(nrow(result$hmp_data) > 0)
      expect_equal(colnames(result$hmp_data), c("Sample1", "Sample2"))  # Example column names
    }
  )
})
