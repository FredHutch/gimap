# Helper function to skip tests when Figshare is unavailable
# This is called within test_that() blocks, not at the top level

skip_if_figshare_unavailable <- function() {
  # Skip on CRAN since these tests require external resources
  testthat::skip_on_cran()

  # Check if Figshare API is reachable
  figshare_available <- tryCatch(
    {
      response <- httr::HEAD("https://api.figshare.com", httr::timeout(5))
      httr::status_code(response) < 400
    },
    error = function(e) FALSE
  )

  if (!figshare_available) {
    testthat::skip("Figshare API is not available (no internet or service down)")
  }
}

# Helper function to skip tests when DepMap metadata format has changed
skip_if_depmap_changed <- function() {
  # Skip on CRAN since these tests require external resources
  testthat::skip_on_cran()

  # Check if DepMap metadata has expected structure
  depmap_valid <- tryCatch(
    {
      depmap_metadata <- readr::read_csv(
        "https://figshare.com/ndownloader/files/35020903",
        show_col_types = FALSE,
        n_max = 1  # Only read first row to check columns
      )
      "stripped_cell_line_name" %in% colnames(depmap_metadata)
    },
    error = function(e) FALSE
  )

  if (!depmap_valid) {
    testthat::skip("DepMap metadata format has changed or is unavailable")
  }
}
