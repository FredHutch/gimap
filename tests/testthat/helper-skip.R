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

# Helper function to create a writable example data directory for tests
test_example_data_dir <- function() {
  data_dir <- file.path(tempdir(), "gimap_example_data")
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(data_dir)
}

# Helper function to skip tests when DepMap metadata format has changed.
# When remote metadata is unavailable or has wrong format, uses bundled fixture
# so tests can run without depending on external DepMap/Figshare format.
skip_if_depmap_changed <- function() {
  # Skip on CRAN since these tests require external resources
  testthat::skip_on_cran()

  # Try remote DepMap metadata first
  depmap_valid <- tryCatch(
    {
      depmap_metadata <- readr::read_csv(
        "https://figshare.com/ndownloader/files/35020903",
        show_col_types = FALSE,
        n_max = 1
      )
      depmap_metadata <- gimap:::normalize_depmap_metadata(depmap_metadata)
      !is.null(depmap_metadata) &&
        nrow(depmap_metadata) > 0 &&
        "stripped_cell_line_name" %in% colnames(depmap_metadata) &&
        "DepMap_ID" %in% colnames(depmap_metadata)
    },
    error = function(e) FALSE
  )

  if (!depmap_valid) {
    # Use bundled fixture so tests still run when external format changes
    fixture_path <- system.file(
      "extdata", "depmap_metadata_fixture.csv",
      package = "gimap", mustWork = FALSE
    )
    if (nzchar(fixture_path) && file.exists(fixture_path)) {
      options(gimap_depmap_metadata_file = fixture_path)
    } else {
      testthat::skip("DepMap metadata format has changed or is unavailable")
    }
  }
}
