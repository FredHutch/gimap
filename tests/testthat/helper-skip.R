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
