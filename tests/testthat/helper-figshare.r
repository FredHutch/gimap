# tests/testthat/helper-figshare-check.R
# Simple solution: just skip tests when Figshare is down

# Function to test if Figshare is accessible
figshare_is_accessible <- function() {
  tryCatch({
    # Try to access a simple Figshare URL with a timeout
    url <- "https://figshare.com/ndownloader/files/35020903"
    con <- url(url, open = "rb")
    on.exit(close(con))
    # Just try to read the first few bytes
    readBin(con, "raw", n = 10)
    return(TRUE)
  }, error = function(e) {
    return(FALSE)
  })
}

# Enhanced skip function that actually checks connectivity
skip_if_figshare_unavailable <- function() {
  # First check if Figshare is accessible
  if (!figshare_is_accessible()) {
    testthat::skip("Figshare is not accessible")
  }
  
  # If Figshare seems accessible, try to get example data
  tryCatch({
    get_example_data("gimap")
  }, error = function(e) {
    testthat::skip("Could not get example data even though Figshare appears accessible")
  })
}

# Helper function for range testing
test_value_in_range <- function(actual, expected, tolerance, name) {
  testthat::expect_true(
    actual >= (expected - tolerance) && actual <= (expected + tolerance),
    info = paste(name, "=", actual, "should be", expected, "±", tolerance)
  )
}