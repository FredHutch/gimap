skip_if_figshare_unavailable <- function() {
  tryCatch({
    httr::HEAD("https://figshare.com", httr::timeout(5))
  }, error = function(e) {
    testthat::skip("Figshare unavailable")
  })
}

test_that("Figshare API download works", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()
  
  data_list <- get_figshare(return_list = TRUE)

  testthat::expect_type(data_list, "list")

  get_figshare(file_name = "Achilles_common_essentials.csv")

  testthat::expect_true(
    file.exists(
      file.path(
        system.file("extdata", package = "gimap"),
        "Achilles_common_essentials.csv"
      )
    )
  )
})
