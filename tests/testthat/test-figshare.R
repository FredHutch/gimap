skip_if_figshare_unavailable <- function() {
  result <- get_figshare("Achilles_common_essentials.csv")
  # Returns TRUE if Figshare is available, FALSE otherwise
  !is.null(result)
}
if (skip_if_figshare_unavailable()) {
  test_that("Figshare API download works", {
    testthat::skip_on_cran()

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
}
