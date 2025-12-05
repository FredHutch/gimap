skip_if_figshare_unavailable <- function() {
  result <- get_figshare("Achilles_common_essentials.csv")
  # Returns TRUE if Figshare is available, FALSE otherwise
  !is.null(result)
}
if (skip_if_figshare_unavailable()) {
  test_that("HTML file is created and content is correct", {
    testthat::skip_on_cran()

    # Does the thing run?
    gimap_dataset <- get_example_data("gimap")

    html_file <- run_qc(gimap_dataset,
      output_file = tempfile(),
      plots_dir = tempdir(),
      open_results = FALSE,
      overwrite = TRUE
    )

    expect_true(file.exists(html_file))

    # Test the other dataset too
    gimap_dataset <- get_example_data("gimap_treatment")

    html_file <- run_qc(gimap_dataset,
      output_file = tempfile(),
      plots_dir = tempdir(),
      open_results = FALSE,
      overwrite = TRUE
    )

    expect_true(file.exists(html_file))
  })
}
