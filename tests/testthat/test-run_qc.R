test_that("HTML file is created and content is correct", {
  skip_if_figshare_unavailable()
  testthat::skip_if_not(
    rmarkdown::pandoc_available(),
    "Pandoc is required to render the QC report (install pandoc or skip this test)"
  )
  data_dir <- test_example_data_dir()

  # Does the thing run?
  gimap_dataset <- get_example_data("gimap", data_dir = data_dir)

  html_file <- run_qc(gimap_dataset,
    output_file = tempfile(),
    open_results = FALSE,
    overwrite = TRUE
  )

  expect_true(file.exists(html_file))

  # Test the other dataset too
  gimap_dataset <- get_example_data("gimap_treatment", data_dir = data_dir)

  html_file <- run_qc(gimap_dataset,
    output_file = tempfile(),
    open_results = FALSE,
    overwrite = TRUE
  )

  expect_true(file.exists(html_file))
})
