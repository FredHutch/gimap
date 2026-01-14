test_that("HTML file is created and content is correct", {
  skip_if_figshare_unavailable()

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
