test_that("Figshare API download works", {
  skip_if_figshare_unavailable()
  data_dir <- test_example_data_dir()

  data_list <- get_figshare(return_list = TRUE)

  testthat::expect_type(data_list, "list")

  get_figshare(file_name = "Achilles_common_essentials.csv", output_dir = data_dir)

  testthat::expect_true(
    file.exists(
      file.path(data_dir, "Achilles_common_essentials.csv")
    )
  )
})
