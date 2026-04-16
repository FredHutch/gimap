test_that("delete_example_data only removes known files under safe roots", {
  d <- test_example_data_dir()
  tf <- file.path(d, "PP_pgPEN_HeLa_counts.txt")
  writeLines("x", tf)
  other <- file.path(d, "user_important_file.csv")
  writeLines("keep", other)

  delete_example_data(data_dir = d)

  expect_false(file.exists(tf))
  expect_true(file.exists(other))
  unlink(other)
})

test_that("delete_example_data refuses unsafe directories", {
  expect_error(
    delete_example_data(data_dir = if (.Platform$OS.type == "unix") "/" else "C:/"),
    "Refusing to delete"
  )
})

test_that("get_example_data does not clobber unrelated options() named like datasets", {
  ops <- options()
  on.exit(options(ops), add = TRUE)
  options(count = "user-value-should-not-be-clobbered")
  skip_if_figshare_unavailable()

  get_example_data("count", data_dir = test_example_data_dir())
  expect_equal(getOption("count"), "user-value-should-not-be-clobbered")
})
