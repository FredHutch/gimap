skip_if_figshare_unavailable <- function() {
  tryCatch(
    {
      get_figshare("Achilles_common_essentials.csv")
    },
    error = function(e) {
      "Figshare unavailable"
    }
  )
}
if (skip_if_figshare_unavailable()[1] != "Figshare unavailable") {
  # Example data for testing
  example_counts <- matrix(1:20, nrow = 4, ncol = 5)
  example_pg_ids <- data.frame(id = 1:4)
  example_sample_metadata <- data.frame(id = 1:5, replicate = factor(c(1, 1, 2, 2, 3)), timepoint = factor(c("T0", "T0", "T1", "T1", "T2")))

  # Test elements inside output list
  test_that("setup_data() works correctly", {
    testthat::skip_on_cran()
    result <- setup_data(
      counts = example_counts,
      pg_ids = example_pg_ids,
      sample_metadata = example_sample_metadata
    )

    expect_s3_class(result, "gimap_dataset")
    expect_equal(result$raw_counts, example_counts)
    expect_equal(result$metadata$pg_ids, example_pg_ids)
    expect_equal(result$metadata$sample_metadata, example_sample_metadata)
    expect_equal(result$counts_per_sample, apply(example_counts, 2, sum))


    counts_timepoint <- get_example_data("count")
    counts_treatment <- get_example_data("count_treatment")
    gimap_dataset <- get_example_data("gimap")
    gimap_dataset <- get_example_data("gimap_treatment")
    metadata <- get_example_data("meta")
    annotation <- get_example_data("annotation")
  })
}
