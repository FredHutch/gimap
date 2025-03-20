test_that("Test Genetic Interaction score calculations", {
  testthat::skip_on_cran()
  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day"
    ) %>%
    calc_gi()

  results <- data.frame(
    rep = c("Day05_RepA_early", "Day22_RepA_late", "Day22_RepB_late", "Day22_RepC_late"),
    intercept = as.numeric(round(c(-0.5110, 0.00210, 0.0105, 0.00501), 3)),
    slope = as.numeric(round(c(0.483, 0.659, 0.646, 0.650), 3))
  )

  gimap_dataset$overall_results$intercept <- round(gimap_dataset$overall_results$intercept, 3)
  gimap_dataset$overall_results$slope <- round(gimap_dataset$overall_results$slope, 3)

  testthat::expect_true(all.equal(gimap_dataset$overall_results$rep, results$rep))
  testthat::expect_identical(
    round(gimap_dataset$overall_results$intercept[1], 3),
    round(results$intercept[1], 3)
  )
  testthat::expect_identical(gimap_dataset$overall_results$rep[1], results$rep[1])

  testthat::expect_identical(
    round(gimap_dataset$gi_scores$mean_expected_cs[1], 3),
    round(-0.2220, 3)
  )
  testthat::expect_identical(
    round(gimap_dataset$gi_scores$mean_observed_cs[1], 3),
    round(-1.515, 3)
  )
  testthat::expect_identical(
    round(gimap_dataset$gi_scores$mean_gi_score[1], 3),
    round(-1.119, 3)
  )
  testthat::expect_identical(
    round(gimap_dataset$gi_scores$p_val[1], 3),
    round(0.001203125, 3)
  )
})


test_that("Test Genetic Interaction score calculations using LFC", {
  testthat::skip_on_cran()
  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      adj_method = "no_adjustment"
    ) %>%
    calc_gi(use_lfc = TRUE)

  testthat::expect_true(class(gimap_dataset)[1] == "list")
})


test_that("Test Genetic Interaction score without normalization", {
  testthat::skip_on_cran()
  gimap_dataset_wo <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      normalize_by_unexpressed = FALSE,
      timepoints = "day",
    ) %>%
    calc_gi()

  gimap_dataset_w <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      normalize_by_unexpressed = TRUE,
      timepoints = "day",
    ) %>%
    calc_gi()


  # Are the GI scores the same? No
  testthat::expect_false(
    all(gimap_dataset_wo$gi_scores$gi_score == gimap_dataset_w$gi_scores$gi_score)
  )

  # Are the log fold change values the same? No
  testthat::expect_false(
    all(gimap_dataset_wo$normalized_log_fc$lfc == gimap_dataset_w$normalized_log_fc$lfc)
  )

  # Are the CRISPR scores the same? No
  testthat::expect_false(
    all(gimap_dataset_wo$normalized_log_fc$crispr_score == gimap_dataset_w$normalized_log_fc$crispr_score)
  )

  table(
    gimap_dataset_w$normalized_log_fc$unexpressed_ctrl_flag,
    gimap_dataset_w$normalized_log_fc$norm_ctrl_flag
  )
})
