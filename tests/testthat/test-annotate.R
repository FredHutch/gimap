test_that("Annotation options", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA")

  # We should see these columns
  testthat::expect_true(all(c(
    "norm_ctrl_flag",
    "log2_tpm_gene1", "log2_tpm_gene2",
    "log2_cn_gene1", "log2_cn_gene2",
    "gene1_expressed_flag", "gene2_expressed_flag"
  ) %in%
    colnames(gimap_dataset$annotation)))

  # It should warn you if you try to say FALSE for cell line_annotate but
  # don't provide a custom_tpm or use normalize_by_unexpressed = FALSE
  testthat::expect_error(
    gimap_dataset <- get_example_data("gimap") %>%
      gimap_filter() %>%
      gimap_annotate(cell_line_annotate = FALSE) %>%
      gimap_normalize(timepoints = "day")
  )

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line_annotate = FALSE) %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = FALSE
    )

  # We should see these columns
  testthat::expect_true(all(c("lfc", "crispr_score", "norm_ctrl_flag") %in%
                              colnames(gimap_dataset$normalized_log_fc)))

  ## Try out using custom TPM data
  custom_tpm <- vroom::vroom(
    file.path(
      system.file("extdata", package = "gimap"),
      "CCLE_expression.csv"
    ),
    show_col_types = FALSE,
    col_select = c("genes", "ACH-001086")
  ) %>%
    dplyr::rename(log2_tpm = `ACH-001086`)

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(
      custom_tpm = custom_tpm,
      cell_line = "HELA"
    )

  gimap_dataset <- gimap_dataset %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = FALSE,
      overwrite = TRUE
    )

  # We should see these columns
  testthat::expect_true(all(c("lfc", "crispr_score", "norm_ctrl_flag") %in%
                              colnames(gimap_dataset$normalized_log_fc)))
})

# =====================================

# test-figshare.R
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

# =====================================

# test-gi-calc.R
test_that("Test Genetic Interaction score calculations", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi()

  testthat::expect_type(gimap_dataset$linear_model, "list")

  # Use range testing instead of exact values
  test_value_in_range(gimap_dataset$gi_scores$mean_expected_cs[1], -0.416, 0.2, "mean_expected_cs[1]")
  test_value_in_range(gimap_dataset$gi_scores$mean_observed_cs[1], -0.549, 0.2, "mean_observed_cs[1]")
  test_value_in_range(gimap_dataset$gi_scores$gi_score[1], -0.147, 0.1, "gi_score[1]")
  test_value_in_range(gimap_dataset$gi_scores$p_val[1], 0.205, 0.1, "p_val[1]")
})

test_that("Test Genetic Interaction score calculations using LFC", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      adj_method = "no_adjustment",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi(use_lfc = TRUE)

  testthat::expect_true(class(gimap_dataset)[1] == "list")
})

test_that("Test Genetic Interaction score without normalization", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()

  gimap_dataset_wo <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      normalize_by_unexpressed = FALSE,
      timepoints = "day",
      missing_ids_file = tempfile()
    ) %>%
    calc_gi()

  gimap_dataset_w <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      normalize_by_unexpressed = TRUE,
      timepoints = "day",
      missing_ids_file = tempfile()
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
})

# =====================================

# test-normalize.R
test_that("Test normalization", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()

  gimap_dataset <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      missing_ids_file = tempfile()
    )

  # make sure the important columns are there
  testthat::expect_true(
    all(c("target_type", "lfc", "rep", "crispr_score", "unexpressed_ctrl_flag")
        %in% colnames(gimap_dataset$normalized_log_fc))
  )

  neg_controls <- gimap_dataset$normalized_log_fc %>%
    dplyr::filter(norm_ctrl_flag == "negative_control") %>%
    dplyr::group_by(rep) %>%
    dplyr::summarize(neg_ctrl_med = median(crispr_score)) %>%
    dplyr::pull(neg_ctrl_med)

  # We expect negative controls to be now equal to 0
  testthat::expect_equal(neg_controls[1:3], c(0, 0, 0))

  pos_controls <- gimap_dataset$normalized_log_fc %>%
    dplyr::filter(norm_ctrl_flag == "positive_control") %>%
    dplyr::group_by(rep) %>%
    dplyr::summarize(pos_ctrl_med = median(crispr_score)) %>%
    dplyr::pull(pos_ctrl_med)

  # We expect positive controls to be now equal to -1
  testthat::expect_equal(
    round(pos_controls),
    round(c(-1, -1, -1))
  )
})

test_that("Test normalization without expression cutoff", {
  testthat::skip_on_cran()
  skip_if_figshare_unavailable()

  gimap_dataset_true <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = TRUE,
      missing_ids_file = tempfile()
    )

  gimap_dataset_false <- get_example_data("gimap") %>%
    gimap_filter() %>%
    gimap_annotate(cell_line = "HELA") %>%
    gimap_normalize(
      timepoints = "day",
      normalize_by_unexpressed = FALSE,
      missing_ids_file = tempfile()
    )

  testthat::expect_true(
    all(gimap_dataset_true$normalized_log_fc$lfc[1:6] !=
          gimap_dataset_false$normalized_log_fc$lfc[1:6])
  )
})
