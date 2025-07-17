skip_if_figshare_unavailable <- function() {
  tryCatch(
    {
      get_example_data("gimap")
    },
    error = function(e) {
      "Figshare unavailable"
    }
  )
}
if (skip_if_figshare_unavailable()[1] != "Figshare unavailable") {
  test_that("Test Genetic Interaction score calculations", {
    testthat::skip_on_cran()
    gimap_dataset <- get_example_data("gimap") %>%
      gimap_filter() %>%
      gimap_annotate(cell_line = "HELA") %>%
      gimap_normalize(
        timepoints = "day",
        missing_ids_file = tempfile()
      ) %>%
      calc_gi()
    
    # Test that all expected data structures exist and are correct type
    testthat::expect_s3_class(gimap_dataset$linear_model, "lm")
    testthat::expect_true(is.data.frame(gimap_dataset$gi_scores))
    testthat::expect_true(nrow(gimap_dataset$gi_scores) > 0)
    
    # Test that required columns exist
    expected_cols <- c("mean_expected_cs", "mean_observed_cs", "gi_score", "p_val", "fdr", "pgRNA_target", "target_type")
    testthat::expect_true(all(expected_cols %in% colnames(gimap_dataset$gi_scores)))
    
    # Test that values are reasonable (not NA, not infinite)
    testthat::expect_false(any(is.na(gimap_dataset$gi_scores$mean_expected_cs)))
    testthat::expect_false(any(is.na(gimap_dataset$gi_scores$mean_observed_cs)))
    testthat::expect_false(any(is.na(gimap_dataset$gi_scores$gi_score)))
    testthat::expect_false(any(is.infinite(gimap_dataset$gi_scores$gi_score)))
    
    # Test that p-values are in valid range [0,1]
    testthat::expect_true(all(gimap_dataset$gi_scores$p_val >= 0 & gimap_dataset$gi_scores$p_val <= 1))
    
    # Test that FDR values are in valid range [0,1]
    testthat::expect_true(all(gimap_dataset$gi_scores$fdr >= 0 & gimap_dataset$gi_scores$fdr <= 1))
    
    # Test that we have the expected target types
    expected_target_types <- c("gene_gene", "gene_ctrl", "ctrl_gene")
    testthat::expect_true(all(gimap_dataset$gi_scores$target_type %in% expected_target_types))
    
    # Test that linear model has reasonable coefficients (not NA or infinite)
    testthat::expect_false(any(is.na(gimap_dataset$linear_model$coefficients)))
    testthat::expect_false(any(is.infinite(gimap_dataset$linear_model$coefficients)))
    
    # Test that we have both single and double targeting results
    testthat::expect_true("gene_gene" %in% gimap_dataset$gi_scores$target_type)
    testthat::expect_true(any(c("gene_ctrl", "ctrl_gene") %in% gimap_dataset$gi_scores$target_type))
  })
  
  
  test_that("Test Genetic Interaction score calculations using LFC", {
    testthat::skip_on_cran()
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
    
    table(
      gimap_dataset_w$normalized_log_fc$unexpressed_ctrl_flag,
      gimap_dataset_w$normalized_log_fc$norm_ctrl_flag
    )
  })
  
  test_that("Test Genetic Interaction score calculations without ctrl_ctrl controls", {
    testthat::skip_on_cran()
    
    # Get the example data and modify it to remove ctrl_ctrl controls
    gimap_dataset <- get_example_data("gimap") %>%
      gimap_filter() %>%
      gimap_annotate(cell_line = "HELA")
    
    # Remove ctrl_ctrl controls from the annotation to simulate Ito et al. dataset
    gimap_dataset$annotation <- gimap_dataset$annotation %>%
      dplyr::filter(target_type != "ctrl_ctrl")
    
    # This should work without error when using no_adjustment and use_lfc = TRUE
    gimap_dataset_no_ctrl <- gimap_dataset %>%
      gimap_normalize(
        timepoints = "day",
        adj_method = "no_adjustment",
        missing_ids_file = tempfile()
      ) %>%
      calc_gi(use_lfc = TRUE)
    
    # Check that the function completed successfully
    testthat::expect_true(class(gimap_dataset_no_ctrl)[1] == "list")
    testthat::expect_false(is.null(gimap_dataset_no_ctrl$gi_scores))
    testthat::expect_false(is.null(gimap_dataset_no_ctrl$linear_model))
    
    # Check that we have GI scores calculated
    testthat::expect_true(nrow(gimap_dataset_no_ctrl$gi_scores) > 0)
    
    # Check that the linear model was created successfully
    testthat::expect_true(class(gimap_dataset_no_ctrl$linear_model)[1] == "lm")
    
    # Check that mean_expected_lfc values are not all NA
    testthat::expect_false(all(is.na(gimap_dataset_no_ctrl$gi_scores$mean_expected_lfc)))
    
    # Check that gi_score values are not all NA
    testthat::expect_false(all(is.na(gimap_dataset_no_ctrl$gi_scores$gi_score)))
    
    # Verify that no ctrl_ctrl controls exist in the final results
    testthat::expect_false("ctrl_ctrl" %in% gimap_dataset_no_ctrl$gi_scores$target_type)
    
    # Check that the LFC data structure exists (since use_lfc = TRUE)
    testthat::expect_false(is.null(gimap_dataset_no_ctrl$lfc))
    testthat::expect_true("single_lfc" %in% names(gimap_dataset_no_ctrl$lfc))
    testthat::expect_true("double_lfc" %in% names(gimap_dataset_no_ctrl$lfc))
  })
  
  test_that("Test proper error messages for incompatible settings", {
    testthat::skip_on_cran()
    
    # Get the example data and modify it to remove ctrl_ctrl controls
    gimap_dataset <- get_example_data("gimap") %>%
      gimap_filter() %>%
      gimap_annotate(cell_line = "HELA")
    
    # Remove ctrl_ctrl controls from the annotation
    gimap_dataset$annotation <- gimap_dataset$annotation %>%
      dplyr::filter(target_type != "ctrl_ctrl")
    
    # This should fail during normalization when trying to use negative_control_adj without ctrl_ctrl
    # This is expected behavior - user should use "no_adjustment" when no ctrl_ctrl available
    testthat::expect_error(
      gimap_dataset %>%
        gimap_normalize(
          timepoints = "day",
          adj_method = "negative_control_adj",
          missing_ids_file = tempfile()
        ),
      "negative_control.*doesn't exist"
    )
    
    # This should fail when trying to use CRISPR scores without proper normalization
    # User should set use_lfc = TRUE when using no_adjustment
    testthat::expect_error(
      gimap_dataset %>%
        gimap_normalize(
          timepoints = "day",
          adj_method = "no_adjustment",
          missing_ids_file = tempfile()
        ) %>%
        calc_gi(use_lfc = FALSE),
      "No CRISPR scores found"
    )
  })
}
