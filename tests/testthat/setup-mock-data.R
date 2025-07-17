# tests/testthat/setup-mock-data.R
# =====================================
# Direct Mock Setup - Forces mocks immediately when needed
# =====================================

# Create mock data immediately (always available)
.mock_data_env <- new.env()

create_mock_achilles_data <- function() {
  essential_genes <- c(
    "ACTB", "GAPDH", "TUBB", "RPL13A", "RPS18", "HPRT1", "GUSB", "TBP",
    "POLR2A", "YWHAZ", "B2M", "SDHA", "UBC", "PPIA", "RPL32", "RPS29",
    "HMBS", "TFRC", "PGK1", "LDHA", "ENO1", "PKM", "ALDOA", "TPI1"
  )

  data.frame(
    gene = essential_genes,
    essential_score = runif(length(essential_genes), -2, -0.5),
    stringsAsFactors = FALSE
  )
}

create_mock_expression_data <- function() {
  essential_genes <- c(
    "ACTB", "GAPDH", "TUBB", "RPL13A", "RPS18", "HPRT1", "GUSB", "TBP",
    "POLR2A", "YWHAZ", "B2M", "SDHA", "UBC", "PPIA", "RPL32", "RPS29",
    "HMBS", "TFRC", "PGK1", "LDHA", "ENO1", "PKM", "ALDOA", "TPI1",
    "PGAM1", "ACTA2", "MYH9", "VIM", "TUBA1A", "HIST1H4C"
  )

  additional_genes <- c(
    "TSPAN6", "TNMD", "DPM1", "SCYL3", "C1orf112", "FGR", "CFH", "FUCA2",
    "GCLC", "NFYA", "STPG1", "NIPAL3", "LAS1L", "ENPP4", "SEMA3F", "CFTR",
    "ANKIB1", "CYP51A1", "KRIT1", "RAD52", "BAD", "LAP3", "CD99", "HS3ST1", "AOC1"
  )

  all_genes <- unique(c(essential_genes, additional_genes))

  hela_id <- "ACH-001086"
  pc9_id <- "ACH-001113"
  other_cell_lines <- c(
    "ACH-001289", "ACH-001339", "ACH-001538", "ACH-000242", "ACH-000708",
    "ACH-000327", "ACH-000233", "ACH-000461", "ACH-000705", "ACH-001794",
    "ACH-002023", "ACH-000528", "ACH-001655", "ACH-000167", "ACH-000792",
    "ACH-001098", "ACH-000570"
  )

  cell_line_ids <- c(hela_id, pc9_id, other_cell_lines)

  set.seed(123)
  n_genes <- length(all_genes)
  n_cell_lines <- length(cell_line_ids)

  expression_matrix <- matrix(
    runif(n_genes * n_cell_lines, min = 0, max = 12),
    nrow = n_genes,
    ncol = n_cell_lines,
    dimnames = list(all_genes, cell_line_ids)
  )

  essential_indices <- which(all_genes %in% essential_genes)
  expression_matrix[essential_indices, ] <- expression_matrix[essential_indices, ] + 2

  low_expr_genes <- sample(1:n_genes, size = max(1, floor(n_genes * 0.1)))
  expression_matrix[low_expr_genes, ] <- runif(length(low_expr_genes) * n_cell_lines, 0, 1)

  zero_expr_genes <- sample(1:n_genes, size = max(1, floor(n_genes * 0.05)))
  expression_matrix[zero_expr_genes, ] <- 0

  data.frame(
    genes = all_genes,
    expression_matrix,
    stringsAsFactors = FALSE
  )
}

create_mock_ccle_data <- function() {
  expression_data <- create_mock_expression_data()
  data.frame(
    genes = expression_data$genes,
    `ACH-001086` = expression_data$ACH.001086,
    stringsAsFactors = FALSE
  )
}

# Initialize mock data immediately
.mock_data_env$achilles_data <- create_mock_achilles_data()
.mock_data_env$expression_data <- create_mock_expression_data()
.mock_data_env$ccle_data <- create_mock_ccle_data()
.mock_data_env$mocks_applied <- FALSE

# =====================================
# ROBUST MOCK FUNCTIONS
# =====================================

# Mock get_figshare that handles all error types
robust_mock_get_figshare <- function(file_name = NULL, output_dir = NULL, return_list = FALSE) {

  if (return_list) {
    return(list(
      files = data.frame(
        name = c("Achilles_common_essentials.csv", "CCLE_expression.csv"),
        id = c(1, 2),
        size = c(1000, 5000)
      )
    ))
  }

  if (!is.null(file_name)) {
    if (file_name == "Achilles_common_essentials.csv") {
      temp_file <- tempfile(fileext = ".csv")
      write.csv(.mock_data_env$achilles_data, temp_file, row.names = FALSE)
      return(temp_file)

    } else if (file_name == "CCLE_expression.csv") {
      temp_file <- tempfile(fileext = ".csv")
      write.csv(.mock_data_env$ccle_data, temp_file, row.names = FALSE)
      return(temp_file)
    }
  }

  return(NULL)
}

# Mock ctrl_genes that returns essential genes
robust_mock_ctrl_genes <- function() {
  return(.mock_data_env$achilles_data$gene)
}

# =====================================
# FORCE MOCK APPLICATION
# =====================================

force_apply_mocks <- function() {
  if (.mock_data_env$mocks_applied) {
    return(TRUE)
  }

  tryCatch({
    if ("gimap" %in% loadedNamespaces()) {

      # Force replace get_figshare
      if (exists("get_figshare", envir = asNamespace("gimap"))) {
        assignInNamespace("get_figshare", robust_mock_get_figshare, ns = "gimap")
        message("✓ Forced mock get_figshare applied")
      }

      # Force replace ctrl_genes
      if (exists("ctrl_genes", envir = asNamespace("gimap"))) {
        assignInNamespace("ctrl_genes", robust_mock_ctrl_genes, ns = "gimap")
        message("✓ Forced mock ctrl_genes applied")
      }

      .mock_data_env$mocks_applied <- TRUE
      return(TRUE)
    }
    return(FALSE)
  }, error = function(e) {
    message("Error applying mocks: ", e$message)
    return(FALSE)
  })
}

# Test if Figshare is working by trying a simple call
test_figshare_working <- function() {
  if (!"gimap" %in% loadedNamespaces()) {
    return(FALSE)
  }

  tryCatch({
    # Try the original function
    original_fn <- get("get_figshare", envir = asNamespace("gimap"))
    result <- original_fn(return_list = TRUE)
    return(!is.null(result))
  }, error = function(e) {
    # Check for specific HTTP errors
    if (inherits(e, "http_error") || inherits(e, "http_403") || inherits(e, "http_400")) {
      return(FALSE)
    }
    return(FALSE)
  })
}

# =====================================
# AUTOMATIC MOCK APPLICATION
# =====================================

apply_mocks_if_needed <- function() {
  # Always try to load gimap first
  if (!"gimap" %in% loadedNamespaces()) {
    tryCatch({
      library(gimap, quietly = TRUE)
    }, error = function(e) {
      message("Could not load gimap: ", e$message)
      return(FALSE)
    })
  }

  # Test if Figshare is working
  if (!test_figshare_working()) {
    message("Figshare not working, applying mocks...")
    force_apply_mocks()
  } else {
    message("Figshare working, keeping original functions")
  }
}

# =====================================
# UTILITY FUNCTIONS
# =====================================

get_mock_data <- function(type = c("achilles", "expression", "ccle")) {
  type <- match.arg(type)
  switch(type,
         "achilles" = .mock_data_env$achilles_data,
         "expression" = .mock_data_env$expression_data,
         "ccle" = .mock_data_env$ccle_data
  )
}

create_custom_tpm_mock <- function() {
  data.frame(
    genes = .mock_data_env$ccle_data$genes,
    log2_tpm = .mock_data_env$ccle_data$`ACH.001086`,
    stringsAsFactors = FALSE
  )
}

are_mocks_active <- function() {
  return(.mock_data_env$mocks_applied)
}

# Function to manually force mocks (for debugging)
force_mocks_now <- function() {
  .mock_data_env$mocks_applied <- FALSE
  force_apply_mocks()
}

# =====================================
# INITIALIZATION
# =====================================

# Try to apply mocks immediately if gimap is already loaded
if ("gimap" %in% loadedNamespaces()) {
  apply_mocks_if_needed()
}

# Set up hooks for when gimap loads
setHook(packageEvent("gimap", "onLoad"), function(...) {
  apply_mocks_if_needed()
})

setHook(packageEvent("gimap", "attach"), function(...) {
  apply_mocks_if_needed()
})

# =====================================
# SHARED TEST HELPER FUNCTIONS
# =====================================

# Universal skip function for all test files
skip_if_figshare_unavailable <- function() {
  tryCatch(
    {
      # Load gimap if not loaded
      if (!"gimap" %in% loadedNamespaces()) {
        library(gimap)
      }

      # Force apply mocks immediately
      apply_mocks_if_needed()

      # Try to get example data
      result <- get_example_data("gimap")
      return(result)
    },
    error = function(e) {
      # If example data fails, force mocks and try again
      message("Example data failed, forcing mocks: ", e$message)
      force_mocks_now()

      tryCatch({
        get_example_data("gimap")
      }, error = function(e2) {
        message("Still failing after mocks: ", e2$message)
        return("Figshare unavailable")
      })
    }
  )
}

# Helper function for range testing
test_value_in_range <- function(actual, expected, tolerance, name) {
  testthat::expect_true(
    actual >= (expected - tolerance) && actual <= (expected + tolerance),
    info = paste(name, "=", actual, "should be", expected, "±", tolerance)
  )
}

# Helper to get appropriate test data based on current mode
get_test_custom_tpm <- function() {
  if (are_mocks_active()) {
    return(create_custom_tpm_mock())
  } else {
    # Try to use real CCLE data
    tryCatch({
      vroom::vroom(
        file.path(system.file("extdata", package = "gimap"), "CCLE_expression.csv"),
        show_col_types = FALSE,
        col_select = c("genes", "ACH-001086")
      ) %>%
        dplyr::rename(log2_tpm = `ACH-001086`)
    }, error = function(e) {
      # Fall back to mock if real data fails
      return(create_custom_tpm_mock())
    })
  }
}

# Helper to check if we're using mocks or real data
using_mock_data <- function() {
  return(are_mocks_active())
}

# Debugging function for test issues
debug_test_setup <- function() {
  cat("=== TEST SETUP DEBUG ===\n")
  cat("gimap loaded:", "gimap" %in% loadedNamespaces(), "\n")
  cat("Mocks active:", are_mocks_active(), "\n")

  if ("gimap" %in% loadedNamespaces()) {
    # Test get_figshare
    tryCatch({
      result <- get_figshare(return_list = TRUE)
      cat("get_figshare works: TRUE\n")
    }, error = function(e) {
      cat("get_figshare error:", class(e)[1], "\n")
    })

    # Test ctrl_genes
    tryCatch({
      genes <- gimap:::ctrl_genes()
      cat("ctrl_genes works: TRUE (", length(genes), "genes )\n")
    }, error = function(e) {
      cat("ctrl_genes error:", class(e)[1], "\n")
    })
  }

  cat("=== END DEBUG ===\n")
}

# =====================================
# STATUS MESSAGE
# =====================================

cat("=====================================\n")
if ("gimap" %in% loadedNamespaces()) {
  if (are_mocks_active()) {
    cat("✓ Mocks applied to gimap functions\n")
  } else {
    cat("⚠ Figshare working, using real functions\n")
  }
} else {
  cat("⚠ gimap not loaded yet, mocks will apply when loaded\n")
}
cat("✓ HELA: ACH-001086, PC9: ACH-001113\n")
cat("✓ Helper functions available to all tests\n")
cat("=====================================\n")
