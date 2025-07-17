# tests/testthat/setup-mock-data.R
# =====================================
# Conditional Mock Data Setup for gimap Package Tests
# =====================================
# This file runs automatically before all tests
# Creates mock data as FALLBACK ONLY when Figshare is inaccessible
# Preserves real Figshare functionality when available

# =====================================
# FIGSHARE ACCESSIBILITY TEST
# =====================================

test_figshare_access <- function() {
  tryCatch(
    {
      # Try to access real Figshare functionality
      if (exists("get_figshare", envir = asNamespace("gimap"))) {
        # Test if we can get the file list (lightweight test)
        result <- get("get_figshare", envir = asNamespace("gimap"))(return_list = TRUE)
        if (!is.null(result)) {
          return(TRUE) # Figshare is accessible
        }
      }
      return(FALSE)
    },
    error = function(e) {
      return(FALSE) # Figshare is not accessible
    }
  )
}

# =====================================
# MOCK DATA CREATION FUNCTIONS (FALLBACK ONLY)
# =====================================

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
  # Essential genes for normalization
  essential_genes <- c(
    "ACTB", "GAPDH", "TUBB", "RPL13A", "RPS18", "HPRT1", "GUSB", "TBP",
    "POLR2A", "YWHAZ", "B2M", "SDHA", "UBC", "PPIA", "RPL32", "RPS29",
    "HMBS", "TFRC", "PGK1", "LDHA", "ENO1", "PKM", "ALDOA", "TPI1",
    "PGAM1", "ACTA2", "MYH9", "VIM", "TUBA1A", "HIST1H4C"
  )

  # Additional genes from real data examples
  additional_genes <- c(
    "TSPAN6", "TNMD", "DPM1", "SCYL3", "C1orf112", "FGR", "CFH", "FUCA2",
    "GCLC", "NFYA", "STPG1", "NIPAL3", "LAS1L", "ENPP4", "SEMA3F", "CFTR",
    "ANKIB1", "CYP51A1", "KRIT1", "RAD52", "BAD", "LAP3", "CD99", "HS3ST1", "AOC1"
  )

  all_genes <- unique(c(essential_genes, additional_genes))

  # Cell line IDs (DepMap Achilles format)
  hela_id <- "ACH-001086" # HELA cell line
  pc9_id <- "ACH-001113" # PC9 cell line
  other_cell_lines <- c(
    "ACH-001289", "ACH-001339", "ACH-001538", "ACH-000242", "ACH-000708",
    "ACH-000327", "ACH-000233", "ACH-000461", "ACH-000705", "ACH-001794",
    "ACH-002023", "ACH-000528", "ACH-001655", "ACH-000167", "ACH-000792",
    "ACH-001098", "ACH-000570"
  )

  cell_line_ids <- c(hela_id, pc9_id, other_cell_lines)

  # Create realistic expression matrix (log2 TPM + 1 scale)
  set.seed(123) # For reproducible results
  n_genes <- length(all_genes)
  n_cell_lines <- length(cell_line_ids)

  expression_matrix <- matrix(
    runif(n_genes * n_cell_lines, min = 0, max = 12),
    nrow = n_genes,
    ncol = n_cell_lines,
    dimnames = list(all_genes, cell_line_ids)
  )

  # Make essential genes more highly expressed (realistic for housekeeping genes)
  essential_indices <- which(all_genes %in% essential_genes)
  expression_matrix[essential_indices, ] <- expression_matrix[essential_indices, ] + 2

  # Add some low expression genes (10% of genes)
  low_expr_genes <- sample(1:n_genes, size = max(1, floor(n_genes * 0.1)))
  expression_matrix[low_expr_genes, ] <- runif(length(low_expr_genes) * n_cell_lines, 0, 1)

  # Add some unexpressed genes (5% of genes with zero expression)
  zero_expr_genes <- sample(1:n_genes, size = max(1, floor(n_genes * 0.05)))
  expression_matrix[zero_expr_genes, ] <- 0

  # Convert to data frame with genes as first column
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
    `ACH-001086` = expression_data$ACH.001086, # HELA only for CCLE format
    stringsAsFactors = FALSE
  )
}

# =====================================
# CONDITIONAL MOCK DATA STORAGE
# =====================================

# Only create mock data environment if needed
.mock_data_env <- NULL
.figshare_accessible <- NULL

initialize_mock_data_if_needed <- function() {
  if (is.null(.mock_data_env)) {
    .mock_data_env <<- new.env()
    .mock_data_env$achilles_data <- create_mock_achilles_data()
    .mock_data_env$expression_data <- create_mock_expression_data()
    .mock_data_env$ccle_data <- create_mock_ccle_data()
    message("Mock data initialized as fallback")
  }
}

# =====================================
# CONDITIONAL MOCK FUNCTION IMPLEMENTATIONS
# =====================================

# Wrapper that tries real Figshare first, then falls back to mock
conditional_get_figshare <- function(file_name = NULL, output_dir = NULL, return_list = FALSE) {
  # Try real get_figshare first
  tryCatch(
    {
      real_get_figshare <- get("get_figshare", envir = asNamespace("gimap"))
      result <- real_get_figshare(file_name = file_name, output_dir = output_dir, return_list = return_list)
      return(result)
    },
    error = function(e) {
      # If real Figshare fails, use mock
      message("Figshare inaccessible, using mock data for: ", file_name %||% "file list")

      # Initialize mock data if not already done
      initialize_mock_data_if_needed()

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
  )
}

# Wrapper for ctrl_genes that tries real function first
conditional_ctrl_genes <- function() {
  tryCatch(
    {
      # Try to use real ctrl_genes function
      real_ctrl_genes <- get("ctrl_genes", envir = asNamespace("gimap"))
      result <- real_ctrl_genes()
      return(result)
    },
    error = function(e) {
      # If real function fails, use mock
      message("ctrl_genes failed, using mock essential genes")
      initialize_mock_data_if_needed()
      return(.mock_data_env$achilles_data$gene)
    }
  )
}

# =====================================
# CONDITIONAL MOCK SETUP
# =====================================

setup_conditional_mocks <- function() {
  # Only set up mocks if gimap is loaded
  if (!"gimap" %in% loadedNamespaces()) {
    return(FALSE)
  }

  # Check if Figshare is accessible
  figshare_works <- test_figshare_access()

  if (figshare_works) {
    message("✓ Figshare is accessible - using real Figshare functions")
    return(TRUE)
  } else {
    message("⚠ Figshare not accessible - setting up fallback mocks")

    # Replace functions with conditional wrappers
    if (exists("get_figshare", envir = asNamespace("gimap"))) {
      # Store original function
      .mock_data_env$original_get_figshare <- get("get_figshare", envir = asNamespace("gimap"))
      assignInNamespace("get_figshare", conditional_get_figshare, ns = "gimap")
    }

    if (exists("ctrl_genes", envir = asNamespace("gimap"))) {
      # Store original function
      .mock_data_env$original_ctrl_genes <- get("ctrl_genes", envir = asNamespace("gimap"))
      assignInNamespace("ctrl_genes", conditional_ctrl_genes, ns = "gimap")
    }

    return(FALSE)
  }
}

# =====================================
# UTILITY FUNCTIONS FOR TESTS
# =====================================

# Function to get mock data (only if mocks are active)
get_mock_data <- function(type = c("achilles", "expression", "ccle")) {
  if (is.null(.mock_data_env)) {
    stop("Mock data not initialized. This function only works when Figshare is inaccessible.")
  }

  type <- match.arg(type)

  switch(type,
    "achilles" = .mock_data_env$achilles_data,
    "expression" = .mock_data_env$expression_data,
    "ccle" = .mock_data_env$ccle_data
  )
}

# Function to create custom TPM data (only if mocks are active)
create_custom_tpm_mock <- function() {
  if (is.null(.mock_data_env)) {
    stop("Mock data not initialized. This function only works when Figshare is inaccessible.")
  }

  data.frame(
    genes = .mock_data_env$ccle_data$genes,
    log2_tpm = .mock_data_env$ccle_data$`ACH.001086`,
    stringsAsFactors = FALSE
  )
}

# Function to check if mocks are currently active
are_mocks_active <- function() {
  return(!is.null(.mock_data_env))
}

# Function to check Figshare status
check_figshare_status <- function() {
  if (!"gimap" %in% loadedNamespaces()) {
    cat("gimap package not loaded\n")
    return("unknown")
  }

  if (test_figshare_access()) {
    cat("✓ Figshare is accessible\n")
    return("accessible")
  } else {
    cat("✗ Figshare is not accessible\n")
    return("inaccessible")
  }
}

# =====================================
# INITIALIZATION
# =====================================

# Set up hook to apply conditional mocks when gimap is loaded
setHook(packageEvent("gimap", "onLoad"), function(...) {
  setup_conditional_mocks()
})
