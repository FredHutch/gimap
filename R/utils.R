utils::globalVariables(c(
  "id", "pg_ids", "plot_theme()", "negative_control", "positive_control", "mean_observed_cs", "timepoints", "value", "timepoint_avg", "target_type",
  "unexpressed_ctrl_flag", "median", "lfc_adj", "median", "gRNA1_seq", "gRNA2_seq",
  "control_gRNA_seq", "crispr_score", "pgRNA_target", "mean_double_control_crispr",
  "pgRNA_target", "targeting_gRNA_seq", "mean_single_crispr", "double_crispr_score",

  "single_crispr_score_1", "single_crispr_score_2", "pgRNA_target_double", "mean_single_crispr_1",
  "mean_single_crispr_2", "mean_double_control_crispr_2",
  "expected_crispr", "term", "estimate", "mean_expected_crispr", "intercept", "slope",
  "p_val_ttest", "p_val_wil", "fde_vals_ttest", "fdr_vals_wil", "double_gi_score",
  "single_gi_score_1", "single_gi_score_2", "gene", "DepMap_ID",
 "gene1_symbol", "gene2_symbol", "expressed_flag", "norm_ctrl_flag", "bool_vals",
  "filter_name", "counts", "numzero", "name", "value", "lfc_plasmid_vs_late", "lfc_adj",
  "double_gi_score", "count_normalized", "construct",
  "filterFlag", "plasmid_log2_cpm", "log2_cpm", "gene_symbol", "gene_symbol_1", "gene_symbol_2",
  "mean_double_control_crispr_1", "expected_crispr_double", "expected_crispr_single_1",
  "expected_crispr_single_2", "fdr_vals_ttest", "read_table", "stripped_cell_line_name",
  "comparison", ".", "col_names", "lfc_adj1", "t.test", "wilcox.test", "p.adjust",
  "cor", "quantile", "var", "browseURL", "single_crispr", "mean_single_crispr_2",
  "mean_single_crispr_1", "expected_single_crispr", "double_crispr", "double_gi_score",
  "fdr", "lfc", "mean_expected_cs", "mean_gi_score", "mean_single_crispr",
  "expected_double_crispr", "p_val", "single_gi_score", "Rank", "broad_target_type",
  "logfdr", "pointColor", "both", "mean_score", "gi_score", "Day05_RepA", "id"
))

#' Check if internet/URL is available
#' @description Internal helper function to check if an internet resource is
#' reachable before attempting to download.
#' @param url The URL to check availability for
#' @param timeout_seconds Timeout in seconds for the check
#' @return TRUE if the URL is reachable, FALSE otherwise
#' @keywords internal
check_internet_available <- function(url = "https://api.figshare.com",
                                     timeout_seconds = 5) {
  tryCatch(
    {
      response <- httr::HEAD(url, httr::timeout(timeout_seconds))
      httr::status_code(response) < 400
    },
    error = function(e) {
      FALSE
    }
  )
}

#' Build manual download instructions for Figshare files
#' @description Internal helper to build a warning message for manual downloads.
#' @param item Figshare item ID.
#' @param files Character vector of file names.
#' @return A formatted message string.
#' @keywords internal
figshare_manual_download_message <- function(item, files) {
  files_text <- paste0(files, collapse = ", ")
  paste0(
    "You can manually download these files from Figshare item ", item, ": ",
    files_text, ". ",
    "Use `gimap::get_figshare(file_name = ..., item = \"", item,
    "\", output_dir = <data_dir>)` or see README \"Manual data download (Figshare)\"."
  )
}

#' Resolve and create data directory
#' @description Internal helper to resolve a writable data directory for downloads.
#' @param data_dir Optional data directory. If NULL or empty, uses a user data dir.
#' @return A normalized, existing directory path.
#' @keywords internal
gimap_data_dir <- function(data_dir = NULL) {
  if (is.null(data_dir) || length(data_dir) == 0) {
    data_dir <- tools::R_user_dir("gimap", "data")
  } else if (is.na(data_dir) || !nzchar(data_dir)) {
    data_dir <- tools::R_user_dir("gimap", "data")
  }

  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = FALSE)
  if (!dir.exists(data_dir)) {
    dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  }
  return(data_dir)
}

#' Session cache for Figshare example files (under [tempdir()])
#' @description Not exported; avoids persisting example downloads outside the session
#' temp directory unless the caller passes an explicit `data_dir`.
#' @return Normalized path to an existing directory.
#' @keywords internal
.gimap_example_cache_dir <- function() {
  d <- file.path(tempdir(), "gimap-example-data")
  d <- normalizePath(d, winslash = "/", mustWork = FALSE)
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
  }
  normalizePath(d, winslash = "/", mustWork = TRUE)
}

#' Default directory for [get_example_data()] and related helpers
#' @keywords internal
resolve_example_data_dir <- function(data_dir) {
  if (is.null(data_dir) || length(data_dir) == 0L ||
    (length(data_dir) == 1L && (is.na(data_dir) || !nzchar(data_dir)))) {
    .gimap_example_cache_dir()
  } else {
    gimap_data_dir(data_dir)
  }
}

#' Basenames of all files managed by [get_example_data()]
#' @keywords internal
.gimap_example_filenames <- function() {
  c(
    "PP_pgPEN_HeLa_counts.txt",
    "counts_pgPEN_PC9_example.tsv",
    "pgRNA_ID_pgPEN_library_comp.csv",
    "gimap_dataset_timepoint.RDS",
    "gimap_dataset_treatment.RDS",
    "pgPEN_annotations.txt"
  )
}

#' Whether `dir` is allowed as a root for [delete_example_data()]
#' @keywords internal
.is_safe_example_data_delete_root <- function(dir) {
  dir <- normalizePath(dir, winslash = "/", mustWork = FALSE)
  legacy <- normalizePath(
    tools::R_user_dir("gimap", "data"),
    winslash = "/", mustWork = FALSE
  )
  td <- normalizePath(tempdir(), winslash = "/", mustWork = FALSE)
  cache <- normalizePath(
    file.path(tempdir(), "gimap-example-data"),
    winslash = "/", mustWork = FALSE
  )
  dir == legacy ||
    dir == cache ||
    startsWith(dir, paste0(td, .Platform$file.sep))
}


#' Returns example data for package
#' @description This function loads and returns example data for the package.
#' Which dataset is returned must be specified. Data will be downloaded from Figshare
#' the first time it is used.
#' @param which_data One of `"count"`, `"count_treatment"`, `"meta"`, `"gimap"`,
#' `"gimap_treatment"`, or `"annotation"`; specifies which example dataset is returned.
#' @param data_dir Where files are stored. If `NULL` (default), example files are
#' written under `file.path(tempdir(), "gimap-example-data")` for the current R
#' session (they are not kept after the session ends). Pass an explicit directory
#' if you want a persistent copy (for example to mirror files elsewhere).
#' @param refresh_data If `TRUE`, removes known example filenames from `data_dir`
#' (only under safe locations; see [delete_example_data()]) before re-downloading.
#' @export
#' @returns the respective example data either as a data frame or a specialized
#' gimap_dataset depending on what was requested.
#'
#' @examples \dontrun{
#'
#' counts_timepoint <- get_example_data("count")
#' counts_treatment <- get_example_data("count_treatment")
#' gimap_timepoint_dataset <- get_example_data("gimap")
#' gimap_treatment_dataset <- get_example_data("gimap_treatment")
#' metadata <- get_example_data("meta")
#' annotation <- get_example_data("annotation")
#' }
get_example_data <- function(which_data,
                             data_dir = NULL,
                             refresh_data = FALSE) {
  data_dir <- resolve_example_data_dir(data_dir)

  file_name <- switch(which_data,
    "count" = "PP_pgPEN_HeLa_counts.txt",
    "count_treatment" = "counts_pgPEN_PC9_example.tsv",
    "meta" = "pgRNA_ID_pgPEN_library_comp.csv",
    "gimap" = "gimap_dataset_timepoint.RDS",
    "gimap_treatment" = "gimap_dataset_treatment.RDS",
    "annotation" = "pgPEN_annotations.txt"
  )

  if (is.null(file_name)) {
    stop(
      "`which_data` must be one of: count, count_treatment, meta, gimap, ",
      "gimap_treatment, annotation",
      call. = FALSE
    )
  }

  # If data is to be refreshed delete old data
  if (isTRUE(refresh_data)) {
    delete_example_data(data_dir = data_dir)
  }

  file_path <- file.path(data_dir, file_name)

  if (!grepl("RDS$", file_name)) {
    if (!file.exists(file_path)) {
      download_result <- get_figshare(
        file_name = file_name,
        item = "28264271",
        output_dir = data_dir
      )
      # Handle case where download failed
      if (is.null(download_result)) {
        warning(
          "Could not download example data '", which_data, "'. ",
          "The file is not available locally and could not be fetched from Figshare. ",
          figshare_manual_download_message("28264271", file_name)
        )
        return(NULL)
      }
    }
  } else {
    # For RDS files, check if underlying data can be obtained
    result <- tryCatch(
      {
        save_example_timepoint_data(data_dir = data_dir)
        save_example_treatment_data(data_dir = data_dir)
        TRUE
      },
      error = function(e) {
        message(
          "Could not prepare example data '", which_data, "': ", e$message
        )
        return(FALSE)
      }
    )
    if (!isTRUE(result)) {
      return(NULL)
    }
  }

  # Check if file exists before trying to read
  if (!file.exists(file_path)) {
    warning(
      "Example data file not found: ", file_path, "\n",
      "The data could not be downloaded. Please check your internet connection.\n",
      figshare_manual_download_message("28264271", file_name)
    )
    return(NULL)
  }

  dataset <- switch(which_data,
    "count" = readr::read_tsv(file_path,
      show_col_types = FALSE
    ),
    "count_treatment" = readr::read_tsv(file_path,
      show_col_types = FALSE
    ),
    "meta" = readr::read_csv(file_path,
      skip = 1,
      show_col_types = FALSE
    ),
    "gimap" = readr::read_rds(file_path),
    "gimap_treatment" = readr::read_rds(file_path),
    "annotation" = readr::read_tsv(file_path, show_col_types = FALSE)
  )
  return(dataset)
}


#' Folder used for Figshare example data
#' @param data_dir Optional directory. If `NULL`, returns the session example
#' cache under [tempdir()] (same default as [get_example_data()]). Otherwise
#' resolves via [gimap_data_dir()].
#' @export
#' @return Normalized path to the folder where example files are stored.
example_data_folder <- function(data_dir = NULL) {
  resolve_example_data_dir(data_dir)
}

#' Set up example count data
#' @param data_dir Optional data directory override.
#' @export
#' @return Returns the file path to folder where the example data is stored
save_example_timepoint_data <- function(data_dir = NULL) {
  data_dir <- resolve_example_data_dir(data_dir)
  example_data <- get_example_data("count", data_dir = data_dir) %>%
    dplyr::select(!Day05_RepA)

  example_pg_metadata <- get_example_data("meta", data_dir = data_dir)

  example_counts <- example_data %>%
    dplyr::select(c("Day00_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC")) %>%
    as.matrix()

  example_pg_id <- example_data %>%
    dplyr::select("id")

  example_pg_metadata <- example_data %>%
    dplyr::select(c("id", "seq_1", "seq_2"))

  example_sample_metadata <- data.frame(
    col_names = c("Day00_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC"),
    day = as.numeric(c("0", "22", "22", "22")),
    rep = as.factor(c("RepA", "RepA", "RepB", "RepC"))
  )

  gimap_dataset <- setup_data(
    counts = example_counts,
    pg_ids = example_pg_id,
    sample_metadata = example_sample_metadata
  )

  saveRDS(gimap_dataset, file.path(data_dir, "gimap_dataset_timepoint.RDS"))
}

#' Set up example count data
#' @param data_dir Optional data directory override.
#' @export
#' @return Returns the file path to folder where the example data is stored
save_example_treatment_data <- function(data_dir = NULL) {
  data_dir <- resolve_example_data_dir(data_dir)
  example_data <- get_example_data("count_treatment", data_dir = data_dir)

  example_pg_metadata <- get_example_data("meta", data_dir = data_dir)

  example_counts <- example_data %>%
    select(c("pretreatment", "dmsoA", "dmsoB", "drug1A", "drug1B")) %>%
    as.matrix()

  example_pg_id <- example_data %>%
    dplyr::select("id")

  example_pg_metadata <- example_pg_metadata %>%
    dplyr::select(c("pgRNA_ID", "target1_sgRNA_seq", "target1_sgRNA_seq"))

  example_sample_metadata <- data.frame(
    col_names = c("pretreatment", "dmsoA", "dmsoB", "drug1A", "drug1B"),
    drug_treatment = as.factor(c("pretreatment", "dmso", "dmso", "drug", "drug"))
  )

  gimap_dataset <- setup_data(
    counts = example_counts,
    pg_ids = example_pg_id,
    sample_metadata = example_sample_metadata
  )

  saveRDS(gimap_dataset, file.path(data_dir, "gimap_dataset_treatment.RDS"))
}

plot_options <- function() {
  list(theme_bw(base_size = 12))
}

#' Default creds path
#' @param app_name What app set up are you looking for? Supported apps are 'google' 'calendly' and 'github'
encrypt_creds_path <- function(app_name) {
  list.files(
    pattern = paste0("figshare_encrypt.rds"),
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )
}
#' Get file path to an key encryption RDS
key_encrypt_creds_path <- function() {
  list.files(
    pattern = "encrypt_pass.rds",
    recursive = TRUE,
    system.file("extdata", package = "gimap"),
    full.names = TRUE
  )
}
#' Handler function for GET requests from Figshare
#' @param item What is the item we are retrieving?
#' @param file_name Which item are we downloading?
#' @param output_dir Where should the file be saved?
#' @param return_list Should the list of files be returned instead of the file
#' @importFrom utils menu installed.packages
#' @import httr
#' @importFrom jsonlite fromJSON
#' @importFrom openssl aes_cbc_decrypt
#' @returns Downloads necessary annotation files from Figshare and reads them
#' in as data frames.
#' @export
#'
#' @examples \dontrun{
#'
#' get_figshare(
#'   return_list = TRUE,
#'   output_dir = tempdir()
#' )
#'
#' get_figshare(
#'   file_name = "Achilles_common_essentials.csv",
#'   output_dir = tempdir()
#' )
#' }
get_figshare <- function(file_name = NA,
                         item = "19700056",
                         output_dir = tempdir(),
                         return_list = FALSE) {
  if (is.null(output_dir)) output_dir <- system.file("extdata", package = "gimap")

  # Check if Figshare is available before attempting to connect
 if (!check_internet_available("https://api.figshare.com")) {
    message(
      "Cannot connect to Figshare. ",
      "Please check your internet connection and try again later."
    )
    return(NULL)
  }

  decrypted <- tryCatch(
    {
      openssl::aes_cbc_decrypt(
        readRDS(encrypt_creds_path()),
        key = readRDS(key_encrypt_creds_path())
      )
    },
    error = function(e) {
      message("Could not decrypt Figshare credentials: ", e$message)
      return(NULL)
    }
  )

  if (is.null(decrypted)) {
    return(NULL)
  }

  url <- file.path("https://api.figshare.com/v2/articles", item)

  # Figshare API get with error handling
  result <- tryCatch(
    {
      httr::GET(
        url,
        httr::progress(),
        httr::add_headers(
          Authorization = paste0("Bearer ", unserialize(decrypted)$client_secret)
        ),
        httr::accept_json()
      )
    },
    error = function(e) {
      message(
        "Failed to connect to Figshare API: ", e$message, "\n",
        "Please check your internet connection and try again later."
      )
      return(NULL)
    }
  )

  if (is.null(result)) {
    return(NULL)
  }

  if (httr::status_code(result) != 200) {
    message(
      "Figshare API returned an error (HTTP ", httr::status_code(result), ").\n",
      "The resource may be temporarily unavailable. Please try again later."
    )
    return(NULL)
  }

  # Process and return results
  result_content <- httr::content(result, "text",
    encoding = "UTF-8"
  )
  result_list <- jsonlite::fromJSON(result_content)

  if (return_list) {
    return(result_list$files)
  }
  file_id <- result_list$files %>%
    dplyr::filter(name == file_name) %>%
    dplyr::pull(id)

  message("Downloading: ", file_name)
  result <- tryCatch(
    {
      httr::GET(
        file.path("https://api.figshare.com/v2/file/download/", file_id),
        httr::progress(),
        httr::add_headers(
          Authorization = paste0("Bearer ", unserialize(decrypted)$client_secret)
        ),
        httr::accept_json()
      )
    },
    error = function(e) {
      message(
        "Failed to download file from Figshare: ", e$message, "\n",
        "Please check your internet connection and try again later."
      )
      return(NULL)
    }
  )

  if (is.null(result)) {
    return(NULL)
  }

  if (httr::status_code(result) != 200) {
    message(
      "Failed to download file from Figshare (HTTP ", httr::status_code(result), ").\n",
      "The resource may be temporarily unavailable. Please try again later."
    )
    return(NULL)
  }

  result_content <- httr::content(result, "text",
    encoding = "UTF-8"
  )

  writeLines(result_content, file.path(output_dir, file_name))

  return(file.path(output_dir, file_name))
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL


#' Delete cached Figshare example files
#' @description Removes only the known example filenames produced by
#' [get_example_data()] under `data_dir`. Paths are never taken from [options()],
#' so unrelated files cannot be deleted by mistake. Deletion is only allowed when
#' `data_dir` is the session example cache ([example_data_folder()]), the legacy
#' per-user `tools::R_user_dir("gimap", "data")`, or any directory under
#' [tempdir()] (for example test directories).
#' @param data_dir Directory whose example files should be removed. The default
#' (`NULL`) uses the same session cache as [get_example_data()].
#' @export
#' @return Invisibly, the character vector of file paths that were removed (may
#' be length zero if nothing existed).
#' @examples
#' delete_example_data()
#'
delete_example_data <- function(data_dir = NULL) {
  data_dir <- resolve_example_data_dir(data_dir)
  data_dir <- normalizePath(data_dir, winslash = "/", mustWork = TRUE)

  if (!.is_safe_example_data_delete_root(data_dir)) {
    stop(
      "Refusing to delete: `data_dir` must be the gimap example cache ",
      "(see ?example_data_folder), a directory under tempdir(), or the ",
      "legacy per-user gimap data directory from tools::R_user_dir(\"gimap\", \"data\").",
      call. = FALSE
    )
  }

  files <- file.path(data_dir, .gimap_example_filenames())
  existed <- file.exists(files)
  to_remove <- files[existed]

  if (length(to_remove) == 0L) {
    message("No gimap example data files found under:\n  ", data_dir)
    return(invisible(character(0)))
  }

  unlink(to_remove)
  message(
    "Removed ", length(to_remove), " gimap example file(s) from:\n  ",
    data_dir, "\n",
    paste0("  - ", basename(to_remove), collapse = "\n")
  )
  invisible(to_remove)
}
