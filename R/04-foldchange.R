#' Calculate log fold change for a
#' @description This calculates the log fold change for a gimap dataset based on the annotation and metadata provided.
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param replicates Specifies the column name of the metadata set up in `$metadata$sample_metadata` that has a factor that represents the replicates.
#' @param timepoints Specifies the column name of the metadata set up in `$metadata$sample_metadata` that has a factor that represents the timepoints.
#' @export
#' @examples \dontrun{
#'
#' gimap_dataset <- get_example_data("gimap")
#'
#' # Highly recommended but not required
#' run_qc(gimap_dataset)
#'
#' gimap_dataset <- gimap_dataset %>%
#'   gimap_filter() %>%
#'   gimap_annotate() %>%
#'   calc_lfc()
#'
#' # To see results
#' gimap_dataset$log_fc
#' }
calc_lfc <- function(.data = NULL,
                     gimap_dataset,
                     replicates = NULL,
                     timepoints = NULL) {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  # TODO: we need to think about what happens if there are or are not replicates
  if (!is.null(replicates)) {
    if (!(replicates %in% colnames(gimap_dataset$metadata$sample_metadata))) {
      stop("The column name specified for 'replicates' does not exist in gimap_dataset$metadata$sample_metadata")
    }
  }

  # TODO: we need to think about what happens if there are or are not timepoints
  if (!is.null(timepoints)) {
    if (!(timepoints %in% colnames(gimap_dataset$metadata$sample_metadata))) {
      stop("The column name specified for 'timepoints' does not exist in gimap_dataset$metadata$sample_metadata")
    }
  }

  if (is.null(gimap_dataset$annotations)) {
    stop(
      "No annotations are stored in this gimap_dataset, annotations are needed so we know what genes should be used as controls.",
      "Please run gimap_annotate() function on your gimap_dataset and then retry this function."
    )
  }

  # TODO:  Here's where the log fold change calculations and other handling will go based on the code in:
  # https://github.com/FredHutch/GI_mapping/blob/main/workflow/scripts/03-filter_and_calculate_LFC.Rmd

  # lfc_plasmid_vs_late = log2_cpm - plasmid_log2_cpm,
  # lfc_early_vs_late = log2_cpm - early_log2_cpm

  gimap_dataset$log_fc <- NULL # TODO: the log fold changes calculated can be returned here

  return(gimap_dataset)
}
