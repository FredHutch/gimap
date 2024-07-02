#' This is a title for a function
#' @description Create results table that has CRISPR scores, Wilcoxon rank-sum test and t tests.
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param test options include 'wilcoxon' and 't-test'. By default, both will be run.
#' @param overwrite default is FALSE; whether to overwrite the QC Report file
#' @param output_file default is `GI_Results`; name of the output GI results file
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
#'   gimap_normalize(
#'     timepoints = "day",
#'     replicates = "rep") %>%
#'   calc_crispr() %>%
#'   calc_gi()
#'
#' }
calc_gi <- function(.data = NULL,
                    gimap_dataset) {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")


  ## calculate expected double-targeting GI score by summing the two mean single-targeting
  ## CRISPR scores for that paralog pair
  gimap_dataset$crispr_score %>%
    dplyr::mutate(expected_crispr = mean_single_target_crispr_1 + mean_single_target_crispr_2)



  gimap_dataset$results <- NULL # TODO: Final step is genetic interactions results table should be saved here.

  # https://github.com/FredHutch/GI_mapping/blob/main/workflow/scripts/04-calculate_GI_scores.Rmd

  return(gimap_dataset)
}
