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
#'   calc_lfc() %>%
#'   calc_gi()
#'
#' }
calc_gi <- function(gimap_dataset) {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  gimap_dataset$results <- NULL #TODO: Final step is genetic interactions results table should be saved here.

  return(gimap_dataset)
}




