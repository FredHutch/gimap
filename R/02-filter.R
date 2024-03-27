#' A function to run QC
#' @description This is a function here's where we describe what it does
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param filter_type Can be one of the following: `zero_count_only`, `low_plasmid_cpm_only` or `both`.
#' You should decide on the appropriate filter based on the results of your QC report.
#' @returns a filtered version of the gimap_dataset returned in the $filtered_data section
#' @export
#' @examples \dontrun{
#'
#' gimap_dataset <- get_example_data("gimap")
#'
#' # Highly recommended but not required
#' run_qc(gimap_dataset)
#'
#'
#' gimap_dataset <- gimap_filter(gimap_dataset)
#'
#' # To see filtered data
#' gimap_dataset$filtered_data
#'
#' }
gimap_filter <- function(.data = NULL,
                         gimap_dataset,
                         filter_type = "both") {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  gimap_dataset$filtered <- NULL #TODO: Filtered version of the data can be stored here

  return(gimap_dataset)
}
