#' Annotate gimap data
#' @description In this function, a `gimap_dataset` is annotated as far as which genes should be used as controls.
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param gene_id_type Specify what kind of gene IDs are specified in the `pg_ids`. By default will assume gene symbol.
#' @param control_genes A list of genes that should be labeled as control genes. These will be used for log fold change calculations.
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
#'   gimap_annotate()
#'
#' # To see anotations
#' gimap_dataset$annotations
#' }
gimap_annotate <- function(.data = NULL,
                           gimap_dataset,
                           annotation_file = NULL) {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  if (!is.null(annotation_file)) {
    if (!file.exists(annotation_file)) stop("The annotation_file specified cannot be found. Please double check the file path")
  }

  # We'll take a look at the gimap_dataset$pg_ids and see what kinds of gene ids are there
  # If we need to do gene conversion we'd do something like:

  # biocLite('org.Hs.eg.db')
  # mapIds(org.Hs.eg.db, <column of gene IDs>, 'ENTREZID', 'SYMBOL')
  # https://github.com/FredHutch/GI_mapping/blob/main/workflow/scripts/02-get_pgRNA_annotations.Rmd

  # This file is from https://depmap.org/portal/download/all/ and from DepMap Public 19Q3 All Files
  # Essential gene labeling is from inst/extdata/Achilles_common_essentials.csv
  # ctrl vs gene labels are from inst/extdata/pgPEN_annotations.txt

  # This is the core of the code we'll need but we'll need to refactor
  #d.annot <- d.annot %>%
  #mutate(norm_ctrl_flag = case_when(
  #  target_type == "gene_gene" ~ "double_targeting",
  #  target_type == "gene_ctrl" & gene1_essential_flag == TRUE ~ "positive_control",
  #  target_type == "ctrl_gene" & gene2_essential_flag == TRUE ~ "positive_control",
  #  target_type == "gene_ctrl" & gene1_essential_flag != TRUE ~ "single_targeting",
  #  target_type == "ctrl_gene" & gene2_essential_flag != TRUE ~ "single_targeting",
  #  target_type == "ctrl_ctrl" ~ "negative_control")) %>%
  #mutate(norm_ctrl_flag = factor(norm_ctrl_flag, levels = c("negative_control",
  #                                                          "positive_control",
  #                                                          "single_targeting",
  #                                                          "double_targeting")))

  gimap_dataset$annotation <- NULL #TODO: Final step is annotations that line up to the same order as the pg gene data should be stored here.

  return(gimap_dataset)
}
