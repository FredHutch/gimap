#' Calculate log fold change for a
#' @description This calculates the log fold change for a gimap dataset based on the annotation and metadata provided.
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param timepoints Specifies the column name of the metadata set up in `$metadata$sample_metadata` that has a factor that represents the timepoints. The column used for timepoints must be numeric or at least ordinal.
#' @param normalized Default is TRUE meaning that we should expect to look for normalized data in the gimap_dataset.
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
#'   calc_crispr()
#'
#' # To see results
#' gimap_dataset$crispr_score
#' }
calc_crispr <- function(.data = NULL,
                     gimap_dataset,
                     normalized = TRUE) {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  if (is.null(gimap_dataset$normalized_log_fc) && normalized) {
    stop("No normalized data found in this gimap_dataset. Make sure you have run the gimap_normalize() function or set normalized = FALSE")
  }

  # If data is normalized and normalized is TRUE, we set this to the source data
  if (!is.null(gimap_dataset$normalized_log_fc) && normalized) source_data <- gimap_dataset$normalized_log_fc

  # If normalized is set to FALSE, we use rawish data but attach annotation
  if (!normalized) {
    source_data <- gimap_dataset$transformed_data$log2_cpm %>%
      as.data.frame() %>%
      dplyr::mutate(pg_ids = gimap_dataset$metadata$pg_ids$id) %>%
      tidyr::pivot_longer(-pg_ids) %>%
      dplyr::left_join(gimap_dataset$metadata$sample_metadata, by = c("name" = "col_names")) %>%
      dplyr::group_by(timepoints, pg_ids) %>%
      dplyr::summarize(timepoint_avg = mean(value)) %>%
      tidyr::pivot_wider(values_from = timepoint_avg,
                         names_from = timepoints)
  }

  # Calculate medians based on single, double targeting as well as if they are unexpressed control genes
  medians_df <- source_data %>%
    dplyr::group_by(target_type, unexpressed_ctrl_flag) %>%
    dplyr::summarize(median = median(lfc_adj)) %>%
    dplyr::filter(unexpressed_ctrl_flag)

  # Third adjustment
  lfc_df <- source_data %>%
    dplyr::left_join(medians_df, by = "target_type") %>%
    dplyr::mutate(
      # Since the pgPEN library uses non-targeting controls, we adjusted for the
      # fact that single-targeting pgRNAs generate only two double-strand breaks
      # (1 per allele), whereas the double-targeting pgRNAs generate four DSBs.
      # To do this, we set the median (adjusted) LFC for unexpressed genes of each group to zero.
      crispr_score = dplyr::case_when(
        target_type == "single_targeting" ~ lfc_adj - median,
        target_type == "double_targeting" ~ lfc_adj - median,
        TRUE ~ lfc_adj
      ),
      n_genes_expressed = dplyr::case_when(
        gene1_expressed_flag == FALSE & gene2_expressed_flag == FALSE ~ "0",
        gene1_expressed_flag == TRUE & gene2_expressed_flag == FALSE ~ "1",
        gene1_expressed_flag == FALSE & gene2_expressed_flag == TRUE ~ "1",
        gene1_expressed_flag == TRUE & gene2_expressed_flag == TRUE ~ "2")
    )

  message("Calculating CRISPR score")

  # Calculate for single targets
  single_target_df <- lfc_df %>%
    dplyr::filter(target_type %in% c("ctrl_gene", "gene_ctrl")) %>%
    mutate(targeting_gRNA_seq = case_when(
      target_type == "gene_ctrl" ~ gRNA1_seq,
      target_type == "ctrl_gene" ~ gRNA2_seq
    ),
    gene_symbol = dplyr::case_when(
      target_type == "gene_ctrl" ~ gene1_symbol,
      target_type == "ctrl_gene" ~ gene2_symbol
    )) %>%
    group_by(pgRNA_target, targeting_gRNA_seq) %>%
    mutate(single_target_crispr = mean(crispr_score)) %>%
    dplyr::select(pgRNA_target, targeting_gRNA_seq, single_target_crispr)

  # Summarize to target level and save that
  double_target_df <- lfc_df %>%
    dplyr::filter(target_type == "gene_gene") %>%
    dplyr::group_by(pgRNA_target, target_type) %>%
    dplyr::summarize(double_target_crispr = mean(crispr_score)) %>%
    dplyr::inner_join(dplyr::select(lfc_df, pgRNA_target, gRNA1_seq, gRNA2_seq),
                      by = "pgRNA_target") %>%
    dplyr::select(-target_type) %>%
    tidyr::pivot_longer(c(-double_target_crispr, -pgRNA_target),
                        names_to = "sequence",
                        values_to = "double_targeting_sequence")

  crispr_df <- single_target_df %>%
    dplyr::left_join(double_target_df,
                     by = c("targeting_gRNA_seq" = "double_targeting_sequence"),
                     relationship = "many-to-many",
                     suffix = c("_single", "_double")) %>%
    dplyr::select(
      pgRNA_target_single,
      pgRNA_target_double,
      single_target_crispr,
      double_target_crispr,
      targeting_gRNA_seq
    )

  # Save at the target level
  gimap_dataset$crispr_score <- crispr_df

  return(gimap_dataset)
}
