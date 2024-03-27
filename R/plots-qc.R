#' Create a CDF for the pgRNA normalized counts
#' @description This function uses pivot_longer to rearrange the data for plotting and then plots a CDF of the normalized counts
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the transformed data
#' @param qc_obj The object that has the qc stuff stored
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom tidyr pivot_longer
#' @importFrom ggplot2 ggplot labs
#' @return counts_cdf a ggplot
#' @examples \dontrun{
#'
#' }
#'
qc_cdf <- function(gimap_dataset, wide_ar = 0.75) {
  long_form <-
    tidyr::pivot_longer(data.frame(gimap_dataset$transformed_data$count_norm),
      everything(),
      names_to = "sample",
      values_to = "count_normalized"
    )

  counts_cdf <- ggplot(long_form, aes(x = count_normalized, color = sample)) +
    stat_ecdf() +
    labs(
      x = "-log10(count/total_count)",
      y = "Expected_pgRNAs",
      color = "Sample"
    ) +
    plot_options() +
    plot_theme() +
    theme(aspect.ratio = wide_ar)

  return(counts_cdf)
}

#' Create a histogram for the pgRNA log2 CPMs, faceted by sample
#' @description This function uses pivot_longer to rearrange the data for plotting and then plots sample specific histograms of the pgRNA cpm's
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the transformed data
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom tidyr pivot_longer
#' @import ggplot2
#' @return sample_cpm_histogram a ggplot
#' @examples \dontrun{
#'
#' }
#'
qc_sample_hist <- function(gimap_dataset, wide_ar = 0.75) {
  long_form <-
    tidyr::pivot_longer(data.frame(gimap_dataset$transformed_data$log2_cpm),
      everything(),
      names_to = "sample",
      values_to = "log2_cpm"
    )

  sample_cpm_histogram <- ggplot(long_form, aes(x = log2_cpm, fill = sample)) +
    geom_histogram(color = "black", binwidth = 0.5) +
    plot_options() +
    plot_theme() +
    theme(
      aspect.ratio = wide_ar,
      legend.position = "none"
    ) +
    facet_wrap(~sample, scales = "free_y", ncol = ceiling(ncol(gimap_dataset$raw_counts) / 2))

  return(sample_cpm_histogram)
}

#' Create a correlation heatmap for the pgRNA CPMs
#' @description This function uses the `cor` function to find correlations between the sample CPM's and then plots a heatmap of these
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the transformed data
#' @param ... Additional arguments are passed in to the pheatmap function.
#' @importFrom magrittr %>%
#' @importFrom pheatmap pheatmap
#' @return `sample_cor_heatmap` a pheatmap
#' @examples \dontrun{
#'
#' }
#'
qc_cor_heatmap <- function(gimap_dataset, ...) {
  cpm_cor <- gimap_dataset$transformed_data$cpm %>%
    cor() %>%
    round(2) %>%
    data.frame()

  sample_cor_heatmap <-
    pheatmap::pheatmap(cpm_cor,
      border_color = "white",
      cellwidth = 20,
      cellheight = 20,
      treeheight_row = 20,
      treeheight_col = 20,
      ...
    )

  return(sample_cor_heatmap)
}

#' Create a histogram with plasmid log2 CPM values and ascertain a cutoff for low values
#' @description Find the distribution of plasmid (day0 data) pgRNA log2 CPM values, and ascertain a cutoff or filter for low log2 CPM values.
#' Assumes the first column of the dataset is the day0 data; do I need a better
#' method to tell, especially if there are reps?
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the transformed data
#' @param cutoff default is NULL, the cutoff for low log2 CPM values for the plasmid time period
#' @param wide_ar aspect ratio, default is 0.75
#' @importFrom magrittr %>%
#' @import ggplot2
#' @return a named list

qc_plasmid_histogram <- function(gimap_dataset, cutoff = NULL, wide_ar = 0.75) {
  to_plot <- data.frame(gimap_dataset$transformed_data$log2_cpm[, 1]) %>% `colnames<-`(c("log2_cpm"))

  plasmid_cpm_histogram <- ggplot(to_plot, aes(x = log2_cpm)) +
    geom_histogram(binwidth = 0.2, color = "black", fill = "gray60") +
    plot_options() +
    plot_theme() +
    theme(aspect.ratio = wide_ar)

  if (is.null(cutoff)) {
    # if cutoff is null, suggest a cutoff and plot with suggested
    quantile_info <- quantile(to_plot$log2_cpm)
    plasmid_cpm_stats <- data.frame(
      stat = c("median", "Q1", "Q3", "lower_outlier"),
      log2_cpm_value = c(
        quantile_info["50%"],
        quantile_info["25%"],
        quantile_info["75%"],
        quantile_info["25%"] - (1.5 * (quantile_info["75%"] - quantile_info["25%"]))
      )
    )
    cutoff <- plasmid_cpm_stats[which(plasmid_cpm_stats$stat == "lower_outlier"), "log2_cpm_value"]
  } else {
    plasmid_cpm_stats <- NULL
  }

  # plot with the cutoff
  plasmid_cpm_hist_wcutoff <- plasmid_cpm_histogram +
    geom_vline(
      xintercept = cutoff,
      linetype = "dashed"
    )


  plasmid_cpm_filter <- unlist(lapply(1:nrow(to_plot), function(x) to_plot$log2_cpm[x] < cutoff))

  plasmid_filter_df <- data.frame("Plasmid_log2cpmBelowCutoff" = c(FALSE, TRUE), n = c(sum(!plasmid_cpm_filter), sum(plasmid_cpm_filter))) %>%
    mutate(percent = round(((n / sum(n)) * 100), 2))

  return(list(
    plasmid_hist_nocutoff = plasmid_cpm_histogram,
    plasmid_stats = plasmid_cpm_stats,
    used_log2_cpm_cutoff = cutoff,
    plasmid_hist_cutoff = plasmid_cpm_hist_wcutoff,
    plasmid_filter = plasmid_cpm_filter,
    plasmid_filter_report = plasmid_filter_df
  ))
}
