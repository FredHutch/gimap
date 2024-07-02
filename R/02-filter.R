#' A function to run filtering
#' @description This is a function here's where we describe what it does
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param filter_type Can be one of the following: `zero_count_only`, `low_plasmid_cpm_only` or `rep_variation`, `zero_in_last_time_point` or a vector that includes multiple of these filters.
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
#' }
#'

gimap_filter <- function(.data = NULL,
                         gimap_dataset,
                         filter_type = "both") {
  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")

  message("Filtering data with these filters: ", filter_type)

  gimap_dataset$filtered <- NULL # TODO: Filtered version of the data can be stored here

  return(gimap_dataset)
}

# Possible filters

#' Create a filter for pgRNAs which have a raw count of 0 for any sample/time point
#' @description This function flags and reports which and how many pgRNAs have a raw count of 0 for any sample/time point
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the raw count data
#' @param filter_zerocount_target_col default is NULL; Which sample column(s) should be used to check for counts of 0? If NULL and not specified, downstream analysis will select all sample columns
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate
#' @importFrom purrr reduce map
#' @return a named list with the filter `filter` specifying which pgRNA have a count zero for at least one sample/time point and a report df `reportdf` for the number and percent of pgRNA which have a count zero for at least one sample/time point
#' @examples \dontrun{
#'   gimap_dataset <- get_example_data("gimap")
#'   qc_filter_zerocounts(gimap_dataset)
#'
#'   #or to specify a different column (or set of columns to select)
#'   qc_filter_zerocount(gimap_dataset, filter_zerocount_target_col = 1:2)
#' }
#'

qc_filter_zerocounts <- function(gimap_dataset, filter_zerocount_target_col = NULL){

  if (is.null(filter_zerocount_target_col)) {filter_zerocount_target_col <- c(1:ncol(gimap_dataset$raw_counts))}

  if (!all(filter_zerocount_target_col %in% 1:ncol(gimap_dataset$raw_counts))) {
    stop("The columns selected do not exist. `filter_zerocount_target_col` needs to correspond to the index of the columns in `gimap_dataset$raw_counts` that you need to filter by")
   }

  counts_filter <- data.frame(gimap_dataset$raw_counts[,filter_zerocount_target_col]) %>% map(~.x %in% c(0)) %>% reduce(`|`)

  zerocount_df <- data.frame("RawCount0" = c(FALSE, TRUE), n = c(sum(!counts_filter), sum(counts_filter))) %>%
    mutate(percent = round(((n/sum(n))*100),2))

  return(list(filter = counts_filter, reportdf = zerocount_df))

}

#' Create a filter for pgRNAs which have a low log2 CPM value for the plasmid/Day 0 sample/time point
#' @description This function flags and reports which and how many pgRNAs have low log2 CPM values for the plasmid/Day 0 sample/time point. If more than one column is specified as the plasmid sample,
#' we pool all the replicate samples to find the lower outlier and flag constructs for which any plasmid replicate has a log2 CPM value below the cutoff
#' @param gimap_dataset The special gimap_dataset from the `setup_data` function which contains the log2 CPM transformed data
#' @param filter_plasmid_target_col default is NULL, and if NULL, will select the first column only; this parameter specifically should be used to specify the plasmid column(s) that will be selected
#' @importFrom magrittr %>%
#' @importFrom dplyr mutate across if_any
#' @importFrom tidyr pivot_wider pivot_longer
#' @importFrom janitor clean_names
#' @return a named list with the filter `plasmid_filter` specifying which pgRNAs have low plasmid log2 CPM (column of interest is `plasmid_cpm_filter`) and a report df `plasmid_filter_report` for the number and percent of pgRNA which have a low plasmid log2 CPM
#' @examples \dontrun{
#'   gimap_dataset <- get_example_data("gimap")
#'
#'   qc_filter_plasmid(gimap_dataset)
#'
#'   #or to specify a cutoff value to be used in the filter rather than the lower outlier default
#'   qc_filter_plasmid(gimap_dataset, cutoff=2)
#'
#'   #or to specify a different column (or set of columns to select)
#'   qc_filter_plasmid(gimap_dataset, filter_plasmid_target_col = 1:2)
#'
#'   # or to specify a cutoff value that will be used in the filter rather than the lower outlier default as well as to specify a different column (or set of columns) to select
#'   qc_filter_plasmid(gimap_dataset, cutoff=1.75, filter_plasmid_target_col=1:2)
#'
#' }
#'

qc_filter_plasmid <- function(gimap_dataset, cutoff = NULL, filter_plasmid_target_col = NULL){

  if (is.null(filter_plasmid_target_col)) {filter_plasmid_target_col <- c(1)}

  if (!all(filter_plasmid_target_col %in% 1:ncol(gimap_dataset$transformed_data$log2_cpm))) {
    stop("The columns selected do not exist. `filter_plasmid_target_col` needs to correspond to the index of the columns in `gimap_dataset$transformed_data$log2_cpm` that you need to filter by")
  }

  plasmid_data <- data.frame(gimap_dataset$transformed_data$log2_cpm[, filter_plasmid_target_col]) %>% `colnames<-`(rep(c("plasmid_log2_cpm"), length(filter_plasmid_target_col))) %>% clean_names()

  if (length(filter_plasmid_target_col >1)){ #if more than one column was selected, collapse all of the columns into the same vector using pivot_longer to store in a df with the name of the rep and number for row/construct
    plasmid_data <- plasmid_data %>% mutate(construct = rownames(plasmid_data)) %>%
      pivot_longer(starts_with("plasmid_log2_cpm"),
                   values_to = "plasmid_log2_cpm",
                   names_to = "rep")
  }

  if (is.null(cutoff)) {
    # if cutoff is null, use lower outlier
    quantile_info <- quantile(plasmid_data$plasmid_log2_cpm)

    cutoff <- quantile_info["25%"] - (1.5 * (quantile_info["75%"] - quantile_info["25%"])) #later step make a function for this in utils since it's used more than once
  }

  if (length(filter_plasmid_target_col >1)){ #if more than one column was selected, take collapsed/pooled data and compare it to the cutoff
                                            #then pivot_wider so that the constructs are in the same row and we can use if_any to report if any of the replicates were flagged by the cutoff
                                            #return just that summary column (reporting if any are TRUE) as the filter
    plasmid_data <- plasmid_data %>%
      mutate(filterFlag = plasmid_log2_cpm < cutoff) %>%
      pivot_wider(id_cols = construct, names_from = rep, values_from = filterFlag)
    plasmid_cpm_filter <- plasmid_data %>%
      mutate(plasmid_cpm_filter=  if_any(.cols = starts_with('plasmid_log2_cpm'))) %>%
      select(plasmid_cpm_filter)

  } else {

    plasmid_cpm_filter <- as.data.frame(plasmid_data$plasmid_log2_cpm < cutoff) %>%`colnames<-`("plasmid_cpm_filter")

  }


  plasmid_filter_df <- data.frame("Plasmid_log2cpmBelowCutoff" = c(FALSE, TRUE), n = c(sum(!plasmid_cpm_filter), sum(plasmid_cpm_filter))) %>%
    mutate(percent = round(((n / sum(n)) * 100), 2)) #later step make a function for this in utils since it's used more than once

  return(list(
    plasmid_filter = plasmid_cpm_filter,
    plasmid_filter_report = plasmid_filter_df
  ))

}

