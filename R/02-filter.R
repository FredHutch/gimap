#' A function to run filtering
#' @description This function applies filters to the gimap data. By default it runs both the zero count (across all samples) and the low plasmid cpm filters, but users can select a subset of these filters or even adjust the behavior of each filter
#' @param .data Data can be piped in with %>% or |> from function to function. But the data must still be a gimap_dataset
#' @param gimap_dataset A special dataset structure that is setup using the `setup_data()` function.
#' @param filter_type Can be one of the following: `zero_count_only`, `low_plasmid_cpm_only` or `both`. Potentially in the future also `rep_variation`, `zero_in_last_time_point` or a vector that includes multiple of these filters.
#' @param filter_zerocount_target_col default is NULL; Which sample column(s) should be used to check for counts of 0? If NULL and not specified, downstream analysis will select all sample columns
#' @param filter_plasmid_target_col default is NULL, and if NULL, will select the first column only; this parameter specifically should be used to specify the plasmid column(s) that will be selected
#' @param cutoff default is NULL, relates to the low_plasmid_cpm filter; the cutoff for low log2 CPM values for the plasmid time period; if not specified, The lower outlier (defined by taking the difference of the lower quartile and 1.5 * interquartile range) is used
#' @param min_n_filters default is 1; this parameter defines at least how many/the minimum number of independent filters have to flag a pgRNA construct before the construct is filtered when using a combination of filters
#' You should decide on the appropriate filter based on the results of your QC report.
#' @importFrom purrr reduce
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
#' # If you want to only use a single filter or some subset, specify which using the filter_type parameter
#' gimap_dataset <- gimap_filter(gimap_dataset, filter_type = "zero_count_only") 
#' #or 
#' gimap_dataset <- gimap_filter(gimap_dataset, filter_type = "low_plasmid_cpm_only")
#' 
#' # 
#'
#' }
#'

gimap_filter <- function(.data = NULL,
                         gimap_dataset,
                         filter_type = "both",
                         cutoff = NULL,
                         filter_zerocount_target_col = NULL,
                         filter_plasmid_target_col = NULL,
                         min_n_filters = 1) {

  if (!is.null(.data)) gimap_dataset <- .data

  if (!("gimap_dataset" %in% class(gimap_dataset))) stop("This function only works with gimap_dataset objects which can be made with the setup_data() function.")
  
  #check filter type input to make sure that it is a supportable input
  if (!(filter_type %in% c("both", "zero_count_only", "low_plasmid_cpm_only"))) stop("Specification for `filter_type` not understood; Need to use 'both', 'zero_count_only', or 'low_plasmid_cpm_only'")
  
  zc_filter <- NULL
  p_filter <- NULL
  #*ADD any new filters here* assigning it a NULL value
  
  #This section calls the appropriate filtering functions and assigns results to the filter variables assigned NULL earlier (they will stay NULL if there filter wasn't selected to be run according to the input to the function)
  if (filter_type == "both"){
    zc_filter <- qc_filter_zerocounts(gimap_dataset, filter_zerocount_target_col = filter_zerocount_target_col)$filter
    p_filter <- qc_filter_plasmid(gimap_dataset, cutoff = cutoff, filter_plasmid_target_col = filter_plasmid_target_col)$plasmid_filter
  } else if (filter_type == "zero_count_only"){
    zc_filter <- qc_filter_zerocounts(gimap_dataset, filter_zerocount_target_col = filter_zerocount_target_col)$filter
  } else if(filter_type == "low_plasmid_cpm_only"){
    p_filter <- qc_filter_plasmid(gimap_dataset, cutoff = cutoff, filter_plasmid_target_col = filter_plasmid_target_col)$plasmid_filter
  }
  
  
  possible_filters <- list(zc_filter, p_filter)
  #*ADD any new filters here* within the list of `possible_filters`
  
  #this first cbinds each filter enumerated in possible_filters together (no matter how many there are, and ignores the NULLs) using the reduce function
  #then it finds the row sum (how many are filters flagged each construct e.g., number of TRUE in each row), 
  #and finally compares the row sum to the `min_n_filters` parameter to report TRUEs and FALSEs according to whether each construct is flagged by the minimum number of required filters
  #TRUE means it should be filtered, FALSE means it shouldn't be filtered
  combined_filter <- rowSums(reduce(possible_filters, cbind)) >= min_n_filters 
  
  
  gimap_dataset$filtered <- NULL #TODO: Filtered version of the data can be stored here

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
#' @param cutoff default is NULL, the cutoff for low log2 CPM values for the plasmid time period; if not specified, The lower outlier (defined by taking the difference of the lower quartile and 1.5 * interquartile range) is used
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