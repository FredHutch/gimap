#' Making a new gimap dataset
#' @description This function allows people to have their data ready to be processed by gimap
#' @param counts a matrix of data that contains the counts where rows are each paired_guide target and columns are each sample
#' @param pg_metadata metadata associated with the pgRNA constructs that correspond to the rows of the counts data
#' @param sample_metadata metadata associated with the samples of the dataset that correspond to the columns of the counts data
#' @return A special gimap_dataset to be used with the other functions in this package.
#' @export
#' @examples \dontrun{
#' data <- example_data() %>%
#'   dplyr::select(c("Day00_RepA", "Day05_RepA", "Day22_RepA", "Day22_RepB", "Day22_RepC")) %>%
#'   as.matrix()
#'
#' counts_data <- setup_data(data)
#' }
setup_data <- function(counts = NULL, pg_metadata = NULL, sample_metadata = NULL) {

  new_data <- gimap_data <- list(
    raw_counts =  NULL,
    counts_per_sample = NULL,
    coverage = NULL,
    transformed_data = list(
      long_form = NULL,
      count_norm = NULL,
      count_norm = NULL,
      cpm = NULL,
      log2_cpm = NULL),
    metadata = list(pg_metadata = NULL,
                    sample_metadata = NULL)
  )

  class(new_data) <- c("list", "gimap_dataset")

  if (is.null(counts)) stop("counts cannot be NULL")
  if (!is.matrix(counts)) stop("counts can only be in the form of a matrix")

  # If they don't give sample metadata, then we will make up a row id
  if (is.null(sample_metadata)) sample_metadata <- data.frame(id = 1:nrow(counts))
  if (!is.data.frame(sample_metadata)) stop("metadata can only be in the form of a data.frame")
  if (nrow(sample_metadata) != ncol(counts)) stop("the number of rows in the sample metadata is not equal to the number of columns in the counts" )

  if (!(length(unique(sample_metadata[, 1])) == length(sample_metadata[, 1]))) stop("The first column in sample metadata must be a unique ID")

  new_data$metadata$sample_metadata <- sample_metadata

  # If they don't give paired guide metadata, then we will make up a row id
  if (is.null(pg_metadata)) pg_metadata <- data.frame(id = 1:ncol(counts))
  if (!is.data.frame(pg_metadata)) stop("metadata can only be in the form of a data.frame")
  if (nrow(pg_metadata) != nrow(counts)) stop("the number of rows in the pg_info is not equal to the number of rows in the counts" )

  # we need the first column to be a unique id
  if (!(length(unique(pg_metadata[, 1])) == length(pg_metadata[, 1]))) stop("The first column in paired guide metadata must be a unique ID")
  new_data$metadata$pg_metadata <- pg_metadata

  # Store the raw counts
  new_data$raw_counts <- counts

  # Calculate the counts per sample
  new_data$counts_per_sample <- apply(counts, 2, sum)

  # Transform the data
  new_data$transformed_data$count_norm <- apply(counts, 2, function(x) -log10((x+1)/sum(x)))
  new_data$transformed_data$cpm <- apply(counts, 2, function(x) (x/sum(x))*1e6)
  new_data$transformed_data$log2_cpm <- log2(new_data$transformed_data$cpm +1)

  return(new_data)
}
