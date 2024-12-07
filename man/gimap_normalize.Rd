% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04-normalize.R
\name{gimap_normalize}
\alias{gimap_normalize}
\title{Normalize Log fold changes}
\usage{
gimap_normalize(
  .data = NULL,
  gimap_dataset,
  timepoints = NULL,
  treatments = NULL,
  control_name = NULL,
  num_ids_wo_annot = 20,
  rm_ids_wo_annot = TRUE,
  missing_ids_file = "missing_ids_file.csv"
)
}
\arguments{
\item{.data}{Data can be piped in with a tidyverse pipe from function to function. But the data must still be a gimap_dataset}

\item{gimap_dataset}{A special dataset structure that is setup using the `setup_data()` function.}

\item{timepoints}{Specifies the column name of the metadata set up in `$metadata$sample_metadata`
that has a factor that represents the timepoints. Timepoints will be made into three categories:
plasmid for the earliest time point, early for all middle timepoints and late for the latest timepoints.
The late timepoints will be the focus for the calculations. The column used for timepoints must be numeric or at least ordinal.}

\item{treatments}{Specifies the column name of the metadata set up in `$metadata$sample_metadata`
that has a factor that represents column that specifies the treatment applied to each. The replicates will be kept collapsed to an average.}

\item{control_name}{A name that specifies the data either in the treatments column that should be used as the control. This could be the Day 0 of treatment or an untreated sample.
For timepoints testing it will be assumed that the mininmum timepoint is the control.}

\item{num_ids_wo_annot}{default is 20; the number of pgRNA IDs to display to console if they don't have corresponding annotation data;
ff there are more IDs without annotation data than this number, the output will be sent to a file rather than the console.}

\item{rm_ids_wo_annot}{default is TRUE; whether or not to filter out pgRNA IDs from the input dataset that don't have corresponding annotation data available}

\item{missing_ids_file}{If there are missing IDs and a file is saved, where do you want this file to be saved? Provide a file path.}
}
\description{
This calculates the log fold change for a gimap dataset based on the annotation and metadata provided.
}
\examples{
\dontrun{

gimap_dataset <- get_example_data("gimap")

# Highly recommended but not required
run_qc(gimap_dataset)

gimap_dataset <- gimap_dataset \%>\%
  gimap_filter() \%>\%
  gimap_annotate(cell_line = "HELA") \%>\%
  gimap_normalize(
    timepoints = "day",
  )

# To see results
gimap_dataset$normalized_log_fc
}
}
