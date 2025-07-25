% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/02-gimap_filter.R
\name{qc_filter_plasmid}
\alias{qc_filter_plasmid}
\title{Create a filter for pgRNAs which have a low log2 CPM value for the
plasmid/Day 0 sample/time point}
\usage{
qc_filter_plasmid(
  gimap_dataset,
  cutoff = NULL,
  filter_plasmid_target_col = NULL
)
}
\arguments{
\item{gimap_dataset}{The special gimap_dataset from the `setup_data` function
which contains the log2 CPM transformed data}

\item{cutoff}{default is NULL, the cutoff for low log2 CPM values for the
plasmid time period; if not specified, The lower outlier (defined by taking
the difference of the lower quartile and 1.5 * interquartile range) is used}

\item{filter_plasmid_target_col}{default is NULL, and if NULL, will select
the first column only; this parameter specifically should be used to specify
the plasmid column(s) that will be selected}
}
\value{
a named list with the filter `filter` specifying which pgRNAs have
low plasmid log2 CPM (column of interest is `plasmid_cpm_filter`) and a
report df `reportdf` for the number and percent of pgRNA which have a low
plasmid log2 CPM
}
\description{
This function flags and reports which and how many pgRNAs have
low log2 CPM values for the plasmid/Day 0 sample/time point. If more than one
column is specified as the plasmid sample,
we pool all the replicate samples to find the lower outlier and flag
constructs for which any plasmid replicate has a log2 CPM value below the
cutoff
}
\examples{
\dontrun{
gimap_dataset <- get_example_data("gimap", data_dir = tempdir())

qc_filter_plasmid(gimap_dataset)

# or to specify a cutoff value to be used in the filter rather than the lower
# outlier default
qc_filter_plasmid(gimap_dataset, cutoff = 2)

# or to specify a different column (or set of columns to select)
qc_filter_plasmid(gimap_dataset, filter_plasmid_target_col = 1:2)

# or to specify a cutoff value that will be used in the filter rather than
# the lower outlier default as well as to specify a different column (or set
# of columns) to select
qc_filter_plasmid(gimap_dataset,
  cutoff = 1.75,
  filter_plasmid_target_col = 1:2
)
}

}
