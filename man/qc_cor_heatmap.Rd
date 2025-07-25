% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-qc.R
\name{qc_cor_heatmap}
\alias{qc_cor_heatmap}
\title{Create a correlation heatmap for the pgRNA CPMs}
\usage{
qc_cor_heatmap(gimap_dataset)
}
\arguments{
\item{gimap_dataset}{The special gimap_dataset from the `setup_data` function which contains the transformed data}
}
\value{
`sample_cor_heatmap` a pheatmap
}
\description{
This function uses the `cor` function to find correlations between the
 sample CPM's and then plots a heatmap of these
}
\examples{
\dontrun{
gimap_dataset <- get_example_data("gimap")
qc_cor_heatmap(gimap_dataset)
}
}
