% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots-gi.R
\name{plot_exp_v_obs_scatter}
\alias{plot_exp_v_obs_scatter}
\title{Expected vs Observed CRISPR Scatterplot}
\usage{
plot_exp_v_obs_scatter(gimap_dataset, facet_rep = FALSE)
}
\arguments{
\item{gimap_dataset}{A special dataset structure that is originally setup using `setup_data()` and has had gi scores calculated with `calc_gi()`.}

\item{facet_rep}{Should the replicates be wrapped with facet_wrap()?}
}
\value{
A ggplot2 scatterplot of the target level observed vs expected
CRISPR scores.
}
\description{
This plot is meant to be functionally equivalent to Fig S5K (for HeLa, equivalent of Fig 3a for PC9).
Scatter plot of target-level observed versus expected CRISPR scores in the screen.
The solid line is the linear regression line for the negative control (single KO) pgRNAs,
while dashed lines indicate the lower and upper quartile residuals.
}
\examples{
\dontrun{

gimap_dataset <- get_example_data("gimap") \%>\%
  gimap_filter() \%>\%
  gimap_annotate(cell_line = "HELA") \%>\%
  gimap_normalize(
    timepoints = "day",
    missing_ids_file = tempfile()
  ) \%>\%
  calc_gi()

# To plot results
plot_exp_v_obs_scatter(gimap_dataset)
plot_rank_scatter(gimap_dataset)
plot_volcano(gimap_dataset)
}
}
