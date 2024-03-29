% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_sl_stats.R
\name{get_auc}
\alias{get_auc}
\title{Get AUC stat}
\usage{
get_auc(probs, trues, plot_roc = TRUE)
}
\arguments{
\item{probs}{(num) vector of probabilities}

\item{trues}{(int) vector of true labels (0/1)}

\item{plot_roc}{(lgl, default = TRUE), do you want the ROC plot will
be shown?}
}
\value{
AUC value (invisibly)
}
\description{
Get AUC stat
}
\examples{
get_auc(runif(100), sample(0:1, 100, replace = TRUE))
}
