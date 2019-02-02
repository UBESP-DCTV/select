#' PROLOGUE RCT data
#'
#' A dataset containing the data for the PROLOGUE Randomized Clinical
#' Trial (\url{https://www.ncbi.nlm.nih.gov/pubmed/27351380}).
#'
#' @note Patient in the dataset are $463$ but there were one row per
#'       patient per adverse event they has experienced.
#'       The only differences in the rows for a same patient are in
#'       the variables starting with "ae_" indicating information about
#'       adverse events (ae).
#'       If you do not need those information you can reduce the dataset
#'       without loosing any information different from ae with the code
#'       `distinct(select(prologue, -starts_with("ae_")))`.
#'
#' @format A [tibble][tibble::tibble-package] with 473 rows and 260
#'         variables.
#' @source \url{
#'     https://datadryad.org/resource/doi:10.5061/dryad.qt743/2
#' }
"prologue"