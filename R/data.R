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



#' SAIS RCT data
#'
#' A dataset containing the data for the SAIS Randomized Clinical
#' Trial (\url{https://doi.org/10.1371/journal.pone.0164255.s004} per
#' i dati baseline, e
#' \url{https://doi.org/10.1371/journal.pone.0164255.s005} per i dati di
#' follow-up).
#'
#' @note Patient in the dataset are $104$ but there were one row per
#'       time (i.e., baseline or follow-up (AKA "fup")).
#'
#' @format A [tibble][tibble::tibble-package] with 208 rows and 43
#'         variables.
#' @source \url{https://doi.org/10.1371/journal.pone.0164255.s004} and
#'         \url{https://doi.org/10.1371/journal.pone.0164255.s005}
"sais"
