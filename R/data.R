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
#' @details The variabels `bmi`, `ldl_adj`, `hypertension_adj`, and
#'          `dislipidemia_adj` were computed to be selected in the
#'          same way as in the SAIS (adjusted) dataset. I.e.
#'          dislipidemia is defined according to the
#'          American Heart Associations criteria, while hypertension
#'          is calculated accordingly to the 2017 American College of
#'          Cardiology/American Heart Association Hypertension Guideline
#'          definition. All this values were computed only at the
#'          baseline.
#'
#'          The variable `was_analyzed` subselect the records (393) for
#'          the 385 patients finally included in the original
#'          PROLOGUE study.
#'
#' @format A [tibble][tibble::tibble-package] with 473 rows and 262
#'         variables.
#' @source \url{
#'     https://datadryad.org/resource/doi:10.5061/dryad.qt743/2
#' }
"prologue"



#' SAIS RCT data
#'
#' A dataset containing the data for the SAIS Randomized Clinical
#' Trial (\url{https://doi.org/10.1371/journal.pone.0164255.s004} for
#' the baseline data, and
#' \url{https://doi.org/10.1371/journal.pone.0164255.s005} for the
#' follow-up data.
#'
#' @details The variabels `hypertension_adj`, `dislipidemia_adj`, and
#'          `total_colesterol` were computed to be selected in the
#'          same way as in the PROLOGUE (adjusted) dataset. I.e.
#'          dislipidemia is defined according to the
#'          American Heart Associations criteria, while hypertension
#'          is calculated accordingly to the 2017 American College of
#'          Cardiology/American Heart Association Hypertension Guideline
#'          definition. All this values were computed only at the
#'          baseline.
#'
#' @note Patient in the dataset are $104$ but there were one row per
#'       time (i.e., baseline or follow-up (AKA "fup")).
#'
#' @format A [tibble][tibble::tibble-package] with 208 rows and 46
#'         variables. (see details)
#' @source \url{https://doi.org/10.1371/journal.pone.0164255.s004} and
#'         \url{https://doi.org/10.1371/journal.pone.0164255.s005}
"sais"
