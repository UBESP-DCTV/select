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
#'          definition., All, this values were computed only at the
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



#' Selected information from SAIS RCT data
#'
#' A dataset containing the selected baseline information for patient of
#' the \code{\link{sais}} study in the sitagliptin arm.
#'
#' @details The variabels selected are: `age`, `gender`, `bmi`,
#'          `hypertension_adj`, `dyslipidemia_adj`, `adiponectin`,
#'          `sbp`, `dbp`, `hba1c`, `fpg`, `ldl`, and `delta_hba1c`
#'          (the outcome)
#'
#' @format A [tibble][tibble::tibble-package] with 43 rows and 15
#'         variables. (see details)
#' @source \url{https://doi.org/10.1371/journal.pone.0164255.s004} and
#'         \url{https://doi.org/10.1371/journal.pone.0164255.s005}
"sais_selected"









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
#'          definition., All, this values were computed only at the
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







#' PROLOGUE RCT data
#'
#' Imputed version of the \code{\link{prologue}} database.
#'
#'
#' @details Variables representing dates and variables with more than
#'          400 missing data were removed before the imputation proces.
#'
#' @format A [tibble][tibble::tibble-package] with 473 rows and 228
#'         variables.
#' @source \url{
#'     https://datadryad.org/resource/doi:10.5061/dryad.qt743/2
#' }
"prologue_miced"



#' Selected information from PROLOGUE RCT data
#'
#' A dataset containing the imputed (for the missing entries)
#' and selected baseline information for patient of the
#' \code{\link{prologue}} study (both arms). Only information of
#' patients analyzed in the original study (see source) were included.
#'
#' @details The variabels selected are: `age`, `gender`, `bmi`,
#'          `hypertension_adj`, `dyslipidemia_adj`, `adiponectin`,
#'          `sbp`, `dbp`, `hba1c`, `fpg`, `ldl`, and `delta_hba1c`
#'          (the outcome).
#'
#'          Imputation were performed using MICE algorithm with 20
#'          iterations. Only the overmentioned variables (with the
#'          exclusion of the outcome) were imputed. For the imputation,
#'         , All, the information in the full \code{\link{prologue}}
#'          dataset where used.
#'
#' @format A [tibble][tibble::tibble-package] with 385 rows and 15
#'         variables. (see details)
#' @source \url{
#'     https://datadryad.org/resource/doi:10.5061/dryad.qt743/2
#' }
"prologue_miced_selected"





#' SuperLearner list of algorithms
#'
#' A list of, All, the algorithms (i.e. pairs of selection criteria and
#' learners) used to train the SuperLearner for our project.
#'
#' @details Algorithm selected are (for each item, the first reported
#'     name is the learner, the following are the selection methods
#'     used for it):
#' \describe{
#'     \item{SL.bartMachine }{SL.bartMachine, All, screen.randomForest}
#'     \item{SL.biglasso    }{SL.biglasso, All, screen.randomForest}
#'     \item{SL.caret       }{SL.caret, All, screen.randomForest}
#'     \item{SL.caret.rpart }{SL.caret.rpart, All, screen.randomForest}
#'     \item{SL.earth       }{SL.earth, All, screen.randomForest}
#'     \item{SL.glmnet      }{SL.glmnet, All, screen.randomForest}
#'     \item{SL.ipredbagg   }{SL.ipredbagg, All, screen.randomForest}
#'     \item{SL.ksvm        }{SL.ksvm, All, screen.randomForest}
#'     \item{SL.lm          }{SL.lm, All, screen.randomForest}
#'     \item{SL.mean        }{SL.mean, All, screen.randomForest}
#'     \item{SL.polymars    }{SL.polymars, All, screen.randomForest}
#'     \item{SL.randomForest}{SL.randomForest, All, screen.randomForest}
#'     \item{SL.ranger      }{SL.ranger, All, screen.randomForest}
#'     \item{SL.rpart       }{SL.rpart, All, screen.randomForest}
#'     \item{SL.rpartPrune  }{SL.rpartPrune, All, screen.randomForest}
#'     \item{SL.speedglm    }{SL.speedglm, All, screen.randomForest}
#'     \item{SL.speedlm     }{SL.speedlm, All, screen.randomForest}
#'     \item{SL.xgboost     }{SL.xgboost, All, screen.randomForest}
#' }
"sl_library"
