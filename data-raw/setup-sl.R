#' ---
#' title: "Setup SL"
#' author: "Corrado Lanera"
#' date: "`r Sys.Date()`"
#' output:
#'   prettydoc::html_pretty:
#'     theme: architect
#'     highlight: github
#' ---

#+ knitr setup, include = FALSE
knitr::opts_chunk$set(
  collapse = TRUE,
  comment  = "#>"
)

#+ package load
library(SuperLearner)

#' ## Learners
#'
#' Impostiamo i parametri del SuperLearner che ci interessano, in
#' particolare i criteri di selezione delle variabili e i week learner
#' di interesse.
#' Nello specifico sono stati inclusi tutti i learner adeguati al tipo
#' di dati e che non davano problemi nelle computazioni di prova.
#' Stesso criterio per i criteri di selezione.


#+ sl-setup
base_learners <- ls("package:SuperLearner", pattern = "^[S]L") %>%
    setdiff(c(
        'SL.template', 'SL.bayesglm', 'SL.cforest', 'SL.knn',
        'SL.loess', 'SL.leekasso', 'SL.nnet', 'SL.nnls', 'SL.logreg',
        'SL.ridge', 'SL.svm', 'SL.gam', 'SL.glm', 'SL.step',
        'SL.step.forward', 'SL.step.interaction', 'SL.stepAIC',
        'SL.glm.interaction', "SL.dbarts", "SL.gbm", "SL.qda",
        "SL.lda", "SL.kernelKnn", "SL.extraTrees"
))

var_select <- setdiff(
    c("All", ls("package:SuperLearner", pattern = "screen")),
    c(
        'screen.template', 'write.screen.template', 'screen.corP',
        'screen.corRank', 'screen.glmnet', 'screen.SIS', 'screen.ttest'
    )
)

sl_library <- purrr::map(base_learners, ~ c(., var_select))
names(sl_library) <- base_learners



use_data(sl_library, overwrite = TRUE)
