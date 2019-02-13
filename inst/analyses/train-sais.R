#' ---
#' title: "SL with train on SAIS"
#' author: "@CorradoLanera"
#' date: "`r Sys.Date()`"
#' output:
#'   prettydoc::html_pretty:
#'     theme: cayman
#'     highlight: github
#' vignette: >
#'   %\VignetteIndexEntry{SL exploration on PROLOGUE data}
#'   %\VignetteEngine{knitr::rmarkdown}
#'   %\VignetteEncoding{UTF-8}
#' ---

#+ document-setup, include = FALSE
options(width = 100)

knitr::opts_chunk$set(
    collapse  = TRUE,
    comment   = "#>",
    out.width = "100%",

    fig.path  = "inst/analyses/figure/train-sais_",

    cache     = TRUE,
    autodep   = TRUE,

    warning   = FALSE
)


#' ## Setup
#'
#' Prima di cominciare carichiamo i pacchetti necessari

#+ package-loading, cache = FALSE
library(select)

library(SuperLearner)
library(Hmisc)
library(mice)

library(depigner)
library(DT)
library(pander)
    panderOptions("table.split.table", Inf)


library(tidyverse)
library(here)


#' ## Caricamento dati
#'
#' Importiamo i dati che ci interessano, ovvero quelli per i trial
#' SAIS (su cui addestreremo il nostromodello SL) e i dati PROLOGUE
#' (su cui lo testeremo).
#'
#' Ricordiamo che useremo solo le variabili comuni a entrambi i trial.
#' Inoltre ricordimao anche che i dati mancanti di tali variabili per lo
#' studio PROLOGUE sono stati imputati a partire dal dataset completo.
#'

#+ data-load
data("sais_selected", "prologue_miced_selected")

datatable(sais_selected)
datatable(prologue_miced_selected)

#' ## Learners
#'
#' Ora carichiamo i parametri del SuperLearner, ovvero le combinazioni
#' scelte di criteri di selezione delle variabili e di weak learners.

#+ SL-setup
data("SL.libraryD")
SL.libraryD


#' ## Addestramento
#'
#' Procediamo quindi all'addestramento.
#'
#' Innazitutto isoliamo per la base di dati di addestramento le
#' informazioni da usare e le informazioni per la risposta.

#+ training-preparation
training_data <- dplyr::select(sais_selected, -delta_hba1c) %>%
    as.data.frame() # required by SL

true_values <- (sais_selected[["delta_hba1c"]] == "HbA1c <= -0.5") %>%
    as.integer() # required by SL


#' Addestriamo il modello su SAIS e valutiamone le performance in
#' termini di AUC.

#+ SL-train-SAIS
model_auc <- train_sl(training_data, true_values, method = "method.AUC")
get_sl_stats(model_auc$sl, true_values)



model_nnls <- fit_sl(sais_selected, training_vars, SL.libraryD,
    method = "method.NNLS"
)
message(paste(
    "tempo impiegato per il train (NNLS):", model_nnls$times
))
model_nnls







#' A questo punto sul train il risultato è

#+ train-result




#' ## Test
#'
#' Occupiamoci quindi ora del test

#+ SL-test-PROLOGUE
estimated <- predict.SuperLearner(fit_train,
    newdata = prologue_test_miced[training_vars]#,
)

prologue_predictediction <- tibble(
    observed = as.integer(prologue_test_miced[["delta_hba1c"]]) - 1,
    probs    = estimated$pred[, 1]
)

roc_pred <- ROCR::prediction(
    predictions = prologue_predictediction[["probs"]],
    labels = prologue_predictediction[["observed"]]
)



ROCR::performance(roc_pred,
    measure = "tpr", x.measure = "fpr"
) %>% ROCR::plot()


map(seq(from = 0.5, to = 0.9, by = 0.05), ~{
    data_to_use
})




prologue %>%
    dplyr::select(-starts_with("ae_")) %>%
    dplyr::filter(!history_myocardial_infarction) %>%
    dplyr::distinct()

