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


#' ## Import and variable selection
#'
#' Importiamo i dati che ci interessano, ovvero quelli per i trial
#' SAIS (su cui addestreremo il nostromodello SL) e i dati PROLOGUE
#' (su cui lo testeremo).

#+ data-load
data("sais")
data("prologue")

datatable(sais)
datatable(prologue)

#' Selezioniamo quindi le variabili comuni a entrambi i trial,
#' che utilizzeremo per le analisi, ovvero;
#' - Age
#' - Gender
#' - BMI
#' - Hypertension
#' - Dyslipidemia
#' - Adiponectin
#' - SBP
#' - DBP
#' - HbA1c al baseline
#' - FPG
#' - LDL
#'
#' Inoltre calcoliamo e selezioniamo come outcome la differenza tra i
#' livelli percentuali di HbA1c a 12 mesi e al baseline e consideriamo
#' come cut-off il livello di delta pari a $-0.5$.
#'
#' Inoltre consideriamo solo i dati provenienti dal ramo dei trattati
#' con sitagliptin.

#+ data-preparation
sais_train <- sais %>%
    dplyr::filter(allocation == "sitagliptin") %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
        delta_hba1c = (diff(hba1c) <= -0.5) %>%
            factor(
                levels = c(FALSE, TRUE),
                labels = c("HbA1c > -0.5", "HbA1c <= -0.5")
            )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(time == "baseline") %>%
    dplyr::select(
        age, gender, bmi,
        hypertension_adj, dislipidemia_adj, adiponectin,
        sbp, dbp,
        hba1c,
        fpg, ldl,

        delta_hba1c
    ) %>%
    dplyr::filter(!is.na(delta_hba1c))

datatable(sais_train)

prologue_test <- prologue %>%
    dplyr::mutate(bmi = body_weight_0m / (height_background / 100)^2) %>%
    # dplyr::filter(allocation == "sitagliptin") %>%
    dplyr::mutate(
        delta_hba1c = ((hba1c_ngsp_12m - hba1c_ngsp_0m) <= -0.5) %>%
            factor(
                levels = c(FALSE, TRUE),
                labels = c("HbA1c > -0.5", "HbA1c <= -0.5")
            )
    ) %>%
    dplyr::select(
        age, sex, bmi,
        hypertension_adj, dislipidemia_adj, hmw_adiponectin_0m,
        sbp_0m, dbp_0m,
        hba1c_ngsp_0m,
        fbs_0m, small_dense_ldl_0m,

        delta_hba1c
    ) %>%
    dplyr::filter(!is.na(delta_hba1c))

datatable(prologue_test)

#' A questo punto imputiamo i dati mancanti eventuali negli studi
#' considerati e trasformiamo tutto in dati numerici (SL vuole dati
#' completi e numerici)

#+ data-adjust
{
    set.seed(123)
    sais_train_miced <- sais_train %>%
        as.data.frame() %>%
        mutate_all(as.numeric) %>%
        mice(method = 'rf', visitSequence = 'monotone', seed = 8700) %>%
        mice::complete(5)
}

{
    set.seed(123)
    prologue_test_miced <- prologue_test %>%
        as.data.frame() %>%
        mutate_all(as.numeric) %>%
        mice(method = 'rf', visitSequence = 'monotone', seed = 8700) %>%
        mice::complete(5)
}


#' ## Learners
#'
#' Ora definiamo i parametri del SuperLearner che ci interessano, in
#' particolare i criteri di selezione delle variabili e i week learner
#' di interesse.

#+ sl-setup
#+ SL-setup
base_learners <- ls("package:SuperLearner", pattern = "^[S]L") %>%
    setdiff(c(
        'SL.template', 'SL.bayesglm', 'SL.cforest', 'SL.knn',
        'SL.loess', 'SL.leekasso', 'SL.nnet', 'SL.nnls', 'SL.logreg',
        'SL.ridge', 'SL.svm', 'SL.gam', 'SL.glm', 'SL.step',
        'SL.step.forward', 'SL.step.interaction', 'SL.stepAIC',
        'SL.glm.interaction', "SL.dbarts", "SL.gbm", "SL.qda"
))

var_select <- c(
        "All",
        ls("package:SuperLearner", pattern = "screen")
    ) %>%
    setdiff(c(
        'screen.template', 'write.screen.template', 'screen.corP',
        'screen.corRank', 'screen.glmnet', 'screen.SIS', 'screen.ttest'
    ))

SL.libraryD <- purrr::map(base_learners, ~ c(., var_select))
names(SL.libraryD) <- base_learners


#' ## Addestramento
#'
#' Addestriamo quinid il primo modello su SAIS

#+ SL-train-SAIS
{
    set.seed(123)
    tic_train <- Sys.time()
    fit_train <- SuperLearner(
        X = dplyr::select(sais_train_miced, -delta_hba1c) %>%
            as.data.frame(),
        Y = sais_train_miced[["delta_hba1c"]] - 1,

        family     = binomial(),
        method     = 'method.AUC',#'method.NNLS',#
        SL.library = SL.libraryD,

        cvControl  = SuperLearner.CV.control(
            V          = 5L,
            shuffle    = TRUE,
            stratifyCV = TRUE
        )
    )
    toc_train <- Sys.time()
}

#' A questo punto sul train il risultato è

#+ train-result
message(paste("tempo impiegato per il train:", toc_train - tic_train))
fit_train

#' ## Test
#'
#' Occupiamoci quindi ora del test

#+ SL-test-PROLOGUE
estimated <- predict.SuperLearner(fit_train,
    newX = dplyr::select(prologue_test_miced, -delta_hba1c) %>%
        as.data.frame()#,
    #
    # X = dplyr::select(sais_train_miced, -delta_hba1c),
    # Y = sais_train_miced[["delta_hba1c"]] - 1
)

estimated$pred

prologue_predictediction <- tibble(
    observed = prologue_test_miced[["delta_hba1c"]] - 1,
    probs    = estimated$pred[, 1]
)

