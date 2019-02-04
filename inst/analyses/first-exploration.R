#' ---
#' title: "SL exploration on PROLOGUE data"
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

    fig.path  = "inst/analyses/first-exploration_",

    cache     = TRUE,
    autodep   = TRUE,

    warning   = FALSE
)


#' ## Setup
#'
#' Prima di tutto carichiamo i pacchetti necessari

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


#' A questo punto possiamo caricare i dati e sistemarli come ci è utile
#' per le analisi che dovremo fare.
#'
#' In particolare i predittori saranno:
#' - Age, years
#' - Sex
#' - Body mass index, kg/m2
#' - Hypertension (history of)
#' - Dyslipidemia (history of)
#' - Adiponectin
#' - Myocardial infarction
#' - Percutaneous coronary intervention
#' - Coronary artery bypass grafting
#' - Chronic heart failure
#' - Arrhythmia
#' - Stroke
#' - Systolic blood pressure, mm Hg
#' - Diastolic blood pressure, mm Hg
#' - HbA1c, percent
#' - Fasting plasma glucose (FPG), mmol/l
#' - Low-density lipoprotein cholesterol (LDL), mmol/l
#' - Serum creatinine, μmol/l
#'
#' Mentre la variabile di risposta sarà:
#' - delta HbA1c a 12 mesi
#'
#' dicotomizzata secondo due scenari (success vs failure):
#' 1. <= -1   vs > -1
#' 2. <= -0.5 vs > -0.5
#'
#' limitatamente all'arm sitagliptin)

#+ data-tidy
data("prologue")

used_data <- prologue %>%
    mutate(
        bmi = body_weight_0m / (height_background / 100)^2
    ) %>%
    filter(allocation == "sitagliptin") %>%
    mutate(delta_hba1c = hba1c_ngsp_12m - hba1c_ngsp_0m) %>%
    select(
        age, sex, bmi,
        history_hypertension, history_dyslipidemia,
        hmw_adiponectin_0m, history_myocardial_infarction,
        history_pci, history_cabg, history_cardiac_insufficiency,
        history_arrhythmia, ev_stroke, sbp_0m, dbp_0m, hba1c_ngsp_0m,
        fbs_0m, small_dense_ldl_0m, creatinine_0m,

        delta_hba1c
    ) %>%
    filter(!is.na(delta_hba1c))


scenario_1 <- used_data %>%
    mutate(
        delta_hba1c_1 = factor(delta_hba1c > -1,
            levels = c(TRUE, FALSE),
            labels = c("HbA1c > -1", "HbA1c <= -1")
        )
    ) %>%
    select(-delta_hba1c)

scenario_05 <- used_data %>%
    mutate(
        delta_hba1c_05 = factor(delta_hba1c > -0.5,
            levels = c(TRUE, FALSE),
            labels = c("HbA1c > -0.5", "HbA1c <= -0.5")
        )
    ) %>%
    select(-delta_hba1c)

#' A questo punto diamo uno sguardo ai dati e alle loro correlazioni
#'
#' ### Scenario delta-HbA1c 0 VS 12 mesi con cut-off a -1

#+ scenario-1
datatable(scenario_1)

spearman2(delta_hba1c_1 ~ .,
    data = mutate_all(scenario_1, as.numeric)
) %>%
 plot()


summary(delta_hba1c_1 ~ .,
    data       = scenario_1,
    method     = "reverse",
    continuous = 3,
    test       = TRUE
) %>%
    tidy_summary(
        prtest = "P",
        exclude1 = FALSE, long = TRUE,
        digits = 3
    ) %>%
    pander()

#' Notiamo che in questo caso l'unica variabile significativamente
#' diversa tra i due gruppi di outcome è proprio il valore baseline
#' del valore della risposta, il quale potrebbe quindi diventare un
#' predittore piuttosto importante.


#' ### Scenario delta-HbA1c 0 VS 12 mesi con cut-off a -0.5

#+ scenario-05
datatable(scenario_05)

spearman2(delta_hba1c_05 ~ .,
    data = mutate_all(scenario_05, as.numeric)
) %>%
 plot()

summary(delta_hba1c_05 ~ .,
    data       = scenario_05,
    method     = "reverse",
    continuous = 3,
    test       = TRUE
) %>%
    tidy_summary(
        prtest = "P",
        exclude1 = FALSE, long = TRUE,
        digits = 3
    ) %>%
    pander()


#' Nel caso in cui il cut-off sia posto a -0.5 invece risultano
#' significative anche hmw_adiponectin_0m e
#' history_myocardial_infarction. La correlazione con l'outcome,
#' ad eccezione del valore baseline per la risposta, resta comunque
#' non localmente rilevante.
#'
#' ## SuperLearner
#' iniziamo impostando le basi del SL

#+ SL-setup
base_learners <- ls("package:SuperLearner", pattern = "^[S]L") %>%
    setdiff(c(
        'SL.template', 'SL.bayesglm', 'SL.cforest', 'SL.knn',
        'SL.loess', 'SL.leekasso', 'SL.nnet', 'SL.nnls', 'SL.logreg',
        'SL.ridge', 'SL.svm', 'SL.gam', 'SL.glm', 'SL.step',
        'SL.step.forward', 'SL.step.interaction', 'SL.stepAIC',
        'SL.glm.interaction'
))

var_select <- c("All",
        ls("package:SuperLearner", pattern = "screen")
    ) %>%
    setdiff(c(
        'screen.template', 'write.screen.template', 'screen.corP',
        'screen.corRank', 'screen.glmnet', 'screen.SIS', 'screen.ttest'
    ))

SL.libraryD <- purrr::map(base_learners, ~ c(., var_select))
names(SL.libraryD) <- base_learners

#+data-for-SL
scenario_1 <- scenario_1 %>%
    mutate_all(as.numeric) %>%
    as.data.frame() %>%
    mice(
        # method = 'rf',
        visitSequence = 'monotone',
        seed = 8700
    ) %>%
    mice::complete(5)

scenario_05 <- scenario_05 %>%
    mutate_all(as.numeric) %>%
    as.data.frame() %>%
    mice(
        method = 'rf',
        visitSequence = 'monotone',
        seed = 8700
    ) %>%
    mice::complete(5)

#+ SL-run-1
{
    set.seed(123)
    tic_1 <- Sys.time()
    fit_1 <- CV.SuperLearner(
        X          = scenario_1[colnames(scenario_1) != "delta_hba1c_1"],
        Y          = scenario_1[["delta_hba1c_1"]] - 1,
        # id         = "id", # not implemented

        family     = binomial(),
        method     = 'method.AUC',#'method.NNLS',#
        SL.library = SL.libraryD,

        cvControl  = list(
            V          = 5,
            shuffle    = FALSE,
            stratifyCV = TRUE
        ),

        verbose    = TRUE
    )
    toc_1 <- Sys.time()
}

summary(fit_1)


#+ SL-run-2
{
    set.seed(123)
    tic_05 <- Sys.time()
    fit_05 <- CV.SuperLearner(
        X          =  scenario_05[colnames(scenario_05) != "delta_hba1c_05"],
        Y          =  scenario_05[["delta_hba1c_05"]] - 1,
        # id         = "id", # not implemented

        family     = binomial(),
        method     = 'method.AUC',#'method.NNLS',#
        SL.library = SL.libraryD,

        cvControl  = list(
            V          = 5,
            shuffle    = FALSE,
            stratifyCV = TRUE
        ),

        verbose    = TRUE
    )
    toc_05 <- Sys.time()
}

{
    set.seed(123)
    tic_05_10f <- Sys.time()
    fit_05_10f <- CV.SuperLearner(
        X          =  scenario_05[colnames(scenario_05) != "delta_hba1c_05"],
        Y          =  scenario_05[["delta_hba1c_05"]] - 1,
        # id         = "id", # not implemented

        family     = binomial(),
        method     = 'method.AUC',#'method.NNLS',#
        SL.library = SL.libraryD,

        cvControl  = list(
            V          = 10,
            shuffle    = FALSE,
            stratifyCV = TRUE
        )#,
#
#         verbose    = TRUE
    )
    toc_05_10f <- Sys.time()
}



#' ## Summaries
#+ summaries
stats_fit_05 <- summary(fit_05)
stats_fit_05$Risk.SL %>% summary()
summary(fit_05)$Table %>%
    as_tibble() %>%
    dplyr::select(-se)
toc_05 - tic_05

stats_fit_05_10f <- summary(fit_05_10f)
stats_fit_05_10f$Risk.SL %>% summary()
summary(fit_05_10f)$Table %>%
    as_tibble() %>%
    dplyr::select(-se)
toc_05_10f - tic_05_10f


# saveRDS(fit_05_10f, file = "sl_prologue_leq-05_cv10.RDS")

