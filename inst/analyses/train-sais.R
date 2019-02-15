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

    warning   = FALSE
)


#' ## Setup
#'
#' Prima di cominciare carichiamo i pacchetti necessari

#+ package-loading, cache = FALSE

library(SuperLearner)

# devtools::install_github("UBESP-DCTV/select")    # NOTE: it's private!
library(select)

# devtools::install_github(CorradoLanera/depigner) # NOTE: it's open
library(depigner)
library(DT)
library(pander)
    panderOptions("table.split.table", Inf)

library(tidyverse)
library(Hmisc)


set.seed(1)

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

# ?sais_select
datatable(sais_selected)

# ?prologue_miced_selected
datatable(prologue_miced_selected)

#' ## Learners
#'
#' Ora carichiamo i parametri del SuperLearner, ovvero le combinazioni
#' scelte di criteri di selezione delle variabili e di weak learners.

#+ SL-setup
data("sl_library")
# ?sl_library
sl_library


#' ## Addestramento
#'
#' Procediamo quindi all'addestramento.
#'
#' Innazitutto isoliamo per la base di dati di addestramento le
#' informazioni da usare e le informazioni per la risposta.

#+ training-preparation
training_data <- dplyr::select(sais_selected,
        -id, -allocation, -delta_hba1c_dbl, -delta_hba1c_fct
    ) %>%
    as.data.frame() # required by SL

true_values <- as.integer( # required by SL
        sais_selected[["delta_hba1c_fct"]] == "HbA1c <= -0.5"
    )

datatable(training_data)
true_values

#' Addestriamo il modello su SAIS e valutiamone le performance in
#' termini di AUC.

#+ SL-train-SAIS

# ?train_sl
# train_sl

#+ results = 'hide'
model_auc <- train_sl(training_data, true_values,
    SL.libraryD = sl_library, method = "method.AUC"
)


#' Il risultato sul traing è quindi

#+ train-result

# ?get_sl_stats
# get_sl_stats
get_sl_stats(model_auc, true_values)


#' Il modello risulta molto buono ai vari cutoff potenziali che
#' presenta. Salviamo la sequenza di questi cutoff in modo da
#' considerare i "primi" come i cut-off rappresentanti
#' "bassa probabilità" di risposta.
train_cutoff <- sort(unique(model_auc$SL.predict))



#' ## Test
#'
#' Occupiamoci quindi ora di applicare il modello SuperLearner ai
#' dati PROLOGUE.

#+ SL-test-PROLOGUE
test_data <- prologue_miced_selected %>%
    dplyr::select(
        -delta_hba1c_dbl, -delta_hba1c_fct, -id, -allocation
    ) %>%
    as.data.frame() # required by SL

true_test <- as.integer(
        prologue_miced_selected[["delta_hba1c_fct"]] == "HbA1c <= -0.5"
) # required by SL

estimated_test <- predict(model_auc, test_data)[["pred"]][, 1]
test_cutoff <- sort(unique(estimated_test))

#' Anche se non siamo interessati, guardiamo l'AUC in predizione (che
#' ci aspettiamo non sia buona, infatto lo scopo non è predire
#' l'outcome!)

#+ train-auc

# ?get_auc
get_auc(estimated_test, true_test)

#' ## Analisi cut-off
#' A questo punto, prendiamo i primi 5 livelli di cut-off del training
#' set e filtriamo i dati in funzione degli stessi.
#' Dopodichè, per ciascun livello di cut off ristimiamo sia un modello
#' globale che modelli separati per trattamento dei pazienti PROLOGUE
#' rimasti dopo il filtro dato dal cut-off di essere rispondenti

data("prologue_miced")
# ?prologue_miced

#' Prima di tutto aggiungiamo le informazioni di allocazione per
#' aggiustare i modelli tagliati
models <- prologue_miced %>%
    dplyr::select(
        id, dm_treatment, statin, al_sbp, al_hb_a1c_jds, al_max_imt
    ) %>%
    dplyr::distinct() %>%
    dplyr::right_join(dplyr::select(prologue_miced_selected,
        id, gender, age, delta_hba1c_dbl, allocation
    )) %>%
    dplyr::mutate(
        allocation = relevel(allocation, ref = "conventional")
    )

#' Quindi creiamo una versione del database per ogni livello di cutoff
#' (tra quelli differenti trovati nel training set... visto che ci
#' interessa avere un uso prospettico...forse..mmm),
#' tagliamo ciascuno a quel livello, addestriamo un modello per ciascuno
#' (quello senza taglio non va aggiustato), e quindi estraiamo le
#' statistiche di interesse.

train_cutted_models <- models %>%
    tidyr::crossing(cutoff = c(0, train_cutoff[1:10])) %>%
    dplyr::group_by(cutoff) %>%
    tidyr::nest(.key = "full_prologue") %>%
    dplyr::mutate(

        filtered_prologue = purrr::map2(full_prologue, cutoff, ~ {
            dplyr::filter(.x, estimated_test >= .y)
        }),

        model = purrr::map(filtered_prologue, ~{

            if (nrow(.x) == 385) {
                glm(delta_hba1c_dbl ~ allocation, data = .x)
            } else {
                glm(
                    delta_hba1c_dbl ~ dm_treatment + statin + age +
                        gender + al_sbp + al_hb_a1c_jds + al_max_imt +
                        allocation,
                    data = .x
                )
            }
        }),

        size = purrr::map_int(filtered_prologue, nrow),
        aic  = purrr::map_dbl(model, ~ broom::glance(.)[["AIC"]]),

        coef_allocation = purrr::map(model, ~{
            broom::tidy(.) %>%
                dplyr::filter(term == "allocationsitagliptin") %>%
                dplyr::select(estimate, std.error, p.value)
        }),
        treat_estimate = purrr::map_dbl(coef_allocation, "estimate"),
        treat_se       = purrr::map_dbl(coef_allocation, "std.error"),
        treat_p        = purrr::map_dbl(coef_allocation, "p.value"),
        treat_lower    = treat_estimate - 1.96 * treat_se,
        treat_upper    = treat_estimate + 1.96 * treat_se,

        # utile per le etichette dell'asse x dei grafici
        x_labelled     = round(cutoff, 3) %>% paste0("\nN = ", size)
    ) %>%
    dplyr::select(-coef_allocation)


#' ## Andamento della qualità dei modelli e sample size per talgio
train_cutted_models %>%
    tidyr::gather("key", "value", aic, size) %>%
    ggplot(aes(x = cutoff, y = value, colour = key)) +
    geom_point() +
    geom_line() +
    ggtitle("pruning cutoff Vs performance and sample size")



#' ## Effetto per livello di taglio
train_cutted_models %>%
    ggplot(aes(x = cutoff, y = treat_estimate)) +
    geom_smooth(aes(y = treat_upper), se = FALSE, colour = "red") +
    geom_smooth(se = FALSE, colour = "blue") +
    geom_smooth(aes(y = treat_lower), se = FALSE, colour = "green") +
    geom_pointrange(aes(ymin = treat_lower, ymax = treat_upper),
        colour = "orange"
    ) +
    geom_abline(slope = 0, intercept = 0) +
    ggtitle("Treatment effect (and 95% CI) by cutoff level") +
    scale_x_continuous(
        breaks = train_cutted_models$cutoff[-c(4, 9)],
        labels = train_cutted_models$x_labelled[-c(4, 9)]
    ) +
    theme_classic()



#' ### Analisi dati "migliori" Vs base
base_data <- train_cutted_models$filtered_prologue[[1]]
best_data <- train_cutted_models$filtered_prologue[[7]]

#' Distribuzione confondenti per braccio (tagliato)
summary(allocation ~ ., data = best_data[-1],
    method = "reverse",
    overall = TRUE,
    continuous = 4,
    test = TRUE
    ) %>%
    depigner::tidy_summary(
        prtest   = "P",
        exclude1 = FALSE,
        long     = TRUE
    ) %>%
    mutate_all(funs(str_replace_all(., "&nbsp;", " "))) %>%
    set_names(c(" ", names(.)[-1])) %>%
    pander::pander()


#' Distribuzione confondenti per braccio (originale)
summary(allocation ~ ., data = base_data[-1],
    method = "reverse",
    overall = TRUE,
    continuous = 4,
    test = TRUE
    ) %>%
    depigner::tidy_summary(
        prtest   = "P",
        exclude1 = FALSE,
        long     = TRUE
    ) %>%
    mutate_all(funs(str_replace_all(., "&nbsp;", " "))) %>%
    set_names(c(" ", names(.)[-1])) %>%
    pander::pander()



#' ## Analisi di potenza
d_adj <- sd(prologue_miced_selected$hba1c)

pwr_base <- pwr::pwr.t.test(n = 197, d = -0.1/d_adj)
ss_cutted <- pwr::pwr.t.test(d = -0.15/d_adj, power = pwr_base$power)

#' Se volessi la stessa potenza dello studio originale per individuare
#' un effetto più grande (pari a quello dello studio tagliato) quanto
#' sarebbe stata la sample size? (spoiler: molto meno della gente
#' rimasta nel braccio tagliato... ma quesa cosa che senso ha? è
#' negativa? Ha senso? ... forse no. Da pensarci con calma, freschi e
#' riposati)
pwr_base
ss_cutted


#' Ma come va il talgio rispetto ai bracci?
train_cutted_models %>%
    mutate(
        ss_conventional = purrr::map_int(filtered_prologue, ~{
            dplyr::filter(., allocation == "conventional") %>%
                nrow()
        }),
        ss_sitagliptin = purrr::map_int(filtered_prologue, ~{
            dplyr::filter(., allocation == "sitagliptin") %>%
                nrow()
        })
    ) %>%
    dplyr::select(cutoff, ss_conventional, ss_sitagliptin) %>%
    tidyr::gather("key", "value", ss_conventional, ss_sitagliptin) %>%
    ggplot(aes(x = cutoff, y = value, colour = key)) +
    geom_line()




#' ## Facciamo lo stesso talgiando per i cutoff determinati dal test set
#' (che non ha molto senso...forse... anche se, se lo vediamo
#' retrospettico, forse si... pensiamoci.)

test_cutted_models <- models %>%
    tidyr::crossing(cutoff = c(0, test_cutoff[1:10])) %>%
    dplyr::group_by(cutoff) %>%
    tidyr::nest(.key =  "full_prologue") %>%
    dplyr::mutate(

        filtered_prologue = purrr::map2(full_prologue, cutoff, ~ {
            dplyr::filter(.x, estimated_test >= .y)
        }),

        model = purrr::map(filtered_prologue, ~{glm(
            delta_hba1c_dbl ~ dm_treatment + statin + age + gender +
                al_sbp + al_hb_a1c_jds + al_max_imt + allocation,
            data = .
        )}),

        size = purrr::map_int(filtered_prologue, nrow),
        aic  = purrr::map_dbl(model, ~{
            broom::glance(.)[["AIC"]]
        }),
        coef_allocation = purrr::map(model, ~{
            broom::tidy(.) %>%
                dplyr::filter(term == "allocationsitagliptin") %>%
                dplyr::select(estimate, std.error, p.value)
        }),
        treat_estimate = purrr::map_dbl(coef_allocation, "estimate"),
        treat_se       = purrr::map_dbl(coef_allocation, "std.error"),
        treat_p        = purrr::map_dbl(coef_allocation, "p.value"),
        treat_lower    = treat_estimate - 1.96 * treat_se,
        treat_upper    = treat_estimate + 1.96 * treat_se,
        x_labelled     = round(cutoff, 3) %>% paste0("\nN = ", size)
    ) %>%
    dplyr::select(-coef_allocation)


test_cutted_models %>%
    tidyr::gather("key", "value", aic, size) %>%
    ggplot(aes(x = cutoff, y = value, colour = key)) +
    geom_point() +
    geom_line() +
    ggtitle("pruning cutoff Vs performance and sample size")



test_cutted_models %>%
    ggplot(aes(x = cutoff, y = treat_estimate)) +
    geom_pointrange(aes(ymin = treat_lower, ymax = treat_upper),
        colour = "orange"
    ) +
    ggtitle("Treatment effect (and 95% CI) by cutoff level") +
    theme_classic()


test_cutted_models[-1, ] %>%
    ggplot(aes(x = cutoff, y = treat_estimate)) +
    geom_pointrange(aes(ymin = treat_lower, ymax = treat_upper),
        colour = "orange"
    ) +
    scale_x_continuous(
        breaks = test_cutted_models$cutoff,
        labels = test_cutted_models$x_labelled
    ) +
    ggtitle("Treatment effect (and 95% CI) by cutoff level") +
    theme_classic()

