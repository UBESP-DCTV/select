#' ---
#' title: "Import RCTs data"
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
requireNamespace("lubridate")
library(tidyverse)
library(readxl)
library(janitor)
library(here)
library(mice)

# SAIS ------------------------------------------------------------



#' ## SAIS study data

#' ### import

#+ sais-import
sais_1_path <- here(
    "data-raw",
    "journal.pone.0164255.s004.CSV"
)
sais_2_path <- here(
    "data-raw",
    "journal.pone.0164255.s005.CSV"
)

sais_data <-  list(sais_1_path, sais_2_path) %>%
    map(~{
        read_csv(.x, na = c("", ".", " ", "ND")) %>%
        mutate(id = row_number()) %>%
        clean_names() %>%
        mutate(
            id = id,
            allocation = factor(medication_glimepiride_0_sitagliptin_1,
                levels = c(0, 1),
                labels = c("glimepiride", "sitagliptin")
            ),
            age    = as.integer(age_years),
            gender = factor(gender_male_1_female_0,
                levels = c(1, 0),
                labels = c("male", "female")
            ),
            fmd_percent = percent_fmd_percent,
            sbp         = sbp_mm_hg,
            dbp         = dbp_mm_hg,
            c_sbp       = c_sbp_mm_hg,
            augmentation_index = augmentation_index,
            cardiac_index = cardiac_index_l_min_m2,
            tpri = tpri,
            lf_hf_ratio_of_rri = lf_hf_ratio_of_rri,
            height = height_cm,
            weight = weight_kg,
            bmi = bmi_kg_m2,
            fpg = fpg_mmol_l,
            hba1c = hb_a1c_percent,
            iri = iri_u_ml,
            pro_insulin = pro_insulin_pmol_l,
            proinsulin_iri_ratio = proinsulin_iri_ratio,
            cpr = cpr,
            c_peptide_index = c_peptide_index,
            glucagon = glucagon_pg_ml,
            ldl = ldl_mg_dl,
            hdl = hdl_mg_dl,
            triglyceride = triglyceride_mg_dl,
            rlp = rlp_mg_dl,
            adiponectin = adiponectin_g_ml,
            sod_activity = sod_activity_u_ml,
            total_pai_1 = total_pai_1_ng_ml,
            nt_pro_bnp = nt_pro_bnp_pg_ml,
            tnf_a = tnf_pg_ml,
            hs_crp = hs_crp_ng_ml,
            u_alb = u_alb_mg_g_cre,
            d_roms = d_ro_ms
        ) %>%
        dplyr::select(-c(
            medication_glimepiride_0_sitagliptin_1, age_years,
            gender_male_1_female_0, percent_fmd_percent, sbp_mm_hg,
            dbp_mm_hg, c_sbp_mm_hg, augmentation_index,
            cardiac_index_l_min_m2, height_cm, weight_kg, bmi_kg_m2,
            fpg_mmol_l, hb_a1c_percent, iri_u_ml, pro_insulin_pmol_l,
            glucagon_pg_ml, ldl_mg_dl, hdl_mg_dl, triglyceride_mg_dl,
            rlp_mg_dl, adiponectin_g_ml, sod_activity_u_ml,
            total_pai_1_ng_ml, nt_pro_bnp_pg_ml, tnf_pg_ml,
            hs_crp_ng_ml, u_alb_mg_g_cre, d_ro_ms
        ))
    }) %>%
    set_names(c("baseline", "fup"))


sais_data[["baseline"]] <- sais_data[["baseline"]] %>%
    mutate(
        current_smoker = current_smoker_yes_1_no_0 == 1,
        hypertension   = hyper_tension_yes_1_no_0  == 1,
        dislipidemia   = dislipidemia_yes_1_no_0   == 1,
        statin         = statin_yes_1_no_0         == 1,
        metformin      = metformin_yes_1_no_0      == 1,
        angiotensin    = angiotensin_converting_enzyme_inhibitors_angiotensin_ii_receptor_blockers_yes_1_no_0 == 1,
        diabetic_nephropathy = diabetic_nephropathy_yes_1_no_0 == 1
    ) %>%
    dplyr::select(-c(
        current_smoker_yes_1_no_0, hyper_tension_yes_1_no_0,
        dislipidemia_yes_1_no_0, statin_yes_1_no_0,
        metformin_yes_1_no_0, diabetic_nephropathy_yes_1_no_0,
        angiotensin_converting_enzyme_inhibitors_angiotensin_ii_receptor_blockers_yes_1_no_0
    ))

#' ### merge

#+ sais join
sais <- reduce(sais_data, full_join,
    by     = c("id", "allocation", "age", "gender"),
    suffix = c("__X__baseline", "__X__fup")
)



sais <- sais %>%
    gather("key", "value", matches("(__X__baseline$)|(__X__fup$)")) %>%
    separate(key, c("key", "time"), sep = "__X__") %>%
    spread("key", "value") %>%
    mutate(
        time = as.factor(time),
        hypertension_adj = (sbp >= 130) | (dbp >= 80),
        total_colesterol = ldl + hdl + (triglyceride/5),
        dyslipidemia_adj = (total_colesterol >= 200) |
                                        (ldl >  130) |
                                        (hdl <   35) |
                               (triglyceride >= 150)
    )

glimpse(sais)
#' ### save

#+ save-sais, error = TRUE
usethis::use_data(sais, overwrite = TRUE)








# PROLOGUE --------------------------------------------------------

#' ## PROLOGUE study data

#' ### import

#+ prologue import
prologue_path <- here(
    "data-raw",
    "PROLOGUE_raw data for submission 20160506.xlsx"
)

prologue_sheets <-  prologue_path %>%
    excel_sheets() %>%
    # Sheet's names start with a number followed by a period.
    str_subset("^[0-9]+\\. ")

# set good names because they will be used by `map()`
names(prologue_sheets) <- prologue_sheets %>%
    str_replace_all(c(
        "^[0-9]+\\. " = "",
        " " = "_"
    )) %>%
    str_to_lower()


prologue_raw_list <- map(prologue_sheets, ~{
    read_xlsx(prologue_path, sheet = .x, na = c("", ".", " ")) %>%
    clean_names()
})

walk(prologue_raw_list, glimpse)


#' ### fix type

#+ prologue fix
prologue_raw_list[["allocation"]] <- prologue_raw_list[["allocation"]] %>%
    mutate(
        allocation = factor(allocation,
            levels = c(1, 2),
            labels = c("sitagliptin", "conventional")
        ),
        age = as.integer(age),
        sex = factor(sex,
            levels = c(1, 2),
            labels = c("male", "female")
        ),
        dm_treatment = factor(dm_treatment,
            levels = c(1, 2),
            labels = c("non-pharmacological", "pharmacological")
        ),
        statin = statin == 1,
        al_sbp = as.integer(al_sbp)
    )

glimpse(prologue_raw_list[["allocation"]])




prologue_raw_list[["imt"]] <- prologue_raw_list[["imt"]] %>%
    mutate(
        allocation = factor(allocation,
            levels = c(1, 2),
            labels = c("sitagliptin", "conventional")
        ),
        age = as.integer(age),
        sex = factor(sex,
            levels = c(1, 2),
            labels = c("male", "female")
        ),
        flag_fas = flag_fas == 0
    )

glimpse(prologue_raw_list[["imt"]])




prologue_raw_list[["other_data"]] <- prologue_raw_list[["other_data"]] %>%
    mutate_at(
        vars(
            history_hypertension:history_cerebrovascular_disorder_other,
            history_myocardial_infarction:history_cardiovascular_disorder_other
        ),
        funs(. == 1)
    ) %>%
    mutate(
        smoke_background = factor(smoke_background,
            levels = c(0, 2, 1),
            labels = c("no", "past", "yes"),
            ordered = TRUE
        ),
        alcohol_background = if_else(alcohol_background == 9,
            NA,
            alcohol_background == 1
        )
    ) %>%
    mutate_at(
        vars(patient_condition_0m:patient_condition_24m),
        funs(factor(., levels = c(1, 2), labels = c("good", "poor")))
    ) %>%
    mutate_at(
        vars(sitagliptin_per_day_0m:sitagliptin_per_day_24m),
        # values are 25, 50, 100, coded as 1, 2, 3.
        funs(25 * 2^(. - 1))
    ) %>%
    mutate_at(vars(su_0m:ace_i_24m), funs(. == 1))

glimpse(prologue_raw_list[["other_data"]])





prologue_raw_list[["adjudicated_event"]] <- prologue_raw_list[["adjudicated_event"]] %>%
    mutate_at(
        vars(ev_sudden_death:ev_insulin),
        funs(. == 1)
    ) %>%
    mutate(
        ev_outcome = factor(ev_outcome,
            levels = c(1, 2, 3, 4, 5),
            labels = c(
                "recovery", "remission", "recovery w/ after-effect",
                "no-recovery", "death to the adverse event"
            ),
            ordered = TRUE
        ),
        ev_status = factor(ev_status,
            levels = c(1, 2, 3),
            labels = c(
                "continuance", "dose reduction", "discontinuance"
            ),
            ordered = TRUE
        )
    )


glimpse(prologue_raw_list[["adjudicated_event"]])





prologue_raw_list[["adverse_effect"]] <- prologue_raw_list[["adverse_effect"]] %>%
    mutate(
        ae_classification = factor(ae_classification,
            levels = c(1, 2),
            labels = c("hypoglycemia", "others")
        ),
        ae_level = factor(ae_level,
            levels = c(1, 2, 3),
            labels = c("low", "moderate", "high"),
            ordered = TRUE
        ),
        ae_severity_level_1 = factor(ae_severity_level_1,
            levels = c(0, 1),
            labels = c("non-severe", "severe"),
            ordered = TRUE
        ),
        ae_severity_level_2 = factor(ae_severity_level_1,
            levels = c(1, 2, 3, 4, 5),
            labels = c(
                "death", "critical", "hospitalization",
                "permanent or extreme dysfunction", "heritable effect"
            ),
            ordered = TRUE
        ),
        ae_status = factor(ae_status,
            levels = c(1, 2, 3),
            labels = c(
                "continuance", "dose reduction", "discontinuance"
            ),
            ordered = TRUE
        ),
        ae_status_other_1 = ae_status_other_1 == 1,
        ae_outcome = factor(ae_outcome,
            levels = c(1, 2, 3, 4, 5),
            labels = c(
                "recovery", "remission", "recovery w/ after-effect",
                "no-recovery", "death to the adverse event"
            ),
            ordered = TRUE
        ),
        ae_effect_relation = factor(ae_effect_relation,
            levels = c(0, 3, 2, 1),
            labels = c(
                "no correlation", "possible correlation",
                "probable correlation", "correlation"
            ),
            ordered = TRUE
        )
    )

glimpse(prologue_raw_list[["adverse_effect"]])




#' ### merge

#+ prologue join
prologue <- reduce(prologue_raw_list, full_join, by = "id") %>%
    group_by(id) %>%
    mutate(
        ae_quantity = n(),
        ae_many     = ae_quantity > 1
    ) %>%
    ungroup()
message(paste(
    "Some IDs are duplicated because they experienced more than one",
    "adverse event (ae). For them there were one row each ae."
))

glimpse(prologue)

prologue %>%
    dplyr::select(id, ends_with(".x"), ends_with(".y")) %>%
    dplyr::mutate(
        same_sex = sex.x == sex.y,
        sex = any(!same_sex, na.rm = TRUE),
        same_age = age.x == age.y,
        age = any(!same_age, na.rm = TRUE),
        same_allocation = allocation.x == allocation.y,
        allocation = any(!same_allocation, na.rm = TRUE)
    )

prologue <- prologue %>%
    dplyr::select(-age.y, -sex.y, -allocation.y) %>%
    dplyr::rename(
        age = age.x,
        sex = sex.x,
        allocation = allocation.x
    )

prologue <- prologue %>%
    mutate(
        bmi     = body_weight_0m / (height_background / 100)^2,
        ldl_adj = t_cholesterol_0m -
                  hdl_cholesterol_0m -
                  (tg_0m/5),

        hypertension_adj = (sbp_0m >= 130) | (dbp_0m >= 80),
        dyslipidemia_adj =   (t_cholesterol_0m >= 200) |
                                      (ldl_adj >  130) |
                           (hdl_cholesterol_0m <   35) |
                                        (tg_0m >= 150)
    )

missing_fup <- select(prologue, ends_with("_24m")) %>%
        is.na()

was_analyzed <- rowSums(missing_fup) != ncol(missing_fup)

prologue[["was_analyzed"]] <- was_analyzed

prologue %>% filter(was_analyzed)

#' ### save

#+ save-prologue, error = TRUE
usethis::use_data(prologue, overwrite = TRUE)
















# SAIS selected ---------------------------------------------------

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
#' con sitagliptin e, per lo studio PROLOGUE, solo i record di pazienti
#' effettivamente analizzati nello studio.

sais_selected <- sais %>%
    dplyr::filter(allocation == "sitagliptin") %>%
    dplyr::group_by(id) %>%
    dplyr::mutate(
        delta_hba1c_dbl = diff(hba1c),
        delta_hba1c_fct = (delta_hba1c_dbl <= -0.5) %>%
            factor(
                levels = c(FALSE, TRUE),
                labels = c("HbA1c > -0.5", "HbA1c <= -0.5")
            )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(time == "baseline") %>%
    dplyr::select(
        id, allocation,
        age, gender, bmi,
        hypertension_adj, dyslipidemia_adj, adiponectin,
        sbp, dbp,
        hba1c,
        fpg, ldl,

        delta_hba1c_dbl, delta_hba1c_fct
    ) %>%
    dplyr::filter(!is.na(delta_hba1c_dbl))

usethis::use_data(sais_selected, overwrite = TRUE)






# PROLOG MICED SELECTED -------------------------------------------

#' Nello studio PROLOGUE, per le variabili di interesse, sono rpesenti
#' dati mancanti. Operiamo imputazione di di questi considerando però
#' tutte le covariate originariamente a disposizione. D'ltro canto,
#' essendo il delta HbA1c l'outcome di interesse, filtriamo
#' preventivamente i pazienti che non hanno questo dato mancante.
#' L'imputazione la effettuaimo solo sulle variabili di interesse.

#+ data-adjust

useful_prologue <- prologue[
        purrr::map_lgl(prologue, ~!lubridate::is.POSIXct(.))
    ] %>%
    janitor::remove_empty() %>%
    dplyr::select_if(~ {sum(is.na(.)) < 400})

{
    set.seed(123)
    prologue_miced <- as.data.frame(useful_prologue) %>%
        mice::mice(
            m = 1,
            method = "rf",
            visitSequence = 'monotone',
            maxit = 20,                    # Stef van Buuren 2018, p.216
            seed  = 8700
        ) %>%
        mice::complete()
}

prologue_miced_selected <- prologue_miced %>%
    dplyr::mutate(
        delta_hba1c_dbl = hba1c_ngsp_12m - hba1c_ngsp_0m,
        delta_hba1c_fct = (delta_hba1c_dbl <= -0.5) %>%
            factor(
                levels = c(FALSE, TRUE),
                labels = c("HbA1c > -0.5", "HbA1c <= -0.5")
            )
    ) %>%
    dplyr::distinct(id, .keep_all = TRUE) %>%
    dplyr::filter(was_analyzed, !is.na(delta_hba1c_dbl)) %>%
    dplyr::select(
        id, allocation,
        age, sex, bmi,
        hypertension_adj, dyslipidemia_adj, hmw_adiponectin_0m,
        sbp_0m, dbp_0m,
        hba1c_ngsp_0m,
        fbs_0m, ldl_adj,

        delta_hba1c_dbl, delta_hba1c_fct
    ) %>%
    set_names(names(sais_selected))

usethis::use_data(prologue_miced, prologue_miced_selected,
    overwrite = TRUE
)


