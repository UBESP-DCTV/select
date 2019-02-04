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
library(tidyverse)
library(readxl)
library(janitor)
library(here)


#' ## PROLOGUE study data

#' ### import

#+ prologue import
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
            treatment = factor(medication_glimepiride_0_sitagliptin_1,
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
        uangiotensin_converting_enzyme_inhibitors_angiotensin_ii_receptor_blockers_yes_1_no_0
    ))

#' ### merge

#+ sais join
sais <- reduce(sais_data, full_join,
    by     = c("id", "treatment", "age", "gender"),
    suffix = c("__X__baseline", "__X__fup")
)

sais <- sais %>%
    gather("key", "value", matches("(__X__baseline$)|(__X__fup$)")) %>%
    separate(key, c("key", "time"), sep = "__X__") %>%
    spread("key", "value") %>%
    mutate(time = as.factor(time))

glimpse(sais)
#' ### save

#+ save, error = TRUE
usethis::use_data(sais, overwrite = TRUE)

