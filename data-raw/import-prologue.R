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
        ae_many = ae_quantity > 1
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

#+ save, error = TRUE
usethis::use_data(prologue, overwrite = TRUE)

