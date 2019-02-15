* Fixed a bug in `data("prologue")` (wrong variable selected), and 
  added `data("prologue_miced")` to have access to the full imputed
  dataset.
* Added startup warning to check if the package `SuperLearner` is
  attached.

# select 0.2.0

* Updated `inst/analyses/train-sails.R` up to the first train of the
  SuperLearner on SAIS data.
* Adedd `train_sl()` and `get_sl_stats()` wrapper functions to train 
  a SuperLearner and get its stats (time, weak learners risks and
  weights, plot of the ROC, and AUC and Risk scores)
* Added `SL.libraryD` data with the SuperLearner configurations.
* Added `data-raw/setup-sl.R` script to create configuration data for
  teh training of SuperLearner (accordingly to our decisions).
* Added `sais_selected` and `prologue_miced_selected` datasets.
* Merged `data-raw/import-prologue.R` and `data-raw/import-sais.R` in
  `data-raw/import-data.R` including alsa the code to generate and store
  selected (variables) and imputed version of the datasets useful for
  the model. 

# select 0.1.1

* Added `was_analyzed` variable to the `prologue` db to identify records
  of patient analyzed in the original study.
* Fixed `prologue` definition of ldl and dyslipidemia.

# select 0.1.0

* Added `hypertension_adj`, `dislipidemia_adj`, and `total_colesterol`,
  in both trials to consider the same levels accordingly to the same 
  cut-off/criteria.
* SAIS RCT data imported, merged and provided as a tibble
  named `sais`.
* first exploration analyses for SL on PROLOGUE data in `inst/analyses`.
* Updaute `prologue` dataset to fix a bug related to a duplicate 
  baseline characteristics columns.

# select 0.0.1

* PROLOGUE RCT data imported, merged and provided as a tibble
  named `prologue`.

# select 0.0.0.9000

* Added basic support: git/GitHub, docs, CI, test, spellcheck, badges,
  tibbles, news.
