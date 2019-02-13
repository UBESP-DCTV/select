#' Get SuperLearner stats
#'
#' @param sl_model a \code{\link[SuperLearner]{SuperLearner}}
#' @param true_values (int) vector of correct outcome
#' @param show_time (lgl, default = TRUE) do you want training time will
#'        be printed?
#' @param show_weights (lgl, default = TRUE) do you want waek lerner
#'        risks and weigths will be printed?
#' @param plot_roc (lgl, default = TRUE), do you want the ROC plot will
#'        be shown?
#'
#' @return (named double) AUC value (invisibly)
#' @export
#'
#' @examples
#' \dontrun{
#'     library(select)
#'     data("sais_selected", "SL.libraryD")
#'     SL.libraryD
#'
#'     regressors <- as.data.frame(sais_selected[-12])
#'     outcome    <- as.integer(sais_selected[[12]] == "HbA1c <= -0.5")
#'
#'     model <- train_sl(regressors, outcome)
#'
#'     get_sl_stats(model, outcome)
#' }
get_sl_stats <- function(sl_model, true_values,
    show_time    = TRUE,
    show_weights = TRUE,
    plot_roc     = TRUE
) {
    if (show_time) {
        print("tempo impiegato per l'addestramento del SuperLearner:")
        print(sl_model$times)
    }

    if (show_weights) print(sl_model)

    prediction_obj <- ROCR::prediction(sl_model$SL.predict, true_values)

    if (plot_roc) {
        ROCR::performance(prediction_obj, "tpr", "fpr") %>%
            ROCR::plot(
                colorize = TRUE,
                colorkey.pos = "top",
                colorkey.relwidth = 0.25,

                main = "SL ROC"
            )
    }

    sl_auc <- ROCR::performance(prediction_obj, "auc")@y.values[[1]] %>%
        purrr::set_names("AUC")

    message("SL AUC : ", sl_auc)
    message("SL Risk: ", (1 - sl_auc))
    invisible(sl_auc)
}
