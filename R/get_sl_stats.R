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

    get_auc(sl_model[["SL.predict"]], true_values, plot_roc = plot_roc)
}


#' Get AUC stat
#'
#' @param probs (num) vector of probabilities
#' @param trues (int) vector of true labels (0/1)
#' @param plot_roc (lgl, default = TRUE), do you want the ROC plot will
#'        be shown?
#'
#' @return AUC value (invisibly)
#' @export
#'
#' @examples
#' get_auc(runif(100), sample(0:1, 100, replace = TRUE))
get_auc <- function(probs, trues, plot_roc = TRUE) {

    prediction_obj <- ROCR::prediction(probs, trues)

    if (plot_roc) {
        ROCR::performance(prediction_obj, "tpr", "fpr") %>%
            ROCR::plot(
                colorize = TRUE,
                colorkey.pos = "top",
                colorkey.relwidth = 0.25,

                main = "ROC"
            )
    }

    sl_auc <- ROCR::performance(prediction_obj, "auc")@y.values[[1]] %>%
        purrr::set_names("AUC")

    message("AUC : ", sl_auc)
    message("Risk: ", (1 - sl_auc))
    invisible(sl_auc)
}
