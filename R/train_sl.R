#' wrap for training a SuperLearner
#'
#' @param training_data (data.frame) predictors data
#' @param true_values (int) vector of outcome
#' @param SL.libraryD Either a character vector of prediction algorithms
#'                    a list containing character vectors.
#' @param method (default = "method.NNLS") A list (or a function to
#'               create a list) containing details on estimating the
#'               coefficients for the super learner and the model to
#'               combine the individual algorithms in the library.
#' @param seed (default = 123) the seed for reproducibility
#'
#' @return an object of class \code{\link[SuperLearner]{SuperLearner}}
#' @export
#'
#' @examples
#' \dontrun{
#'     library(select)
#'     data("sais_selected", "sl_library")
#'     sl_library
#'
#'     fit_sl(
#'         as.data.frame(sais_selected[-12]),
#'         as.integer(sais_selected[[12]] == "HbA1c <= -0.5")
#'     )
#' }
train_sl <- function(training_data, true_values,
    SL.libraryD,
    method  = "method.NNLS",
    seed    = 123
) {
    set.seed(seed)

    SuperLearner::SuperLearner(
        X = training_data,
        Y = true_values,

        family     = stats::binomial(),
        method     = method,
        SL.library = SL.libraryD,

        cvControl  = SuperLearner::SuperLearner.CV.control(
            V          = 5L,
            shuffle    = FALSE,
            stratifyCV = TRUE
        )
    )
}
