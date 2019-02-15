.onAttach <- function(libname, pkgname) {

    if (!"SuperLearner" %in% .packages()) {
        packageStartupMessage(paste0(
            "WARNING:\n",
            "  To use the package 'select' you have to explicitely\n",
            "  `library(SuperLearner)`, and you don't.\n",
            "   Please, `library(SuperLearner)`."
        ))
    }
}
