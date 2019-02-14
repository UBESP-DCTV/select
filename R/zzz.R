.onAttach <- function(libname, pkgname) {

    if (!"SuperLearner" %in% .packages()) {
        packageStartupMessage(paste0(
            "WARNING: To use the package 'select' you have to ",
            "explicitely `library(SuperLearner)`.\n",
            "And you don't. Please `library(SuperLearner)`."
        ))
    }
}
