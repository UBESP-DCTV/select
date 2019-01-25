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
library(here)
