#' clbrief: A package for downloading data from the Research Briefings API
#'
#' The Research Briefings API contains data on all research briefings published
#' at researchbriefings.parliament.uk. This package provides functions for
#' downloading data on briefings.
#'
#' @docType package
#' @name clbrief
#' @importFrom magrittr %>%
#' @importFrom rlang .data
NULL

# Tell R CMD check about new operators
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".", ":="))
}
