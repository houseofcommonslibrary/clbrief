### Functions for processing data returned from the API as JSON

#' Get a tibble of summary data on each briefing
#'
#' \code{get_briefings} takes a list of briefings returned from the API as JSON
#' and returns a tibble of summary data those briefings, with one row per
#' briefing.
#'
#' Note that data which has a one to many realtionship with a briefing is not
#' included in the tibble returned by this function. See the other data
#' processing functions for extracting nested data.
#'
#' @param briefings_json A list of briefings as returned by
#'   \code{fetch_briefings_json}
#' @return A tibble of summary data on briefings.
#' @export

get_briefings <- function(briefings_json) {

    briefings <- purrr::map(briefings_json, function(briefing) {

        code <- briefing$identifier$`_value`
        date <- as.Date(briefing$date$`_value`)
        description <- NA
        abstract <- NA

        if (length(briefing$description) > 0) {
            description <- briefing$description[[1]]
        }

        if (length(briefing$abstract$`_value`) > 0) {
            abstract <- briefing$abstract[[1]]
        }

        list(
            code = code,
            title = briefing$title,
            publisher = briefing$publisher$prefLabel$`_value`,
            type = briefing$type,
            subtype = briefing$subType$prefLabel$`_value`,
            date = date,
            url = stringr::str_c(URL_BP, code),
            description = description,
            abstract = abstract)
    })

    dplyr::bind_rows(briefings)
}

#' Get a tibble of topics for each briefing
#'
#' \code{get_topics} takes a list of briefings returned from the API
#' as JSON and returns a tibble of topics for those briefings, with one row per
#' unqiue combination of briefing and topic.
#'
#' @param briefings_json A list of briefings as returned by
#'   \code{fetch_briefings_json}
#' @return A tibble of data on briefings and their topics in long form.
#' @export

get_topics <- function(briefings_json) {

    topics <- purrr::map(briefings_json, function(briefing) {

        code <- briefing$identifier$`_value`
        title <- briefing$title
        publisher <- briefing$publisher$prefLabel$`_value`
        type = briefing$type
        subtype = briefing$subType$prefLabel$`_value`
        date <- as.Date(briefing$date$`_value`)
        url <- stringr::str_c(URL_BP, code)
        purrr::map(briefing$topic, function(topic) {
            list(
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                subtype = subtype,
                date = date,
                url = url,
                topic = topic$prefLabel$`_value`)
        })
    })

    dplyr::bind_rows(unlist(topics, recursive = FALSE))
}

#' Get a tibble of sections for each briefing
#'
#' \code{get_sections} takes a list of briefings returned from the API as JSON
#' and returns a tibble of authors for those briefings, with one row per unqiue
#' combination of briefing and author.
#'
#' @param briefings_json A list of briefings as returned by
#'   \code{fetch_briefings_json}
#' @return A tibble of data on briefings and their section in long form.
#' @export

get_sections <- function(briefings_json) {

    sections <- purrr::map(briefings_json, function(briefing) {

        code <- briefing$identifier$`_value`
        title <- briefing$title
        publisher <- briefing$publisher$prefLabel$`_value`
        type = briefing$type
        subtype = briefing$subType$prefLabel$`_value`
        date <- as.Date(briefing$date$`_value`)
        url <- stringr::str_c(URL_BP, code)
        purrr::map(briefing$section, function(section) {
            list(
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                subtype = subtype,
                date = date,
                url = url,
                section = section$prefLabel$`_value`)
        })
    })

    dplyr::bind_rows(unlist(sections, recursive = FALSE))
}
