### Functions for fetching data from the Research Briefings API

#' Fetch data on all briefings up to a given number as JSON
#'
#' \code{fetch_briefings_json} fetches data on all briefings up to a given
#' number derived from the number of pages of results and the pagesize.
#'
#' @param pages The number of pages to pull from the API. The default is 1.
#' @param pagesize The number of briefings per page. The default is 500 and the
#'   maximum allowed value is 500.
#' @param pause The number of seconds to pause between page requests.
#' @return A list of briefings as JSON.
#' @export

fetch_briefings_json <- function(pages = 1, pagesize = 500, pause = 1) {

    if (pagesize > 500) stop("pagesize cannot be more than 500")

    urls <- purrr::map_chr(0:(pages-1), function(page) {
        stringr::str_glue("{URL_API_FEED}?_pageSize={pagesize}&_page={page}")
    })

    results <- purrr::map(urls, function(url) {
        Sys.sleep(pause)
        response <- httr::GET(url)
        text <- httr::content(response, as = "text", encoding = "utf-8")
        json <- jsonlite::fromJSON(text, simplifyVector = FALSE)
        json$result$items
    })

    unlist(results, recursive = FALSE)
}

#' Fetch summary data on briefings and return as a tibble
#'
#' \code{fetch_briefings} fetches data on all briefings up to a given
#' number derived from the number of pages of results and the pagesize and
#' returns summary data on each briefing.
#'
#' Note that data which has a one to many realtionship with a briefing is not
#' included in the tibble returned by this function. See the the other
#' functions for extracting nested data.
#'
#' @param pages The number of pages to pull from the API. The default is 1.
#' @param pagesize The number of briefings per page. The default is 500 and the
#'   maximum allowed value is 500.
#' @param pause The number of seconds to pause between page requests.
#' @return A tibble of sumamry data on briefings.
#' @export

fetch_briefings <- function(pages = 1, pagesize = 500, pause = 1) {

    briefings_json <- fetch_briefings_json(pages = pages,
                                           pagesize = pagesize,
                                           pause = pause)
    get_briefings(briefings_json)
}

#' Fetch data on briefing topics and return as a tibble
#'
#' \code{fetch_topics} fetches data on all briefings up to a given number
#' derived from the number of pages of results and the pagesize and returns
#' the topics associated with each briefing.
#'
#' @param pages The number of pages to pull from the API. The default is 1.
#' @param pagesize The number of briefings per page. The default is 500 and the
#'   maximum allowed value is 500.
#' @param pause The number of seconds to pause between page requests.
#' @return A tibble of data on briefings and their topics in long form.
#' @export

fetch_topics <- function(pages = 1, pagesize = 500, pause = 1) {

    briefings_json <- fetch_briefings_json(pages = pages,
                                           pagesize = pagesize,
                                           pause = pause)
    get_topics(briefings_json)
}

#' Fetch data on the sections that produced a briefing and return as a tibble
#'
#' \code{fetch_sections} fetches data on all briefings up to a given number
#' derived from the number of pages of results and the pagesize and returns
#' the sections responsible for producing each briefing.
#'
#' @param pages The number of pages to pull from the API. The default is 1.
#' @param pagesize The number of briefings per page. The default is 500 and the
#'   maximum allowed value is 500.
#' @param pause The number of seconds to pause between page requests.
#' @return A tibble of data on briefings and their sections in long form.
#' @export

fetch_sections <- function(pages = 1, pagesize = 500, pause = 1) {

    briefings_json <- fetch_briefings_json(pages = pages,
                                           pagesize = pagesize,
                                           pause = pause)
    get_sections(briefings_json)
}

#' Fetch all json data for a set of briefings
#'
#' \code{fetch_all_json} gets all of the available data for a set of briefings.
#' This provides some data that is not available in the main research briefings
#' feed and must be downloaded separately from the API for each individual
#' briefing paper. This function will therefore make multiple calls to the API.
#'
#' @param resources A vector of resource urls for a set of briefing papers.
#'   These are the urls stored in the first column of the dataframe returned by
#'   \code{get_briefings} and \code{fetch_briefings}.
#' @param pause The number of seconds to pause between page requests.
#' @return A list of briefings as json.
#' @export

fetch_all_json <- function(resources, pause = 1) {

    purrr::map(resources, function(resource) {
        Sys.sleep(pause)
        tokens <- stringr::str_split(resource, "/")[[1]]
        id <- tokens[length(tokens)]
        url <- stringr::str_glue("{URL_API_BRIEFING}{id}.json")
        response <- httr::GET(url)
        text <- httr::content(response, as = "text", encoding = "utf-8")
        jsonlite::fromJSON(text, simplifyVector = FALSE)
    })
}

