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
        stringr::str_c(URL_API_BASE,
                       stringr::str_glue("?_pageSize={pagesize}&_page={page}"))
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

