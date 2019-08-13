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
        description <- NA
        abstract <- NA

        if (length(briefing$description) > 0) {
            description <- briefing$description[[1]]
        }

        if (length(briefing$abstract$`_value`) > 0) {
            abstract <- briefing$abstract[[1]]
        }

        list(
            resource = briefing$`_about`,
            code = code,
            title = briefing$title,
            publisher = briefing$publisher$prefLabel$`_value`,
            type = briefing$subType$prefLabel$`_value`,
            date = as.Date(briefing$date$`_value`),
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

        resource <- briefing$`_about`
        code <- briefing$identifier$`_value`
        title <- briefing$title
        publisher <- briefing$publisher$prefLabel$`_value`
        type <- briefing$subType$prefLabel$`_value`
        date <- as.Date(briefing$date$`_value`)
        url <- stringr::str_c(URL_BP, code)

        purrr::map(briefing$topic, function(topic) {
            list(
                resource = resource,
                code = code,
                title = title,
                publisher = publisher,
                type = type,
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
#' and returns a tibble of sections for those briefings, with one row per
#' unqiue combination of briefing and sections.
#'
#' @param briefings_json A list of briefings as returned by
#'   \code{fetch_briefings_json}
#' @return A tibble of data on briefings and their section in long form.
#' @export

get_sections <- function(briefings_json) {

    sections <- purrr::map(briefings_json, function(briefing) {

        resource <- briefing$`_about`
        code <- briefing$identifier$`_value`
        title <- briefing$title
        publisher <- briefing$publisher$prefLabel$`_value`
        type <- briefing$subType$prefLabel$`_value`
        date <- as.Date(briefing$date$`_value`)
        url <- stringr::str_c(URL_BP, code)

        purrr::map(briefing$section, function(section) {
            list(
                resource = resource,
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                date = date,
                url = url,
                section = section$prefLabel$`_value`)
        })
    })

    dplyr::bind_rows(unlist(sections, recursive = FALSE))
}


#' Get a tibble of authors for each briefing
#'
#' \code{get_authors} takes a list of briefings returned from the API as JSON
#' and returns a tibble of authors for those briefings, with one row per unqiue
#' combination of briefing and author. Data on authors is not available from
#' the main research breifings feed so this function requires the json produced
#' from \code{fetch_all_json}.
#'
#' @param briefings_all_json A list of briefings as returned by
#'   \code{fetch_all_json}
#' @return A tibble of data on briefings and their authors in long form.
#' @export

get_authors <- function(briefings_all_json) {

    authors <- purrr::map(briefings_all_json, function(briefing) {

        briefing <- briefing$result$primaryTopic
        resource <- briefing$`_about`
        code <- briefing$identifier$`_value`
        title <- briefing$title
        publisher <- briefing$publisher$prefLabel$`_value`
        type <- briefing$subType$prefLabel$`_value`
        date <- as.Date(briefing$date$`_value`)
        url <- stringr::str_c(URL_BP, code)

        # Get creator authors where they exit
        creators <- NULL
        conjson <- briefing$creator

        if (! is.null(conjson)) {
            if (! is.null(names(conjson))) {

                if (! is.null(conjson$givenName$`_value`)) {
                    given_name <- conjson$givenName$`_value`
                } else {
                    given_name <- NA
                }

                if (! is.null(conjson$familyName$`_value`)) {
                    family_name <- conjson$familyName$`_value`
                } else {
                    family_name <- NA
                }

                creators <- list(list(
                    given_name = given_name,
                    family_name = family_name))

            } else {

                creators <- purrr::map(conjson, function(creator) {

                    if (! is.null(creator$givenName$`_value`)) {
                        given_name <- creator$givenName$`_value`
                    } else {
                        given_name <- NA
                    }

                    if (! is.null(creator$familyName$`_value`)) {
                        family_name <- creator$familyName$`_value`
                    } else {
                        family_name <- NA
                    }

                    list(
                        given_name = given_name,
                        family_name = family_name)
                })
            }
        }

        creators <- purrr::map(creators, function(creator) {
            list(
                resource = resource,
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                date = date,
                url = url,
                given_name = creator$given_name,
                family_name = creator$family_name,
                author_type = "Creator")
        })

        # Get contributor authors where they exit
        contributors <- NULL
        rcjson <- briefing$researchContributor

        if (! is.null(rcjson)) {
            if (! is.null(names(rcjson))) {

                if (! is.null(rcjson$givenName$`_value`)) {
                    given_name <- rcjson$givenName$`_value`
                } else {
                    given_name <- NA
                }

                if (! is.null(rcjson$givenName$`_value`)) {
                    family_name <- rcjson$familyName$`_value`
                } else {
                    family_name <- NA
                }

                contributors  <- list(list(
                    given_name = given_name,
                    family_name = family_name))

            } else {

                contributors  <- purrr::map(rcjson, function(contributor) {

                    if (! is.null(contributor$givenName$`_value`)) {
                        given_name <- contributor$givenName$`_value`
                    } else {
                        given_name <- NA
                    }

                    if (! is.null(contributor$familyName$`_value`)) {
                        family_name <- contributor$familyName$`_value`
                    } else {
                        family_name <- NA
                    }

                    list(
                        given_name = given_name,
                        family_name = family_name)
                })
            }
        }

        contributors <- purrr::map(contributors, function(contributor) {
            list(
                resource = resource,
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                date = date,
                url = url,
                given_name = contributor$given_name,
                family_name = contributor$family_name,
                author_type = "Contributor")
        })

        c(creators, contributors)
    })

    dplyr::bind_rows(unlist(authors, recursive = FALSE))
}


#' Get a tibble of documents for each briefing
#'
#' \code{get_documents} takes a list of briefings returned from the API as JSON
#' and returns a tibble of documents for those briefings, with one row per
#' unqiue combination of briefing and document. Data on documents is not
#' available from the main research breifings feed so this function requires
#' the json produced from \code{fetch_all_json}.
#'
#' @param briefings_all_json A list of briefings as returned by
#'   \code{fetch_all_json}
#' @return A tibble of data on briefings and their documents in long form.
#' @export

get_documents <- function(briefings_all_json) {

    docs <- purrr::map(briefings_all_json, function(briefing) {

        briefing <- briefing$result$primaryTopic
        resource <- briefing$`_about`
        code <- briefing$identifier$`_value`
        title <- briefing$title
        publisher <- briefing$publisher$prefLabel$`_value`
        type <- briefing$subType$prefLabel$`_value`
        date <- as.Date(briefing$date$`_value`)
        url <- stringr::str_c(URL_BP, code)

        # Get briefing documents where they exit
        documents <- NULL
        docjson <- briefing$briefingDocument

        if (! is.null(docjson)) {
            if (! is.null(names(docjson))) {
                documents <- list(list(
                    document_title = docjson$attachmentTitle,
                    document_url = docjson$fileUrl,
                    document_filetype = docjson$mediaType,
                    document_filesize = docjson$sizeOfFile[[1]]))
            } else {
                documents <- purrr::map(docjson, function(document) {
                    list(
                        document_title = document$attachmentTitle,
                        document_url = document$fileUrl,
                        document_filetype = document$mediaType,
                        document_filesize = document$sizeOfFile[[1]])
                })
            }
        }

        documents <- purrr::map(documents, function(document) {
            list(
                resource = resource,
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                date = date,
                url = url,
                document_title = document$document_title,
                document_url = document$document_url,
                document_filetype = document$document_filetype,
                document_filesize = document$document_filesize,
                document_type = "Briefing Document")
        })

        # Get attachments where they exit
        attachments <- NULL
        attjson <- briefing$attachment

        if (! is.null(attjson)) {
            if (! is.null(names(attjson))) {
                attachments <- list(list(
                    document_title = attjson$attachmentTitle,
                    document_url = attjson$fileUrl,
                    document_filetype = attjson$mediaType,
                    document_filesize = attjson$sizeOfFile[[1]]))
            } else {
                attachments <- purrr::map(attjson, function(attachment) {
                    list(
                        document_title = attachment$attachmentTitle,
                        document_url = attachment$fileUrl,
                        document_filetype = attachment$mediaType,
                        document_filesize = attachment$sizeOfFile[[1]])
                })
            }
        }

        attachments <- purrr::map(attachments, function(attachment) {
            list(
                resource = resource,
                code = code,
                title = title,
                publisher = publisher,
                type = type,
                date = date,
                url = url,
                document_title = attachment$document_title,
                document_url = attachment$document_url,
                document_filetype = attachment$document_filetype,
                document_filesize = attachment$document_filesize,
                document_type = "Attachment")
        })

        c(documents, attachments)
    })

    dplyr::bind_rows(unlist(docs, recursive = FALSE))
}
