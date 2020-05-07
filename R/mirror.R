### Functions for downloading and storing a mirror of the data in the API

# Create mirror ---------------------------------------------------------------

#' Create a mirror from csvs
#'
#' @param dbfile Path to create a SQLite briefings database.
#' @param dbdir Path to the directory containing the csvs to import.
#' @export

create_mirror <- function(dbfile = MIRROR_DB, dbdir = DATABASE_DIR) {
    create_db(dbfile = dbfile, dbdir = dbdir)
}

# Extract mirror --------------------------------------------------------------

#' Extract a mirror to csvs
#'
#' @param dbfile Path to a SQLite briefings database.
#' @param dbdir Path to the directory containing the csvs to import.
#' @export

extract_mirror <- function(dbfile = MIRROR_DB, dbdir = DATABASE_DIR) {
    extract_db(dbfile = dbfile, dbdir = dbdir)
}

# Update mirror ---------------------------------------------------------------

#' Update the mirror
#'
#' @param dbfile Path to a SQLite briefings database.
#' @param backup_dbfile Path to a backup SQLite briefings database.
#' @export

update_mirror <- function(
    dbfile = MIRROR_DB,
    backup_dbfile = MIRROR_BACKUP_DB) {

    # Define an update function to succeed or fail atomically
    update_tables <- function(dbc) {

        # Get briefings json and parse the data
        briefings_json <- fetch_briefings_json()
        briefings_data <- get_briefings(briefings_json)
        topics_data <- get_topics(briefings_json)
        sections_data <- get_sections(briefings_json)

        # Update briefings, topics and authors from briefings_json
        briefings_snapshot <- update_mirror_briefings(briefings_data, dbc)
        topics_snapshot <- update_mirror_topics(topics_data, dbc)
        sections_snapshot <- update_mirror_sections(sections_data, dbc)

        # Get briefings all json and parse the data
        all_json <- fetch_all_json(briefings_snapshot$resource)
        authors_data <- get_authors(all_json)
        documents_data <- get_documents(all_json)

        # Update authors and documents from all_json
        authors_snapshot <- update_mirror_authors(authors_data, dbc)
        documents_snapshot <- update_mirror_documents(documents_data, dbc)
    }

    # Check database exists
    if (! file.exists(dbfile)) {
        stop("Cannot update the database: file does not exist")
    }

    # Backup current database
    backup_success <- file.copy(dbfile, backup_dbfile, overwrite = TRUE)
    if(! backup_success) stop("Cannot update the database: backup failed")

    # Get database connection
    dbc <- DBI::dbConnect(RSQLite::SQLite(), dbfile)

    # Try to update the database but rollback if necessary
    tryCatch({
        DBI::dbBegin(dbc)
        update_tables(dbc)
        DBI::dbCommit(dbc)
        DBI::dbDisconnect(dbc)
    }, error = function(c) {
        DBI::dbRollback(dbc)
        DBI::dbDisconnect(dbc)
        stop(stringr::str_glue("Error updating database: {c$message}"))
    })
}

# Update mirror tables --------------------------------------------------------

#' Update the briefings table with new data from the api
#'
#' @param briefings_data Data on briefings returned from \code{get_briefings}.
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

update_mirror_briefings <- function(
    briefings_data, dbc = NULL, dbfile = MIRROR_DB) {

    # Determine if the function sould create its own conection or not
    has_own_connection <- FALSE

    if (is.null(dbc)) {
        dbc <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        has_own_connection <- TRUE
    }

    # Get a snapshot of new records
    date_dbl <- DBI::dbGetQuery(dbc, "SELECT MAX(date) FROM briefings;")[1,]
    from_date <- as.Date(date_dbl, lubridate::origin)
    to_date <- lubridate::today("GMT")

    briefings_snapshot <- briefings_data %>%
        dplyr::filter(date >= from_date & date <= to_date)

    # Build and run the query to delete updated briefings
    query <- "DELETE FROM briefings WHERE resource = :resource"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(briefings_snapshot, function(...) {
        briefing <- list(...)
        DBI::dbBind(rs, param = list(resource = briefing$resource))
    })

    DBI::dbClearResult(rs)

    # Build and run the query to insert each row
    query <- "
        INSERT INTO briefings(
            resource,
            code,
            title,
            publisher,
            type,
            date,
            url,
            description,
            abstract)
        VALUES(
            :resource,
            :code,
            :title,
            :publisher,
            :type,
            :date,
            :url,
            :description,
            :abstract)"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(briefings_snapshot, function(...) {
        briefing <- list(...)
        DBI::dbBind(
            rs,
            param = list(
                resource = briefing$resource,
                code = briefing$code,
                title = briefing$title,
                publisher = briefing$publisher,
                type = briefing$type,
                date = briefing$date,
                url = briefing$url,
                description = briefing$description,
                abstract = briefing$abstract))
    })

    DBI::dbClearResult(rs)
    if(has_own_connection) DBI::dbDisconnect(dbc)

    # Return inserted rows
    briefings_snapshot
}

#' Update the topics table with new data from the api
#'
#' @param topics_data Data on topics returned from \code{get_topics}.
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

update_mirror_topics <- function(
    topics_data, dbc = NULL, dbfile = MIRROR_DB) {

    # Determine if the function sould create its own conection or not
    has_own_connection <- FALSE

    if (is.null(dbc)) {
        dbc <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        has_own_connection <- TRUE
    }

    # Get a snapshot of new records
    date_dbl <- DBI::dbGetQuery(dbc, "SELECT MAX(date) FROM topics;")[1,]
    from_date <- as.Date(date_dbl, lubridate::origin)
    to_date <- lubridate::today("GMT")

    topics_snapshot <- topics_data %>%
        dplyr::filter(date >= from_date & date <= to_date)

    # Build and run the query to delete updated briefings
    query <- "DELETE FROM topics WHERE resource = :resource"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(topics_snapshot, function(...) {
        topic <- list(...)
        DBI::dbBind(rs, param = list(resource = topic$resource))
    })

    DBI::dbClearResult(rs)

    # Build and run the query to insert each row
    query <- "
        INSERT INTO topics (
            resource,
            code,
            title,
            publisher,
            type,
            date,
            url,
            topic)
        VALUES (
            :resource,
            :code,
            :title,
            :publisher,
            :type,
            :date,
            :url,
            :topic)"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(topics_snapshot, function(...) {
        topic <- list(...)
        DBI::dbBind(
            rs,
            param = list(
                resource = topic$resource,
                code = topic$code,
                title = topic$title,
                publisher = topic$publisher,
                type = topic$type,
                date = topic$date,
                url = topic$url,
                topic = topic$topic))
    })

    DBI::dbClearResult(rs)
    if(has_own_connection) DBI::dbDisconnect(dbc)

    # Return inserted rows
    topics_snapshot
}

#' Update the sections table with new data from the api
#'
#' @param sections_data Data on sections returned from \code{get_sections}.
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

update_mirror_sections <- function(
    sections_data, dbc = NULL, dbfile = MIRROR_DB) {

    # Determine if the function sould create its own conection or not
    has_own_connection <- FALSE

    if (is.null(dbc)) {
        dbc <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        has_own_connection <- TRUE
    }

    # Get a snapshot of new records
    date_dbl <- DBI::dbGetQuery(dbc, "SELECT MAX(date) FROM sections;")[1,]
    from_date <- as.Date(date_dbl, lubridate::origin)
    to_date <- lubridate::today("GMT")

    sections_snapshot <- sections_data %>%
        dplyr::filter(date >= from_date & date <= to_date)

    # Build and run the query to delete updated briefings
    query <- "DELETE FROM sections WHERE resource = :resource"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(sections_snapshot, function(...) {
        section <- list(...)
        DBI::dbBind(rs, param = list(resource = section$resource))
    })

    DBI::dbClearResult(rs)

    # Build and run the query to insert each row
    query <- "
        INSERT INTO sections (
            resource,
            code,
            title,
            publisher,
            type,
            date,
            url,
            section)
        VALUES (
            :resource,
            :code,
            :title,
            :publisher,
            :type,
            :date,
            :url,
            :section)"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(sections_snapshot, function(...) {
        section <- list(...)
        DBI::dbBind(
            rs,
            param = list(
                resource = section$resource,
                code = section$code,
                title = section$title,
                publisher = section$publisher,
                type = section$type,
                date = section$date,
                url = section$url,
                section = section$section))
    })

    DBI::dbClearResult(rs)
    if(has_own_connection) DBI::dbDisconnect(dbc)

    # Return inserted rows
    sections_snapshot
}

#' Update the authors table with new data from the api
#'
#' @param authors_data Data on authors returned from \code{get_authors}.
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

update_mirror_authors <- function(
    authors_data, dbc = NULL, dbfile = MIRROR_DB) {

    # Determine if the function sould create its own conection or not
    has_own_connection <- FALSE

    if (is.null(dbc)) {
        dbc <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        has_own_connection <- TRUE
    }

    # Get a snapshot of new records
    date_dbl <- DBI::dbGetQuery(dbc, "SELECT MAX(date) FROM authors;")[1,]
    from_date <- as.Date(date_dbl, lubridate::origin)
    to_date <- lubridate::today("GMT")

    authors_snapshot <- authors_data %>%
        dplyr::filter(date >= from_date & date <= to_date)

    # Build and run the query to delete updated briefings
    query <- "DELETE FROM authors WHERE resource = :resource"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(authors_snapshot, function(...) {
        author <- list(...)
        DBI::dbBind(rs, param = list(resource = author$resource))
    })

    DBI::dbClearResult(rs)

    # Build and run the query to insert each row
    query <- "
        INSERT INTO authors (
            resource,
            code,
            title,
            publisher,
            type,
            date,
            url,
            given_name,
            family_name,
            author_type)
        VALUES (
            :resource,
            :code,
            :title,
            :publisher,
            :type,
            :date,
            :url,
            :given_name,
            :family_name,
            :author_type)"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(authors_snapshot, function(...) {
        author <- list(...)
        DBI::dbBind(
            rs,
            param = list(
                resource = author$resource,
                code = author$code,
                title = author$title,
                publisher = author$publisher,
                type = author$type,
                date = author$date,
                url = author$url,
                given_name = author$given_name,
                family_name = author$family_name,
                author_type = author$author_type))
    })

    DBI::dbClearResult(rs)
    if(has_own_connection) DBI::dbDisconnect(dbc)

    # Return inserted rows
    authors_snapshot
}

#' Update the documents table with new data from the api
#'
#' @param documents_data Data on documents returned from \code{get_documents}.
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

update_mirror_documents <- function(
    documents_data, dbc = NULL, dbfile = MIRROR_DB) {

    # Determine if the function sould create its own conection or not
    has_own_connection <- FALSE

    if (is.null(dbc)) {
        dbc <- DBI::dbConnect(RSQLite::SQLite(), dbfile)
        has_own_connection <- TRUE
    }

    # Get a snapshot of new records
    date_dbl <- DBI::dbGetQuery(dbc, "SELECT MAX(date) FROM documents;")[1,]
    from_date <- as.Date(date_dbl, lubridate::origin)
    to_date <- lubridate::today("GMT")

    documents_snapshot <- documents_data %>%
        dplyr::filter(date >= from_date & date <= to_date)

    # Build and run the query to delete updated briefings
    query <- "DELETE FROM documents WHERE resource = :resource"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(documents_snapshot, function(...) {
        document <- list(...)
        DBI::dbBind(rs, param = list(resource = document$resource))
    })

    DBI::dbClearResult(rs)

    # Build and run the query to insert each row
    query <- "
        INSERT INTO documents (
            resource,
            code,
            title,
            publisher,
            type,
            date,
            url,
            document_title,
            document_url,
            document_filetype,
            document_filesize,
            document_type)
        VALUES (
            :resource,
            :code,
            :title,
            :publisher,
            :type,
            :date,
            :url,
            :document_title,
            :document_url,
            :document_filetype,
            :document_filesize,
            :document_type)"

    rs <- DBI::dbSendStatement(dbc, query)

    purrr::pmap(documents_snapshot, function(...) {
        document <- list(...)
        DBI::dbBind(
            rs,
            param = list(
                resource = document$resource,
                code = document$code,
                title = document$title,
                publisher = document$publisher,
                type = document$type,
                date = document$date,
                url = document$url,
                document_title = document$document_title,
                document_url = document$document_url,
                document_filetype = document$document_filetype,
                document_filesize = document$document_filesize,
                document_type = document$document_type))
    })

    DBI::dbClearResult(rs)
    if(has_own_connection) DBI::dbDisconnect(dbc)

    # Return inserted rows
    documents_snapshot
}

# Get tables ------------------------------------------------------------------

#' Get the briefings table from the mirror as a tibble
#'
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

get_mirror_briefings <- function(dbc = NULL, dbfile = MIRROR_DB) {
    get_db_table("briefings", dbc = dbc, dbfile = dbfile)
}

#' Get the topics table from the mirror as a tibble
#'
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

get_mirror_topics <- function(dbc = NULL, dbfile = MIRROR_DB) {
    get_db_table("topics", dbc = dbc, dbfile = dbfile)
}

#' Get the sections table from the mirror as a tibble
#'
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

get_mirror_sections <- function(dbc = NULL, dbfile = MIRROR_DB) {
    get_db_table("sections", dbc = dbc, dbfile = dbfile)
}

#' Get the authors table from the mirror as a tibble
#'
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

get_mirror_authors <- function(dbc = NULL, dbfile = MIRROR_DB) {
    get_db_table("authors", dbc = dbc, dbfile = dbfile)
}

#' Get the documents table from the mirror as a tibble
#'
#' @param dbc A connection to a SQLite briefings database.
#' @param dbfile Path to a SQLite briefings database.
#' @export

get_mirror_documents <- function(dbc = NULL, dbfile = MIRROR_DB) {
    get_db_table("documents", dbc = dbc, dbfile = dbfile)
}
