### Functions for downloading and archiving data on Research Briefings

#' Create initial archive files
#'
#' @param archives The name of the folder to use for the archives.
#' @export

create_archives <- function(archives = DEFAULT_ARCHIVES) {

    from_date <- lubridate::today(Sys.timezone()) - lubridate::days(1)

    if (file.exists(file.path(archives, BRIEFINGS_ARCHIVE)) ||
        file.exists(file.path(archives, TOPICS_ARCHIVE)) ||
        file.exists(file.path(archives, SECTIONS_ARCHIVE)) ||
        file.exists(file.path(archives, AUTHORS_ARCHIVE)) ||
        file.exists(file.path(archives, DOCUMENTS_ARCHIVE))) {
        stop("Cannot create archives: archive files already exist")
    }

    # Get briefing data from feed
    briefings_json <- fetch_briefings_json()

    briefings <- get_briefings(briefings_json) %>%
        dplyr::filter(date == from_date)

    topics <- get_topics(briefings_json) %>%
        dplyr::filter(date == from_date)

    sections <- get_sections(briefings_json) %>%
        dplyr::filter(date == from_date)

    # Get additional briefing data from briefing items
    all_json <- fetch_all_json(briefings$resource)

    authors <- get_authors(all_json) %>%
        dplyr::filter(date == from_date)

    documents <- get_documents(all_json) %>%
        dplyr::filter(date == from_date)

    # Save to disk
    readr::write_excel_csv(briefings, file.path(archives, BRIEFINGS_ARCHIVE))
    readr::write_excel_csv(topics, file.path(archives, TOPICS_ARCHIVE))
    readr::write_excel_csv(sections, file.path(archives, SECTIONS_ARCHIVE))
    readr::write_excel_csv(authors, file.path(archives, AUTHORS_ARCHIVE))
    readr::write_excel_csv(documents, file.path(archives, DOCUMENTS_ARCHIVE))
}

#' Backup the current archive files
#'
#' @param archives The name of the folder to use for the archives.
#' @export

backup_archives <- function(archives = DEFAULT_ARCHIVES) {

    # Check archive files exist
    if (! file.exists(file.path(archives, BRIEFINGS_ARCHIVE)) ||
        ! file.exists(file.path(archives, TOPICS_ARCHIVE)) ||
        ! file.exists(file.path(archives, SECTIONS_ARCHIVE)) ||
        ! file.exists(file.path(archives, AUTHORS_ARCHIVE)) ||
        ! file.exists(file.path(archives, DOCUMENTS_ARCHIVE))) {
        stop("Cannot backup archives: archive files do not exist")
    }

    # Backup briefings
    briefings_archive <- readr::read_csv(
        file.path(archives, BRIEFINGS_ARCHIVE), col_types = readr::cols())
    readr::write_excel_csv(
        briefings_archive, file.path(archives, BRIEFINGS_ARCHIVE_BACKUP))

    # Backup topics
    topics_archive <- readr::read_csv(
        file.path(archives, TOPICS_ARCHIVE), col_types = readr::cols())
    readr::write_excel_csv(
        topics_archive, file.path(archives, TOPICS_ARCHIVE_BACKUP))

    # Backup sections
    sections_archive <- readr::read_csv(
        file.path(archives, SECTIONS_ARCHIVE), col_types = readr::cols())
    readr::write_excel_csv(
        sections_archive, file.path(archives,  SECTIONS_ARCHIVE_BACKUP))

    # Backup authors
    authors_archive <- readr::read_csv(
        file.path(archives, AUTHORS_ARCHIVE), col_types = readr::cols())
    readr::write_excel_csv(
        authors_archive, file.path(archives, AUTHORS_ARCHIVE_BACKUP))

    # Backup documents
    documents_archive <- readr::read_csv(
        file.path(archives, DOCUMENTS_ARCHIVE), col_types = readr::cols())
    readr::write_excel_csv(
        documents_archive, file.path(archives, DOCUMENTS_ARCHIVE_BACKUP))
}

#' Update the current archive files
#'
#' @param archives The name of the folder to use for the archives.
#' @export

update_archives <- function(archives = DEFAULT_ARCHIVES) {

    # Check archive files exist
    if (! file.exists(file.path(archives, BRIEFINGS_ARCHIVE)) ||
        ! file.exists(file.path(archives, TOPICS_ARCHIVE)) ||
        ! file.exists(file.path(archives, SECTIONS_ARCHIVE)) ||
        ! file.exists(file.path(archives, AUTHORS_ARCHIVE)) ||
        ! file.exists(file.path(archives, DOCUMENTS_ARCHIVE))) {
        stop("Cannot update archives: archive files do not exist")
    }

    # Get briefings json and parse the data
    briefings_json <- fetch_briefings_json()
    briefings_data <- get_briefings(briefings_json)
    topics_data <- get_topics(briefings_json)
    sections_data <- get_sections(briefings_json)

    # Backup current archives
    backup_archives(archives)

    # Update briefings
    briefings_archive <- readr::read_csv(
        file.path(archives, BRIEFINGS_ARCHIVE), col_types = readr::cols())
    briefings_snapshot <- update_archive(
        briefings_archive,
        briefings_data,
        file.path(archives, BRIEFINGS_ARCHIVE))

    # Update topics
    topics_archive <- readr::read_csv(
        file.path(archives, TOPICS_ARCHIVE), col_types = readr::cols())
    topics_snapshot <- update_archive(
        topics_archive,
        topics_data,
        file.path(archives, TOPICS_ARCHIVE))

    # Update sections
    sections_archive <- readr::read_csv(
        file.path(archives, SECTIONS_ARCHIVE), col_types = readr::cols())
    sections_snapshot <- update_archive(
        sections_archive,
        sections_data,
        file.path(archives, SECTIONS_ARCHIVE))

    # Get briefings all json and parse the data
    all_json <- fetch_all_json(briefings_snapshot$resource)
    authors_data <- get_authors(all_json)
    documents_data <- get_documents(all_json)

    # Update authors
    authors_archive <- readr::read_csv(
        file.path(archives, AUTHORS_ARCHIVE), col_types = readr::cols())
    authors_snapshot <- update_archive(
        authors_archive,
        authors_data,
        file.path(archives, AUTHORS_ARCHIVE))

    # Update documents
    documents_archive <- readr::read_csv(
        file.path(archives, DOCUMENTS_ARCHIVE), col_types = readr::cols())
    documents_snapshot <- update_archive(
        documents_archive,
        documents_data,
        file.path(archives, DOCUMENTS_ARCHIVE))
}

#' Update a specific archive
#' @keywords internal

update_archive <- function(archive, data, archive_file) {

    # Calculate dates
    from_date <- max(archive$date)
    to_date <- lubridate::today("GMT")

    # Filter the snapshot for new briefings
    snapshot <- data %>%
        dplyr::filter(date > from_date & date < to_date)

    # Create the new archive
    new_archive <- dplyr::bind_rows(archive, snapshot) %>%
        dplyr::arrange(dplyr::desc(date))

    # Update the archive file
    readr::write_excel_csv(new_archive, archive_file)

    # Return the filtered snapshot
    snapshot
}

#' Revert the current archive files to the backup archive files
#'
#' @param archives The name of the folder to use for the archives.
#' @param confirm The user must manually set confirm to TRUE on the function
#'   call to revert the data. This is to guard against user error.
#' @export

revert_archives <- function(archives = DEFAULT_ARCHIVES, confirm = FALSE) {

    # Check archive files exist
    if (! file.exists(file.path(archives, BRIEFINGS_ARCHIVE_BACKUP)) ||
        ! file.exists(file.path(archives, TOPICS_ARCHIVE_BACKUP)) ||
        ! file.exists(file.path(archives, SECTIONS_ARCHIVE_BACKUP)) ||
        ! file.exists(file.path(archives, AUTHORS_ARCHIVE)) ||
        ! file.exists(file.path(archives, DOCUMENTS_ARCHIVE))) {
        stop("Cannot revert archives: backup files do not exist")
    }

    # Check for confirmation and revert
    if (is.logical(confirm) && confirm == TRUE) {

        # Revert briefings archive
        briefings_archive <- readr::read_csv(
            file.path(archives, BRIEFINGS_ARCHIVE_BACKUP),
            col_types = readr::cols())
        readr::write_excel_csv(
            briefings_archive,
            file.path(archives, BRIEFINGS_ARCHIVE))
        file.remove(file.path(archives, BRIEFINGS_ARCHIVE_BACKUP))

        # Revert topics archive
        topics_archive <- readr::read_csv(
            file.path(archives, TOPICS_ARCHIVE_BACKUP),
            col_types = readr::cols())
        readr::write_excel_csv(
            topics_archive,
            file.path(archives, TOPICS_ARCHIVE))
        file.remove(file.path(archives, TOPICS_ARCHIVE_BACKUP))

        # Revert sections archive
        sections_archive <- readr::read_csv(
            file.path(archives, SECTIONS_ARCHIVE_BACKUP), col_types = readr::cols())
        readr::write_excel_csv(
            sections_archive,
            file.path(archives, SECTIONS_ARCHIVE))
        file.remove(file.path(archives, SECTIONS_ARCHIVE_BACKUP))

        # Revert authors archive
        authors_archive <- readr::read_csv(
            file.path(archives, AUTHORS_ARCHIVE_BACKUP), col_types = readr::cols())
        readr::write_excel_csv(
            authors_archive,
            file.path(archives, AUTHORS_ARCHIVE))
        file.remove(file.path(archives, AUTHORS_ARCHIVE_BACKUP))

        documents_archive <- readr::read_csv(
            file.path(archives, DOCUMENTS_ARCHIVE_BACKUP),
            col_types = readr::cols())
        readr::write_excel_csv(
            documents_archive,
            file.path(archives, DOCUMENTS_ARCHIVE))
        file.remove(file.path(archives, DOCUMENTS_ARCHIVE_BACKUP))

        invisible()

    } else {

        stop("Cannot revert archives: you must confim the revert")
    }
}
