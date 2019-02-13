### Functions for downloading and archiving data on Research Briefings

#' Create initial archive files
#' @export

create_archives <- function() {

    from_date <- lubridate::today("GMT") - lubridate::days(1)

    if (file.exists(BRIEFINGS_ARCHIVE) ||
        file.exists(TOPICS_ARCHIVE) ||
        file.exists(SECTIONS_ARCHIVE) ||
        file.exists(AUTHORS_ARCHIVE) ||
        file.exists(DOCUMENTS_ARCHIVE)) {
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
    readr::write_excel_csv(briefings, BRIEFINGS_ARCHIVE)
    readr::write_excel_csv(topics, TOPICS_ARCHIVE)
    readr::write_excel_csv(sections, SECTIONS_ARCHIVE)
    readr::write_excel_csv(authors, AUTHORS_ARCHIVE)
    readr::write_excel_csv(documents, DOCUMENTS_ARCHIVE)
}

#' Backup the current archive files
#' @export

backup_archives <- function() {

    # Check archive files exist
    if (! file.exists(BRIEFINGS_ARCHIVE) ||
        ! file.exists(TOPICS_ARCHIVE) ||
        ! file.exists(SECTIONS_ARCHIVE) ||
        ! file.exists(AUTHORS_ARCHIVE) ||
        ! file.exists(DOCUMENTS_ARCHIVE)) {
        stop("Cannot backup archives: archive files do not exist")
    }

    # Backup briefings
    briefings_archive <- readr::read_csv(
        BRIEFINGS_ARCHIVE, col_types = readr::cols())
    readr::write_excel_csv(briefings_archive, BRIEFINGS_ARCHIVE_BACKUP)

    # Backup topics
    topics_archive <- readr::read_csv(
        TOPICS_ARCHIVE, col_types = readr::cols())
    readr::write_excel_csv(topics_archive, TOPICS_ARCHIVE_BACKUP)

    # Backup sections
    sections_archive <- readr::read_csv(
        SECTIONS_ARCHIVE, col_types = readr::cols())
    readr::write_excel_csv(sections_archive, SECTIONS_ARCHIVE_BACKUP)

    # Backup authors
    authors_archive <- readr::read_csv(
        AUTHORS_ARCHIVE, col_types = readr::cols())
    readr::write_excel_csv(authors_archive, AUTHORS_ARCHIVE_BACKUP)

    # Backup documents
    authors_archive <- readr::read_csv(
        AUTHORS_ARCHIVE, col_types = readr::cols())
    readr::write_excel_csv(authors_archive, AUTHORS_ARCHIVE_BACKUP)
}

#' Update the current archive files
#' @export

update_archives <- function() {

    # Check archive files exist
    if (! file.exists(BRIEFINGS_ARCHIVE) ||
        ! file.exists(TOPICS_ARCHIVE) ||
        ! file.exists(SECTIONS_ARCHIVE) ||
        ! file.exists(AUTHORS_ARCHIVE) ||
        ! file.exists(DOCUMENTS_ARCHIVE)) {
        stop("Cannot update archives: archive files do not exist")
    }

    # Get briefings json and parse the data
    briefings_json <- fetch_briefings_json()
    briefings_snapshot <- get_briefings(briefings_json)
    topics_snapshot <- get_topics(briefings_json)
    sections_snapshot <- get_sections(briefings_json)

    all_json <- fetch_all_json(briefings_snapshot$resource)
    authors_snapshot <- get_authors(all_json)
    documents_snapshot <- get_documents(all_json)

    # Backup current archives
    backup_archives()

    # Update briefings
    briefings_archive <- readr::read_csv(
        BRIEFINGS_ARCHIVE, col_types = readr::cols())
    update_archive(briefings_archive, briefings_snapshot, BRIEFINGS_ARCHIVE)

    # Update topics
    topics_archive <- readr::read_csv(
        TOPICS_ARCHIVE, col_types = readr::cols())
    update_archive(topics_archive, topics_snapshot, TOPICS_ARCHIVE)

    # Update sections
    sections_archive <- readr::read_csv(
        SECTIONS_ARCHIVE, col_types = readr::cols())
    update_archive(sections_archive, sections_snapshot, SECTIONS_ARCHIVE)

    # Update authors
    authors_archive <- readr::read_csv(
        AUTHORS_ARCHIVE, col_types = readr::cols())
    update_archive(authors_archive, authors_snapshot, AUTHORS_ARCHIVE)

    # Update documents
    documents_archive <- readr::read_csv(
        DOCUMENTS_ARCHIVE, col_types = readr::cols())
    update_archive(documents_archive, documents_snapshot, DOCUMENTS_ARCHIVE)
}

#' Update a specific archive
#' @keywords internal

update_archive <- function(archive, snapshot, archive_file) {

    # Calculate dates
    from_date <- max(archive$date)
    to_date <- lubridate::today("GMT")

    # Filter the snapshot for new briefings
    snapshot <- snapshot %>%
        dplyr::filter(date > from_date & date < to_date)

    # Create the new archive
    new_archive <- dplyr::bind_rows(archive, snapshot) %>%
        dplyr::arrange(dplyr::desc(date))

    # Update the archive file
    readr::write_excel_csv(new_archive, archive_file)
}

#' Revert the current archive files to the backup archive files
#'
#' @param confirm The user must manually set confirm to TRUE on the function
#'   call to revert the data. This is to guard against user error,
#' @export

revert_archives <- function(confirm = FALSE) {

    # Check archive files exist
    if (! file.exists(BRIEFINGS_ARCHIVE_BACKUP) ||
        ! file.exists(TOPICS_ARCHIVE_BACKUP) ||
        ! file.exists(SECTIONS_ARCHIVE_BACKUP) ||
        ! file.exists(AUTHORS_ARCHIVE) ||
        ! file.exists(DOCUMENTS_ARCHIVE)) {
        stop("Cannot revert archives: backup files do not exist")
    }

    # Check for confirmation and revert
    if (is.logical(confirm) && confirm == TRUE) {

        # Revert briefings archive
        briefings_archive <- readr::read_csv(
            BRIEFINGS_ARCHIVE_BACKUP, col_types = readr::cols())
        readr::write_excel_csv(briefings_archive, BRIEFINGS_ARCHIVE)
        file.remove(BRIEFINGS_ARCHIVE_BACKUP)

        # Revert topics archive
        topics_archive <- readr::read_csv(
            TOPICS_ARCHIVE_BACKUP, col_types = readr::cols())
        readr::write_excel_csv(topics_archive, TOPICS_ARCHIVE)
        file.remove(TOPICS_ARCHIVE_BACKUP)

        # Revert sections archive
        sections_archive <- readr::read_csv(
            SECTIONS_ARCHIVE_BACKUP, col_types = readr::cols())
        readr::write_excel_csv(sections_archive, SECTIONS_ARCHIVE)
        file.remove(SECTIONS_ARCHIVE_BACKUP)

        # Revert authors archive
        authors_archive <- readr::read_csv(
            AUTHORS_ARCHIVE_BACKUP, col_types = readr::cols())
        readr::write_excel_csv(authors_archive, AUTHORS_ARCHIVE)
        file.remove(AUTHORS_ARCHIVE_BACKUP)

        documents_archive <- readr::read_csv(
            DOCUMENTS_ARCHIVE_BACKUP, col_types = readr::cols())
        readr::write_excel_csv(documents_archive, DOCUMENTS_ARCHIVE)
        file.remove(DOCUMENTS_ARCHIVE_BACKUP)

        invisible()

    } else {

        stop("Cannot revert archives: you must confim the revert")
    }
}
