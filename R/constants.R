### Package constants

# URLs ------------------------------------------------------------------------

URL_API_FEED = "https://lda.data.parliament.uk/researchbriefings.json"
URL_API_BRIEFING = "https://lda.data.parliament.uk/researchbriefings/"
URL_BP = "https://researchbriefings.parliament.uk/ResearchBriefing/Summary/"

# Archiving -------------------------------------------------------------------

ARCHIVES_DIR <- "archives"

BRIEFINGS_ARCHIVE <- file.path(ARCHIVES_DIR, "briefings.csv")
TOPICS_ARCHIVE <- file.path(ARCHIVES_DIR, "topics.csv")
SECTIONS_ARCHIVE <- file.path(ARCHIVES_DIR, "sections.csv")
AUTHORS_ARCHIVE <- file.path(ARCHIVES_DIR, "authors.csv")
DOCUMENTS_ARCHIVE <- file.path(ARCHIVES_DIR, "documents.csv")

BRIEFINGS_ARCHIVE_BACKUP <- file.path(ARCHIVES_DIR, "backup_briefings.csv")
TOPICS_ARCHIVE_BACKUP <- file.path(ARCHIVES_DIR, "backup_topics.csv")
SECTIONS_ARCHIVE_BACKUP <- file.path(ARCHIVES_DIR, "backup_sections.csv")
AUTHORS_ARCHIVE_BACKUP <- file.path(ARCHIVES_DIR, "backup_authors.csv")
DOCUMENTS_ARCHIVE_BACKUP <- file.path(ARCHIVES_DIR, "backup_documents.csv")
