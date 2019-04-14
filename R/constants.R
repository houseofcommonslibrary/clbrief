### Package constants

# URLs ------------------------------------------------------------------------

URL_API_FEED = "https://lda.data.parliament.uk/researchbriefings.json"
URL_API_BRIEFING = "https://lda.data.parliament.uk/researchbriefings/"
URL_BP = "https://researchbriefings.parliament.uk/ResearchBriefing/Summary/"

# Archiving -------------------------------------------------------------------

DEFAULT_ARCHIVES <- "archives"

BRIEFINGS_ARCHIVE <- "briefings.csv"
TOPICS_ARCHIVE <- "topics.csv"
SECTIONS_ARCHIVE <- "sections.csv"
AUTHORS_ARCHIVE <- "authors.csv"
DOCUMENTS_ARCHIVE <- "documents.csv"

BRIEFINGS_ARCHIVE_BACKUP <- "backup_briefings.csv"
TOPICS_ARCHIVE_BACKUP <- "backup_topics.csv"
SECTIONS_ARCHIVE_BACKUP <- "backup_sections.csv"
AUTHORS_ARCHIVE_BACKUP <- "backup_authors.csv"
DOCUMENTS_ARCHIVE_BACKUP <- "backup_documents.csv"

# Database --------------------------------------------------------------------

DATABASE_DIR <- "database"
BRIEFINGS_DB <- file.path(DATABASE_DIR, "briefings.sqlite")
BACKUP_DB <- file.path(DATABASE_DIR, "briefings-backup.sqlite")

BRIEFINGS_CSV <- "briefings.csv"
TOPICS_CSV <- "topics.csv"
SECTIONS_CSV <- "sections.csv"
AUTHORS_CSV <- "authors.csv"
DOCUMENTS_CSV <- "documents.csv"
