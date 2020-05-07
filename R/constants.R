### Package constants

# URLs ------------------------------------------------------------------------

URL_API_FEED = "https://lda.data.parliament.uk/researchbriefings.json"
URL_API_BRIEFING = "https://lda.data.parliament.uk/researchbriefings/"
URL_BP = "https://researchbriefings.parliament.uk/ResearchBriefing/Summary/"

# Database --------------------------------------------------------------------

DATABASE_DIR <- "database"

BRIEFINGS_DB <- file.path(DATABASE_DIR, "briefings.sqlite")
BRIEFINGS_BACKUP_DB <- file.path(DATABASE_DIR, "briefings-backup.sqlite")

MIRROR_DB <- file.path(DATABASE_DIR, "mirror.sqlite")
MIRROR_BACKUP_DB <- file.path(DATABASE_DIR, "mirror-backup.sqlite")

BRIEFINGS_CSV <- "briefings.csv"
TOPICS_CSV <- "topics.csv"
SECTIONS_CSV <- "sections.csv"
AUTHORS_CSV <- "authors.csv"
DOCUMENTS_CSV <- "documents.csv"
