# Import ChESS species misclassification data to DB

# Install libraries
library(tidyverse)
library(RPostgreSQL)

# Set up working environment
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              #port = Sys.getenv("pep_port"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Remove any existing records before reimport
RPostgreSQL::dbSendQuery(con, paste("DELETE FROM species_misclass.tbl_detections_reviewed WHERE review_description LIKE \'%chess%\'"))

# Prepare data for import
group <- read.csv("C:/Users/Stacie.Hardy/Work/Work/Projects/AS_CHESS/Data/CHESS_CompiledSpeciesID_CodeKey_Round1_20171108.csv")

next_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM species_misclass.tbl_detections_reviewed")
next_id$max <- ifelse(is.na(next_id$max), 0, next_id$max)

data <- read.csv("C:/Users/Stacie.Hardy/Work/Work/Projects/AS_CHESS/Data/CHESS_SpeciesID_QAQC_20170418_SKH.csv") %>%
  rename(image_name = thermal_image_name,
         species = SPECIES_ID,
         species_confidence = SPECIES_CONFIDENCE,
         age_class = AGE_CLASS,
         age_class_confidence = AGE_CLASS_CONFIDENCE,
         alt_species = ALT_SPECIES_ID,
         alt_age_class = ALT_AGE_CLASS,
         num_seal = NUMBER_OF_SEALS,
         hotspot_type = HOTSPOT_TYPE) %>%
  mutate(id = 1:n() + next_id$max,
         project_schema = 'surv_chess',
         review_file = 'CHESS_SpeciesID_QAQC_20170418_SKH.csv',
         project_detection_id = as.character(hotspot_id)) %>%
  mutate(species = ifelse(species == "Bearded Seal", "bearded_seal", species),
         species = ifelse(species == "Ringed Seal", "ringed_seal", species),
         species = ifelse(species == "Fox", "fox", species),
         species = ifelse(species == "UNK Seal", "unknown_seal", species),
         species = ifelse(species == "", NA, species)) %>%
  mutate(species_confidence = ifelse(species_confidence == "100%", "positive", species_confidence),
         species_confidence = ifelse(species_confidence == "Likely", "likely", species_confidence),
         species_confidence = ifelse(species_confidence == "Guess", "guess", species_confidence),
         species_confidence = ifelse(species_confidence == "", NA, species_confidence)) %>%
  mutate(age_class = ifelse(age_class == "Non-Pup", "nonpup", age_class),
         age_class = ifelse(age_class == "Pup", "pup", age_class),
         age_class = ifelse(age_class == "Mom-Pup Pair", "mom_pup", age_class),
         age_class = ifelse(age_class == "", NA, age_class)) %>%
  mutate(age_class_confidence = ifelse(age_class_confidence == "100%", "positive", age_class_confidence),
         age_class_confidence = ifelse(age_class_confidence == "Likely", "likely", age_class_confidence),
         age_class_confidence = ifelse(age_class_confidence == "Guess", "guess", age_class_confidence),
         age_class_confidence = ifelse(age_class_confidence == "", NA, age_class_confidence)) %>%
  mutate(alt_species = ifelse(alt_species == "Bearded Seal", "bearded_seal", alt_species),
         alt_species = ifelse(alt_species == "Ringed Seal", "ringed_seal", alt_species),
         alt_species = ifelse(alt_species == "Spotted Seal", "spotted_seal", alt_species),
         alt_species = ifelse(alt_species == "UNK Pinniped", "unknown_pinniped", alt_species),
         alt_species = ifelse(alt_species == "", NA, alt_species)) %>%
  mutate(alt_age_class = ifelse(alt_age_class == "Non-Pup", "nonpup", alt_age_class),
         alt_age_class = ifelse(alt_age_class == "Pup", "pup", alt_age_class),
         alt_age_class = ifelse(alt_age_class == "", NA, alt_age_class)) %>%
  mutate(hotspot_type = ifelse(hotspot_type == "Animal", "animal", hotspot_type),
         hotspot_type = ifelse(hotspot_type == "Evidence of Seal", "evidence_of_seal", hotspot_type),
         hotspot_type = ifelse(hotspot_type == "Not Discernible", "not_discernible", hotspot_type),
         hotspot_type = ifelse(hotspot_type == "Anomaly", "anomaly", hotspot_type),
         hotspot_type = ifelse(hotspot_type == "", NA, hotspot_type)) %>%
  mutate(description = project_detection_id)

data <- data %>%
  left_join(group, by = "description") %>%
  mutate(review_description = ifelse(is.na(type), "chess_review2", "chess_review1")) %>%
  rename(id = id.x) %>%
  select(id, project_schema, image_name, project_detection_id, species, species_confidence, age_class, age_class_confidence, review_file, reviewer, review_description,
         alt_species, alt_age_class, num_seal, hotspot_type)

# Write data to DB
RPostgreSQL::dbWriteTable(con, c("species_misclass", "tbl_detections_reviewed"), data, append = TRUE, row.names = FALSE)

# Update review_type field
RPostgreSQL::dbSendQuery(con, "UPDATE species_misclass.tbl_detections_reviewed r SET review_type = 'original_detection' FROM surv_chess.tbl_process p WHERE project_detection_id = hotspot_id AND review_description LIKE \'%chess%\' AND r.reviewer = p.reviewer")
RPostgreSQL::dbSendQuery(con, "UPDATE species_misclass.tbl_detections_reviewed SET review_type = 'random_selection' WHERE review_description LIKE \'%chess%\' AND review_type IS NULL")

# Disconnect from DB
RPostgreSQL::dbDisconnect(con)
rm(con, group)
