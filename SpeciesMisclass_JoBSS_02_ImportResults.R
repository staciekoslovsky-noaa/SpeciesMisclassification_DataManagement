# Process species misclassification data to the DB: JoBSS 

# Install libraries
library(tidyverse)
library(RPostgreSQL)

##### JoBSS Quartile 1 Variables
project_schema <- "surv_jobss"
wd <- "J:\\SpeciesMisclassification\\JoBSS\\Quartile1"
selections <- "JoBSS_Quartile1Review_RandomSelection.csv"
reviews <- c("JoBSS_Quartile1Review_SpeciesID_CLC.csv", "JoBSS_Quartile1Review_SpeciesID_GMB.csv", "JoBSS_Quartile1Review_SpeciesID_SMW.csv")
reviewer <- c("CLC", "GMB", "SMW")
review_description <- "jobss_quartile1"

##### JoBSS Quartile 3 Variables
# wd <- "J:\\SpeciesMisclassification\\JoBSS\\Quartile2"
# selections <- "JoBSS_Quartile3Review_RandomSelection.csv"
# reviews <- c("JoBSS_Quartile3Review_SpeciesID_CLC.csv", "JoBSS_Quartile3Review_SpeciesID_GMB.csv", "JoBSS_Quartile3Review_SpeciesID_SMW.csv")
# reviewer <- c("CLC", "GMB", "SMW")
# review_description <- "jobss_quartile3"

# Set up working environment
"%notin%" <- Negate("%in%")
setwd(wd)
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              #port = Sys.getenv("pep_port"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# Delete data from tables (if needed)
RPostgreSQL::dbSendQuery(con, paste("DELETE FROM species_misclass.tbl_detections_reviewed WHERE review_description = \'", review_description, "\'", sep = ""))

# Import data that were randomly selected for species misclassification
next_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM species_misclass.tbl_detections_reviewed")
next_id$max <- ifelse(is.na(next_id$max), 0, next_id$max)

random <- read.csv(selections, stringsAsFactors = FALSE)
random <- random %>%
  mutate(id = 1:n() + next_id$max,
         project_schema = project_schema,
         project_detection_id = detection_id,
         species = detection_type,
         review_file = 'original_detection_file',
         reviewer = processing_completed_by,
         review_type = 'original_detection',
         review_description = review_description) %>%
  select(id, project_schema, image_name, project_detection_id, 
         species, review_file, reviewer, review_type, review_description,
         bound_left, bound_bottom, bound_right, bound_top)

RPostgreSQL::dbWriteTable(con, c("species_misclass", "tbl_detections_reviewed"), random, append = TRUE, row.names = FALSE)
RPostgreSQL::dbSendQuery(con, paste("UPDATE species_misclass.tbl_detections_reviewed m
                                    SET species_confidence = p.species_confidence,
                                    age_class = p.age_class,
                                    age_class_confidence = p.age_class_confidence
                                    FROM ", project_schema, ".tbl_detections_processed_rgb p
                                    WHERE p.detection_id = m.project_detection_id
                                    AND review_description = \'", review_description, "\'
                                    AND review_type = 'original_detection'", sep = ""))

# Import data that were reviewed for species misclassification

for (i in 1:length(reviews)) {
  next_id <- RPostgreSQL::dbGetQuery(con, "SELECT max(id) FROM species_misclass.tbl_detections_reviewed")
  next_id$max <- ifelse(is.na(next_id$max), 0, next_id$max)
  
  reviewed <- read.csv(reviews[i], skip = 2, header = FALSE, stringsAsFactors = FALSE, col.names = c("detection", "image_name", "frame_number", "bound_left", "bound_bottom", "bound_right", "bound_top", "score", "length", "detection_type", "type_score", 
                                                                                                     "att1", "att2", "att3", "att4", "att5", "att6", "att7", "att8"))
  
  reviewed <- data.frame(lapply(reviewed, function(x) {gsub("\\(trk-atr\\) *", "", x)})) %>%
    mutate(id = 1:n() + next_id$max,
           project_schema = project_schema,
           image_name = basename(image_name),
           project_detection_id = "",
           species = detection_type,
           species_confidence = ifelse(grepl("^species_confidence", att1), gsub("species_confidence *", "", att1),
                                       ifelse(grepl("^species_confidence", att2), gsub("species_confidence *", "", att2),
                                              ifelse(grepl("^species_confidence", att3), gsub("species_confidence *", "", att3), 
                                                     ifelse(grepl("^species_confidence", att4), gsub("species_confidence *", "", att4),
                                                            ifelse(grepl("^species_confidence", att5), gsub("species_confidence *", "", att5), 
                                                                   ifelse(grepl("^species_confidence", att6), gsub("species_confidence *", "", att6),
                                                                          ifelse(grepl("^species_confidence", att7), gsub("species_confidence *", "", att7), 
                                                                                 ifelse(grepl("^species_confidence", att8), gsub("species_confidence *", "", att8), "NA")))))))),
           age_class = ifelse(grepl("^age_class[[:space:]]", att1), gsub("age_class *", "", att1),
                              ifelse(grepl("^age_class[[:space:]]", att2), gsub("age_class *", "", att2),
                                     ifelse(grepl("^age_class[[:space:]]", att3), gsub("age_class *", "", att3), 
                                            ifelse(grepl("^age_class[[:space:]]", att4), gsub("age_class *", "", att4),
                                                   ifelse(grepl("^age_class[[:space:]]", att5), gsub("age_class *", "", att5), 
                                                          ifelse(grepl("^age_class[[:space:]]", att6), gsub("age_class *", "", att6),
                                                                 ifelse(grepl("^age_clas[[:space:]]s", att7), gsub("age_class *", "", att7), 
                                                                        ifelse(grepl("^age_class[[:space:]]", att8), gsub("age_class *", "", att8), "NA")))))))),
           age_class_confidence = ifelse(grepl("^age_class_confidence", att1), gsub("age_class_confidence *", "", att1),
                                         ifelse(grepl("^age_class_confidence", att2), gsub("age_class_confidence *", "", att2),
                                                ifelse(grepl("^age_class_confidence", att3), gsub("age_class_confidence *", "", att3), 
                                                       ifelse(grepl("^age_class_confidence", att4), gsub("age_class_confidence *", "", att4),
                                                              ifelse(grepl("^age_class_confidence", att5), gsub("age_class_confidence *", "", att5), 
                                                                     ifelse(grepl("^age_class_confidence", att6), gsub("age_class_confidence *", "", att6),
                                                                            ifelse(grepl("^age_class_confidence", att7), gsub("age_class_confidence *", "", att7), 
                                                                                   ifelse(grepl("^age_class_confidence", att8), gsub("age_class_confidence *", "", att8), "NA")))))))),
           review_file = reviews[i],
           reviewer = reviewer[i],
           review_type = 'random_selection',
           review_description = review_description) %>%
    select(id, project_schema, image_name, project_detection_id, 
           species, species_confidence, age_class, age_class_confidence, 
           review_file, reviewer, review_type, review_description,
           bound_left, bound_bottom, bound_right, bound_top)
  
  RPostgreSQL::dbWriteTable(con, c("species_misclass", "tbl_detections_reviewed"), reviewed, append = TRUE, row.names = FALSE)
  
  RPostgreSQL::dbSendQuery(con, paste("UPDATE species_misclass.tbl_detections_reviewed r
                                      SET project_detection_id = o.project_detection_id
                                      FROM (SELECT * FROM species_misclass.tbl_detections_reviewed WHERE review_type = 'original_detection' AND review_description = \'", review_description, "\') o
                                      WHERE r.image_name = o.image_name
                                      AND r.bound_left = o.bound_left
                                      AND r.bound_top = o.bound_top
                                      AND r.review_description = \'", review_description, "\'
                                      AND r.review_type = 'random_selection'
                                      AND r.project_detection_id = \'\'", sep = ""))
  

}

# Disconnect from DB
RPostgreSQL::dbDisconnect(con)
rm(con)
