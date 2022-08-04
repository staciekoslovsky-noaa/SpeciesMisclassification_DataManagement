# JoBSS: Process species misclassification data

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("tidyverse")

# Run code -------------------------------------------------------
con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

# Get data from the DB
data <- RPostgreSQL::dbGetQuery(con, "SELECT * FROM species_misclass.tbl_jobss_quartile1 UNION SELECT * FROM species_misclass.tbl_jobss_quartile3")

# Convert text to coded values
data <- data %>%
  mutate(image_number = as.integer(factor(image_name))) %>%
  mutate(hotspot_number = as.integer(factor(project_detection_id))) %>%
  mutate(obs_id = ifelse(data$reviewer == "CLC", 1, 
                            ifelse(data$reviewer == "GMB", 2, 
                                   ifelse(data$reviewer == "SMW", 3, 0))),
         skill_level = 1,
         alt_ft = ins_altitude * 3.28084,
         sp_id_conf = "x",
         age_class_conf = "x", 
         side = ifelse(grepl("_C_", image_name), 1,
                       ifelse(grepl("_L_", image_name), 2, 3)),
         quartile = ifelse(review_description == "jobss_quartile1", "q1", "q3")) %>%
  mutate(sp_id_conf = ifelse(species == "ringed_seal", 
                             ifelse(species_confidence == "positive", "rd1",
                                    ifelse(species_confidence == "likely", "rd2", 
                                           ifelse(species_confidence == "guess", "rd3", "rd?"))), sp_id_conf)) %>%
  mutate(sp_id_conf = ifelse(species == "bearded_seal", 
                           ifelse(species_confidence == "positive", "bd1",
                                  ifelse(species_confidence == "likely", "bd2", 
                                         ifelse(species_confidence == "guess", "bd3", "bd?"))), sp_id_conf)) %>%
  mutate(sp_id_conf = ifelse(species == "unknown_seal", "unk", sp_id_conf)) %>%
  mutate(age_class_conf = ifelse(age_class == "nonpup", 
                                 ifelse(age_class_confidence == "positive", "a1",
                                        ifelse(age_class_confidence == "likely", "a2", 
                                               ifelse(age_class_confidence == "guess", "a3", "a?"))), age_class_conf)) %>%
  mutate(age_class_conf = ifelse(age_class == "pup", 
                                 ifelse(age_class_confidence == "positive", "p1",
                                        ifelse(age_class_confidence == "likely", "p2", 
                                               ifelse(age_class_confidence == "guess", "p3", "p?"))), age_class_conf)) %>%
  mutate(age_class_conf = ifelse(is.na(age_class), "unk", age_class_conf)) %>%
  mutate(review_step = ifelse(review_file == "original_detection_file", "o", "m")) %>%
  select(project_detection_id, hotspot_number, image_name, image_number, quartile, obs_id, skill_level, side, alt_ft, sp_id_conf, age_class_conf, review_step)

# Prepare code key for exported data ----------------------------------------------------
codes <- rbind((data %>%
                  select(project_detection_id, hotspot_number) %>%
                  unique() %>%
                  mutate(type = "hotspot_number") %>%
                  rename(description = project_detection_id,
                         id = hotspot_number) %>%
                  select(type, id, description)), 
               (data %>%
                  select(image_name, image_number) %>%
                  unique() %>%
                  mutate(type = "image_number") %>%
                  rename(description = image_name,
                         id = image_number) %>%
                  select(type, id, description)
                ))

codes <- rbind(codes, c("quartile", "q1", "Quartile 1"))
codes <- rbind(codes, c("quartile", "q3", "Quartile 3"))

codes <- rbind(codes, c("obs_id", "1", "CLC"))
codes <- rbind(codes, c("obs_id", "2", "GMB"))
codes <- rbind(codes, c("obs_id", "3", "SMW"))

codes <- rbind(codes, c("side", "1", "Center"))
codes <- rbind(codes, c("side", "2", "Left"))
codes <- rbind(codes, c("side", "3", "Right"))
T
codes <- rbind(codes, c("sp_id_conf", "rd1", "Ringed, Positive"))
codes <- rbind(codes, c("sp_id_conf", "rd2", "Ringed, Likely"))
codes <- rbind(codes, c("sp_id_conf", "rd3", "Ringed, Guess"))

codes <- rbind(codes, c("sp_id_conf", "bd1", "Bearded, Positive"))
codes <- rbind(codes, c("sp_id_conf", "bd2", "Bearded, Likely"))
codes <- rbind(codes, c("sp_id_conf", "bd3", "Bearded, Guess"))

codes <- rbind(codes, c("sp_id_conf", "unk", "Unknown Seal"))

codes <- rbind(codes, c("age_class_conf", "a1", "Non-Pup, Positive"))
codes <- rbind(codes, c("age_class_conf", "a2", "Non-Pup, Likely"))
codes <- rbind(codes, c("age_class_conf", "a3", "Non-Pup, Guess"))

codes <- rbind(codes, c("age_class_conf", "p1", "Pup, Positive"))
codes <- rbind(codes, c("age_class_conf", "p2", "Pup, Likely"))
codes <- rbind(codes, c("age_class_conf", "p3", "Pup, Guess"))

codes <- rbind(codes, c("age_class_conf", "unk", "Unknown Seal"))

codes <- rbind(codes, c("review_step", "o", "Original Processing"))
codes <- rbind(codes, c("review_step", "m", "Species Misclassification"))

# Export data
export <- data %>%
  select(hotspot_number, image_number, quartile, obs_id, skill_level, side, alt_ft, sp_id_conf, age_class_conf, review_step)

write.csv(export, "C://skh//JoBSS_CompiledSpeciesID_20220804.csv", row.names = FALSE)
write.csv(codes, "C://skh//JoBSS_CompiledSpeciesID_CodeKey_20220804.csv", row.names = FALSE)

# Disconnect and clean up
RPostgreSQL::dbDisconnect(con)
rm(con, data)
