# Ice Seals 2025: Randomly select seals for species identification
# S. Koslovsky, June 2026

# Create functions -----------------------------------------------
# Function to install packages needed
install_pkg <- function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dep = TRUE)
    if (!require(x, character.only = TRUE)) stop("Package not found")
  }
}

# Install libraries ----------------------------------------------
install_pkg("RPostgreSQL")
install_pkg("tidyverse")

# Run code -------------------------------------------------------
# Extract data from DB ------------------------------------------------------------------
con <- RPostgreSQL::dbConnect(
  PostgreSQL(),
  dbname = Sys.getenv("pep_db"),
  host = Sys.getenv("pep_ip"),
  user = Sys.getenv("pep_admin"),
  password = Sys.getenv("admin_pw")
)

process <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT r.detection, r.image_name, r.frame_number, r.bound_left, r.bound_top, r.bound_right, r.bound_bottom,
                                   r.score, r.length, i.detection_type AS detection_type_ir, r.detection_type, r.age_class, r.type_score, r.processed_detection_id 
                                   FROM surv_ice_seals_2025.tbl_detections_processed_rgb r
                                   INNER JOIN surv_ice_seals_2025.tbl_detections_processed_ir i USING (processed_detection_id)
                                   WHERE r.detection_type LIKE \'%seal%\' 
                                   AND r.detection_type NOT LIKE \'%unknown%\' AND r.detection_type NOT LIKE \'%dead%\' AND r.detection_type NOT LIKE \'%water%\'
                                   AND r.age_class <> \'NA\'
                                   AND i.detection_type <> \'animal_duplicate\'"
)

process_summ <- process %>%
  group_by(detection_type, age_class) %>%
  summarize(count = n(), .groups = 'drop') %>%
  mutate(proportion = count / sum(count))

# Create random selections of seals------------------------------------------------------
set.seed(465396)
process_rand <- process %>%
  mutate(group1 = 'not_selected', group2 = 'not_selected')

# Ribbon seals (all selected for both group 1 and group 2)
rand_ribbon_1 <- process_rand %>%
  filter(detection_type == 'ribbon_seal') %>%
  slice_sample(n = 43) %>%
  mutate(group1 = 'selected')

rand_ribbon_2 <- process_rand %>%
  filter(detection_type == 'ribbon_seal') %>%
  slice_sample(n = 43) %>%
  mutate(group2 = 'selected')

rand_ribbon <- rand_ribbon_1 %>%
  rbind(rand_ribbon_2)
rm(rand_ribbon_1, rand_ribbon_2)

# Spotted seals (all selected and split across group 1 and group 2)
rand_spotted_1_nonpup <- process_rand %>%
  filter(detection_type == 'spotted_seal' & age_class == 'nonpup') %>%
  slice_sample(n = 69) %>%
  mutate(group1 = 'selected')
rand_spotted_1_pup <- process_rand %>%
  filter(detection_type == 'spotted_seal' & age_class == 'pup') %>%
  slice_sample(n = 20) %>%
  mutate(group1 = 'selected')

rand_spotted_2_nonpup <- process_rand %>%
  filter(detection_type == 'spotted_seal' & age_class == 'nonpup') %>%
  anti_join(rand_spotted_1_nonpup, by = 'processed_detection_id') %>%
  mutate(group2 = 'selected')
rand_spotted_2_pup <- process_rand %>%
  filter(detection_type == 'spotted_seal' & age_class == 'pup') %>%
  anti_join(rand_spotted_1_pup, by = 'processed_detection_id') %>%
  mutate(group2 = 'selected')

rand_spotted <- rand_spotted_1_nonpup %>%
  rbind(rand_spotted_1_pup) %>%
  rbind(rand_spotted_2_nonpup) %>%
  rbind(rand_spotted_2_pup)
rm(
  rand_spotted_1_nonpup,
  rand_spotted_1_pup,
  rand_spotted_2_nonpup,
  rand_spotted_2_pup
)

# Bearded seals (select random 100 for each group based on proportion of nonpup:pup (87:13))
rand_bearded_1_nonpup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'nonpup') %>%
  slice_sample(n = 87) %>%
  mutate(group1 = 'selected')
rand_bearded_1_pup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'pup') %>%
  slice_sample(n = 13) %>%
  mutate(group1 = 'selected')

rand_bearded_2_nonpup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'nonpup') %>%
  anti_join(rand_bearded_1_nonpup, by = 'processed_detection_id') %>%
  slice_sample(n = 87) %>%
  mutate(group2 = 'selected')
rand_bearded_2_pup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'pup') %>%
  anti_join(rand_bearded_1_pup, by = 'processed_detection_id') %>%
  slice_sample(n = 13) %>%
  mutate(group2 = 'selected')

rand_bearded <- rand_bearded_1_nonpup %>%
  rbind(rand_bearded_1_pup) %>%
  rbind(rand_bearded_2_nonpup) %>%
  rbind(rand_bearded_2_pup)
rm(
  rand_bearded_1_nonpup,
  rand_bearded_1_pup,
  rand_bearded_2_nonpup,
  rand_bearded_2_pup
)

# Ringed seals (select random 100 for each group based on proportion of nonpup:pup (98:2))
rand_ringed_1_nonpup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'nonpup') %>%
  slice_sample(n = 98) %>%
  mutate(group1 = 'selected')
rand_ringed_1_pup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'pup') %>%
  slice_sample(n = 2) %>%
  mutate(group1 = 'selected')

rand_ringed_2_nonpup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'nonpup') %>%
  anti_join(rand_ringed_1_nonpup, by = 'processed_detection_id') %>%
  slice_sample(n = 98) %>%
  mutate(group2 = 'selected')
rand_ringed_2_pup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'pup') %>%
  anti_join(rand_ringed_1_pup, by = 'processed_detection_id') %>%
  slice_sample(n = 2) %>%
  mutate(group2 = 'selected')

rand_ringed <- rand_ringed_1_nonpup %>%
  rbind(rand_ringed_1_pup) %>%
  rbind(rand_ringed_2_nonpup) %>%
  rbind(rand_ringed_2_pup)
rm(
  rand_ringed_1_nonpup,
  rand_ringed_1_pup,
  rand_ringed_2_nonpup,
  rand_ringed_2_pup
)

# Random ringed and bearded seals (select random 18 for each group based on proportion of species and age_class)
# Group 1: 13 ringed non-pup, 1 ringed pup, 4 bearded nonpup, 0 bearded pup
rand18_ringed_1_nonpup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'nonpup') %>%
  anti_join(rand_ringed, by = "processed_detection_id") %>%
  slice_sample(n = 13) %>%
  mutate(group1 = 'selected')
rand18_ringed_1_pup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'pup') %>%
  anti_join(rand_ringed, by = "processed_detection_id") %>%
  slice_sample(n = 1) %>%
  mutate(group1 = 'selected')

rand18_bearded_1_nonpup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'nonpup') %>%
  anti_join(rand_bearded, by = "processed_detection_id") %>%
  slice_sample(n = 4) %>%
  mutate(group1 = 'selected')
rand18_bearded_1_pup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'pup') %>%
  anti_join(rand_bearded, by = "processed_detection_id") %>%
  slice_sample(n = 0) %>%
  mutate(group1 = 'selected')

rand18_group1 <- rand18_ringed_1_nonpup %>%
  rbind(rand18_ringed_1_pup) %>%
  rbind(rand18_bearded_1_nonpup) %>%
  rbind(rand18_bearded_1_pup)
rm(
  rand18_ringed_1_nonpup,
  rand18_ringed_1_pup,
  rand18_bearded_1_nonpup,
  rand18_bearded_1_pup
)


# Group 2: 13 ringed non-pup, 0 ringed pup, 4 bearded nonpup, 1 bearded pup
rand18_ringed_2_nonpup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'nonpup') %>%
  anti_join(rand_ringed, by = "processed_detection_id") %>%
  anti_join(rand18_group1, by = "processed_detection_id") %>%
  slice_sample(n = 13) %>%
  mutate(group2 = 'selected')
rand18_ringed_2_pup <- process_rand %>%
  filter(detection_type == 'ringed_seal' & age_class == 'pup') %>%
  anti_join(rand_ringed, by = "processed_detection_id") %>%
  anti_join(rand18_group1, by = "processed_detection_id") %>%
  slice_sample(n = 0) %>%
  mutate(group2 = 'selected')

rand18_bearded_2_nonpup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'nonpup') %>%
  anti_join(rand_bearded, by = "processed_detection_id") %>%
  anti_join(rand18_group1, by = "processed_detection_id") %>%
  slice_sample(n = 4) %>%
  mutate(group2 = 'selected')
rand18_bearded_2_pup <- process_rand %>%
  filter(detection_type == 'bearded_seal' & age_class == 'pup') %>%
  anti_join(rand_bearded, by = "processed_detection_id") %>%
  anti_join(rand18_group1, by = "processed_detection_id") %>%
  slice_sample(n = 1) %>%
  mutate(group2 = 'selected')

rand18 <- rand18_group1 %>%
  rbind(rand18_ringed_2_nonpup) %>%
  rbind(rand18_ringed_2_pup) %>%
  rbind(rand18_bearded_2_nonpup) %>%
  rbind(rand18_bearded_2_pup)
rm(
  rand18_group1,
  rand18_ringed_2_nonpup,
  rand18_ringed_2_pup,
  rand18_bearded_2_nonpup,
  rand18_bearded_2_pup
)

randoms <- rand_bearded %>%
  rbind(rand_ribbon) %>%
  rbind(rand_ringed) %>%
  rbind(rand_spotted) %>%
  rbind(rand18) %>%
  select(
    detection,
    image_name,
    frame_number,
    bound_left,
    bound_top,
    bound_right,
    bound_bottom,
    score,
    length,
    detection_type,
    type_score
  ) %>%
  mutate(detection = )

write.csv(
  randoms,
  "J:\\SpeciesMisclassification\\IceSeals2025\\IceSeals_2025_RandomSelection.csv",
  row.names = FALSE,
  quote = FALSE,
)

color_images <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT image_name, image_dir FROM surv_ice_seals_2025.tbl_images WHERE image_type = \'rgb_image\'"
)

color_randoms <- color_images %>%
  inner_join(randoms, by = "image_name") %>%
  unique()


randoms$detection_type <- ""

random_clc <- process %>%
  subset(processing_completed_by != 'CLC') %>%
  arrange(image_name) %>%
  rowid_to_column(var = "id") %>%
  mutate(detection = id - 1) %>%
  group_by(image_name) %>%
  mutate(frame_number = cur_group_id() - 1) %>%
  ungroup() %>%
  select(
    detection,
    image_name,
    frame_number,
    bound_left,
    bound_top,
    bound_right,
    bound_bottom,
    score,
    length,
    detection_type,
    type_score
  )

random_smw <- process %>%
  subset(processing_completed_by != 'SMW') %>%
  arrange(image_name) %>%
  rowid_to_column(var = "id") %>%
  mutate(detection = id - 1) %>%
  group_by(image_name) %>%
  mutate(frame_number = cur_group_id() - 1) %>%
  ungroup() %>%
  select(
    detection,
    image_name,
    frame_number,
    bound_left,
    bound_top,
    bound_right,
    bound_bottom,
    score,
    length,
    detection_type,
    type_score
  )

random_gmb <- process %>%
  subset(processing_completed_by != 'GMB') %>%
  arrange(image_name) %>%
  rowid_to_column(var = "id") %>%
  mutate(detection = id - 1) %>%
  group_by(image_name) %>%
  mutate(frame_number = cur_group_id() - 1) %>%
  ungroup() %>%
  select(
    detection,
    image_name,
    frame_number,
    bound_left,
    bound_top,
    bound_right,
    bound_bottom,
    score,
    length,
    detection_type,
    type_score
  )

# Export random selections of seals -----------------------------------------------------
write.table(
  random_clc,
  "J:\\SpeciesMisclassification\\JoBSS\\Quartile1\\JoBSS_Quartile1Review_SpeciesID_CLC.csv",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE,
  sep = ","
)
write.table(
  random_smw,
  "J:\\SpeciesMisclassification\\JoBSS\\Quartile1\\JoBSS_Quartile1Review_SpeciesID_SMW.csv",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE,
  sep = ","
)
write.table(
  random_gmb,
  "J:\\SpeciesMisclassification\\JoBSS\\Quartile1\\JoBSS_Quartile1Review_SpeciesID_GMB.csv",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE,
  sep = ","
)

write.table(
  unique(random_clc$image_name),
  "J:\\SpeciesMisclassification\\JoBSS\\Quartile1\\Images\\_JoBSS_Quartile1Review_SpeciesID_CLC_images.txt",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE
)
write.table(
  unique(random_smw$image_name),
  "J:\\SpeciesMisclassification\\JoBSS\\Quartile1\\Images\\_JoBSS_Quartile1Review_SpeciesID_SMW_images.txt",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE
)
write.table(
  unique(random_gmb$image_name),
  "J:\\SpeciesMisclassification\\JoBSS\\Quartile1\\Images\\_JoBSS_Quartile1Review_SpeciesID_GMB_images.txt",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE
)

# Copy images to server for processing --------------------------------------------------
color_images <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT image_name, image_dir FROM surv_jobss.tbl_images WHERE image_type = \'rgb_image\'"
)
color_images$rand_select <- ifelse(
  color_images$image_name %in% process$image_name,
  "Select",
  ""
)
color_images <- color_images %>%
  filter(rand_select == "Select")

color <- paste(color_images$image_dir, color_images$image_name, sep = "/")

file.copy(color, "J:\\SpeciesMisclassification\\IceSeals2025\\Images")

# Disconnect for database and delete unnecessary variables ------------------------------
dbDisconnect(con)
rm(con)
