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
rm(install_pkg)

# Run code -------------------------------------------------------
# Extract data from DB ------------------------------------------------------------------
# Done once on 6/17/2026 and saved to CSV, since there are known changes coming to the data after remaining QA/QC :/
# con <- RPostgreSQL::dbConnect(
#   PostgreSQL(),
#   dbname = Sys.getenv("pep_db"),
#   host = Sys.getenv("pep_ip"),
#   user = Sys.getenv("pep_admin"),
#   password = Sys.getenv("admin_pw")
# )

# process <- RPostgreSQL::dbGetQuery(
#   con,
#   "SELECT r.detection, r.image_name, r.frame_number, r.bound_left, r.bound_top, r.bound_right, r.bound_bottom,
#                                    r.score, r.length, i.detection_type AS detection_type_ir, r.detection_type, r.age_class, r.type_score, r.processed_detection_id
#                                    FROM surv_ice_seals_2025.tbl_detections_processed_rgb r
#                                    INNER JOIN surv_ice_seals_2025.tbl_detections_processed_ir i USING (processed_detection_id)
#                                    WHERE r.detection_type LIKE \'%seal%\'
#                                    AND r.detection_type NOT LIKE \'%unknown%\' AND r.detection_type NOT LIKE \'%dead%\' AND r.detection_type NOT LIKE \'%water%\'
#                                    AND r.age_class <> \'NA\'
#                                    AND i.detection_type <> \'animal_duplicate\'"
# )

# RPostgreSQL::dbDisconnect(con)
# rm(con)

# write.csv(
#   process,
#   "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_StartingProcessDataset.csv",
#   row.names = FALSE,
#   quote = FALSE,
# )

process <- read.csv(
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_StartingProcessDataset.csv"
)

process_summ <- process %>%
  group_by(detection_type, age_class) %>%
  summarize(count = n(), .groups = 'drop')

# Create random selections of seals------------------------------------------------------
# Round 1 processing -------------------------------------------
# Ribbon seals (all selected, excluding known duplicates)
rand_ribbon <- process %>%
  filter(detection_type == 'ribbon_seal') %>%
  # remove known duplicates
  filter(processed_detection_id != 'surv_ice_seals_2025_fl207_L_114')

# Spotted seals (all selected, excluding known duplicates)
rand_spotted <- process %>%
  filter(detection_type == 'spotted_seal') %>%
  # remove known duplicates
  filter(processed_detection_id != 'surv_ice_seals_2025_fl207_L_117') %>%
  filter(processed_detection_id != 'surv_ice_seals_2025_fl207_R_34') %>%
  filter(processed_detection_id != 'surv_ice_seals_2025_fl235_C_3') %>%
  filter(processed_detection_id != 'surv_ice_seals_2025_fl235_C_36') %>%
  filter(processed_detection_id != 'surv_ice_seals_2025_fl235_C_78')

# Bearded seals (select random 200 non-pup and 40 pup)
set.seed(1)
rand_bearded_nonpup <- process %>%
  filter(detection_type == 'bearded_seal' & age_class == 'nonpup') %>%
  slice_sample(n = 200)
set.seed(2)
rand_bearded_pup <- process %>%
  filter(detection_type == 'bearded_seal' & age_class == 'pup') %>%
  slice_sample(n = 40)

rand_bearded <- rand_bearded_nonpup %>%
  rbind(rand_bearded_pup)
rm(
  rand_bearded_nonpup,
  rand_bearded_pup
)

# Bearded seals (select random 200 non-pup and 40 pup)
set.seed(3)
rand_ringed_nonpup <- process %>%
  filter(detection_type == 'ringed_seal' & age_class == 'nonpup') %>%
  slice_sample(n = 200)
set.seed(4)
rand_ringed_pup <- process %>%
  filter(detection_type == 'ringed_seal' & age_class == 'pup') %>%
  slice_sample(n = 40)

rand_ringed <- rand_ringed_nonpup %>%
  rbind(rand_ringed_pup)
rm(
  rand_ringed_nonpup,
  rand_ringed_pup
)

# Create merged rd1 dataset and summarize
randoms_rd1 <- rand_bearded %>%
  rbind(rand_ribbon) %>%
  rbind(rand_ringed) %>%
  rbind(rand_spotted)

rm(rand_ribbon, rand_ringed, rand_spotted, rand_bearded)

randoms_rd1_summ <- randoms_rd1 %>%
  group_by(detection_type, age_class) %>%
  summarize(count = n(), .groups = 'drop')

# Prepare data for export (v1)
randoms_rd1_export <- randoms_rd1 %>%
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
    type_score,
    processed_detection_id
  ) %>%
  arrange(image_name) %>%
  mutate(detection = row_number() - 1) %>%
  ungroup() %>%
  group_by(image_name) %>%
  mutate(frame_number = cur_group_id() - 1) %>%
  ungroup()

write.csv(
  randoms_rd1_export,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_v1.csv",
  row.names = FALSE,
  quote = FALSE,
)

randoms_rd1_export <- randoms_rd1_export %>%
  select(-processed_detection_id)

write.csv(
  randoms_rd1_export,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_v1_forDIVE.csv",
  row.names = FALSE,
  quote = FALSE,
)

color_images <- RPostgreSQL::dbGetQuery(
  con,
  "SELECT image_name, image_dir FROM surv_ice_seals_2025.tbl_images WHERE image_type = \'rgb_image\'"
)

randoms_rd1_images <- color_images %>%
  inner_join(randoms_rd1_export, by = "image_name") %>%
  mutate(image_path = paste0(image_dir, "/", image_name)) %>%
  select(image_path) %>%
  arrange(image_path) %>%
  unique()

write.table(
  randoms_rd1_images,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_v1_images.txt",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE
)


# Round 2 processing -------------------------------------------
# After reviewing the data, a number of slight adjustments need to be made
# 1. Remove duplicate seals and poor image quality
randoms_rd2 <- randoms_rd1 %>%
  filter(processed_detection_id != 'surv_ice_seals_2025_fl207_L_152') %>% #314 duplicate spotted seal - not replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl207_L_153') %>% #315 duplicate spotted seal - not replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl210_L_202') %>% #342 duplicate bearded nonpup - to be replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl218_L_16') %>% #409 duplicate ringed pup - to be replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl221_R_28') %>% #448 duplicate spotted nonpup - not replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl227_C_50') %>% #478 duplicate bearded nonpup - to be replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl237_L_499') %>% #590 uncertain ringed nonpup (bad BB) - to be replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl237_R_1017') %>% #670 bad BB (bearded nonpup) - to be replaced
  filter(processed_detection_id != 'surv_ice_seals_2025_fl242_L_10') #684 duplicate bearded nonpup - to be replaced

# 2. Select replacement seals:
# bearded nonpup 4
# ringed pup: 1
# ringed nonpup: 1
set.seed(5)
randoms_rd2_replace_bearded_nonpup <- process %>%
  filter(detection_type == 'bearded_seal' & age_class == 'nonpup') %>%
  anti_join(randoms_rd2, by = "processed_detection_id") %>%
  slice_sample(n = 4) %>%
  mutate(detection_type = "bearded_seal_new")

set.seed(6)
randoms_rd2_replace_ringed_nonpup <- process %>%
  filter(detection_type == 'ringed_seal' & age_class == 'nonpup') %>%
  anti_join(randoms_rd2, by = "processed_detection_id") %>%
  slice_sample(n = 1) %>%
  mutate(detection_type = "ringed_seal_new")

set.seed(7)
randoms_rd2_replace_ringed_pup <- process %>%
  filter(detection_type == 'ringed_seal' & age_class == 'pup') %>%
  anti_join(randoms_rd2, by = "processed_detection_id") %>%
  slice_sample(n = 1) %>%
  mutate(detection_type = "ringed_seal_new")

randoms_rd2 <- randoms_rd2 %>%
  rbind(randoms_rd2_replace_bearded_nonpup) %>%
  rbind(randoms_rd2_replace_ringed_nonpup) %>%
  rbind(randoms_rd2_replace_ringed_pup)
rm(
  randoms_rd2_replace_bearded_nonpup,
  randoms_rd2_replace_ringed_nonpup,
  randoms_rd2_replace_ringed_pup
)

# 3. Update BB
randoms_rd2 <- randoms_rd2 %>%
  # Correct #6 surv_ice_seals_2025_fl108_C_677
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_677',
      as.integer(5978),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_677',
      as.integer(7554),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_677',
      as.integer(6055),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_677',
      as.integer(7612),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #8 surv_ice_seals_2025_fl108_C_679
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_679',
      as.integer(9052),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_679',
      as.integer(5326),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_679',
      as.integer(9130),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl108_C_679',
      as.integer(5414),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #71 surv_ice_seals_2025_fl120_L_1345
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl120_L_1345',
      as.integer(5000),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl120_L_1345',
      as.integer(5885),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl120_L_1345',
      as.integer(5100),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl120_L_1345',
      as.integer(5952),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #121 surv_ice_seals_2025_fl123_R_67
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_67',
      as.integer(4153),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_67',
      as.integer(8629),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_67',
      as.integer(4248),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_67',
      as.integer(8757),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #122 surv_ice_seals_2025_fl123_R_292
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_292',
      as.integer(4201),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_292',
      as.integer(8725),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_292',
      as.integer(4266),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl123_R_292',
      as.integer(8780),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #165 surv_ice_seals_2025_fl127_C_105
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl127_C_105',
      as.integer(299),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl127_C_105',
      as.integer(8414),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl127_C_105',
      as.integer(412),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl127_C_105',
      as.integer(8491),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #575 surv_ice_seals_2025_fl237_L_87
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_87',
      as.integer(1978),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_87',
      as.integer(4194),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_87',
      as.integer(2055),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_87',
      as.integer(4246),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #576 surv_ice_seals_2025_fl237_L_209
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_209',
      as.integer(1376),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_209',
      as.integer(2072),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_209',
      as.integer(1451),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_209',
      as.integer(2128),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #577 surv_ice_seals_2025_fl237_L_210
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_210',
      as.integer(1266),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_210',
      as.integer(1972),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_210',
      as.integer(1329),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_210',
      as.integer(2023),
      as.integer(bound_bottom)
    )
  ) %>%
  # Correct #607 surv_ice_seals_2025_fl237_L_1346
  mutate(
    bound_left = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_1346',
      as.integer(2366),
      as.integer(bound_left)
    ),
    bound_top = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_1346',
      as.integer(3734),
      as.integer(bound_top)
    ),
    bound_right = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_1346',
      as.integer(2466),
      as.integer(bound_right)
    ),
    bound_bottom = ifelse(
      processed_detection_id == 'surv_ice_seals_2025_fl237_L_1346',
      as.integer(3800),
      as.integer(bound_bottom)
    )
  )

# 3. Export replacement seals to confirm they are OK and not duplicates of remaining seals!
randoms_rd2_export <- randoms_rd2 %>%
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
    type_score,
    processed_detection_id
  ) %>%
  arrange(image_name) %>%
  mutate(detection = row_number() - 1) %>%
  ungroup() %>%
  group_by(image_name) %>%
  mutate(frame_number = cur_group_id() - 1) %>%
  ungroup()

write.csv(
  randoms_rd2_export,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_v2.csv",
  row.names = FALSE,
  quote = FALSE,
)

randoms_rd2_export <- randoms_rd2_export %>%
  select(-processed_detection_id)

write.csv(
  randoms_rd2_export,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_v2_forDIVE.csv",
  row.names = FALSE,
  quote = FALSE,
)

randoms_rd2_images <- color_images %>%
  inner_join(randoms_rd2_export, by = "image_name") %>%
  mutate(image_path = paste0(image_dir, "/", image_name)) %>%
  select(image_path) %>%
  arrange(image_path) %>%
  unique()

write.table(
  randoms_rd2_images,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals_2025_RandomSelection_v2_images.txt",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE
)

# 4. All newly added seals were OK, so remove "new_" prefix from detection_type
randoms_rd2 <- randoms_rd2 %>%
  mutate(detection_type = gsub('_new', '', detection_type))

randoms_rd2_summ <- randoms_rd2 %>%
  group_by(detection_type, age_class) %>%
  summarize(count = n(), .groups = 'drop')


# Create review datasets------------------------------------------------------
# Process data into 7 review sets -- with each review set receiving all ribbon seals and each remaining seal being reviewed by 3 people
# And each review set receiving a proportional number of nonpups and pups for each species

# Create ribbon_seal review subset ---------------
review_ribbon_seal <- randoms_rd2 %>%
  filter(detection_type == "ribbon_seal") %>%
  mutate(subset_id = as.integer(0))

review_ribbon_seal <- review_ribbon_seal %>%
  mutate(subset_id = as.integer(1)) %>%
  rbind(review_ribbon_seal) %>%
  mutate(subset_id = ifelse(subset_id == 0, as.integer(2), subset_id)) %>%
  rbind(review_ribbon_seal) %>%
  mutate(subset_id = ifelse(subset_id == 0, as.integer(3), subset_id)) %>%
  rbind(review_ribbon_seal) %>%
  mutate(subset_id = ifelse(subset_id == 0, as.integer(4), subset_id)) %>%
  rbind(review_ribbon_seal) %>%
  mutate(subset_id = ifelse(subset_id == 0, as.integer(5), subset_id)) %>%
  rbind(review_ribbon_seal) %>%
  mutate(subset_id = ifelse(subset_id == 0, as.integer(6), subset_id)) %>%
  rbind(review_ribbon_seal) %>%
  mutate(subset_id = ifelse(subset_id == 0, as.integer(7), subset_id))

# Create bearded_seal nonpup review subset ---------------
# From Gemini
# Create randomly ordered sub_combos
set.seed(8)
sub_combos <- combn(7, 3, simplify = FALSE) %>%
  sample()

# Create review subset
review_bearded_seal_nonpup <- randoms_rd2 %>%
  filter(detection_type == "bearded_seal" & age_class == "nonpup") %>%
  # Create a unique ID for each record just to track them
  mutate(record_id = row_number()) %>%
  # Cycle through the 35 combinations sequentially (1 to 35, 1 to 35...)
  mutate(combo_idx = (row_number() - 1) %% length(sub_combos) + 1) %>%
  # Map the actual subset lists to the rows
  mutate(subset_id = sub_combos[combo_idx]) %>%
  # Unnest expands each row into 3 separate rows, one for each assigned subset
  unnest(cols = c(subset_id)) %>%
  select(-combo_idx, -record_id)
# End "From Gemini"

# Create bearded_seal pup review subset ---------------
# Create randomly ordered sub_combos
set.seed(9)
sub_combos <- combn(7, 3, simplify = FALSE) %>%
  sample()

# Create review subset
review_bearded_seal_pup <- randoms_rd2 %>%
  filter(detection_type == "bearded_seal" & age_class == "pup") %>%
  mutate(record_id = row_number()) %>%
  mutate(combo_idx = (row_number() - 1) %% length(sub_combos) + 1) %>%
  mutate(subset_id = sub_combos[combo_idx]) %>%
  unnest(cols = c(subset_id)) %>%
  select(-combo_idx, -record_id)

# Create ringed_seal nonpup review subset ---------------
# Create randomly ordered sub_combos
set.seed(10)
sub_combos <- combn(7, 3, simplify = FALSE) %>%
  sample()

# Create review subset
review_ringed_seal_nonpup <- randoms_rd2 %>%
  filter(detection_type == "ringed_seal" & age_class == "nonpup") %>%
  mutate(record_id = row_number()) %>%
  mutate(combo_idx = (row_number() - 1) %% length(sub_combos) + 1) %>%
  mutate(subset_id = sub_combos[combo_idx]) %>%
  unnest(cols = c(subset_id)) %>%
  select(-combo_idx, -record_id)

# Create ringed_seal pup review subset ---------------
# Create randomly ordered sub_combos
set.seed(11)
sub_combos <- combn(7, 3, simplify = FALSE) %>%
  sample()

# Create review subset
review_ringed_seal_pup <- randoms_rd2 %>%
  filter(detection_type == "ringed_seal" & age_class == "pup") %>%
  mutate(record_id = row_number()) %>%
  mutate(combo_idx = (row_number() - 1) %% length(sub_combos) + 1) %>%
  mutate(subset_id = sub_combos[combo_idx]) %>%
  unnest(cols = c(subset_id)) %>%
  select(-combo_idx, -record_id)

# Create spotted_seal nonpup review subset ---------------
# Create randomly ordered sub_combos
set.seed(12)
sub_combos <- combn(7, 3, simplify = FALSE) %>%
  sample()

# Create review subset
review_spotted_seal_nonpup <- randoms_rd2 %>%
  filter(detection_type == "spotted_seal" & age_class == "nonpup") %>%
  mutate(record_id = row_number()) %>%
  mutate(combo_idx = (row_number() - 1) %% length(sub_combos) + 1) %>%
  mutate(subset_id = sub_combos[combo_idx]) %>%
  unnest(cols = c(subset_id)) %>%
  select(-combo_idx, -record_id)

# Create spotted_seal pup review subset ---------------
# Create randomly ordered sub_combos
set.seed(13)
sub_combos <- combn(7, 3, simplify = FALSE) %>%
  sample()

# Create review subset
review_spotted_seal_pup <- randoms_rd2 %>%
  filter(detection_type == "spotted_seal" & age_class == "pup") %>%
  mutate(record_id = row_number()) %>%
  mutate(combo_idx = (row_number() - 1) %% length(sub_combos) + 1) %>%
  mutate(subset_id = sub_combos[combo_idx]) %>%
  unnest(cols = c(subset_id)) %>%
  select(-combo_idx, -record_id)

# Create final review dataset --------------------------
review <- review_ribbon_seal %>%
  rbind(review_bearded_seal_nonpup) %>%
  rbind(review_bearded_seal_pup) %>%
  rbind(review_ringed_seal_nonpup) %>%
  rbind(review_ringed_seal_pup) %>%
  rbind(review_spotted_seal_nonpup) %>%
  rbind(review_spotted_seal_pup) %>%
  mutate(sp_ageclass = paste0(detection_type, '_', age_class)) %>%
  mutate(sp_age_detect = paste0(sp_ageclass, '-', processed_detection_id))
rm(
  review_ribbon_seal,
  review_bearded_seal_nonpup,
  review_bearded_seal_pup,
  review_ringed_seal_nonpup,
  review_ringed_seal_pup,
  review_spotted_seal_nonpup,
  review_spotted_seal_pup,
  sub_combos
)

review_summ <- review %>%
  janitor::tabyl(subset_id, sp_ageclass)

ggplot(review, aes(x = subset_id, y = sp_age_detect, fill = sp_ageclass)) +
  geom_tile()

# Assign reviewers to subset_id randomly -------------------------------------------------------
set.seed(14)
reviewers <- data.frame(
  reviewer = c('clc', 'elr', 'gmb', 'hlz', 'jli', 'smw', 'spd')
) %>%
  mutate(subset_id = sample(1:7, replace = FALSE))

# Create final export datasets for reviewes -------------------------------------------------------
review_by_reviewer <- review %>%
  inner_join(reviewers, by = "subset_id")

for (i in 1:nrow(reviewers)) {
  reviewer_i <- reviewers$reviewer[i]
  # Create reviewer subset (including processed_detection_id for SMK tracking)
  reviewer_subset <- review_by_reviewer %>%
    filter(reviewer == reviewer_i) %>%
    mutate(detection_type = 'to_review') %>%
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
      type_score,
      processed_detection_id
    )

  # Create image list
  reviewer_images <- color_images %>%
    inner_join(reviewer_subset, by = "image_name") %>%
    mutate(image_path = paste0(image_dir, "/", image_name)) %>%
    select(image_path) %>%
    arrange(image_path) %>%
    unique()

  # Export file for SMK tracking
  write.csv(
    reviewer_subset,
    paste0(
      "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals2025_SpeciesMisclass_",
      reviewer_i,
      ".csv"
    ),
    row.names = FALSE,
    quote = FALSE,
  )

  # Remove processed_detection_id for reviewers
  reviewer_subset <- reviewer_subset %>%
    select(-processed_detection_id)

  # Export reviewer detection file and image list
  write.table(
    reviewer_subset,
    paste0(
      "J:\\SpeciesMisclassification\\IceSeals2025\\IceSeals2025_SpeciesMisclass_",
      reviewer_i,
      ".csv"
    ),
    row.names = FALSE,
    quote = FALSE,
    col.names = FALSE,
    sep = ","
  )

  write.table(
    reviewer_images,
    paste0(
      "J:\\SpeciesMisclassification\\IceSeals2025\\IceSeals2025_SpeciesMisclass_",
      reviewer_i,
      "_images.txt"
    ),
    row.names = FALSE,
    quote = FALSE,
    col.names = FALSE
  )
  rm(reviewer_i, reviewer_subset, reviewer_images)
}

write.table(
  review_by_reviewer,
  "C:\\Users\\Stacie.Hardy\\Work\\SMK\\Projects\\AS_IceSeals2025\\SpeciesMisclass\\IceSeals2025_SpeciesMisclass_ALL.csv",
  row.names = FALSE,
  quote = FALSE,
  col.names = FALSE,
  sep = ","
)
