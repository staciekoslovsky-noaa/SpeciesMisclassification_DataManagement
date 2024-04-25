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

install_pkg("tidyverse")
install_pkg("filesstrings")
install_pkg("RPostgreSQL")

con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              password = Sys.getenv("admin_pw"))

image_list <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT image_name 
                                      FROM species_misclass.tbl_detections_reviewed 
                                      WHERE project_schema = \'surv_boss\' 
                                      ORDER BY image_name") %>%
  mutate(image_name = paste0("//akc0ss-n086/NMML_Polar_Imagery_2/SpeciesMisclassification/BOSS/Images/", image_name)) 

annotations <- RPostgreSQL::dbGetQuery(con, "SELECT DISTINCT image_name, project_detection_id, bound_left, bound_top, bound_right, bound_bottom 
                                       FROM species_misclass.tbl_detections_reviewed 
                                       WHERE project_schema = \'surv_boss\'
                                       ORDER BY image_name") %>%
  mutate(image_name = paste0("//akc0ss-n086/NMML_Polar_Imagery_2/SpeciesMisclassification/BOSS/Images/", image_name)) %>%
  mutate(viame_id = (1:n())-1,
         score = -1,
         length = 1,
         detection_type = 'to_review',
         type_score = 1,
         hotspot_id = paste0('(trk-atr) hotspot_id ', project_detection_id)) %>%
  group_by(image_name) %>%
  mutate(frame_id = cur_group_id()-1) %>%
  ungroup %>%
  select(viame_id, image_name, frame_id, bound_left, bound_top, bound_right, bound_bottom, score, length, detection_type, type_score, hotspot_id)
    
write.table(image_list, "//akc0ss-n086/NMML_Polar_Imagery_2/SpeciesMisclassification/BOSS/TemplateForReview/images.txt", row.names = FALSE, col.names = FALSE, quote = FALSE)
write.table(annotations, "//akc0ss-n086/NMML_Polar_Imagery_2/SpeciesMisclassification/BOSS/TemplateForReview/annotations__TEMPLATE.csv", sep = ',', row.names = FALSE, col.names = FALSE, quote = FALSE)

dbDisconnect(con)