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

install_pkg("filesstrings")
install_pkg("RPostgreSQL")

con <- RPostgreSQL::dbConnect(PostgreSQL(), 
                              dbname = Sys.getenv("pep_db"), 
                              host = Sys.getenv("pep_ip"), 
                              user = Sys.getenv("pep_admin"), 
                              rstudioapi::askForPassword(paste("Enter your DB password for user account: ", Sys.getenv("pep_admin"), sep = "")))

# BOSS misclassification images
files <- RPostgreSQL::dbGetQuery(con, "SELECT image_dir || \'\\' || image_name as file_path
                                        FROM surv_boss.tbl_images
                                        WHERE image_name IN (SELECT DISTINCT image_name FROM species_misclass.tbl_detections_reviewed WHERE project_schema = 'surv_boss')
                                        ORDER BY image_name
                                        ")

file.copy(files$file_path[i], "//akc0ss-n086/NMML_Polar_Imagery_2/SpeciesMisclassification/BOSS/Images")

dbDisconnect(con)