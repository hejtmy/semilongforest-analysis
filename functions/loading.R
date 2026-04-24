library(googlesheets4)

load_physio_data <- function(overwrite = FALSE) {
  file_path <- "temp/physio_data.csv"
  if (!file.exists(file_path) || overwrite) {
    dir.create("temp/", showWarnings = FALSE)
    SHEET_ID <- "1cdE8aARrkwTAWVcQjXupD7j7hfmU0VWjNbGqAW7dnXA"
    physio_data <- read_sheet(SHEET_ID, sheet = "physio")
    write.csv(physio_data, file_path, row.names = FALSE)
  } 
  physio_data <- read.csv(file_path)
  return(physio_data)
}