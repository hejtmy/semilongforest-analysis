library(dplyr)
library(tidyr)
source("functions/processing/helpers.R")

poms_scales_czech <- list(
    anger = c(19, 2, 25, 21, 11, 31),
    vitality = c(32, 9, 5, 24, 35, 13),
    fatigue = c(29, 18, 37, 26, 3, 17),
    depression = c(4, 12, 33, 23, 14, 20, 15),
    confusion = c(36, 34, 10, 6),
    tension = c(22, 16, 1))

process_poms <- function(poms_data) {
    poms_data <- poms_data %>%
        tidyr::unnest(cols = c(pomsf_matrix_1, pomsf_matrix_2, pomsf_matrix_3, pomsf_matrix_4, 
                               pomsf_matrix_5, pomsf_matrix_6))
    # calculate the scale scores for each participant, using the scales named poms_number
    for (scale in names(poms_scales_czech)) {
        poms_data[[scale]] <- rowSums(poms_data[paste0("pomsf_", poms_scales_czech[[scale]])])
    }
    return(poms_data)
}

process_all_poms <- function(surveys) {
  poms_names <- names(surveys)[grepl("poms", names(surveys))]
  poms_names
  df_poms <- data.frame()
  for (name in poms_names) {
    survey <- surveys[[name]]
    out <- process_poms(survey$data)
    out$survey <- name
    # the name is in forms like "1-poms-sf-pre", we want to extract the number and the timepoint
    out$session_order <- extract_session_timepoint(name, "poms-sf")$session_order
    out$timepoint <- extract_session_timepoint(name, "poms-sf")$timepoint
    df_poms <- rbind(df_poms, out)
  }
  return(df_poms)
}
