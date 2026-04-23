ssq_scores <- list(
  ssq_nausea = c(1, 6, 7, 8, 9, 15, 16),
  ssq_oculomotor = c(1, 2, 3, 4, 5, 9, 11),
  ssq_desorientation = c(5, 8, 10, 11, 12, 13, 14),
  vrsq_oculomotor = c(1, 2, 4, 5),
  vrsq_disorientation = c(3, 10, 11, 12, 13, 14)
)

process_ssq <- function(ssq_data) {
  ssq_data <- ssq_data %>%
    tidyr::unnest(cols = c(ssq_matrix_1, ssq_matrix_2, ssq_matrix_3))
  ssq_results <- ssq_data %>%
    # from the questions 1-16 remove the word item from the value
    rowwise() %>%
    mutate(ssq_sum = sum(across(starts_with("ssq"), as.numeric)),
           ssq_nausea = sum(c_across(starts_with("ssq")[ssq_scores$ssq_nausea])),
           ssq_nausea_score = ssq_nausea * 9.54,
           ssq_oculomotor = sum(c_across(starts_with("ssq")[ssq_scores$ssq_oculomotor])),
           ssq_oculomotor_score = ssq_oculomotor * 7.58,
           ssq_desorientation = sum(c_across(starts_with("ssq")[ssq_scores$ssq_desorientation])),
           ssq_desorientation_score = ssq_desorientation * 13.92,
           ssq_score = (ssq_desorientation + ssq_oculomotor + ssq_nausea) * 3.74,
           vrsq_oculomotor = sum(c_across(starts_with("ssq")[ssq_scores$vrsq_oculomotor])) * 100 / 12,
           vrsq_disorientation = sum(c_across(starts_with("ssq")[ssq_scores$vrsq_disorientation])) * 100 / 15,
           vrsq_total = (vrsq_oculomotor + vrsq_disorientation) / 2) %>%
    ungroup()
  return(ssq_results)
}

process_all_ssq <- function(surveys) {
  ssq_surveys <- surveys[grepl("ssq", names(surveys))]
  df_ssq <- data.frame()
  for (survey_name in names(ssq_surveys)) {
    ssq_data <- ssq_surveys[[survey_name]]$data
    ssq_results <- process_ssq(ssq_data)
    ssq_results$survey_name <- survey_name
    ssq_results$session_order <- extract_session_timepoint(survey_name, "ssq")$session_order
    df_ssq <- bind_rows(df_ssq, ssq_results)
  }
  return(df_ssq)
}
