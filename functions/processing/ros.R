source("functions/processing/helpers.R")

process_ros <- function(ros_data) {
    ros_data <- ros_data %>%
      rowwise() %>%
      mutate(ros_score = sum(c_across(starts_with("ros_scale")))) %>%
      ungroup()
    return(ros_data)
}

process_all_ros <- function(surveys) {
  ros_names <- names(surveys)[grepl("ros", names(surveys))]
  df_ros <- data.frame()
  for (name in ros_names) {
    survey <- surveys[[name]]
    out <- process_ros(survey$data)
    out$survey <- name  
    out$session_order <- extract_session_timepoint(name, "ros")$session_order
    out$timepoint <- extract_session_timepoint(name, "ros")$timepoint
    df_ros <- rbind(df_ros, out)
  }
  return(df_ros)
}
