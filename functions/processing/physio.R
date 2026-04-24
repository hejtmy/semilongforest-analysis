process_physio <- function(physio_data) {
  physio_data <- physio_data %>%
    # split blood_pre, blood_post, whici is in dias/sys format into separate columns
    separate(blood_pre, into = c("blood_pre_sys", "blood_pre_dias"), sep = "/") %>%
    separate(blood_post, into = c("blood_post_sys", "blood_post_dias"), sep = "/")
  return(physio_data)
}
