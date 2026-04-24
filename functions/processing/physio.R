process_physio <- function(physio_data) {
  physio_data <- physio_data %>%
    separate(blood_pre,  into = c("blood_pre_sys",  "blood_pre_dias"),  sep = "/",
             convert = TRUE) %>%
    separate(blood_post, into = c("blood_post_sys", "blood_post_dias"), sep = "/",
             convert = TRUE) %>%
    mutate(
      heartrate_pre  = as.numeric(heartrate_pre),
      heartrate_post = as.numeric(heartrate_post),
      hr_delta       = heartrate_post - heartrate_pre,
      sys_delta      = blood_post_sys  - blood_pre_sys,
      dias_delta     = blood_post_dias - blood_pre_dias
    )
  return(physio_data)
}
