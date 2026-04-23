# THe survey is in form sessionnumber-nameofsurvey-timepoint, e.g. "1-poms-sf-pre", "2-ros-post", etc. This function extracts the session number and timepoint from the survey name.
library(stringr)

extract_session_timepoint <- function(survey_name, nameofsurvey) {
  session_order <- str_extract(survey_name, "^[0-9]+")
  timepoint <- str_extract(survey_name, paste0("(?<=", nameofsurvey, "-)(pre|post)$"))
  return(list(session_order = session_order, 
              timepoint = timepoint))
}
