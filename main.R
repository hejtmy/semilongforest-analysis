library(psychotronr)
library(tidyr)
library(ggplot2)
library(here)
source(here("functions/processing.R"))
source(here("functions/loading.R"))

raw <- jsonlite::fromJSON("data/study_export.json")
data <- organize_psychotron_data(raw)
organized <- split_participant_email(data, drop_email = TRUE)
surveys <- organized$surveys


poms_1 <- surveys[["1-poms-sf-pre"]]
poms_1$data
ros_1 <- surveys[["1-ros-pre"]]
ros_1$data
ssq_data <- surveys[["2-ssq"]]$data
physio_data <- load_physio_data()
physio_data <- process_physio(physio_data)

# unnest the poms1_data_poms colum, which is a matrix using regular r without dplyr
df_poms <- process_all_poms(organized$surveys) %>%
  select(session_order, participant, timepoint, 
         anger, vitality, fatigue, depression, confusion, tension) %>%
  pivot_longer(cols = c(anger, vitality, fatigue, depression, confusion, tension), 
               names_to = "scale", values_to = "score")
df_ros <- process_all_ros(organized$surveys) %>%
  select(session_order, participant, timepoint, ros_score)
df_ssq <- process_all_ssq(organized$surveys) %>%
  select(session_order, participant, ssq_score, vrsq_oculomotor, vrsq_disorientation, vrsq_total)

df_ros %>%
  pivot_wider(names_from = timepoint, values_from = ros_score) %>%
  mutate(change = post - pre) %>%
  ggplot(aes(x = session_order, y = change, 
    color = participant, group = participant)) +
  geom_point() +
  geom_line()

# 
df_poms %>%
  pivot_wider(names_from = timepoint, values_from = score) %>%
  mutate(change = post - pre) %>%
  ggplot(aes(x = session_order, y = change, 
             color = participant, group = participant)) +
  geom_point() +
  geom_line() +
  facet_wrap(~scale)
