suppressPackageStartupMessages({
  library(psychotronr)
  library(tidyr)
  library(dplyr)
  library(ggplot2)
  library(knitr)
  library(lmerTest)
  library(broom.mixed)
  library(ggeffects)
  library(googlesheets4)
})

# All paths are resolved from the project root (see knitr::opts_knit$root.dir in _quarto.yml)
source("functions/processing.R")

raw <- jsonlite::fromJSON("data/study_export.json")
.data <- organize_psychotron_data(raw)
organized <- split_participant_email(.data, drop_email = TRUE)
surveys <- organized$surveys
participants <- organized$participants

df_ros <- process_all_ros(surveys) %>%
  select(session_order, participant, timepoint, ros_score) %>%
  mutate(session_order = as.integer(session_order))

df_poms_wide <- process_all_poms(surveys) %>%
  select(session_order, participant, timepoint,
         anger, vitality, fatigue, depression, confusion, tension) %>%
  mutate(session_order = as.integer(session_order),
         tmd = anger + fatigue + depression + confusion + tension - vitality)

df_poms <- df_poms_wide %>%
  pivot_longer(cols = c(anger, vitality, fatigue, depression, confusion, tension, tmd),
               names_to = "scale", values_to = "score")

df_ssq <- process_all_ssq(surveys) %>%
  select(session_order, participant,
         vrsq_oculomotor, vrsq_disorientation, vrsq_total) %>%
  mutate(session_order = as.integer(session_order)) %>%
  # a few participants have duplicate SSQ rows in a session — average them
  group_by(participant, session_order) %>%
  summarise(vrsq_oculomotor = mean(vrsq_oculomotor, na.rm = TRUE),
            vrsq_disorientation = mean(vrsq_disorientation, na.rm = TRUE),
            vrsq_total = mean(vrsq_total, na.rm = TRUE),
            .groups = "drop")

# Within-session benefit (Δ = post − pre) per outcome, per session, per participant
df_ros_delta <- df_ros %>%
  pivot_wider(names_from = timepoint, values_from = ros_score) %>%
  filter(!is.na(pre), !is.na(post)) %>%
  mutate(delta = post - pre, outcome = "ROS") %>%
  select(participant, session_order, outcome, pre, post, delta)

df_poms_delta <- df_poms %>%
  pivot_wider(names_from = timepoint, values_from = score) %>%
  filter(!is.na(pre), !is.na(post)) %>%
  mutate(delta = post - pre) %>%
  rename(outcome = scale) %>%
  select(participant, session_order, outcome, pre, post, delta)

df_delta <- bind_rows(df_ros_delta, df_poms_delta)

# Join VRSQ (session-level, post only) + person-mean-center it
df_analysis <- df_delta %>%
  left_join(df_ssq, by = c("participant", "session_order")) %>%
  group_by(participant) %>%
  mutate(
    vrsq_total_pm = mean(vrsq_total, na.rm = TRUE),
    vrsq_total_wc = vrsq_total - vrsq_total_pm,
    vrsq_oculo_pm = mean(vrsq_oculomotor, na.rm = TRUE),
    vrsq_oculo_wc = vrsq_oculomotor - vrsq_oculo_pm,
    vrsq_disor_pm = mean(vrsq_disorientation, na.rm = TRUE),
    vrsq_disor_wc = vrsq_disorientation - vrsq_disor_pm
  ) %>%
  ungroup()

# Direction: for every outcome, "improvement" = delta * sign_improve.
# ROS: higher is better (→ +1). POMS negative scales: lower is better (→ −1).
# Vitality: higher is better (→ +1). TMD: lower is better (→ −1).
improvement_sign <- c(
  ROS = 1, vitality = 1,
  anger = -1, fatigue = -1, depression = -1,
  confusion = -1, tension = -1, tmd = -1
)

outcome_levels <- c("ROS", "vitality", "tmd",
                    "anger", "tension", "fatigue", "depression", "confusion")
outcome_labels <- c(ROS = "ROS (restoration)", vitality = "POMS: Vitality",
                    tmd = "POMS: TMD",
                    anger = "POMS: Anger", tension = "POMS: Tension",
                    fatigue = "POMS: Fatigue", depression = "POMS: Depression",
                    confusion = "POMS: Confusion")

df_analysis <- df_analysis %>%
  mutate(outcome = factor(outcome, levels = outcome_levels),
         sign_improve = improvement_sign[as.character(outcome)],
         improvement = delta * sign_improve,
         # session_c = 0 at session 1, so the model intercept = estimated Δ at the first session
         session_c = session_order - 1)

# Physiological data
source("functions/loading.R")
df_physio <- load_physio_data() %>%
  process_physio() %>%
  rename(session_order = session) %>%
  mutate(session_order = as.integer(session_order),
         session_c = session_order - 1)

theme_set(theme_minimal(base_size = 12))
