# =============================================================================
# 08_urgent_lists.R
# Urgent procedure lists for one or more booking cut-offs. For each cut-off:
# full list, age-group and sex sub-lists, a list based on observed
# booking-to-surgery time, specialty mix and threshold sensitivity.
# No per-hospital lists are produced.
#
# Inputs : study_data.rds, predictions.csv, achi_codes.csv, list_functions.R
# Outputs (per cut-off h): urgent_surgery_procedure_lists_<h>hr.xlsx,
#          urgent_surgery_other_results_<h>hr.xlsx, list_objects_<h>hr.rds
# =============================================================================

library(tidyverse)
library(janitor)
library(openxlsx)
source("list_functions.R")

cutoffs             <- c(4, 8, 24)   # hours; any of 1, 4, 8, 24
agreement_threshold <- 75            # % of bookings within the cut-off
consensus_threshold <- 0.7

# --- Matching summary --------------------------------------------------------
# How many primary bookings matched their principal coded procedure, and how
# many unmatched ones were matched to an additional coded procedure instead
match_check <- readRDS("study_data.rds") %>%
  left_join(read_csv("predictions.csv", show_col_types = FALSE) %>%
              select(study_row_uid, predicted_correct), by = "study_row_uid") %>%
  mutate(is_primary = proc_no_reduced == "primary")
print(freq_table(filter(match_check,  is_primary), predicted_correct))
print(freq_table(filter(match_check, !is_primary), predicted_correct))

unmatched_primary <- match_check %>%
  filter(is_primary, predicted_correct != "yes", !is.na(planned_procedure)) %>%
  distinct(study_id, planned_procedure)
recovered <- match_check %>%
  filter(!is_primary, predicted_correct == "yes") %>%
  distinct(study_id, planned_procedure) %>%
  semi_join(unmatched_primary, by = c("study_id", "planned_procedure")) %>%
  nrow()
message(sprintf("Unmatched primary bookings: %d | matched via an additional code: %d (%.1f%%)",
                nrow(unmatched_primary), recovered, 100 * recovered / nrow(unmatched_primary)))

# --- Analysis dataset --------------------------------------------------------
study_db <- load_matched_study_db()

message("Bookings: ", nrow(study_db), " | patients: ", n_distinct(study_db$study_id),
        " | procedures: ", n_distinct(study_db$booked_procedure),
        " | surgeons: ", n_distinct(study_db$procedural_consultant))
print(freq_table(study_db, admit_to_book_pd))
print(freq_table(study_db, case_yr))

# --- Lists, one set per cut-off ----------------------------------------------
lists <- map(cutoffs, ~ build_urgent_lists(study_db, urgency = .x,
                                           agreement_threshold = agreement_threshold,
                                           consensus_threshold = consensus_threshold))
names(lists) <- str_c(cutoffs, "hr")

# A single cut-off can also be run on its own, e.g.
# lists_1hr <- build_urgent_lists(study_db, urgency = 1)
