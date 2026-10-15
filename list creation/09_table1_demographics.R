# =============================================================================
# 09_table1_demographics.R
# Table 1: one row per patient, taken from the booking of the principal
# procedure.
#
# Inputs : study_data.rds, predictions.csv, achi_codes.csv, list_functions.R
# Output : table1_demographics.xlsx
# =============================================================================

library(tidyverse)
library(rstatix)
library(openxlsx)
source("list_functions.R")

study_db <- load_matched_study_db()

# Priority and specialty can differ between procedures, so use the principal
# procedure's booking for each patient
patients <- study_db %>%
  filter(proc == "P" | proc_no_reduced == "primary") %>%
  distinct(study_id, .keep_all = TRUE)

n_dropped <- n_distinct(study_db$study_id) - nrow(patients)
if (n_dropped > 0) warning(n_dropped, " patient(s) have no principal-procedure row and are not in Table 1")

freq_rows <- function(var, label) {
  freq_table(patients, vars = var) %>%
    rename(level = 1) %>%
    mutate(level = as.character(level), n = as.character(n), prop = as.character(prop),
           variable = label, .before = 1)
}

table1 <- bind_rows(
  freq_rows("age_gp",                  "Age group"),
  freq_rows("sex",                     "Sex"),
  freq_rows("asa",                     "ASA"),
  freq_rows("emerg_priority_collapse", "Booking priority"),
  freq_rows("hosp",                    "Hospital"),
  freq_rows("specialty_grouped",       "Booking specialty"))

q <- quantile(patients$age, c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
age_summary <- tibble(variable = "Age (years)", level = "median (IQR; range)",
                      n = sprintf("%.0f (%.0f-%.0f; %.0f-%.0f)", q[3], q[2], q[4], q[1], q[5]),
                      prop = NA_character_)

table1 <- bind_rows(age_summary, table1)
print(table1, n = Inf)
write.xlsx(table1, "table1_demographics.xlsx", rowNames = FALSE)
