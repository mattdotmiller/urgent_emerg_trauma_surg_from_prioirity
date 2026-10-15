# =============================================================================
# 02_matching_preprocess.R
# Prepares the hand-labelled booking/ACHI pairs used to train the matcher.
#
# Inputs : matching_labels.csv, study_data.rds, achi_codes.csv,
#          procedure_exclusions.csv, matching_functions.R
# Output : labeled_proc.rds
# =============================================================================

library(tidyverse)
source("matching_functions.R")

achi_db <- read_csv("achi_codes.csv", show_col_types = FALSE)
codes_to_remove <- read_csv("procedure_exclusions.csv", show_col_types = FALSE) %>%
  filter(exclude == TRUE) %>%
  pull(achi_proc_code)

# --- Labels + procedure position from the study dataset ----------------------
labels <- read_csv("matching_labels.csv", show_col_types = FALSE)
stopifnot("row_uid" %in% names(labels))
if (anyDuplicated(labels$row_uid)) warning("Duplicate row_uid in matching_labels.csv")

proc_seq <- readRDS("study_data.rds") %>%
  select(study_id, achi_proc_code = achi_no, proc, planned_procedure)

labeled_proc <- labels %>%
  select(row_uid, study_id, planned_procedure, achi_txt, achi_proc_code = achi_no, correct) %>%
  left_join(proc_seq, by = c("study_id", "achi_proc_code", "planned_procedure")) %>%
  mutate(proc  = sub("^[Pp]roc[_ ]?", "", proc),
         proc  = ifelse(grepl("^[0-9]+$", proc), as.character(as.integer(proc)), proc),
         has_p = any(proc == "P", na.rm = TRUE),
         proc  = ifelse(!has_p & proc == "1", "P", proc),
         proc  = replace_na(proc, "P")) %>%
  select(-has_p) %>%
  filter(!achi_proc_code %in% codes_to_remove) %>%
  left_join(achi_db, by = "achi_proc_code")

# --- Procedure position ------------------------------------------------------
proc_no_levels <- c("primary", paste0("additional_procedure_", 1:8), "additional_procedure_9+")

labeled_proc <- labeled_proc %>%
  rename(proc_no = proc) %>%
  mutate(
    proc_no_num_old = as.integer(str_extract(proc_no, "(?<=additional_procedure_)\\d+")),
    proc_no_num     = ifelse(is.na(proc_no_num_old), as.integer(proc_no), proc_no_num_old),
    proc_no_reduced = factor(case_when(
      proc_no %in% c("primary", "P") ~ "primary",
      proc_no_num <= 8 ~ str_c("additional_procedure_", proc_no_num),
      proc_no_num >= 9 ~ "additional_procedure_9+"), levels = proc_no_levels),
    primary_y_n = factor(if_else(proc_no_reduced == "primary", "yes", "no"))) %>%   # as in script 07
  select(-proc_no_num)

# --- Text: tags, cleaning, rule rewrites, duplicate words --------------------
labeled_proc <- labeled_proc %>%
  preprocess_pairs(achi_db) %>%
  filter(!is.na(achi_txt))

print(table(labeled_proc$oc_relation,   useNA = "ifany"))
print(table(labeled_proc$site_relation, useNA = "ifany"))
print(table(labeled_proc$matched_by_rule))

# --- Label -------------------------------------------------------------------
labeled_proc <- labeled_proc %>%
  mutate(correct = case_when(correct %in% c("y", "yes") ~ "yes",
                             correct %in% c("n", "no")  ~ "no"))
if (anyNA(labeled_proc$correct))
  warning(sum(is.na(labeled_proc$correct)), " rows have an unrecognised label")
labeled_proc <- mutate(labeled_proc, correct = factor(correct, levels = c("no", "yes")))

print(table(labeled_proc$correct, useNA = "ifany"))
saveRDS(labeled_proc, "labeled_proc.rds")
