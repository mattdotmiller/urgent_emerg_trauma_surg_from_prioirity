# =============================================================================
# 01_build_study_dataset.R
# Builds the study dataset: one row per emergency booking x coded ACHI
# procedure from the same admission, for patients admitted with trauma.
#
# Every booking is paired with EVERY coded procedure in its admission. The
# matching pipeline (scripts 02-07) then decides which pairs are true matches.
#
# Inputs (working directory; see README):
#   bookings.csv    one row per emergency theatre booking
#   diagnoses.csv   one row per coded ICD-10-AM diagnosis
#   procedures.csv  one row per coded ACHI procedure
#   achi_codes.csv, procedure_exclusions.csv
#
# Output: study_data.rds
# =============================================================================

library(tidyverse)

read_input <- function(f) read_csv(f, show_col_types = FALSE, guess_max = 1e5)

bookings   <- read_input("bookings.csv")   %>% distinct() %>% mutate(admit_dt_tm = ymd_hms(admit_dt_tm))
diagnoses  <- read_input("diagnoses.csv")
procedures <- read_input("procedures.csv") %>% mutate(admit_dt_tm = ymd_hms(admit_dt_tm),
                                                      proc = as.character(proc))

achi_lookup <- read_csv("achi_codes.csv", show_col_types = FALSE) %>%
  select(achi_no = achi_proc_code, achi_txt_ref = procedure_text)

codes_to_remove <- read_csv("procedure_exclusions.csv", show_col_types = FALSE) %>%
  filter(exclude == TRUE) %>%
  pull(achi_proc_code)

# =============================================================================
# 1. TRAUMA COHORT
# A patient is included if any diagnosis is an ICD-10-AM injury (S, T) or
# external-cause (U, V, W, X) code, excluding the three-character codes below
# (complications of care, adverse effects of drugs, sequelae etc.).
# =============================================================================
ts <- c(36:50, 51:65, 74, 78, 80:88, 90:98)
ys <- c(06, 07, 10:19, 36, 37, 40:98)
us <- c(04, 06, 07, 78:92)
xs <- c(40:49)
icds_to_remove <- c(str_c("t", ts), str_c("y", ys), str_c("u", us), str_c("x", xs))

trauma_ids <- diagnoses %>%
  filter(!is.na(icd)) %>%
  mutate(icd = tolower(icd)) %>%
  filter(!str_extract(icd, "^.\\d\\d") %in% icds_to_remove,
         str_detect(icd, "^[stuvwx]")) %>%
  distinct(study_id) %>%
  pull()

# =============================================================================
# 2. LINK BOOKINGS TO CODED PROCEDURES (same patient, same admission)
# =============================================================================
procedures <- procedures %>%
  mutate(achi_no = str_replace(achi_no, "^(\\d{5})(\\d{2})$", "\\1-\\2")) %>%  # 1234500 -> 12345-00
  left_join(achi_lookup, by = "achi_no") %>%
  mutate(achi_txt = coalesce(achi_txt, achi_txt_ref)) %>%
  select(study_id, admit_dt_tm, achi_no, achi_txt, proc)

linked <- bookings %>%
  filter(study_id %in% trauma_ids) %>%
  left_join(procedures, by = c("study_id", "admit_dt_tm"), relationship = "many-to-many")

# Linkage summary (bookings with no coded admission are dropped below)
linkage <- linked %>%
  group_by(hosp) %>%
  summarise(bookings_not_linked = n_distinct(paste(study_id, admit_dt_tm)[is.na(achi_no)]),
            linked_episodes     = n_distinct(paste(study_id, admit_dt_tm)[!is.na(achi_no)]),
            linked_procedures   = sum(!is.na(achi_no)))
print(bind_rows(linkage, summarise(linkage, hosp = "Total", across(where(is.numeric), sum))))

# =============================================================================
# 3. STUDY DATASET
# =============================================================================
proc_no_levels <- c("primary", paste0("additional_procedure_", 1:8), "additional_procedure_9+")

study_data <- linked %>%
  filter(!is.na(achi_no), !achi_no %in% codes_to_remove) %>%
  distinct(study_id, planned_procedure, achi_no, admit_dt_tm, .keep_all = TRUE) %>%
  mutate(age_gp  = case_when(age < 16 ~ "paeds",
                             age < 65 ~ "adult",
                             age >= 65 ~ "elderly"),
         case_yr = year(case_date),
         age     = pmin(age, 100)) %>%            # top-code age for disclosure control
  select(-case_date) %>%
  # Procedure sequence: "P" = principal, otherwise 1, 2, 3 ...
  # Where an admission has no "P", procedure 1 is the principal.
  group_by(study_id) %>%
  mutate(proc  = sub("^[Pp]roc[_ ]?", "", proc),
         proc  = ifelse(grepl("^[0-9]+$", proc), as.character(as.integer(proc)), proc),
         has_p = any(proc == "P", na.rm = TRUE),
         proc  = ifelse(!has_p & proc == "1", "P", proc)) %>%
  ungroup() %>%
  select(-has_p) %>%
  mutate(proc     = replace_na(proc, "P"),
         proc_num = suppressWarnings(as.integer(proc)),
         proc_no_reduced = factor(case_when(
           proc %in% c("P", "primary")      ~ "primary",
           !is.na(proc_num) & proc_num <= 8 ~ str_c("additional_procedure_", proc_num),
           !is.na(proc_num) & proc_num >= 9 ~ "additional_procedure_9+"),
           levels = proc_no_levels)) %>%
  select(-proc_num) %>%
  mutate(study_row_uid = sprintf("sd_%08d", row_number())) %>%   # stable key used by later scripts
  relocate(study_row_uid)

stopifnot(!anyDuplicated(study_data$study_row_uid))
saveRDS(study_data, "study_data.rds")
message("Saved ", nrow(study_data), " rows (", n_distinct(study_data$study_id),
        " patients) to study_data.rds")
