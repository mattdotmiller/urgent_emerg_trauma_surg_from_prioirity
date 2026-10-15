# =============================================================================
# list_functions.R
# Helper functions for the analysis scripts (08-11), loaded with
# source("list_functions.R"): the matched analysis dataset, urgent procedure
# lists, and the compliance tables. Exported tables are not masked.
# =============================================================================

library(tidyverse)
library(hablar)
library(rstatix)
library(agrmt)

urgency_levels_collapsed <- c("<1hr", "<4hrs", "<8hrs", "<24hrs", "<72hrs")

# =============================================================================
# 1. ANALYSIS DATASET
# Joins matcher predictions to the study dataset and keeps only pairs predicted
# to be true matches, so each row is one booking linked to the ACHI code that
# was actually performed. Harmonises sex, ASA and specialty.
# keep_asa_6: TRUE keeps ASA 6 as its own level; FALSE folds it into
#             "Not documented".
# =============================================================================
load_matched_study_db <- function(keep_asa_6 = FALSE) {
  study_data  <- readRDS("study_data.rds")
  predictions <- read_csv("predictions.csv", show_col_types = FALSE)
  stopifnot("study_row_uid" %in% names(study_data),
            all(c("study_row_uid", "predicted_correct") %in% names(predictions)))

  asa_6 <- if (keep_asa_6) c("6", "6 - Organ Donation", "6E") else character(0)

  db <- study_data %>%
    left_join(select(predictions, study_row_uid, predicted_correct, probability),
              by = "study_row_uid") %>%
    filter(predicted_correct == "yes") %>%
    mutate(
      sex = case_when(sex %in% c("F", "Female") ~ "Female",
                      sex %in% c("M", "Male")   ~ "Male",
                      sex %in% c("I", "Other")  ~ "I"),
      asa = case_when(
        asa %in% c("1 -", "1", "1 - Healthy", "1E") ~ "1",
        asa %in% c("2 -", "2", "2 - Mild Systemic Disease", "2E") ~ "2",
        asa %in% c("3 -", "3", "3 - Moderate to Severe Systemic Disease", "3E") ~ "3",
        asa %in% c("4 -", "4", "4 -5 - Moribund", "4E") ~ "4",
        asa %in% c("5 -", "5", "5 - Moribund", "5E") ~ "5",
        asa %in% asa_6 ~ "6",
        TRUE ~ "Not documented"),
      specialty = str_replace(specialty, " SN", ""),
      specialty_grouped = case_when(
        specialty %in% c("Anaesthetic", "Anaesthetics") ~ "Anaesthetics",
        specialty %in% c("Cardio-Thoracic Surgery", "Cardiothoracic") ~ "Cardiothoracic",
        specialty %in% c("Dental", "Dental IV Sedation") ~ "Dental",
        specialty %in% c("Ear Nose & Throat", "Ear, Nose & Throat", "ENT") ~ "ENT",
        specialty %in% c("Endocrine", "Endocrinology") ~ "Endocrine",
        specialty %in% c("Eye - General", "Eye - Plastic", "Ophthalmology") ~ "Opthal",
        specialty %in% c("General", "General Surgery", "Colorectal", "Upper GI", "Breast/Endo",
                         "Breast", "Liver Surgery", "Paediatric", "Lower GIT") ~ "General Surgery",
        specialty %in% c("Genetics", "Other", "Transplant Surgery", "Respiratory Medicine",
                         "Respiratory Med", "Respiratory", "Renal/Nephrology", "Psychiatry",
                         "Radiation Oncology Treatment") ~ "Other",
        specialty %in% c("Gynaecological Oncology", "Obstetrics", "Obs/Gynae", "Gynaecology") ~ "Obgyn",
        specialty %in% c("Head & Neck", "Head and Neck") ~ "Head and neck",
        specialty %in% c("Oral and Maxillo-Facial Surgery", "Maxillo-Facial") ~ "Maxillo-Facial",
        specialty %in% c("Orthopaedic", "Orthopaedics") ~ "Orthopedics",
        specialty %in% c("Paed Orthopaedic", "Paediatric Dentistry", "Paediatric Surgery") ~ "Paeds Surgery",
        specialty %in% c("Plastic", "Plastics", "Plastics/Burns") ~ "Plastics",
        specialty %in% c("Radiology", "Interventional Radiology") ~ "IR",
        specialty %in% c("Spinal Injuries", "Neurosurgery") ~ "Neurosurgery",
        specialty %in% c("Acute Surgical Unit", "ASU") ~ "Acute Surgical Unit",
        TRUE ~ specialty),
      achi_text = achi_txt) %>%
    rename(booked_procedure = achi_no) %>%
    left_join(read_csv("achi_codes.csv", show_col_types = FALSE) %>% distinct(achi_proc_code, block_text),
              by = c("booked_procedure" = "achi_proc_code")) %>%
    mutate(
      emerg_priority_collapse = factor(emerg_priority_collapse, levels = urgency_levels_collapsed),
      age_gp = factor(age_gp, levels = c("paeds", "adult", "elderly")),
      hosp   = factor(hosp),
      # observed booking-to-surgery interval, banded like the booking priority
      booking_to_surg_collapse = factor(case_when(
        book_to_surgery_pd <= 60      ~ "<1hr",
        book_to_surgery_pd <= 4 * 60  ~ "<4hrs",
        book_to_surgery_pd <= 8 * 60  ~ "<8hrs",
        book_to_surgery_pd <= 24 * 60 ~ "<24hrs",
        book_to_surgery_pd <= 72 * 60 ~ "<72hrs"), levels = urgency_levels_collapsed))

  message("Pairs in study dataset: ", nrow(study_data),
          " | predicted matches kept: ", nrow(db))
  db
}

# Booking specialty of the procedures on an urgent list
urgent_specialty_table <- function(study_db, urgent_codes) {
  study_db %>%
    distinct(booked_procedure, specialty_grouped) %>%
    filter(booked_procedure %in% urgent_codes) %>%
    freq_table(specialty_grouped) %>%
    arrange(desc(prop))
}

# =============================================================================
# 2. URGENT PROCEDURE LISTS
# For each ACHI procedure booked on the day of admission, the proportion of
# bookings in each priority band. A procedure is urgent if the share booked
# within the cut-off (`urgency`, in hours: 1, 4, 8 or 24) is >=
# agreement_threshold (%), or, failing that, if the share is > 50% and the
# Consensus score (Tastle & Wierman) is below consensus_threshold. For
# agreement/consensus, all bands within the cut-off are combined into one.
# =============================================================================
cutoff_bands <- function(urgency) {
  i <- match(urgency, c(1, 4, 8, 24))
  if (is.na(i)) stop("urgency must be one of 1, 4, 8 or 24 (hours)")
  urgency_levels_collapsed[seq_len(i)]
}

get_list_of_procedures <- function(study_db, agreement_threshold = 75, consensus_threshold = 0.7,
                                   urgency = 4, age_gp_filter = NULL, sex_filter = NULL,
                                   hosp_filter = NULL, show_figures_tables = FALSE,
                                   cohort_label = "all") {
  db <- study_db
  if (!is.null(age_gp_filter)) db <- filter(db, age_gp %in% age_gp_filter)
  if (!is.null(sex_filter))    db <- filter(db, sex    %in% sex_filter)
  if (!is.null(hosp_filter))   db <- filter(db, hosp   %in% hosp_filter)
  db <- db %>%
    filter(admit_to_book_pd == "same day") %>%
    convert(fct(booked_procedure, emerg_priority_collapse))

  if (nrow(db) == 0) {
    warning("No rows remain after filtering for cohort: ", cohort_label)
    return(NULL)
  }
  if (show_figures_tables) {
    message(cohort_label, ": ", nrow(db), " bookings | ", n_distinct(db$booked_procedure),
            " procedures | ", n_distinct(db$procedural_consultant), " surgeons | ",
            n_distinct(db$study_id), " patients")
  }

  db_raters <- db %>%
    group_by(booked_procedure) %>%
    summarise(num_of_raters = n(),
              num_of_unique_raters = n_distinct(procedural_consultant), .groups = "drop")

  db_all_rated <- db %>% freq_table(booked_procedure, emerg_priority_collapse)
  db_proc_n    <- db_all_rated %>% group_by(booked_procedure) %>%
    summarise(proc_n = sum(n), .groups = "drop")

  # Agreement and consensus: every band within the cut-off is merged into one
  # (e.g. 4 hrs: <1hr + <4hrs; 8 hrs: <1hr + <4hrs + <8hrs), bands beyond it kept
  within    <- cutoff_bands(urgency)
  agmt_cols <- c(last(within), setdiff(urgency_levels_collapsed, within))
  # Counts per band, in time order, are passed to agreement() and consensus()
  db_agreement <- db %>%
    filter(!is.na(emerg_priority_collapse)) %>%
    mutate(band = if_else(emerg_priority_collapse %in% within, last(within),
                          as.character(emerg_priority_collapse)),
           band = factor(band, levels = agmt_cols)) %>%
    count(booked_procedure, band, .drop = FALSE) %>%
    arrange(booked_procedure, band) %>%
    group_by(booked_procedure) %>%
    summarise(Agreement = agreement(n), Consensus = consensus(n), .groups = "drop")

  # Percentage of bookings in each of the 5 bands
  db_prop <- db_all_rated %>%
    arrange(desc(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = prop)
  for (col in setdiff(urgency_levels_collapsed, names(db_prop))) db_prop[[col]] <- 0

  db_prop <- db_prop %>%
    replace(is.na(.), 0) %>%
    convert(num(`<1hr`, `<4hrs`, `<8hrs`, `<24hrs`, `<72hrs`)) %>%
    left_join(db_agreement, by = "booked_procedure") %>%
    left_join(db_proc_n,    by = "booked_procedure") %>%
    relocate(booked_procedure, `<1hr`, `<4hrs`, `<8hrs`, `<24hrs`, `<72hrs`, Agreement, Consensus) %>%
    mutate(urgent_total_pcnt = rowSums(pick(all_of(cutoff_bands(urgency)))),
           urgent_surgery_by_agmnt = urgent_total_pcnt >= agreement_threshold,
           urgent_surgery_by_cnsus = urgent_total_pcnt < agreement_threshold &
             urgent_total_pcnt > 50 & Consensus < consensus_threshold)

  # Exact binomial 95% CI for the urgent percentage
  ci <- map2(db_prop$urgent_total_pcnt, db_prop$proc_n, function(p, n) {
    k <- round(p / 100 * n)
    if (n == 0 || is.na(k)) c(NA_real_, NA_real_) else binom.test(k, n)$conf.int * 100
  })
  db_prop$urgent_total_pcnt_CI <- str_c(round(map_dbl(ci, 1), 0), " to ", round(map_dbl(ci, 2), 0))

  db_prop <- db_prop %>%
    left_join(distinct(study_db, booked_procedure, achi_text, block_text), by = "booked_procedure") %>%
    relocate(urgent_total_pcnt_CI, .after = urgent_total_pcnt) %>%
    left_join(select(db_raters, booked_procedure, num_of_raters, num_of_unique_raters),
              by = "booked_procedure")

  if (show_figures_tables) {
    print(freq_table(db_prop, urgent_surgery_by_agmnt, urgent_surgery_by_cnsus))
    print(db_prop %>%
            select(booked_procedure, `<1hr`:`<72hrs`) %>%
            pivot_longer(-booked_procedure, names_to = "urgency", values_to = "pcnt") %>%
            mutate(urgency = factor(urgency, levels = urgency_levels_collapsed)) %>%
            ggplot(aes(urgency, pcnt)) + geom_boxplot(width = 0.5) + theme_bw() +
            labs(title = cohort_label, x = "Booking urgency", y = "% of bookings"))
  }
  db_prop
}

list_cols <- c("achi_code", "achi_text", "block_text", "urgent_total_pcnt", "urgent_total_pcnt_CI",
               "proc_n", "num_of_unique_raters", "Agreement", "Consensus")

extract_flagged <- function(lst, flag, label = NULL) {
  out <- lst %>%
    filter(.data[[flag]]) %>%
    rename(achi_code = booked_procedure) %>%
    select(all_of(list_cols))
  if (!is.null(label)) out <- mutate(out, cohort = label)
  out
}
extract_urgent    <- function(lst, label = NULL) extract_flagged(lst, "urgent_surgery_by_agmnt", label)
extract_consensus <- function(lst, label = NULL) extract_flagged(lst, "urgent_surgery_by_cnsus", label)

# Procedures on one list but not the other
diff_lists <- function(sub_urgent, full_urgent, sub_label) {
  bind_rows(
    anti_join(sub_urgent, full_urgent, by = "achi_code") %>% mutate(comparison = str_c(sub_label, " only")),
    anti_join(full_urgent, sub_urgent, by = "achi_code") %>% mutate(comparison = str_c("not in ", sub_label)))
}

# Priority-based vs time-to-surgery-based urgent lists
compare_priority_time <- function(priority_urgent, time_urgent) {
  list(only_priority = anti_join(priority_urgent, time_urgent, by = "achi_code"),
       only_time     = anti_join(time_urgent, priority_urgent, by = "achi_code"),
       in_both       = inner_join(priority_urgent, time_urgent, by = "achi_code",
                                  suffix = c("_priority", "_time")))
}

# Replace booking priority with the observed booking-to-surgery band
as_time_method <- function(study_db) {
  study_db %>%
    filter(!is.na(booking_to_surg_collapse)) %>%
    rename(emerg_priority_collapse_original = emerg_priority_collapse,
           emerg_priority_collapse          = booking_to_surg_collapse)
}

# Number of procedures flagged at each agreement x consensus threshold
threshold_sensitivity <- function(study_db, urgency = 4,
                                  a_th = c(75, 80, 90), c_th = c(0.6, 0.65, 0.7, 0.9)) {
  expand.grid(a_t = a_th, c_t = c_th) %>%
    pmap_dfr(function(a_t, c_t) {
      res <- get_list_of_procedures(study_db, a_t, c_t, urgency = urgency, cohort_label = "sensitivity")
      if (is.null(res)) return(NULL)
      tibble(agreement_threshold = a_t, consensus_threshold = c_t,
             n_by_agreement = sum(res$urgent_surgery_by_agmnt, na.rm = TRUE),
             n_by_consensus = sum(res$urgent_surgery_by_cnsus, na.rm = TRUE))
    })
}

# =============================================================================
# 3. ALL LISTS FOR ONE CUT-OFF
# build_urgent_lists(study_db, urgency = 4) builds the full, age-group, sex and
# time-to-surgery lists, specialty mix and threshold sensitivity for one
# cut-off, writes two workbooks and an .rds of the objects, and returns them:
#   urgent_surgery_procedure_lists_<h>hr.xlsx
#   urgent_surgery_other_results_<h>hr.xlsx
#   list_objects_<h>hr.rds
# =============================================================================
build_urgent_lists <- function(study_db, urgency = 4, agreement_threshold = 75,
                               consensus_threshold = 0.7, write_files = TRUE) {
  cutoff_bands(urgency)
  lst <- function(db = study_db, ...) {
    get_list_of_procedures(db, agreement_threshold, consensus_threshold, urgency = urgency, ...)
  }

  full_list           <- lst(show_figures_tables = TRUE, cohort_label = str_c("All patients, ", urgency, "hr"))
  full_list_urgent    <- extract_urgent(full_list)
  full_list_consensus <- extract_consensus(full_list)

  age_lists <- list(paeds   = lst(age_gp_filter = "paeds",   cohort_label = "Paediatric (<16)"),
                    adult   = lst(age_gp_filter = "adult",   cohort_label = "Adult (16-64)"),
                    elderly = lst(age_gp_filter = "elderly", cohort_label = "Elderly (65+)"))
  age_urgent    <- imap(age_lists, extract_urgent)
  age_consensus <- imap(age_lists, extract_consensus)
  age_diff      <- imap(age_urgent, ~ diff_lists(.x, full_list_urgent, .y))

  male_urgent   <- extract_urgent(lst(sex_filter = "Male",   cohort_label = "Male"),   "male")
  female_urgent <- extract_urgent(lst(sex_filter = "Female", cohort_label = "Female"), "female")
  diff_sex      <- diff_lists(male_urgent, female_urgent, "male vs female")

  # Urgency defined by observed booking-to-surgery time instead of booking priority
  full_list_time        <- lst(as_time_method(study_db), cohort_label = str_c("Time to surgery, ", urgency, "hr"))
  full_list_time_urgent <- extract_urgent(full_list_time)
  diff_priority_vs_time <- compare_priority_time(full_list_urgent, full_list_time_urgent)

  urgent_specialty <- urgent_specialty_table(study_db, full_list_urgent$achi_code)
  threshold_sens   <- threshold_sensitivity(study_db, urgency = urgency)

  message(sprintf("%dhr cut-off: %d procedures urgent by agreement, %d by consensus",
                  urgency, nrow(full_list_urgent), nrow(full_list_consensus)))

  out <- list(urgency = urgency, study_db = study_db, full_list = full_list,
              full_list_time = full_list_time, full_list_urgent = full_list_urgent,
              full_list_consensus = full_list_consensus,
              paeds_urgent = age_urgent$paeds, adult_urgent = age_urgent$adult,
              elderly_urgent = age_urgent$elderly, male_urgent = male_urgent,
              female_urgent = female_urgent, full_list_time_urgent = full_list_time_urgent,
              diff_priority_vs_time = diff_priority_vs_time, threshold_sensitivity = threshold_sens)
  if (!write_files) return(invisible(out))

  h <- str_c("_", urgency, "hr")
  saveRDS(out, str_c("list_objects", h, ".rds"))

  openxlsx::write.xlsx(list(
    "full list — agreement" = full_list_urgent,
    "full list — consensus" = full_list_consensus,
    "paeds — agreement"     = age_urgent$paeds,
    "paeds — consensus"     = age_consensus$paeds,
    "adult — agreement"     = age_urgent$adult,
    "adult — consensus"     = age_consensus$adult,
    "elderly — agreement"   = age_urgent$elderly,
    "elderly — consensus"   = age_consensus$elderly,
    "diff paeds vs full"    = age_diff$paeds,
    "diff adult vs full"    = age_diff$adult,
    "diff elderly vs full"  = age_diff$elderly,
    "male — agreement"      = male_urgent,
    "female — agreement"    = female_urgent,
    "diff sex"              = diff_sex),
    str_c("urgent_surgery_procedure_lists", h, ".xlsx"), rowNames = FALSE)

  openxlsx::write.xlsx(c(
    list("time method — agreement"    = full_list_time_urgent,
         "priority only (not time)"   = diff_priority_vs_time$only_priority,
         "time only (not priority)"   = diff_priority_vs_time$only_time,
         "in both methods"            = diff_priority_vs_time$in_both,
         "threshold sensitivity"      = threshold_sens,
         "urgent procedure specialty" = urgent_specialty),
    compliance_sheets(study_db)),
    str_c("urgent_surgery_other_results", h, ".xlsx"), rowNames = FALSE)

  invisible(out)
}

# =============================================================================
# 4. BOOKING-TO-SURGERY COMPLIANCE TABLES
# =============================================================================
compliance_sheets <- function(study_db) {
  list(
    "compliance overall"     = freq_table(study_db, booking_surgery_cat),
    "compliance by urgency"  = freq_table(study_db, emerg_priority_collapse, booking_surgery_cat),
    "time band x compliance" = janitor::tabyl(study_db, booking_to_surg_collapse, booking_surgery_cat) %>%
      as.data.frame())
}
