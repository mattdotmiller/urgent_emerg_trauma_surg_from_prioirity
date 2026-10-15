# =============================================================================
# 10_priority_vs_time.R
# Compares urgency defined by booking priority with urgency defined by the
# observed booking-to-surgery time, at one cut-off.
#   - Booking level: weighted kappa and Stuart-Maxwell test (5 bands);
#     kappa and McNemar test (within the cut-off vs not)
#   - Procedure level: agreement between the two urgent lists
#   - Procedures urgent by time only: how urgent their bookings were
#
# Inputs : list_objects_<h>hr.rds (script 08), list_functions.R
# Output : priority_vs_time_results_<h>hr.xlsx
# =============================================================================

library(tidyverse)
library(vcd)
library(DescTools)
library(openxlsx)
source("list_functions.R")

urgency <- 4   # cut-off in hours; must have been run in script 08

obj      <- readRDS(str_c("list_objects_", urgency, "hr.rds"))
study_db <- obj$study_db %>% filter(!is.na(booking_to_surg_collapse), !is.na(emerg_priority_collapse))
is_urgent <- function(x) if_else(x %in% cutoff_bands(urgency), "urgent", "not_urgent")
as_sheet  <- function(tab, row_name) as.data.frame.matrix(tab) %>% rownames_to_column(row_name)

# --- Booking level -----------------------------------------------------------
tab5 <- table(priority = study_db$emerg_priority_collapse, time_to_surg = study_db$booking_to_surg_collapse)
kappa_weighted <- Kappa(tab5, weights = "Fleiss-Cohen")
print(tab5); print(kappa_weighted); print(StuartMaxwellTest(tab5))

tab2 <- table(priority = is_urgent(study_db$emerg_priority_collapse),
              time     = is_urgent(study_db$booking_to_surg_collapse))
kappa_2x2 <- Kappa(tab2)
mcnemar_result <- mcnemar.test(tab2)
print(tab2); print(kappa_2x2); print(mcnemar_result)

kappa_summary <- tibble(
  comparison       = c("booking level, 5 bands (weighted)", "booking level, urgent vs not"),
  kappa            = c(kappa_weighted$Weighted["value"], kappa_2x2$Unweighted["value"]),
  kappa_lower_95CI = c(confint(kappa_weighted)["Weighted", 1], confint(kappa_2x2)["Unweighted", 1]),
  kappa_upper_95CI = c(confint(kappa_weighted)["Weighted", 2], confint(kappa_2x2)["Unweighted", 2]),
  mcnemar_p        = c(NA_real_, mcnemar_result$p.value)) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# --- Procedure level: do the two urgent lists agree? --------------------------
# Only procedures assessable under both methods
all_procs <- intersect(obj$full_list$booked_procedure, obj$full_list_time$booked_procedure)
list_2x2 <- table(priority_list = factor(all_procs %in% obj$full_list_urgent$achi_code,      levels = c(TRUE, FALSE)),
                  time_list     = factor(all_procs %in% obj$full_list_time_urgent$achi_code, levels = c(TRUE, FALSE)))

both   <- list_2x2["TRUE", "TRUE"];  p_only  <- list_2x2["TRUE", "FALSE"]
t_only <- list_2x2["FALSE", "TRUE"]; neither <- list_2x2["FALSE", "FALSE"]
kappa_list <- Kappa(list_2x2)
k_val <- unname(kappa_list$Unweighted[1])
k_ci  <- k_val + c(-1, 1) * qnorm(0.975) * unname(kappa_list$Unweighted[2])

list_similarity <- tibble(
  metric = c("Urgent under both methods", "Urgent under priority only", "Urgent under time only",
             "Urgent under neither", "Priority list size", "Time list size",
             "Jaccard index", "Dice coefficient", "Overlap coefficient", "Percent agreement",
             "Cohen's kappa (list membership)", "Kappa 95% CI lower", "Kappa 95% CI upper",
             "Sensitivity (time vs priority)", "Specificity (time vs priority)",
             "PPV (time vs priority)", "NPV (time vs priority)"),
  value = c(both, p_only, t_only, neither, both + p_only, both + t_only,
            round(both / (both + p_only + t_only), 3),
            round(2 * both / (2 * both + p_only + t_only), 3),
            round(both / min(both + p_only, both + t_only), 3),
            round((both + neither) / sum(list_2x2) * 100, 1),
            round(k_val, 3), round(k_ci[1], 3), round(k_ci[2], 3),
            round(both / (both + p_only), 3), round(neither / (neither + t_only), 3),
            round(both / (both + t_only), 3), round(neither / (neither + p_only), 3)))
print(list_similarity)

# --- Procedures urgent by time only: their booking-priority urgency ----------
time_only <- setdiff(obj$full_list_time_urgent$achi_code, obj$full_list_urgent$achi_code)
time_only_priority <- obj$full_list %>%
  filter(booked_procedure %in% time_only) %>%
  mutate(volume = if_else(proc_n >= 10, "n >= 10", "n < 10"),
         pct_within_cutoff = cut(urgent_total_pcnt, c(-Inf, 25, 50, 60, 70, 75),
                                      labels = c("0-25", "25-50", "50-60", "60-70", "70-75"))) %>%
  count(volume, pct_within_cutoff)
print(time_only_priority)

# --- Export -----------------------------------------------------------------
write.xlsx(list(
  "kappa summary"                  = kappa_summary,
  "priority vs time — 5 bands"     = as_sheet(tab5, "priority"),
  "priority vs time — 2 cat"       = as_sheet(tab2, "priority"),
  "list similarity metrics"        = list_similarity,
  "list membership 2x2"            = as_sheet(list_2x2, "priority_list"),
  "time-only: priority urgency"    = time_only_priority,
  "procedure diff — priority only" = obj$diff_priority_vs_time$only_priority,
  "procedure diff — time only"     = obj$diff_priority_vs_time$only_time,
  "procedure diff — in both"       = obj$diff_priority_vs_time$in_both),
  str_c("priority_vs_time_results_", urgency, "hr.xlsx"), rowNames = FALSE)
