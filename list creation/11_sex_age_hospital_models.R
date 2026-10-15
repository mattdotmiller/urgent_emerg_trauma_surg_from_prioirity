# =============================================================================
# 11_sex_age_hospital_models.R   (optional)
# Is booking urgency (booked within the cut-off, default 4 hours) associated
# with sex, age group or hospital, after accounting for the procedure booked?
# Multilevel logistic regression:
#   m0  procedure random intercept
#   m1  + sex + age group
#   m2  + hospital random intercept (crossed with procedure)
# with hospital variance partition and median odds ratios, a case-mix check
# restricted to procedures done at every hospital, and a sensitivity run
# restricted to procedures with >= 25 bookings.
#
# Inputs : study_data.rds, predictions.csv, achi_codes.csv, list_functions.R
# Output : sex_age_hospital_models.xlsx
# =============================================================================

library(tidyverse)
library(rstatix)
library(lme4)
library(optimx)
library(openxlsx)
source("list_functions.R")

urgency          <- 4     # cut-off in hours defining "urgent" (1, 4, 8 or 24)
min_bookings     <- 10    # bookings per procedure (primary run)
min_bookings_sens <- 25   # sensitivity run
min_minority_sex <- 3     # bookings of the less common sex per procedure
min_per_hospital <- 5     # bookings per procedure per hospital (case-mix check)
n_boot           <- 2000  # bootstrap replicates for the MOR confidence intervals

study_db <- load_matched_study_db() %>% filter(sex != "I")

# Outcome: booked within the cut-off vs not. Reference: female, adult.
build_model_data <- function(db, min_n) {
  eligible <- db %>%
    group_by(booked_procedure) %>%
    summarise(n = n(), minority = pmin(sum(sex == "Male", na.rm = TRUE),
                                       sum(sex == "Female", na.rm = TRUE))) %>%
    filter(n >= min_n, minority >= min_minority_sex) %>%
    pull(booked_procedure)
  db %>%
    filter(booked_procedure %in% eligible) %>%
    transmute(study_id, booked_procedure, hosp,
              urgent_yn = factor(if_else(emerg_priority_collapse %in% cutoff_bands(urgency),
                                         "urgent", "not urgent"),
                                 levels = c("not urgent", "urgent")),
              age_gp = fct_relevel(factor(age_gp), "adult", "paeds", "elderly"),
              sex    = factor(sex)) %>%
    drop_na()
}

ctrl <- glmerControl(optimizer = "optimx", optCtrl = list(method = "L-BFGS-B"))

# Variance partition coefficient per random effect (level-1 variance pi^2/3)
vpc_table <- function(model) {
  vc <- as.data.frame(VarCorr(model))
  tibble(group = vc$grp, variance = vc$vcov, VPC = vc$vcov / (sum(vc$vcov) + pi^2 / 3))
}
mor_from_var <- function(v) exp(sqrt(2 * v) * qnorm(0.75))   # median odds ratio

extract_or <- function(model, label) {
  b <- fixef(model); se <- sqrt(diag(vcov(model)))
  tibble(model = label, variable = names(b), OR = exp(b),
         CI_lower = exp(b - 1.96 * se), CI_upper = exp(b + 1.96 * se)) %>%
    mutate(variable = recode(variable, age_gppaeds = "age_gp: paediatric (<16y)",
                             age_gpelderly = "age_gp: older adult (>=65y)"),
           across(where(is.numeric), ~ round(.x, 3)))
}

# --- Primary run -------------------------------------------------------------
u_model_data <- build_model_data(study_db, min_bookings)
message(n_distinct(u_model_data$booked_procedure), " procedures | ", nrow(u_model_data),
        " bookings | ", round(mean(u_model_data$urgent_yn == "urgent") * 100, 1), "% urgent")
for (v in c("age_gp", "sex", "hosp")) print(freq_table(u_model_data, vars = c(v, "urgent_yn")))

m0 <- glmer(urgent_yn ~ 1 + (1 | booked_procedure),
            data = u_model_data, family = binomial, control = ctrl)
m1 <- glmer(urgent_yn ~ sex + age_gp + (1 | booked_procedure),
            data = u_model_data, family = binomial, control = ctrl)
m2 <- glmer(urgent_yn ~ sex + age_gp + (1 | booked_procedure) + (1 | hosp),
            data = u_model_data, family = binomial, control = ctrl)

message("ICC (m0, procedure): ", round(vpc_table(m0)$VPC, 3))
message("m2 singular fit: ", isSingular(m2, tol = 1e-4))
anova_all <- anova(m0, m1, m2)   # m1 vs m2 tests a variance on its boundary: p is conservative
print(anova_all)

or_all <- bind_rows(extract_or(m1, "m1: sex + age_gp"), extract_or(m2, "m2: sex + age_gp + (1|hosp)"))
print(or_all)

# --- Hospital effect size: VPC and median odds ratios --------------------------
vpc_m2 <- vpc_table(m2)
print(vpc_m2)

mor_boot <- function(fit) {
  vc <- vpc_table(fit)
  c(mor_proc = mor_from_var(vc$variance[vc$group == "booked_procedure"]),
    mor_hosp = mor_from_var(vc$variance[vc$group == "hosp"]))
}
set.seed(1234)
boot <- suppressMessages(suppressWarnings(
  bootMer(m2, mor_boot, nsim = n_boot, use.u = FALSE, type = "parametric")))
ci <- apply(boot$t, 2, quantile, probs = c(0.025, 0.975), na.rm = TRUE)
mor_summary <- tibble(component = c("Procedure MOR", "Hospital MOR"), estimate = mor_boot(m2),
                      CI_lower = ci[1, ], CI_upper = ci[2, ]) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3)))
print(mor_summary)

# --- Case-mix check: procedures with >= min_per_hospital bookings at every hospital
common_procs <- u_model_data %>%
  count(booked_procedure, hosp) %>%
  group_by(booked_procedure) %>%
  filter(n_distinct(hosp) == n_distinct(u_model_data$hosp), all(n >= min_per_hospital)) %>%
  distinct(booked_procedure) %>%
  pull()
casemix_data <- filter(u_model_data, booked_procedure %in% common_procs)
message(length(common_procs), " procedures in the case-mix check")

m1_casemix <- update(m1, data = casemix_data)
m2_casemix <- update(m2, data = casemix_data)
vpc_m2_casemix   <- vpc_table(m2_casemix)
anova_m2_casemix <- anova(m1_casemix, m2_casemix)
print(vpc_m2_casemix); print(anova_m2_casemix)

# --- Sensitivity run: >= 25 bookings per procedure ------------------------------
u_model_data_sens <- build_model_data(study_db, min_bookings_sens)
m1_sens <- update(m1, data = u_model_data_sens)
m2_sens <- update(m2, data = u_model_data_sens)
or_sens  <- bind_rows(extract_or(m1_sens, "m1 (>=25 bookings)"),
                      extract_or(m2_sens, "m2 (>=25 bookings)"))
vpc_sens <- vpc_table(m2_sens)
print(or_sens); print(vpc_sens)

# --- Export ------------------------------------------------------------------
lrt_sheet <- function(a, label) as.data.frame(a) %>% rownames_to_column("model") %>% mutate(comparison = label)

write.xlsx(list(
  "OR — m1 and m2"              = or_all,
  "model comparison (LRT)"      = lrt_sheet(anova_all, "m0 vs m1 vs m2"),
  "hospital VPC (m2)"           = vpc_m2,
  "hospital MOR (m2)"           = mor_summary,
  "case-mix check — VPC"        = vpc_m2_casemix,
  "case-mix check — LRT"        = lrt_sheet(anova_m2_casemix, "m1 vs m2, common procedures"),
  "sensitivity — OR (>=25)"     = or_sens,
  "sensitivity — VPC (>=25)"    = vpc_sens),
  "sex_age_hospital_models.xlsx", rowNames = FALSE)
