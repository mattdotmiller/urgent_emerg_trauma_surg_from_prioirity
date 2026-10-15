# =============================================================================
# 05_matching_threshold_sensitivity.R
# How the combined matcher responds to the four fuzzy cut-offs, using the
# deployment model on all labelled rows (descriptive, not validation), plus a
# list of misclassified pairs for review.
#
# Inputs : model_params.rds, siamese_model.weights.h5, train_pairs.csv,
#          train_emb_pp.npy, train_emb_pt.npy, train_labels.npy
# Outputs: threshold_sensitivity.csv, misclassifications.csv, 4 .png plots
# =============================================================================

library(pacman)
p_load(tidyverse, caret, pROC, scales, keras3, jsonlite)
source("matching_functions.R")
# use_condaenv("urgent-surgery")
np <- reticulate::import("numpy")

s  <- load_matching_model()
bp <- s$best_params

emb_pp_r <- as.matrix(np$load("train_emb_pp.npy"))
emb_pt_r <- as.matrix(np$load("train_emb_pt.npy"))
labels_r <- as.integer(np$load("train_labels.npy"))
n_total  <- nrow(emb_pp_r)
labeled_csv <- read_csv("train_pairs.csv", show_col_types = FALSE)

dnn_probs_all <- as.vector(s$model |> predict(
  make_model_inputs(emb_pp_r, emb_pt_r,
                    encode_onehot(labeled_csv$proc_no_reduced, s$proc_no_levels),
                    if (s$use_oc)   encode_onehot(labeled_csv$oc_relation,   s$oc_levels),
                    if (s$use_site) encode_onehot(labeled_csv$site_relation, s$site_levels),
                    use_oc = s$use_oc, use_site = s$use_site), verbose = 0L))

combine <- function(fz) {
  ambig <- is.na(fz)
  list(pred  = ifelse(ambig, ifelse(dnn_probs_all > 0.5, "yes", "no"), fz),
       prob  = ifelse(ambig, dnn_probs_all, as.numeric(fz == "yes")),
       ambig = ambig)
}

evaluate_thresholds <- function(pri_upper, pri_lower, add_upper, add_lower) {
  cmb <- combine(fuzzy_from_settings(labeled_csv, s, pri_upper, pri_lower, add_upper, add_lower))
  m <- compute_metrics(labels_r, cmb$pred, cmb$prob)
  tibble(pri_upper, pri_lower, add_upper, add_lower,
         n_fuzzy_cl = sum(!cmb$ambig), n_dnn = sum(cmb$ambig),
         pct_fuzzy  = round(mean(!cmb$ambig) * 100, 1),
         select(m, accuracy, sensitivity, specificity, ppv, npv, f1, auc))
}

# --- Grid: vary two cut-offs at a time ---------------------------------------
upper <- seq(60, 95, by = 5); lower <- seq(5, 35, by = 5)
full_grid <- bind_rows(
  expand.grid(pri_upper = upper, pri_lower = lower,
              add_upper = bp$add_upper, add_lower = bp$add_lower) |>
    filter(pri_lower < pri_upper) |> mutate(grid = "primary"),
  expand.grid(pri_upper = bp$pri_upper, pri_lower = bp$pri_lower,
              add_upper = upper, add_lower = lower) |>
    filter(add_lower < add_upper) |> mutate(grid = "additional"),
  expand.grid(pri_upper = upper, pri_lower = bp$pri_lower,
              add_upper = upper, add_lower = bp$add_lower) |>
    mutate(grid = "upper_only")) |>
  distinct(pri_upper, pri_lower, add_upper, add_lower, .keep_all = TRUE)

results <- pmap_dfr(select(full_grid, -grid), evaluate_thresholds) |>
  mutate(grid = full_grid$grid,
         is_best = pri_upper == bp$pri_upper & pri_lower == bp$pri_lower &
                   add_upper == bp$add_upper & add_lower == bp$add_lower)
write_csv(results, "threshold_sensitivity.csv")

# --- Plots -------------------------------------------------------------------
palette <- c(accuracy = "#4E79A7", sensitivity = "#F28E2B", specificity = "#59A14F",
             f1 = "#E15759", auc = "#76B7B2")
metrics_long <- function(d) pivot_longer(d, c(accuracy, sensitivity, specificity, f1, auc),
                                         names_to = "metric", values_to = "value")
effect_plot <- function(d, x, best, title) {
  ggplot(metrics_long(d), aes({{ x }}, value, colour = metric)) +
    geom_line(linewidth = 1) + geom_point(size = 2) +
    geom_vline(xintercept = best, linetype = "dashed", colour = "grey40") +
    scale_colour_manual(values = palette) +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0.5, 1)) +
    scale_x_continuous(breaks = upper) +
    labs(title = title, y = "Metric", colour = "Metric") +
    theme_bw(base_size = 11) + theme(legend.position = "bottom")
}
save_plot <- function(p, f, w = 10, h = 6) ggsave(f, p, width = w, height = h, dpi = 150)

save_plot(effect_plot(filter(results, grid == "primary", pri_lower == bp$pri_lower),
                      pri_upper, bp$pri_upper, "Effect of pri_upper (token_set)"),
          "effect_pri_upper.png")
save_plot(effect_plot(filter(results, grid == "additional", add_lower == bp$add_lower),
                      add_upper, bp$add_upper, "Effect of add_upper (token_sort)"),
          "effect_add_upper.png")
save_plot(ggplot(results, aes(pct_fuzzy, f1, colour = auc)) +
            geom_point(alpha = 0.6, size = 2) +
            geom_point(data = filter(results, is_best), colour = "red", size = 4, shape = 18) +
            scale_colour_viridis_c(option = "plasma") +
            scale_x_continuous(labels = percent_format(scale = 1)) +
            labs(title = "Fuzzy rate vs F1", x = "% classified by fuzzy rules", y = "F1", colour = "AUC") +
            theme_bw(base_size = 11), "fuzzy_rate_vs_f1.png", 9)
upper_only <- filter(results, grid == "upper_only")
save_plot(ggplot(upper_only, aes(factor(pri_upper), factor(add_upper), fill = f1)) +
            geom_tile(colour = "white") +
            geom_point(data = filter(upper_only, is_best), colour = "red", size = 4, shape = 18) +
            scale_fill_viridis_c(option = "magma") +
            labs(title = "F1: pri_upper x add_upper", x = "pri_upper", y = "add_upper", fill = "F1") +
            theme_bw(base_size = 11), "heatmap_f1_pri_add_upper.png", 9, 7)

# --- Misclassifications at the best-sensitivity thresholds -------------------
best <- results |> arrange(desc(sensitivity), desc(auc), desc(f1)) |> slice_head(n = 1)
fz   <- fuzzy_from_settings(labeled_csv, s, best$pri_upper, best$pri_lower, best$add_upper, best$add_lower)
cmb  <- combine(fz)

misclassifications <- labeled_csv |>
  mutate(truth = correct, predicted = cmb$pred, probability = round(cmb$prob, 4),
         classified_by = if_else(cmb$ambig, "dnn", "fuzzy"),
         error_type = case_when(truth == "yes" & predicted == "no" ~ "false_negative",
                                truth == "no" & predicted == "yes" ~ "false_positive")) |>
  filter(!is.na(error_type)) |>
  select(any_of("row_uid"), study_id, proc_no_reduced, planned_procedure, achi_txt,
         any_of(c("oc_relation", "site_relation", "site_planned", "site_achi")),
         truth, predicted, probability, classified_by, error_type,
         token_set, token_sort, primary_y_n)

print(count(misclassifications, error_type))
write_csv(misclassifications, "misclassifications.csv")
