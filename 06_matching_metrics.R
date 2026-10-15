# =============================================================================
# 06_matching_metrics.R
# Reporting for the matcher.
#   - final_holdout_metrics.csv (from script 04) is the reported performance;
#     cv_summary.csv is the within-pool stability check.
#   - Everything else here scores the deployment model on the rows it was
#     trained on (resubstitution). It shows where errors cluster and compares
#     the hybrid with a network-only approach; it is not a validation estimate.
#
# Inputs : final_holdout_metrics.csv, cv_summary.csv, model_params.rds,
#          siamese_model.weights.h5, train_pairs.csv, train_emb_*.npy,
#          train_labels.npy
# Outputs: full_dataset_metrics_DESCRIPTIVE_ONLY.csv, oc_relation_strata.csv,
#          site_relation_strata.csv, dnn_only_vs_hybrid.csv,
#          dnn_only_fuzzy_row_impact.csv, dnn_only_cutoff_sweep.csv,
#          dnn_only_roc.png, dnn_only_cutoff_sweep.png
# =============================================================================

library(pacman)
p_load(tidyverse, caret, pROC, scales, keras3, jsonlite)
source("matching_functions.R")
# use_condaenv("urgent-surgery")
np <- reticulate::import("numpy")

print(read_csv("final_holdout_metrics.csv", show_col_types = FALSE), n = Inf, width = Inf)
print(read_csv("cv_summary.csv", show_col_types = FALSE), n = Inf)

# --- Predictions on all labelled rows ----------------------------------------
s <- load_matching_model()
emb_pp_r <- as.matrix(np$load("train_emb_pp.npy"))
emb_pt_r <- as.matrix(np$load("train_emb_pt.npy"))
labels_r <- as.integer(np$load("train_labels.npy"))
pairs    <- read_csv("train_pairs.csv", show_col_types = FALSE)
stopifnot(nrow(pairs) == length(labels_r))

dnn_prob_all <- as.vector(s$model |> predict(
  make_model_inputs(emb_pp_r, emb_pt_r,
                    encode_onehot(pairs$proc_no_reduced, s$proc_no_levels),
                    if (s$use_oc)   encode_onehot(pairs$oc_relation,   s$oc_levels),
                    if (s$use_site) encode_onehot(pairs$site_relation, s$site_levels),
                    use_oc = s$use_oc, use_site = s$use_site), verbose = 0L))

fz    <- fuzzy_from_settings(pairs, s)
clear <- which(!is.na(fz))
ambig <- which(is.na(fz))
pred  <- ifelse(is.na(fz), ifelse(dnn_prob_all > 0.5, "yes", "no"), fz)
prob  <- ifelse(is.na(fz), dnn_prob_all, as.numeric(fz == "yes"))

full_metrics <- bind_rows(
  compute_metrics(labels_r[clear], fz[clear])                       |> mutate(stage = "fuzzy"),
  compute_metrics(labels_r[ambig], pred[ambig], dnn_prob_all[ambig]) |> mutate(stage = "dnn"),
  compute_metrics(labels_r, pred, prob)                             |> mutate(stage = "combined")) |>
  relocate(stage)
write_csv(full_metrics, "full_dataset_metrics_DESCRIPTIVE_ONLY.csv")
print(full_metrics, width = Inf)

# --- Error rates by open/closed and anatomical-site relation ------------------
strata_report <- function(col, levels_vec) {
  tibble(stratum = factor(pairs[[col]], levels = levels_vec), truth = labels_r, pred = pred) |>
    mutate(fn = truth == 1 & pred == "no", fp = truth == 0 & pred == "yes") |>
    group_by(stratum) |>
    summarise(n = n(), n_error = sum(fn | fp), error_rate = round(mean(fn | fp), 4),
              false_negative = sum(fn), false_positive = sum(fp), .groups = "drop") |>
    rename(!!col := stratum)
}
write_csv(strata_report("oc_relation",   s$oc_levels),   "oc_relation_strata.csv")
write_csv(strata_report("site_relation", s$site_levels), "site_relation_strata.csv")

# --- Hybrid vs network-only (every row scored by the network) -----------------
dnn_only <- ifelse(dnn_prob_all > 0.5, "yes", "no")
hybrid_m <- compute_metrics(labels_r, pred, prob)
comparison <- bind_rows(hybrid_m |> mutate(pipeline = "hybrid (fuzzy + DNN)"),
                        compute_metrics(labels_r, dnn_only, dnn_prob_all) |>
                          mutate(pipeline = "dnn_only (all rows)")) |>
  relocate(pipeline)
write_csv(comparison, "dnn_only_vs_hybrid.csv")
print(comparison, width = Inf)

# What network-only would change on rows the fuzzy rules currently decide
fuzzy_fp <- clear[fz[clear] == "yes" & labels_r[clear] == 0]
fuzzy_tp <- clear[fz[clear] == "yes" & labels_r[clear] == 1]
write_csv(tibble(
  metric = c("fuzzy false positives", "  fixed by DNN-only", "  still wrong under DNN-only",
             "fuzzy true positives", "  lost under DNN-only (new false negatives)"),
  n = c(length(fuzzy_fp), sum(dnn_only[fuzzy_fp] == "no"), sum(dnn_only[fuzzy_fp] == "yes"),
        length(fuzzy_tp), sum(dnn_only[fuzzy_tp] == "no"))),
  "dnn_only_fuzzy_row_impact.csv")

# Network-only operating points
sweep <- map_dfr(seq(0.05, 0.95, by = 0.05), function(cut) {
  p <- ifelse(dnn_prob_all > cut, "yes", "no")
  compute_metrics(labels_r, p) |>
    transmute(cutoff = cut, n_pos_called = sum(p == "yes"), accuracy, sensitivity,
              specificity, ppv, npv, f1, balanced_acc)
})
write_csv(sweep, "dnn_only_cutoff_sweep.csv")

roc_all <- roc(labels_r, dnn_prob_all, quiet = TRUE)
match_row <- slice_min(sweep, abs(sensitivity - hybrid_m$sensitivity), n = 1, with_ties = FALSE)
message(sprintf("Hybrid sensitivity %.3f, specificity %.3f | DNN-only at cut-off %.2f: sensitivity %.3f, specificity %.3f | DNN AUC %.3f",
                hybrid_m$sensitivity, hybrid_m$specificity, match_row$cutoff,
                match_row$sensitivity, match_row$specificity, as.numeric(auc(roc_all))))

ggsave("dnn_only_roc.png",
       ggplot(tibble(fpr = 1 - roc_all$specificities, tpr = roc_all$sensitivities), aes(fpr, tpr)) +
         geom_line(linewidth = 1, colour = "#F28E2B") +
         geom_abline(linetype = "dashed", colour = "grey50") +
         scale_x_continuous(labels = percent_format()) + scale_y_continuous(labels = percent_format()) +
         labs(title = sprintf("DNN-only ROC (AUC = %.3f)", as.numeric(auc(roc_all))),
              x = "1 - Specificity", y = "Sensitivity") +
         theme_bw(base_size = 12),
       width = 7, height = 6, dpi = 150)

ggsave("dnn_only_cutoff_sweep.png",
       sweep |>
         pivot_longer(c(sensitivity, specificity, f1), names_to = "metric", values_to = "value") |>
         ggplot(aes(cutoff, value, colour = metric)) +
         geom_line(linewidth = 1) + geom_point(size = 1.5) +
         geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey40") +
         scale_colour_manual(values = c(sensitivity = "#F28E2B", specificity = "#59A14F", f1 = "#E15759")) +
         scale_y_continuous(labels = percent_format(accuracy = 1)) +
         labs(title = "DNN-only: metrics by cut-off", x = "Probability cut-off", y = NULL, colour = NULL) +
         theme_bw(base_size = 12) + theme(legend.position = "bottom"),
       width = 9, height = 6, dpi = 150)
