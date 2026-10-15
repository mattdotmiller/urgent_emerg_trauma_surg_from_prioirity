# =============================================================================
# 07_matching_deploy.R
# Applies the trained matcher to every booking x coded-procedure pair in the
# study dataset. Output rows are keyed on study_row_uid for the analysis scripts.
#
# Inputs : study_data.rds, achi_codes.csv, model_params.rds,
#          siamese_model.weights.h5, [threshold_sensitivity.csv], embed.py,
#          matching_functions.R
# Output : predictions.csv
# =============================================================================

library(pacman)
p_load(tidyverse, tm, reticulate, jsonlite, keras3)
source("matching_functions.R")
# use_condaenv("urgent-surgery")
np <- import("numpy")

bert_model <- "thomas-sounack/BioClinical-ModernBERT-base"  # same as script 03
max_length <- 48                                             # same as script 03
use_sensitivity_thresholds <- TRUE   # take cut-offs from threshold_sensitivity.csv if present
practice_n <- NA                     # e.g. 200 for a quick test run

# --- Model and cut-offs ------------------------------------------------------
s <- load_matching_model()
if (use_sensitivity_thresholds && file.exists("threshold_sensitivity.csv")) {
  best <- read_csv("threshold_sensitivity.csv", show_col_types = FALSE) |>
    arrange(desc(sensitivity), desc(auc), desc(f1)) |> slice_head(n = 1)
  for (k in c("pri_upper", "pri_lower", "add_upper", "add_lower")) s$best_params[[k]] <- best[[k]]
}
message(sprintf("Cut-offs: pri_upper %d, pri_lower %d, add_upper %d, add_lower %d",
                s$best_params$pri_upper, s$best_params$pri_lower,
                s$best_params$add_upper, s$best_params$add_lower))

# --- Preprocess exactly as in training ---------------------------------------
achi_db <- read_csv("achi_codes.csv", show_col_types = FALSE)

pairs <- readRDS("study_data.rds") |>
  rename(achi_proc_code = achi_no) |>
  mutate(row_uid = study_row_uid) |>
  left_join(select(achi_db, achi_proc_code, procedure_chapter, procedure_sub_chapter, block_number),
            by = "achi_proc_code") |>
  mutate(proc_no_reduced = factor(as.character(proc_no_reduced), levels = s$proc_no_levels),
         primary_y_n     = factor(if_else(proc_no_reduced == "primary", "yes", "no"))) |>
  preprocess_pairs(achi_db)

if (!is.na(practice_n)) pairs <- slice(pairs, seq_len(practice_n))

pairs <- pairs |>
  add_fuzzy_scores() |>
  filter(!is.na(achi_txt), !is.na(planned_procedure), !is.na(proc_no_reduced),
         planned_procedure != "NA", achi_txt != "NA") |>
  transmute(study_row_uid = row_uid, study_id, proc = as.character(proc), proc_no_reduced,
            primary_y_n, planned_procedure, achi_txt, oc_relation, site_relation,
            site_planned, site_achi, token_set, token_sort, partial_token_set,
            correct = "unknown") |>
  mutate(across(where(is.factor), as.character))
stopifnot(!anyDuplicated(pairs$study_row_uid))
write_csv(pairs, "deploy_pairs.csv")

# --- Embeddings --------------------------------------------------------------
processx::run(py_config()$python,
              c("embed.py", "--input", "deploy_pairs.csv", "--prefix", "deploy_",
                "--model", bert_model, "--max_length", as.character(max_length),
                "--batch_size", "12"),
              echo = TRUE)
emb_pp_r <- as.matrix(np$load("deploy_emb_pp.npy"))
emb_pt_r <- as.matrix(np$load("deploy_emb_pt.npy"))
stopifnot(nrow(emb_pp_r) == nrow(pairs))

# --- Classify: fuzzy rules first, network for the rest -----------------------
fz    <- fuzzy_from_settings(pairs, s)
ambig <- which(is.na(fz))
prob  <- as.numeric(fz == "yes")
if (length(ambig) > 0) {
  prob[ambig] <- as.vector(s$model |> predict(
    make_model_inputs(emb_pp_r[ambig, , drop = FALSE], emb_pt_r[ambig, , drop = FALSE],
                      encode_onehot(pairs$proc_no_reduced[ambig], s$proc_no_levels),
                      if (s$use_oc)   encode_onehot(pairs$oc_relation[ambig],   s$oc_levels),
                      if (s$use_site) encode_onehot(pairs$site_relation[ambig], s$site_levels),
                      use_oc = s$use_oc, use_site = s$use_site), verbose = 0L))
}

predictions <- pairs |>
  transmute(study_row_uid, study_id, proc_no = proc, proc_no_reduced, planned_procedure,
            achi_txt, oc_relation, site_relation,
            predicted_correct = ifelse(prob >= 0.5, "yes", "no"),
            probability       = round(prob, 4),
            classified_by     = ifelse(is.na(fz), "dnn", "fuzzy"))
write_csv(predictions, "predictions.csv")

message(sprintf("%d pairs | predicted matches %d (%.1f%%) | fuzzy %d | network %d",
                nrow(predictions), sum(predictions$predicted_correct == "yes"),
                mean(predictions$predicted_correct == "yes") * 100,
                nrow(predictions) - length(ambig), length(ambig)))
