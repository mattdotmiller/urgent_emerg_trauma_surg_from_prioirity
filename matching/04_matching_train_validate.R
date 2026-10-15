# =============================================================================
# 04_matching_train_validate.R
# Tunes, validates and trains the two-stage matcher: fuzzy rules classify the
# clear-cut pairs and a siamese network classifies the rest.
#   1. Random 80/20 split: development pool / final holdout
#   2. Tuning within the pool: Optuna (architecture + cut-offs), then a grid
#      search over the fuzzy cut-offs with the architecture fixed
#   3. 5-fold CV within the pool (stability check)
#   4. Train on the pool, score once on the holdout (reported performance)
#   5. Train the deployment model on all rows with the same settings
#
# Inputs : train_pairs.csv, train_metadata.json, train_emb_pp.npy,
#          train_emb_pt.npy, train_labels.npy, matching_functions.R
# Outputs: final_holdout_metrics.csv, cv_per_fold.csv, cv_summary.csv,
#          final_params.csv, development_grid_results.csv, initial_split.csv,
#          siamese_model.weights.h5, model_params.rds
# =============================================================================

library(pacman)
p_load(tidyverse, caret, pROC, jsonlite, keras3, reticulate)
source("matching_functions.R")
# use_condaenv("urgent-surgery")   # Python env with keras, optuna (see README)

seed <- 42L
opt  <- list(use_oc = TRUE,   oc_overlap_floor   = 60,
            use_site = TRUE, site_overlap_floor = 50,
            use_partial = TRUE,
            holdout_frac = 0.2, n_folds = 5L, n_trials = 30L, dev_inner_frac = 0.8,
            grid_upper = seq(60, 95, by = 5), grid_lower = seq(5, 35, by = 5),
            epochs_search = 20L, epochs_final = 50L, batch_size = 32L)

np     <- import("numpy")
optuna <- import("optuna")

# --- Load embeddings + features ---------------------------------------------
emb_pp_r <- as.matrix(np$load("train_emb_pp.npy"))
emb_pt_r <- as.matrix(np$load("train_emb_pt.npy"))
labels_r <- as.integer(np$load("train_labels.npy"))
n_total  <- nrow(emb_pp_r)
emb_dim  <- ncol(emb_pp_r)

meta           <- read_json("train_metadata.json")
proc_no_levels <- unlist(meta$proc_no_levels)
oc_levels      <- unlist(meta$oc_relation_levels)   %||% oc_relation_levels
site_levels    <- unlist(meta$site_relation_levels) %||% site_relation_levels

labeled_csv <- read_csv("train_pairs.csv", show_col_types = FALSE)
stopifnot(nrow(labeled_csv) == n_total)

proc_no_enc <- encode_onehot(labeled_csv$proc_no_reduced, proc_no_levels)
oc_enc      <- encode_onehot(labeled_csv$oc_relation,     oc_levels)
site_enc    <- encode_onehot(labeled_csv$site_relation,   site_levels)

inputs_at <- function(idx) {
  make_model_inputs(emb_pp_r[idx, , drop = FALSE], emb_pt_r[idx, , drop = FALSE],
                    proc_no_enc[idx, , drop = FALSE],
                    if (opt$use_oc)   oc_enc[idx, , drop = FALSE],
                    if (opt$use_site) site_enc[idx, , drop = FALSE],
                    use_oc = opt$use_oc, use_site = opt$use_site)
}

fuzzy_with <- function(idx, p) {
  apply_fuzzy_thresholds(labeled_csv[idx, ], p$pri_upper, p$pri_lower, p$add_upper, p$add_lower,
                         oc_enable = opt$use_oc, oc_overlap_floor = opt$oc_overlap_floor,
                         site_enable = opt$use_site, site_overlap_floor = opt$site_overlap_floor,
                         partial_enable = opt$use_partial, partial_upper = p$partial_upper)
}

fit_dnn <- function(train_idx, val_idx, p, epochs) {
  model <- build_siamese_model(
    emb_dim = emb_dim, proc_no_dim = length(proc_no_levels), oc_dim = length(oc_levels),
    dense_units_1 = p$dense_units_1, dense_units_2 = p$dense_units_2,
    dense_units_3 = p$dense_units_3, dropout_1 = p$dropout_1, dropout_2 = p$dropout_2,
    lr = p$lr, use_oc = opt$use_oc, use_site = opt$use_site, site_dim = length(site_levels),
    compile_metrics = list(metric_auc(name = "auc"), metric_precision(name = "precision"),
                           metric_recall(name = "recall")))
  model |> fit(x = inputs_at(train_idx), y = labels_r[train_idx],
               validation_data = list(inputs_at(val_idx), labels_r[val_idx]),
               epochs = epochs, batch_size = opt$batch_size, verbose = 0L,
               callbacks = list(callback_early_stopping(monitor = "val_auc", patience = 5L,
                                                        restore_best_weights = TRUE, mode = "max")))
  model
}

# Fuzzy rules first, network for the unclassified rows
score_combined <- function(model, p, idx) {
  fz    <- fuzzy_with(idx, p)
  ambig <- which(is.na(fz))
  pred  <- fz
  prob  <- as.numeric(fz == "yes")
  if (length(ambig) > 0) {
    p_dnn <- as.vector(model |> predict(inputs_at(idx[ambig]), verbose = 0L))
    pred[ambig] <- ifelse(p_dnn > 0.5, "yes", "no")
    prob[ambig] <- p_dnn
  }
  list(fuzzy = fz, ambig = ambig, clear = which(!is.na(fz)), pred = pred, prob = prob)
}

stage_metrics <- function(model, p, idx) {
  s <- score_combined(model, p, idx)
  bind_rows(
    compute_metrics(labels_r[idx[s$clear]], s$fuzzy[s$clear]) |> mutate(stage = "fuzzy"),
    if (length(s$ambig) > 0)
      compute_metrics(labels_r[idx[s$ambig]], s$pred[s$ambig], s$prob[s$ambig]) |> mutate(stage = "dnn"),
    compute_metrics(labels_r[idx], s$pred, s$prob) |> mutate(stage = "combined"))
}

# Train the network on rows the fuzzy rules leave unclassified (10% for early stopping)
train_on <- function(idx, p) {
  ambig <- which(is.na(fuzzy_with(idx, p)))
  set.seed(seed)
  val <- sample(ambig, size = floor(0.1 * length(ambig)))
  fit_dnn(idx[setdiff(ambig, val)], idx[val], p, opt$epochs_final)
}

# Train on tune_idx, return combined predictions on hold_idx
dev_fit_predict <- function(p, tune_idx, hold_idx) {
  amb_tr <- which(is.na(fuzzy_with(tune_idx, p)))
  if (length(amb_tr) < 10) return(NULL)
  amb_ho <- which(is.na(fuzzy_with(hold_idx, p)))
  model  <- fit_dnn(tune_idx[amb_tr], hold_idx[amb_ho], p, opt$epochs_search)
  score_combined(model, p, hold_idx)
}

# --- 1. Pool / final holdout split -------------------------------------------
set.seed(seed)
holdout_idx <- sample(seq_len(n_total), size = floor(opt$holdout_frac * n_total))
pool_idx    <- setdiff(seq_len(n_total), holdout_idx)
write_csv(tibble(row = seq_len(n_total),
                 split = ifelse(row %in% holdout_idx, "final_holdout", "pool")),
          "initial_split.csv")

# --- 2. Development tuning (within pool) -------------------------------------
set.seed(seed)
dev_tune_idx <- sample(pool_idx, size = floor(opt$dev_inner_frac * length(pool_idx)))
dev_hold_idx <- setdiff(pool_idx, dev_tune_idx)

objective <- function(trial) {
  p <- list(
    dense_units_1 = trial$suggest_int("dense_units_1", 64L, 256L, step = 64L),
    dense_units_2 = trial$suggest_int("dense_units_2", 32L, 128L, step = 32L),
    dense_units_3 = trial$suggest_int("dense_units_3", 32L, 128L, step = 32L),
    dropout_1     = trial$suggest_float("dropout_1", 0.2, 0.5, step = 0.1),
    dropout_2     = trial$suggest_float("dropout_2", 0.2, 0.5, step = 0.1),
    lr            = trial$suggest_float("lr", 1e-4, 1e-3, log = TRUE),
    pri_upper     = trial$suggest_int("pri_upper", 75L, 85L, step = 5L),
    pri_lower     = trial$suggest_int("pri_lower", 15L, 25L, step = 5L),
    add_upper     = trial$suggest_int("add_upper", 80L, 90L, step = 5L),
    add_lower     = trial$suggest_int("add_lower", 15L, 25L, step = 5L),
    partial_upper = trial$suggest_int("partial_upper", 85L, 98L))
  s <- dev_fit_predict(p, dev_tune_idx, dev_hold_idx)
  if (is.null(s)) return(0.5)
  as.numeric(pROC::auc(pROC::roc(labels_r[dev_hold_idx], s$prob, quiet = TRUE)))
}

study <- optuna$create_study(direction = "maximize", study_name = "development_tuning",
                             storage = "sqlite:///optuna_development.db",
                             load_if_exists = FALSE)
study$optimize(objective, n_trials = opt$n_trials)
arch_params <- study$best_params
message(sprintf("Optuna best development AUC: %.4f", study$best_value))

# Threshold grid: vary two cut-offs at a time, others fixed at Optuna's values
grid <- bind_rows(
  expand.grid(pri_upper = opt$grid_upper, pri_lower = opt$grid_lower) |>
    filter(pri_lower < pri_upper) |>
    mutate(add_upper = arch_params$add_upper, add_lower = arch_params$add_lower),
  expand.grid(add_upper = opt$grid_upper, add_lower = opt$grid_lower) |>
    filter(add_lower < add_upper) |>
    mutate(pri_upper = arch_params$pri_upper, pri_lower = arch_params$pri_lower),
  expand.grid(pri_upper = opt$grid_upper, add_upper = opt$grid_upper) |>
    mutate(pri_lower = arch_params$pri_lower, add_lower = arch_params$add_lower)) |>
  distinct(pri_upper, pri_lower, add_upper, add_lower)

evaluate_combo <- function(pri_upper, pri_lower, add_upper, add_lower) {
  p <- modifyList(arch_params, list(pri_upper = pri_upper, pri_lower = pri_lower,
                                    add_upper = add_upper, add_lower = add_lower))
  s <- dev_fit_predict(p, dev_tune_idx, dev_hold_idx)
  if (is.null(s)) return(NULL)
  m <- compute_metrics(labels_r[dev_hold_idx], s$pred, s$prob)
  tibble(pri_upper, pri_lower, add_upper, add_lower,
         sensitivity = m$sensitivity, specificity = m$specificity,
         accuracy = m$accuracy, f1 = m$f1, auc = m$auc)
}

grid_results <- pmap_dfr(grid, evaluate_combo)
best_thresh  <- grid_results |> arrange(desc(sensitivity), desc(auc), desc(f1)) |> slice_head(n = 1)
final_params <- modifyList(arch_params, as.list(best_thresh[c("pri_upper", "pri_lower",
                                                              "add_upper", "add_lower")]))
write_csv(grid_results, "development_grid_results.csv")
saveRDS(final_params, "development_final_params.rds")

# --- 3. 5-fold CV within pool (robustness check) -----------------------------
set.seed(seed)
folds <- createFolds(labels_r[pool_idx], k = opt$n_folds, returnTrain = TRUE)

cv_per_fold <- imap_dfr(folds, function(tr, i) {
  train_idx <- pool_idx[tr]
  test_idx  <- setdiff(pool_idx, train_idx)
  stage_metrics(train_on(train_idx, final_params), final_params, test_idx) |>
    mutate(fold = match(i, names(folds)))
})

metric_cols <- c("accuracy","sensitivity","specificity","ppv","npv","f1","balanced_acc")
cv_summary <- cv_per_fold |>
  group_by(stage) |>
  summarise(across(all_of(metric_cols),
                   list(mean = ~ round(mean(.x, na.rm = TRUE), 3),
                        sd   = ~ round(sd(.x,   na.rm = TRUE), 3))), .groups = "drop")

write_csv(cv_per_fold, "cv_per_fold.csv")
write_csv(cv_summary,  "cv_summary.csv")
write_csv(as_tibble(final_params), "final_params.csv")
print(cv_summary, n = Inf)

# --- 4. Final holdout validation (headline result) ---------------------------
holdout_metrics <- stage_metrics(train_on(pool_idx, final_params), final_params, holdout_idx) |>
  mutate(split = "final_holdout")
write_csv(holdout_metrics, "final_holdout_metrics.csv")
print(holdout_metrics, n = Inf, width = Inf)

# --- 5. Deployment model (all rows, not re-scored) ---------------------------
deploy_model <- train_on(seq_len(n_total), final_params)
deploy_model$save_weights("siamese_model.weights.h5")
saveRDS(list(best_params = final_params, proc_no_levels = proc_no_levels,
             proc_no_dim = length(proc_no_levels), embedding_dim = emb_dim,
             oc_relation_levels = oc_levels, oc_dim = length(oc_levels),
             use_oc = opt$use_oc, oc_overlap_floor = opt$oc_overlap_floor,
             site_relation_levels = site_levels, site_dim = length(site_levels),
             use_site = opt$use_site, site_overlap_floor = opt$site_overlap_floor,
             use_partial = opt$use_partial,
             nested_cv_summary = cv_summary, final_holdout_metrics = holdout_metrics),
        "model_params.rds")
message("Saved siamese_model.weights.h5 and model_params.rds")
