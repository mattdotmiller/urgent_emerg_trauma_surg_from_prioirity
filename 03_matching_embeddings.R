# =============================================================================
# 03_matching_embeddings.R
# Adds fuzzy string-similarity scores to the labelled pairs and runs embed.py
# to compute BioClinical-ModernBERT embeddings of the booking and ACHI text.
#
# Inputs : labeled_proc.rds, embed.py, matching_functions.R
# Outputs: train_pairs.csv, train_metadata.json,
#          train_emb_pp.npy, train_emb_pt.npy, train_labels.npy
# =============================================================================

library(tidyverse)
library(reticulate)
library(jsonlite)
source("matching_functions.R")

# Python environment with torch, transformers, fuzzywuzzy (see README)
# use_condaenv("urgent-surgery")
bert_model <- "thomas-sounack/BioClinical-ModernBERT-base"  # or a local folder
max_length <- 48   # must match 07_matching_deploy.R

labeled_proc <- readRDS("labeled_proc.rds") %>% add_fuzzy_scores()

export_df <- labeled_proc %>%
  select(row_uid, study_id, proc_no_reduced, planned_procedure, achi_txt, correct,
         primary_y_n, token_set, token_sort, partial_token_set,
         oc_relation, oc_planned, oc_achi, site_relation, site_planned, site_achi) %>%
  mutate(across(where(is.factor), as.character))
write_csv(export_df, "train_pairs.csv")

write_json(list(n_rows               = nrow(export_df),
                proc_no_levels       = levels(labeled_proc$proc_no_reduced),
                oc_relation_levels   = oc_relation_levels,
                site_relation_levels = site_relation_levels,
                positive_rate        = mean(export_df$correct == "yes")),
           "train_metadata.json", pretty = TRUE, auto_unbox = TRUE)

processx::run(py_config()$python,
              c("embed.py", "--input", "train_pairs.csv", "--prefix", "train_",
                "--model", bert_model, "--max_length", as.character(max_length)),
              echo = TRUE)
