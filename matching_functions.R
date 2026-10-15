# =============================================================================
# matching_functions.R
# Helper functions for the matching scripts (02-07), loaded with
# source("matching_functions.R"). Text cleaning, open/closed and anatomical
# site tagging, the rule-based (fuzzy) classifier and the siamese network.
# The marker lists are starting points; adapt them to local booking wording.
# =============================================================================

library(stringr)
library(dplyr)
library(tm)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# 1. TEXT CLEANING
# =============================================================================
.mm_stopwords_vec    <- c(tm::stopwords("english"), "and", "of")
.mm_stopword_pattern <- paste0("\\b", .mm_stopwords_vec, "\\b", collapse = "|")

normalize_achi <- function(x) x %>% tolower() %>% str_squish()

clean_text_basic <- function(x) {
  x %>%
    str_replace_all("[^[:alnum:]]", " ") %>%
    tolower() %>%
    str_replace_all(.mm_stopword_pattern, " ") %>%
    str_squish() %>%
    str_remove_all(r"(\d{1,2}\s?(?:hrs?|mins?))") %>%   # "2hrs", "30 min"
    str_squish()
}

# Booking abbreviations -> ACHI-style wording (first occurrence only)
expand_abbreviations <- function(x) {
  x %>%
    str_replace("\\borif\\b", "open reduction fracture internal fixation") %>%
    str_replace("\\bex fix\\b|\\bex fixator\\b", "external fixation device") %>%
    str_replace("\\bicp monitor\\b", "intracranial pressure icp monitoring device") %>%
    str_replace("\\bnof\\b", "neck femur") %>%
    str_replace("\\bfrontalevd\\b", "evd") %>%
    str_replace("\\bevd\\b", "external ventricular drain") %>%
    str_replace("\\bdhs\\b", "internal fixation fracture trochanteric") %>%
    str_replace("\\bppm\\b", "permanent pacemaker") %>%
    str_replace("\\bcabg\\b", "coronary artery bypass graft") %>%
    str_replace("\\bercp\\b", "endoscopic sphincterotomy biliary stent") %>%
    str_replace("\\bd c\\b", "dilatation curettage") %>%
    str_replace("\\bacdf\\b", "anterior column spinal fusion")
}

# Specialty-specific rewrites that move booking text towards ACHI vocabulary
apply_specialty_text_subs <- function(x) {
  x <- str_replace(x, "craniotomy|burr holes|burr hole|burrholes|craniectomy",
                   "craniotomy intracranial")
  x <- x %>%
    str_replace("\\bnail\\b", "internal fixation fracture") %>%
    str_replace_all("\\bwound\\b", "wound subcutaneous tissue") %>%
    str_replace("neck.*femur", "hip") %>%
    str_replace_all("(hip.*fixation|fixation.*hip|hip.*insertion|insertion.*hip|hip.*replacement|replacement.*hip)",
                    "hip arthroplasty fixation") %>%
    str_replace_all("(hip.*screws|screws.*hip|hip.*screw)", "hip screws internal fixation") %>%
    str_replace("\\bhip arthroplasty\\b", "internal fixation")
  x <- if_else(str_detect(x, "washout|repair|exploration"),
               str_replace(x, "(washout|repair|exploration)", "debride \\1"), x)
  x <- if_else(str_detect(x, "washout|debride|exploration|closure"),
               str_replace(x, "(washout|debride|exploration|closure)", "repair \\1"), x)
  str_replace(x, "\\blaceration\\b", "wound")
}

remove_duplicate_words <- function(x) {
  vapply(str_split(x, "\\s+"),
         function(w) str_squish(paste(unique(w), collapse = " ")), character(1))
}

# Generic verbs/qualifiers that should not, on their own, justify a
# partial-containment match
.mm_stop_verbs <- c(
  "repair","removal","remove","removed","washout","washouts","debride",
  "debridement","exploration","explore","closure","close","insertion","insert",
  "revision","revise","replacement","replace","application","apply","examination",
  "procedure","fracture","fractured","reduction","fixation","internal","external",
  "open","closed","left","right","bilateral","under","anaesthesia","anaesthetic",
  "site","subcutaneous","tissue","wound","device")
.mm_content_stop <- unique(c(.mm_stopwords_vec, .mm_stop_verbs))

# TRUE where booking and ACHI text share at least one non-generic token
has_shared_content_token <- function(planned, achi) {
  p <- tolower(planned %||% ""); a <- tolower(achi %||% "")
  p[is.na(p)] <- ""; a[is.na(a)] <- ""
  mapply(function(x, y) {
    shared <- intersect(setdiff(unique(strsplit(x, "\\s+")[[1]]), ""),
                        setdiff(unique(strsplit(y, "\\s+")[[1]]), ""))
    any(nchar(shared) >= 3 & !shared %in% .mm_content_stop)
  }, p, a, USE.NAMES = FALSE)
}

# =============================================================================
# 2. OPEN / CLOSED TAGGING
# Exposure is what matters: internal fixation / plate / DHS / arthrotomy / CRIF
# imply OPEN even if "closed reduction" or "percutaneous" also appear.
# Percutaneous / K-wire without internal fixation stays CLOSED. Bare "nail" is
# not an open marker (IM nailing is often closed).
# =============================================================================
.oc_open_fixation_pat <- paste("internal fixation", "\\bplate\\b", "\\bplating\\b",
                               "dynamic hip screw", "\\bdhs\\b", "arthrotomy", "\\bcrif\\b",
                               sep = "|")
.oc_closed_pat <- paste("closed reduction", "\\bclosed\\b", "\\bmua\\b", "manipulation",
                        "percutaneous", "\\bk[- ]?wire(s)?\\b", "application of plaster",
                        "\\bplaster\\b", "\\bcast\\b", "external fixation", "external device",
                        "\\bex fix\\b", sep = "|")

tag_open_closed <- function(text) {
  t <- tolower(text %||% ""); t[is.na(t)] <- ""
  open_red   <- str_detect(t, "open reduction|\\borif\\b")
  closed_red <- str_detect(t, "closed reduction")
  case_when(
    open_red & closed_red                  ~ "both",
    open_red                               ~ "open",
    str_detect(t, .oc_open_fixation_pat)   ~ "open",
    str_detect(t, .oc_closed_pat)          ~ "closed",
    str_detect(t, "\\bopen\\b")            ~ "open",
    TRUE                                   ~ "unknown")
}

oc_relation_levels <- c("open_to_open", "open_to_closed", "closed_to_open",
                        "closed_to_closed", "both_planned", "unknown")

derive_oc_relation <- function(planned, achi) {
  p <- tag_open_closed(planned); a <- tag_open_closed(achi)
  factor(case_when(
    p == "both"                   ~ "both_planned",
    p == "open"   & a == "closed" ~ "open_to_closed",
    p == "closed" & a == "open"   ~ "closed_to_open",
    p == "open"   & a == "open"   ~ "open_to_open",
    p == "closed" & a == "closed" ~ "closed_to_closed",
    TRUE                          ~ "unknown"), levels = oc_relation_levels)
}

# =============================================================================
# 3. ANATOMICAL-SITE TAGGING
# Fine regions feed the model; coarse buckets drive the cross-limb override.
# Text matching more than one coarse bucket, or none, is "unknown" and is
# never overridden.
# =============================================================================
.an_region_pat <- c(
  shoulder   = "\\bshoulder|clavicle|clavicular|scapula|acromioclavicular|\\bac joint\\b|glenoid|glenohumeral|proximal humer",
  humerus    = "\\bhumer(us|al)\\b|humeral shaft|\\bmid.?shaft humerus",
  elbow      = "\\belbow|olecranon|radial head|coronoid",
  forearm    = "\\bradius\\b|\\bulna\\b|\\bforearm\\b|both bone|diaphys.*(radi|uln)",
  wrist_hand = "\\bwrist|carpus|carpal|scaphoid|lunate|triquetr|trapezi|pisiform|hamate|capitate|metacarp|\\bphalan|\\bfinger|\\bthumb|\\bhand\\b|distal radius|colles|smith",
  hip_pelvis = "\\bhip\\b|\\bhips\\b|acetabul|\\bpelvi|\\bnof\\b|neck of femur|neck femur|femoral neck|intertrochanteric|sub.?trochanteric|per.?trochanteric|\\btrochanteric|proximal femur",
  femur      = "\\bfemur\\b|\\bfemoral\\b|femoral shaft|\\bthigh\\b|distal femur|supracondylar femur",
  knee       = "\\bknee\\b|patella|tibial plateau|tibial spine|\\bacl\\b|\\bpcl\\b|meniscus|meniscal",
  leg        = "\\btibia\\b|\\btibial\\b|\\bfibula\\b|fibular|\\bleg\\b|\\bshin\\b|tib.?fib",
  ankle      = "\\bankle|malleol|\\bpilon\\b|syndesmo|distal tibia",
  foot       = "\\bfoot\\b|\\bfeet\\b|calcaneus|calcaneal|\\btalus\\b|talar|navicular|cuboid|cuneiform|metatars|\\btoe|hallux|midfoot|hindfoot|forefoot|lisfranc",
  spine      = "\\bspine\\b|spinal|vertebr|cervical|thoracic spine|lumbar|\\bsacr|coccyg|\\bc[1-7]\\b|\\bt(1[0-2]|[1-9])\\b|\\bl[1-5]\\b",
  thorax     = "\\brib\\b|\\bribs\\b|sternum|sternotomy|thoracotomy|\\blung|pleura|chest wall",
  cardiac    = "\\bheart\\b|cardiac|coronary|coronary artery bypass|\\bcabg\\b|pacemaker|\\bppm\\b|\\bvalve\\b|\\baorta\\b|\\baortic\\b",
  abdomen    = "laparotomy|laparoscop|\\babdom|\\bbowel|\\bcolon|append|\\bliver\\b|\\bspleen|\\bgastr|\\bhernia",
  cranium    = "\\bskull|cranium|craniotomy|craniectomy|intracranial|burr hole|\\bevd\\b|ventricular drain")

.an_coarse_map <- c(
  shoulder = "upper_limb", humerus = "upper_limb", elbow = "upper_limb",
  forearm = "upper_limb", wrist_hand = "upper_limb",
  hip_pelvis = "lower_limb", femur = "lower_limb", knee = "lower_limb",
  leg = "lower_limb", ankle = "lower_limb", foot = "lower_limb",
  spine = "axial", thorax = "thorax", cardiac = "thorax",
  abdomen = "abdomen", cranium = "head_neck")

anatomy_region_levels <- c(names(.an_region_pat), "unknown")

anatomy_coarse <- function(region) {
  out <- unname(.an_coarse_map[as.character(region)])
  out[is.na(out)] <- "unknown"
  out
}

tag_anatomy_region <- function(text) {
  t <- tolower(text %||% ""); t[is.na(t)] <- ""
  regions <- names(.an_region_pat)
  hit <- vapply(regions, function(p) str_detect(t, .an_region_pat[[p]]), logical(length(t)))
  if (length(t) == 1L) hit <- matrix(hit, nrow = 1L, dimnames = list(NULL, regions))
  vapply(seq_along(t), function(i) {
    matched <- regions[hit[i, ]]
    if (length(matched) == 0L || length(unique(anatomy_coarse(matched))) > 1L) "unknown"
    else matched[1]
  }, character(1))
}

site_relation_levels <- c("same_region", "related_region", "cross_region", "unknown")

derive_site_relation <- function(planned, achi) {
  rp <- tag_anatomy_region(planned); ra <- tag_anatomy_region(achi)
  factor(case_when(
    rp == "unknown" | ra == "unknown"       ~ "unknown",
    rp == ra                                ~ "same_region",
    anatomy_coarse(rp) == anatomy_coarse(ra) ~ "related_region",
    TRUE                                    ~ "cross_region"), levels = site_relation_levels)
}

# =============================================================================
# 4. PREPROCESSING STEPS (used identically in training and deployment)
# =============================================================================

# Tag open/closed and anatomical site from the ORIGINAL text, before rewrites
add_relation_tags <- function(df) {
  df %>% mutate(
    oc_planned    = tag_open_closed(planned_procedure),
    oc_achi       = tag_open_closed(achi_txt),
    oc_relation   = derive_oc_relation(planned_procedure, achi_txt),
    site_planned  = tag_anatomy_region(planned_procedure),
    site_achi     = tag_anatomy_region(achi_txt),
    site_relation = derive_site_relation(planned_procedure, achi_txt))
}

# Rule-based rewrites that need ACHI chapter/block metadata. Where a generic
# booking term (laparotomy, thoracotomy, spinal level, gastroscopy) sits in a
# compatible ACHI chapter, the booking text is replaced with the ACHI text.
# df needs procedure_chapter, procedure_sub_chapter, block_number.
apply_metadata_overwrites <- function(df, achi_db) {
  pat <- function(words) str_c(words, collapse = "|")
  laparotomy_keywords <- c(
    "0813-0817 Spleen","0850-0869 Oesophagus","0870-0890 Stomach",
    "0891-0903 Small Intestine","0904-0925 Large Intestine","0926-0927 Appendix",
    "0928-0942 Rectum, Anus","0951-0956 Liver","0957-0973 Gall Bladder and Biliary Tract",
    "0974-0982 Pancreas","0983-1004 Abdomen, Peritoneum and Omentum",
    "1005-1011 Other and Multiple Sites of Digestive System","1040-1064 Kidney",
    "1065-1088 Ureter","1089-1111 Bladder","1160-1170 Prostate and Seminal Vesicle")
  thoracotomy_keywords <- c(
    "0532-0542 Trachea","0543-0547 Bronchus","0548-0558 Lung and Pleura",
    "0559-0567 Chest Wall, Mediastinum and Diaphragm",
    "0568-0572 Other and Multiple Sites of Respiratory System",
    "00-0606 Heart - Atrium","0607-0615 Heart - Ventricle","0616-0620 Heart - Septum",
    "0621-0624 Heart - Aortic Valve","0625-0630 Heart - Mitral Valve",
    "0631-0635 Heart - Tricuspid Valve","0636-0638 Heart - Pulmonary Valve",
    "0639-0642 Heart - Myocardium","0643-0646 Heart - Pericardium",
    "0647-0666 Heart - Other Sites","0667-0681 Coronary Arteries","0682-0693 Aorta")
  spinal_keywords <- c("0029-0059 Spinal Canal and Spinal Cord Structures",
                       "0060-0086 Peripheral Nervous System",
                       "1381-1393 Spine \\(Vertebral Column\\)")
  spinal_pattern  <- pat(c(paste0("c", 1:7), paste0("t", 1:10), paste0("l", 1:10)))
  lap_pattern     <- regex("\\blaparotomy\\b|\\blaparoscopy\\b|\\blaparoscopic\\b", ignore_case = TRUE)
  removal_pattern <- regex("removal|replace|replacement|postoperative reopening", ignore_case = TRUE)
  digestive       <- "10 Procedures on digestive system"

  endoscopy_blocks <- achi_db %>%
    filter(procedure_chapter == digestive,
           str_detect(procedure_text, pat(c("endoscop", "esophagoscop", "colonoscopy")))) %>%
    pull(block_number) %>% unique()

  df %>%
    mutate(
      matched_by_rule = case_when(
        str_detect(planned_procedure, spinal_pattern) &
          str_detect(procedure_sub_chapter, pat(spinal_keywords)) ~ "spinal",
        str_detect(planned_procedure, "thoracotomy|sternotomy") &
          str_detect(procedure_sub_chapter, pat(thoracotomy_keywords)) ~ "thoracotomy",
        str_detect(planned_procedure, lap_pattern) &
          str_detect(procedure_sub_chapter, pat(laparotomy_keywords)) &
          !block_number %in% endoscopy_blocks &
          (!str_detect(achi_txt, removal_pattern) |
             (str_detect(achi_txt, removal_pattern) &
                str_detect(planned_procedure, removal_pattern))) ~ "laparotomy",
        TRUE ~ NA_character_),
      planned_procedure = ifelse(!is.na(matched_by_rule), achi_txt, planned_procedure),
      planned_procedure = ifelse(is.na(matched_by_rule),
                                 apply_specialty_text_subs(planned_procedure), planned_procedure),
      planned_procedure = case_when(
        is.na(matched_by_rule) &
          str_detect(planned_procedure, "\\bgastroscopy\\b|\\bfood bolus\\b") &
          str_detect(procedure_chapter, digestive) &
          block_number %in% endoscopy_blocks ~ achi_txt,
        TRUE ~ planned_procedure))
}

# Full text pipeline: tags -> cleaning -> metadata rewrites -> dedup words
preprocess_pairs <- function(df, achi_db) {
  df %>%
    add_relation_tags() %>%
    mutate(achi_txt          = normalize_achi(achi_txt),
           planned_procedure = expand_abbreviations(clean_text_basic(planned_procedure))) %>%
    apply_metadata_overwrites(achi_db) %>%
    mutate(planned_procedure = remove_duplicate_words(planned_procedure))
}

# Fuzzy string scores (python fuzzywuzzy via reticulate)
add_fuzzy_scores <- function(df) {
  fuzz <- reticulate::import("fuzzywuzzy.fuzz")
  df %>%
    rowwise() %>%
    mutate(token_set         = fuzz$token_set_ratio(planned_procedure, achi_txt),
           token_sort        = fuzz$token_sort_ratio(planned_procedure, achi_txt),
           partial_token_set = fuzz$partial_token_set_ratio(planned_procedure, achi_txt)) %>%
    ungroup()
}

# =============================================================================
# 5. RULE-BASED (FUZZY) CLASSIFIER
# Returns "yes" / "no" / NA. NA rows are passed to the neural network.
#   primary procedures   : token_set  >= pri_upper -> yes, <= pri_lower -> no
#   additional procedures: token_sort >= add_upper -> yes, token_set <= add_lower -> no
# then domain overrides, a content-gated partial-containment rule, and two
# vetoes (open booked but closed coded; booked and coded on different limbs).
# =============================================================================
apply_fuzzy_thresholds <- function(data, pri_upper, pri_lower, add_upper, add_lower,
                                   oc_enable = TRUE, oc_overlap_floor = 60,
                                   site_enable = FALSE, site_overlap_floor = 50,
                                   partial_enable = TRUE, partial_upper = 90) {
  pred <- rep(NA_character_, nrow(data))
  pri  <- data$primary_y_n == "yes"
  pred[ pri & data$token_set  >= pri_upper] <- "yes"
  pred[ pri & data$token_set  <= pri_lower] <- "no"
  pred[!pri & data$token_sort >= add_upper] <- "yes"
  pred[!pri & data$token_set  <= add_lower] <- "no"

  pp <- tolower(data$planned_procedure); at <- tolower(data$achi_txt)
  pred <- ifelse(grepl("closed reduction", at) &
                   grepl("open reduction|internal fixation", pp) &
                   !grepl("external fixator|closed|external device", pp), "yes", pred)
  pred <- ifelse(grepl("transcatheter embolisation of blood vessels", at) &
                   grepl("laparotomy", pp), "yes", pred)
  pred <- ifelse(grepl("thoracoscopy", at) & grepl("rib", pp), "yes", pred)
  pred <- ifelse(grepl("closed reduction", at) &
                   grepl("external device|external fixation", pp), "yes", pred)

  # Short booking contained in a longer ACHI string
  if (isTRUE(partial_enable) && "partial_token_set" %in% names(data)) {
    hit <- is.na(pred) & has_shared_content_token(data$planned_procedure, data$achi_txt) &
      !is.na(data$partial_token_set) & data$partial_token_set >= partial_upper
    pred[hit %in% TRUE] <- "yes"
  }

  # Veto: open booked, closed coded
  if (isTRUE(oc_enable)) {
    p_oc <- if ("oc_planned" %in% names(data)) as.character(data$oc_planned) else tag_open_closed(data$planned_procedure)
    a_oc <- if ("oc_achi"    %in% names(data)) as.character(data$oc_achi)    else tag_open_closed(data$achi_txt)
    veto <- p_oc == "open" & a_oc == "closed" & !is.na(data$token_set) &
      data$token_set >= oc_overlap_floor
    pred[veto %in% TRUE] <- "no"
  }

  # Veto: upper vs lower limb
  if (isTRUE(site_enable)) {
    sp <- if ("site_planned" %in% names(data)) as.character(data$site_planned) else tag_anatomy_region(data$planned_procedure)
    sa <- if ("site_achi"    %in% names(data)) as.character(data$site_achi)    else tag_anatomy_region(data$achi_txt)
    cp <- anatomy_coarse(sp); ca <- anatomy_coarse(sa)
    limbs <- c("upper_limb", "lower_limb")
    veto <- cp %in% limbs & ca %in% limbs & cp != ca & !is.na(data$token_set) &
      data$token_set >= site_overlap_floor
    pred[veto] <- "no"
  }
  pred
}

# =============================================================================
# 6. SIAMESE NETWORK
# Inputs (fixed order): booking embedding, ACHI embedding, procedure number,
# [open/closed relation], [site relation].
# =============================================================================
encode_onehot <- function(vec, levels) {
  f <- factor(as.character(vec), levels = levels)
  m <- matrix(0L, nrow = length(f), ncol = length(levels), dimnames = list(NULL, levels))
  idx <- which(!is.na(f))
  if (length(idx) > 0) m[cbind(idx, as.integer(f)[idx])] <- 1L
  m
}

build_siamese_model <- function(emb_dim, proc_no_dim, oc_dim,
                                dense_units_1, dense_units_2, dense_units_3,
                                dropout_1, dropout_2, lr,
                                use_oc = TRUE, use_site = FALSE, site_dim = NULL,
                                compile_metrics = NULL) {
  compile_metrics <- compile_metrics %||% list(metric_auc(name = "auc"))
  if (isTRUE(use_site) && is.null(site_dim)) stop("use_site = TRUE requires site_dim.")

  input_pp  <- layer_input(shape = emb_dim,     name = "emb_pp")
  input_pt  <- layer_input(shape = emb_dim,     name = "emb_pt")
  input_pno <- layer_input(shape = proc_no_dim, name = "proc_no")

  shared_1 <- layer_dense(units = dense_units_1, activation = "relu")
  shared_2 <- layer_dense(units = dense_units_2, activation = "relu")
  enc_pp <- input_pp |> shared_1() |> shared_2()
  enc_pt <- input_pt |> shared_1() |> shared_2()

  # symmetric difference: relu(a - b) + relu(b - a)
  diff <- layer_add(list(
    layer_activation(activation = "relu")(layer_subtract(list(enc_pp, enc_pt))),
    layer_activation(activation = "relu")(layer_subtract(list(enc_pt, enc_pp)))))

  branches <- list(diff, input_pno |> layer_dense(units = 16L, activation = "relu"))
  inputs   <- list(input_pp, input_pt, input_pno)
  if (isTRUE(use_oc)) {
    input_oc <- layer_input(shape = oc_dim, name = "oc_relation")
    branches <- c(branches, list(input_oc |> layer_dense(units = 8L, activation = "relu")))
    inputs   <- c(inputs, list(input_oc))
  }
  if (isTRUE(use_site)) {
    input_site <- layer_input(shape = site_dim, name = "site_relation")
    branches <- c(branches, list(input_site |> layer_dense(units = 8L, activation = "relu")))
    inputs   <- c(inputs, list(input_site))
  }

  output <- layer_concatenate(branches) |>
    layer_dense(units = dense_units_3, activation = "relu") |>
    layer_batch_normalization() |>
    layer_dropout(rate = dropout_1) |>
    layer_dense(units = 32L, activation = "relu") |>
    layer_dropout(rate = dropout_2) |>
    layer_dense(units = 1L, activation = "sigmoid")

  model <- keras_model(inputs = inputs, outputs = output)
  model |> compile(optimizer = optimizer_adam(learning_rate = lr),
                   loss = "binary_crossentropy", metrics = compile_metrics)
  model
}

make_model_inputs <- function(emb_pp, emb_pt, proc_no_enc, oc_enc = NULL, site_enc = NULL,
                              use_oc = TRUE, use_site = FALSE) {
  out <- list(emb_pp, emb_pt, proc_no_enc)
  if (isTRUE(use_oc))   out <- c(out, list(oc_enc))
  if (isTRUE(use_site)) out <- c(out, list(site_enc))
  out
}

# Rebuild the trained model from model_params.rds + weights (scripts 05-07)
load_matching_model <- function() {
  mp <- readRDS("model_params.rds")
  bp <- mp$best_params
  s <- list(
    best_params    = bp,
    proc_no_levels = mp$proc_no_levels,
    oc_levels      = mp$oc_relation_levels   %||% oc_relation_levels,
    site_levels    = mp$site_relation_levels %||% site_relation_levels,
    use_oc         = mp$use_oc               %||% TRUE,
    oc_floor       = mp$oc_overlap_floor     %||% 60,
    use_site       = mp$use_site             %||% FALSE,
    site_floor     = mp$site_overlap_floor   %||% 50,
    use_partial    = mp$use_partial          %||% FALSE,
    partial_upper  = bp[["partial_upper"]]   %||% 90)
  s$model <- build_siamese_model(
    emb_dim = mp$embedding_dim, proc_no_dim = mp$proc_no_dim,
    oc_dim = mp$oc_dim %||% length(s$oc_levels),
    dense_units_1 = bp[["dense_units_1"]], dense_units_2 = bp[["dense_units_2"]],
    dense_units_3 = bp[["dense_units_3"]],
    dropout_1 = bp[["dropout_1"]], dropout_2 = bp[["dropout_2"]], lr = bp[["lr"]],
    use_oc = s$use_oc, use_site = s$use_site,
    site_dim = mp$site_dim %||% length(s$site_levels))
  s$model$load_weights("siamese_model.weights.h5")
  s
}

# Fuzzy classifier using a loaded model's settings
fuzzy_from_settings <- function(data, s, pu = s$best_params[["pri_upper"]],
                                pl = s$best_params[["pri_lower"]],
                                au = s$best_params[["add_upper"]],
                                al = s$best_params[["add_lower"]]) {
  apply_fuzzy_thresholds(data, pu, pl, au, al,
                         oc_enable = s$use_oc, oc_overlap_floor = s$oc_floor,
                         site_enable = s$use_site, site_overlap_floor = s$site_floor,
                         partial_enable = s$use_partial, partial_upper = s$partial_upper)
}

# Classification metrics (truth 0/1, prediction yes/no, optional probability)
compute_metrics <- function(truth_vec, pred_vec, prob_vec = NULL) {
  truth    <- factor(truth_vec, levels = c(0, 1), labels = c("no", "yes"))
  estimate <- factor(ifelse(pred_vec == "yes" | pred_vec == 1, "yes", "no"), levels = c("no", "yes"))
  cm <- caret::confusionMatrix(estimate, truth, positive = "yes")
  out <- tibble::tibble(
    n = length(truth_vec),
    tp = cm$table["yes","yes"], fp = cm$table["yes","no"],
    fn = cm$table["no","yes"],  tn = cm$table["no","no"],
    accuracy = cm$overall[["Accuracy"]],
    sensitivity = cm$byClass[["Sensitivity"]], specificity = cm$byClass[["Specificity"]],
    ppv = cm$byClass[["Pos Pred Value"]], npv = cm$byClass[["Neg Pred Value"]],
    f1 = cm$byClass[["F1"]], balanced_acc = cm$byClass[["Balanced Accuracy"]],
    auc = NA_real_)
  if (!is.null(prob_vec) && length(unique(truth_vec)) == 2) {
    roc_obj <- tryCatch(pROC::roc(as.integer(truth == "yes"), prob_vec, quiet = TRUE),
                        error = function(e) NULL)
    if (!is.null(roc_obj)) out$auc <- as.numeric(pROC::auc(roc_obj))
  }
  out
}
