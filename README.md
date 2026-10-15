# Data-driven identification of urgent emergency and trauma surgery from booking priority

R scripts (and one Python helper) to build a list of urgent surgical procedures
from surgeons' emergency booking priorities, using routinely collected theatre
booking and coded hospital data.

## Papers

This work extends the single-site pilot study:

> Miller M, Jorm L, Gallego B. Data-driven identification of urgent surgical
> procedures for use in trauma outcomes measurement. *Trauma Surg Acute Care
> Open*. 2025;10:e001783. doi:10.1136/tsaco-2025-001783
> ([tsaco.bmj.com/content/10/2/e001783](https://tsaco.bmj.com/content/10/2/e001783))

The scripts in this repository are for the multi-site follow-up study, which
adds an automated method for matching booked procedures to coded procedures. A
link to that paper will be added here when it is published.

The original pilot study scripts are in the `Pilot study scripts` folder, with
their original instructions (the pilot README).

## Running the pipeline

Put every script, both helper files, `embed.py` and your input files in **one
working directory** and run the scripts in number order from that directory
(e.g. open an RStudio project there). Scripts read and write files by name only;
there are no paths to edit. The two helper files are loaded with `source()`:
`matching_functions.R` (text processing, rule-based classifier and neural
network, used by scripts 02–07) and `list_functions.R` (analysis dataset and
urgent-list method, used by scripts 08–11).

| Step | Scripts | |
|---|---|---|
| Build the study dataset | 01 | required |
| Match booked to coded procedures | 02–07 | required |
| Urgent procedure lists | 08–10 | required |
| Sex, age and hospital analysis | 11 | optional |

## Input data

None of the study data are distributed with this repository. You need to supply:

| File | Used by | Contents |
|---|---|---|
| `bookings.csv` | 01 | Emergency theatre bookings |
| `diagnoses.csv` | 01 | Coded ICD-10-AM diagnoses |
| `procedures.csv` | 01 | Coded ACHI procedures |
| `achi_codes.csv` | 01, 02, 07, 08–11 | ACHI code table (licensed; not included) |
| `procedure_exclusions.csv` | 01, 02 | Non-operative ACHI codes to remove |
| `matching_labels.csv` | 02 | Hand-labelled booking–procedure pairs for training the matcher |

Synthetic examples of the three raw files (`bookings.csv`, `diagnoses.csv`,
`procedures.csv`; 20 bookings) are in `example_data/`. They contain no real
patient data, and the ACHI codes are illustrative only. They show the expected
layout and cover the main cases script 01 handles: patients without a trauma
diagnosis (excluded), a booking with no coded admission (counted as unlinked),
a second booking in the same admission, a site that numbers the principal
procedure `1` rather than `P`, anaesthesia and allied-health codes that are
removed by `procedure_exclusions.csv`, and a code written without its hyphen.
`achi_txt` is left blank because ACHI descriptions are licensed. To run the
pipeline on them you also need `achi_codes.csv`, `procedure_exclusions.csv` and,
for the matching scripts, `matching_labels.csv`.

All three raw files share a de-identified patient key, `study_id`. Bookings and
coded procedures are linked on `study_id` plus `admit_dt_tm`, so the admission
date-time must be recorded identically in both sources. If your sources only
share an admission date, convert both to dates before running script 01 and
link on that instead.

### `bookings.csv`

One row per emergency theatre booking, from the operating-theatre management
system. Only bookings made on the day of admission are used to build the
urgent lists, but all bookings can be supplied.

| Column | Type | Description |
|---|---|---|
| `study_id` | text | De-identified patient key |
| `hosp` | text | Site code (e.g. `A`, `B`). Used as a covariate only; lists are not produced per site |
| `admit_dt_tm` | date-time | Admission date-time (`YYYY-MM-DD HH:MM:SS`) |
| `case_date` | date | Date of surgery |
| `age` | number | Age in years at admission |
| `sex` | text | `M`/`Male`, `F`/`Female`, `I`/`Other` |
| `asa` | text | ASA physical status as recorded, e.g. `1`, `2E`, `3 - Moderate to Severe Systemic Disease`, `No anaesthetist` |
| `specialty` | text | Booking surgical specialty, as recorded |
| `procedural_consultant` | text | Pseudonymised identifier of the booking surgeon |
| `planned_procedure` | text | Free-text procedure entered at booking |
| `emerg_priority` | text | Booking priority, as recorded locally |
| `emerg_priority_collapse` | text | Booking priority mapped to one of `<1hr`, `<4hrs`, `<8hrs`, `<24hrs`, `<72hrs` |
| `emerg_priority_mins` | number | Target time for the booked priority, in minutes |
| `admit_to_book_pd` | text | Admission-to-booking interval: `same day`, `same week` or `over 1 week` |
| `book_to_surgery_pd` | number | Minutes from booking to start of surgery |
| `booking_surgery_diff` | number | `book_to_surgery_pd` minus `emerg_priority_mins` |
| `booking_surgery_cat` | text | Whether surgery started within the booked target (`Undertime` / `Overtime`) |

Local priority categories differ between hospitals and must be mapped to the
five standard bands before use. For example: `A-1hr Life threatening` → `<1hr`;
`<=4hrs-Organ or limb threatening` and `C-4hrs Critical` → `<4hrs`.

### `diagnoses.csv`

One row per coded ICD-10-AM diagnosis, from the hospital's admitted patient
(coded episode) data. Used only to identify trauma patients.

| Column | Type | Description |
|---|---|---|
| `study_id` | text | De-identified patient key |
| `icd` | text | ICD-10-AM code, any case, with or without a dot (e.g. `S72.00`, `s7200`) |

A patient is in the trauma cohort if any diagnosis is an injury code (S, T) or
external-cause code (U, V, W, X). Three-character codes for complications of
care and adverse drug effects are excluded first (listed in script 01).

### `procedures.csv`

One row per coded ACHI procedure for each admission.

| Column | Type | Description |
|---|---|---|
| `study_id` | text | De-identified patient key |
| `admit_dt_tm` | date-time | Admission date-time; must match `bookings.csv` |
| `achi_no` | text | ACHI code, `12345-00` or `1234500` |
| `achi_txt` | text | ACHI description; may be blank, in which case it is filled from `achi_codes.csv` |
| `proc` | text | Position in the coded record: `P` for the principal procedure, otherwise `1`, `2`, `3` ... If no `P` is supplied for an admission, `1` is treated as principal |

### `achi_codes.csv`

The ACHI code table for the edition used in your data. This is licensed by IHACPA
and is **not** included here.

| Column | Description |
|---|---|
| `achi_proc_code` | ACHI code, `12345-00` |
| `procedure_text` | Code description |
| `block_number` | ACHI block number |
| `block_text` | Block description |
| `procedure_chapter` | Chapter, e.g. `10 Procedures on digestive system` |
| `procedure_sub_chapter` | Sub-chapter including block range, e.g. `0870-0890 Stomach` |

### `matching_labels.csv`

A sample of candidate pairs from `study_data.rds`, each reviewed by a clinician
and marked as a true match (the booked procedure is the one coded) or not.

| Column | Description |
|---|---|
| `row_uid` | Unique row identifier |
| `study_id` | De-identified patient key |
| `planned_procedure` | Booking free text, as in `study_data.rds` |
| `achi_no` | Candidate ACHI code |
| `achi_txt` | Candidate ACHI description |
| `correct` | `yes`/`y` if the pair is a true match, `no`/`n` if not |

### `procedure_exclusions.csv`

ACHI codes removed before matching because they are not operative procedures
(anaesthesia, airway management, imaging, pharmacotherapy, allied health,
measurement and other non-operative codes). Codes with `exclude = FALSE` are
listed for transparency but kept.

| Column | Description |
|---|---|
| `achi_proc_code` | ACHI code |
| `category` | Exclusion category |
| `exclude` | `TRUE` to remove |
| `reason` | Short justification |

## Pipeline

### 01_build_study_dataset.R

1. Identifies the trauma cohort from `diagnoses.csv`.
2. Joins each booking to every coded procedure from the same admission. Bookings
   with no matching coded admission are counted and printed by site, then dropped.
3. Removes excluded ACHI codes.
4. Derives age group (`paeds` < 16, `adult` 16–64, `elderly` ≥ 65), year of
   surgery and a standardised procedure position. Age is top-coded at 100.
5. Assigns `study_row_uid`, the row key carried through every later script.

Output: `study_data.rds`. Each row is a candidate booking–procedure pair.
It contains the booking columns (without `case_date`) plus `achi_no`,
`achi_txt`, `proc`, `age_gp`, `case_yr`, `proc_no_reduced` (`primary`,
`additional_procedure_1` … `additional_procedure_9+`) and `study_row_uid`.

### Matching booked procedures to coded procedures (02–07)

Script 01 pairs every booking with every coded procedure from the same
admission. Scripts 02–07 decide which pair is the true match, using a two-stage
classifier:

1. **Fuzzy rules.** String-similarity scores (`token_set`, `token_sort`,
   `partial_token_set`) between the cleaned booking text and the ACHI text.
   Pairs above an upper cut-off are matches, below a lower cut-off are not;
   primary and additional procedures have separate cut-offs. Clinical rules
   then force a non-match when an open procedure was booked but a closed one
   coded, or when booking and code refer to different limbs.
2. **Siamese neural network.** Pairs the rules leave undecided are scored by a
   network using BioClinical-ModernBERT embeddings of both texts, the
   procedure position and the open/closed and anatomical-site relations.

| Script | Purpose | Main outputs |
|---|---|---|
| `02_matching_preprocess.R` | Cleans and tags the labelled pairs: abbreviation expansion, open/closed and anatomical-site tags, rule-based rewrites using ACHI chapter and block | `labeled_proc.rds` |
| `03_matching_embeddings.R` | Fuzzy scores; calls `embed.py` for text embeddings | `train_pairs.csv`, `train_*.npy`, `train_metadata.json` |
| `04_matching_train_validate.R` | 80/20 split; Optuna and grid-search tuning within the 80%; 5-fold CV; one evaluation on the 20% holdout; final model on all rows | `final_holdout_metrics.csv` (reported performance), `cv_summary.csv`, `model_params.rds`, `siamese_model.weights.h5` |
| `05_matching_threshold_sensitivity.R` | Performance across fuzzy cut-offs; misclassified pairs | `threshold_sensitivity.csv`, `misclassifications.csv`, plots |
| `06_matching_metrics.R` | Error rates by stratum; hybrid vs network-only comparison (descriptive) | `full_dataset_metrics_DESCRIPTIVE_ONLY.csv`, `*_strata.csv`, `dnn_only_*` |
| `07_matching_deploy.R` | Applies the matcher to every pair in `study_data.rds` | `predictions.csv` |

`predictions.csv` holds one row per pair with `study_row_uid`,
`predicted_correct` (`yes`/`no`), `probability` and `classified_by`
(`fuzzy`/`dnn`). The analysis scripts keep only pairs predicted `yes`.

Only `final_holdout_metrics.csv` should be reported as matcher performance. The
metrics from scripts 05 and 06 use the deployment model on the rows it was
trained on.

#### Python setup

Scripts 03–07 call Python through `reticulate`. Create an environment with
`numpy`, `pandas`, `torch`, `transformers`, `tqdm`, `fuzzywuzzy`
(`python-Levenshtein` recommended), `optuna` and a Keras 3 backend for the R
`keras3` package, then uncomment and edit the `use_condaenv()` line at the top
of each script. `embed.py` downloads `thomas-sounack/BioClinical-ModernBERT-base`
from Hugging Face on first use; to run offline, set `bert_model` in scripts 03
and 07 to a local copy of the model.

### Urgent procedure lists (08–10)

The analysis keeps only booking–procedure pairs predicted as matches
(`predicted_correct == "yes"`), so each row is one booking linked to the ACHI
code performed. Lists use bookings made on the day of admission.

For each ACHI procedure, the bookings are tabulated across the five priority
bands. A procedure is **urgent by agreement** if at least 75% of bookings are
within the cut-off, and **urgent by consensus** if more than 50% are within the
cut-off and the Tastle–Wierman consensus score is below 0.7. Both the urgent
percentage and the agreement and consensus scores treat the bands within the
cut-off as one: at 4 hours `<1hr` and `<4hrs` are combined; at 8 hours `<1hr`,
`<4hrs` and `<8hrs`; at 24 hours all bands up to `<24hrs`. The urgent
percentage is reported with an exact binomial 95% CI.

Agreement and consensus are calculated from the number of bookings in each band,
with the bands in time order. This corrects the pilot study scripts, which
passed a cross-tabulation of band by count to `agreement()` and `consensus()`;
there, each band carried equal weight whatever its number of bookings, so the
Agreement and Consensus values, and the consensus-based lists, will differ from
the 2025 paper. Lists based on the agreement threshold are unaffected.

The cut-off is an argument. `build_urgent_lists(study_db, urgency = 4)` (in
`list_functions.R`) produces every list and table for one cut-off; `urgency`
can be 1, 4, 8 or 24 hours, and the agreement and consensus thresholds are also
arguments. Script 08 runs it for each value in `cutoffs` (default
`c(4, 8, 24)`); edit that line, or call the function yourself, for other
cut-offs. Scripts 10 and 11 have their own `urgency` setting (default 4).

**Small counts are not masked.** The scripts export full counts so the
outputs can be used and checked directly. In the published paper, counts below
10 (procedure bookings, booking surgeons and descriptive table cells) were
suppressed under the ethics approval, so outputs from these scripts will show
values that appear as `<10` in the paper. Apply your own disclosure rules before
publishing results. Lists are not produced per hospital.

| Script | Purpose | Main outputs |
|---|---|---|
| `08_urgent_lists.R` | Matching summary; for each cut-off, the full, age-group and sex lists, a list based on observed booking-to-surgery time, specialty mix and agreement/consensus threshold sensitivity | per cut-off `h`: `urgent_surgery_procedure_lists_<h>hr.xlsx`, `urgent_surgery_other_results_<h>hr.xlsx`, `list_objects_<h>hr.rds` |
| `09_table1_demographics.R` | Table 1, one row per patient from the principal procedure's booking | `table1_demographics.xlsx` |
| `10_priority_vs_time.R` | Booking priority vs time to surgery at one cut-off: weighted kappa, Stuart–Maxwell, McNemar; agreement between the two urgent lists | `priority_vs_time_results_<h>hr.xlsx` |

Script 10 reads `list_objects_<h>hr.rds`, so run 08 with that cut-off first.

### Sex, age and hospital analysis (11, optional)

`11_sex_age_hospital_models.R` asks whether booking urgency (urgent = booked
within the cut-off, default 4 hours) differs by sex, age group or hospital once
the procedure is accounted for. Bookings with sex recorded as `I`/`Other` are
excluded. It uses procedures with ≥10 bookings and ≥3 bookings of the less
common sex, and fits three multilevel logistic models: a procedure random
intercept (m0), plus sex and age group (m1), plus a hospital random intercept
(m2). It reports odds ratios, likelihood-ratio tests, variance partition
coefficients and median odds ratios (parametric bootstrap, 2,000 replicates),
then repeats the hospital comparison on procedures with ≥5 bookings at every
hospital, and the models on procedures with ≥25 bookings.
Output: `sex_age_hospital_models.xlsx`.

## R packages

`tidyverse`, `janitor`, `hablar`, `rstatix`, `agrmt`, `openxlsx`, `vcd`,
`DescTools`, `lme4`, `optimx` (lists and analysis); `pacman`, `tm`,
`reticulate`, `keras3`, `jsonlite`, `processx`, `caret`, `pROC`, `scales`
(matching).

## Use of AI

The scripts in this repository were proofread, tidied and commented with the
help of Claude Opus 5.5 (Anthropic) on 14–15 October 2026. This README was
drafted with, and proofread by, the same model to check it is consistent with
the scripts. All analytical decisions and results are the authors' own.

## Licence

GPL-3.0. See `LICENSE`.
