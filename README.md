## **Data-driven Identification of Urgent Surgical Procedures For Use in Epidemiological Studies in Trauma**

  

_Matthew Miller, Louise Jorm and Blanca Gallego_

  

The accompanying series of R scripts are used to create a list of urgent surgery from surgical booking priority. They can be used for all surgical patients, or limited to those admitted with trauma. Options are included to change the agreement cut off, consensus cut off, age group, and whether to use trauma-only or all surgical patients. Instructions are also given on how to alter the classification of urgency to suit bespoke definitions. R code to generate lists for specific age groups and genders are also provided.

  

Note: unfortunately the ACHI procedure code and ICD diagnosis code tables need to be obtained separately as we don't have a licence to distribute them here.

  

These scripts are written for an Australian study and modifications will be needed for other countries/hospitals. In particular:

  

1. Trauma is defined here as any patient admitted with a diagnosis code from the ICD-10-AM (11th edition) Chapters 19 (Injury, Poisoning, and certain other consequences of external causes) and 20 (External causes of morbidity and mortality). Codes related to hospital complications are excluded as part of the script. This will need to be altered for international codes such as the ICD-10-CM in script 2.
2. The procedure codes used in these scripts are for the Australian Classification of Health Interventions. To use ICD-10-AM procedure codes or ICD-11 Classification of Health Intervention codes you will need to modify script 2.
3. The surgical priorities will be institution specific and will need updating and collapsing. In our institution these were: "<=15minutes-Immediate life threatening", "A-1hr Life threatening incl obstetric", "<=1hr-Life threatening conditions", "B-2hrs Highly critical organ/limb threat", "<=4hrs-Organ or limb threatening", "C-4hrs Critical", "<=8hrs-Non-critical but emergent" , "D-8hrs Urgent", "<=24hrs-Non critical, non-emergent", "E-24hrs Semi-Urgent", "<=72hrsCannot discharge before procedure", "F-72hrs Non-Urgent". You will need to modify script 1, starting around line 33, and change the _case\_when_ criteria. The right hand arguments must remain as "<1hr","<4hrs", "<8hrs", "<24hrs","<72hrs".

## Structure of raw data files

The tables below outline the columns in the databases, including names and levels, that are needed for the scripts to work. Examples using synthetic (fake) data are also given in the file repository.

  

#### Study Data

| **Column name** | **Description** |
| ---| --- |
| study\_id | Anonymised ID |
| sex | M, F |
| asa | "1 - Healthy"<br>"2 - Mild Systemic Disease"<br>"3 - Moderate to Severe Systemic Disease"<br>"4 - Severe Systemic Disease"<br>"5 - Moribund"<br>"See anaesthetist record"<br>"No anaesthetist"<br>"6 - Organ Donor/Transplant" |
| age | In years |
| admit\_dt\_tm | POSIXct YMD HMS |
| case\_date | Date |
| emerg\_book\_dt\_tm | POSIXct YMD HMS |
| admit\_to\_book\_pd | From script 1. "same day", "same week", "over 1 week" |
| specialty | Bespoke to your data |
| procedural\_consultant | Alphanumeric from script 1 |
| planned\_procedure | Free text |
| primary\_procedure | Free text |
| emerg\_priority | Bespoke from your data |
| emerg\_priority\_collapse | From script 1. Must be one of "<1hr","<4hrs", "<8hrs", "<24hrs","<72hrs" |
| episode\_number | numeric |
| coded\_primary\_procedure | ACHI / Procedure code |
| episode\_start\_date | Date |
| additional\_procedure\_1 | ACHI / Procedure code |
| additional\_procedure\_2 | ACHI / Procedure code |
| additional\_procedure\_3 | ACHI / Procedure code |
| additional\_procedure\_4 | ACHI / Procedure code |
| additional\_procedure\_..... | ACHI / Procedure code. No limit on additional columns |
| principle\_diagnosis | ICD code |
| additional\_diagnosis\_1 | ICD code |
| additional\_diagnosis\_2 | ICD code |
| additional\_diagnosis\_3 | ICD code |
| additional\_diagnosis\_4 | ICD code |
| additional\_diagnosis\_...... | ICD code. No limit on additional columns |

#### Procedure data.

| **Column name** | **Description** |
| ---| --- |
| achi\_proc\_code | ACHI procedure code |
| procedure\_text | text |
| block\_number | Number (from ACHI block) |
| block\_text | text |
| procedure\_chapter | Text |
| procedure\_sub\_chapter | Text |
| icd10\_ver | ICD-10 version |

#### ICD-10 data

| **Column name** | **Description** |
| ---| --- |
| four\_dgt\_no | ICD-10 four digit code |
| four\_dgt\_text | Text |
| three\_dgt\_no | ICD-10 three digit code |
| three\_dgt\_text | Text |
| diagnosis\_chapter | Text |
| diagnosis\_subchapter | Text |

## The Scripts.

**Script 1. Preprocessing the data.**

  

The script focusses on preprocessing and summarising data related to study admissions and procedural priorities, collapsing and categorising emergency priorities, and ensuring data security by hashing certain variables. The aim is to make the urgency factor levels consistent with the remaining scripts, to anonymise the procedural consultant and convert the admission time to "same day" or not categories for filtering later.

1. Vector Definition:
    *   Defines a character vector `id_chrs` containing alphanumeric characters (0-9 and a-z), to be used in creating study IDs.Urgency Levels:
    *   Defines a vector `urgency_levels_collapsed` representing collapsed urgency levels.
2. Reads Study Database:
    *   Reads a study database from an Excel file named "study\_db.xlsx" into a data frame called `study_db`.
3. Calculates Admission to Booking Period:
    *   Computes the time difference in hours between the columns `emerg_book_dt_tm` and `admit_dt_tm` and creates a new variable `admit_to_book_pd` representing the admission to booking period.
4. Categorizes Booking Periods:
    *   Creates categories for the admission to booking period based on time duration ("same day," "same week," or "over 1 week").
5. Frequency Table:
    *   Prints a frequency table for the variable `admit_to_book_pd` to summarize the distribution of booking periods.
6. Filters and Collapses Data:
    *   Filters the study database to include only cases with the admission to booking period as "same day."
    *   Groups the data by `study_id`, `age`, and `admit_dt_tm`.
    *   Selects the first case within each group based on the `case_date`.
    *   Collapses emergency priorities into broader categories (e.g., "<1hr", "<4hrs") using the `emerg_priority_collapse` variable.
    *   Converts `emerg_priority_collapse` into a factor variable with specified levels.
7. Anonymise Procedural Consultant:
    *   changes the `procedural_consultant` variable to an alphanumeric character.

  

### **Script 2. Processing procedure data and creating the function.**

This is divided into three parts.

  

#### Part 1

processes procedure data, cleans and matches procedure descriptions.

  

1. Loading Study Data:
    *   The study data is loaded into a variable named `study_db_all`.
2. Downloading ACHI and ICD Codes:
    *   ACHI codes are stored in the `achi_db` variable.
    *   ICD-1-AM codes are stored in the `icd_db` variable.
3. Removing Unwanted Procedure Codes:
    *   Certain procedure codes related to anesthesia, airway management, ventilatory support, vascular access device, etc., are identified and removed from the `achi_db,` combined into a single list called `codes_to_remove`.
4. Extracting Primary Procedure Data:
    *   The primary procedure data is extracted from the `study_db_all` variable and stored in `study_db_coded_primary`.
5. Matching Procedure Codes:
    *   Procedure codes are matched based on both planned and primary procedures as we had two columns that describe the booked procedure. You may need to use only the first one.
    *   Stopwords are removed, and text cleaning is performed on free-text descriptions.
    *   Common words between planned and actual procedures are identified for both planned and primary procedures.
    *   The resulting data frames are named `planned_achi` and `primary_achi`.
6. Creating ACHI Coded Database:
    *   A new database (`achi_study_db`) is created by combining the primary procedure data with planned procedures for each study.
    *   Duplicate entries are removed, and the column is renamed as `booked_procedure`. The `booked_procedure` column is added back to the original study database (`study_db_all`) and unwanted codes specified in `codes_to_remove` are filtered out.
7. Counting Procedure Additions:
    *   The number of procedures for each study is counted, and the maximum count is retained.
    *   The script prints the count of procedures for each study.
8. Checking for missing Values:
    *   The script checks for missing values in the `booked_procedure` column. The final step involves filtering out rows where the `booked_procedure` column is missing.

  

#### Part 2

is an R function, named `get_list_of_procedures`, summarising surgical procedure data. It has the following options:

  

| _cohort_ | can be "all patients" or "trauma" . If all patients, then an extra step in the function classifies the patients as trauma or not for the final table. |
| ---| --- |
| _agreement\_threshold_ | 0-100 |
| _consensus\_threshold_ | 0-1 |
| _urgency_ | can be 1,4,8,12,24 or 72 depending on the time period you are considering as urgent. For example, if it is all procedures booked within a 4-hour time period, then urgency = 4. |
| _show\_figures\_tables_ | TRUE or FALSE if you want some summary information |

The steps are:

  

1. Trauma Identification:
    *   The function starts by identifying trauma cases in the dataset. It uses specific diagnosis codes to classify episodes of care as trauma or non-trauma.
    *   The identified trauma study IDs are stored in the variable `trauma_study_ids`.
2. Filtering Dataset:
    *   The function then filters the original study database (`study_db`) to include only cases with trauma study IDs (`study_db_trauma`).
3. Main Functionality:
    *   The main functionality of the function is to create a table of procedures and urgency levels based on specified criteria such as agreement and consensus thresholds.
    *   It provides an option to filter data based on age range and urgency level.
    *   It calculates various statistics, including the number of unique procedures, unique booking proceduralists, and individual patients.
    *   It generates tables and figures summarizing the urgency levels, agreement, and consensus among proceduralists.
    *   It assesses the distribution of urgency levels across all booked procedures.
4. Trauma Analysis (Optional):
    *   If the analysis is for the entire cohort (`cohort == "all patients"`), the function further analyzes trauma cases within the dataset.
    *   Trauma cases are identified based on diagnosis codes, and the results are added to the main dataframe.
5. Primary Procedure Identification:
    *   The function identifies primary procedures by comparing booked procedures with a list of primary procedures.
    *   It calculates confidence intervals for the proportion of urgent cases and adds this information to the main dataframe.

  

#### Part 3.

runs the `get_list_of_procedures` function. Code is also provided to create urgency tables and save them to an excel workbook, and to create age group and gender specific lists.

  

1. Run Procedure Analysis Function for Trauma:
    *   The `get_list_of_procedures` function is applied.
2. Join ACHI Code Descriptions:
    *   The resulting list of procedures (`full_list`) is joined with the ACHI code descriptions (`achi_text`), providing additional information about each procedure.
3. Filter Procedures by Urgency (Agreement and Consensus):
    *   Two subsets of procedures are created based on urgency classification:
        *   `full_list_urgent`: Procedures classified as urgent by agreement.
        *   `full_list_urgent_consensus`: Procedures classified as urgent by consensus.
4. Frequency Tables for Trauma and Primary Procedures:
    *   Frequency tables are generated for trauma cases and primary procedures for both the agreement and consensus subsets.
5. Generate Lists for Different Age Groups:
    *   Lists of procedures are generated for different age groups, including pediatrics (`paeds_list`), adults (`adult_list`), and the elderly (`elderly_list`).
    *   Urgent procedures for each age group are also identified and categorized.
6. Identify Differences in Procedures Between Age Groups:
    *   Differences in procedures between age groups are identified, and separate data frames are created to show procedures specific to each age group.
7. Frequency Tables for Differences in Age Groups:
    *   Frequency tables are generated to show the distribution of procedures that are specific to age groups, considering trauma and primary procedure classifications.
8. Generate Lists for Different Gender Groups:
    *   Lists of procedures are generated for male and female trauma patients.
9. Write Results to Excel Workbook:
    *   The results, including urgent procedures, additional procedures by consensus, and various age and gender-specific lists, are written to an Excel workbook named "urgent\_surgery\_lists.xlsx."

  

### **Script 3. Demographic analysis and regression modelling.**

This part produces demographic data and some regression analysis. The final part aims to analyse and visualise the distribution of urgency levels based on different consensus cutoffs in a trauma cohort, to help decide an appropriate consensus threshold.

#### Demographic Analysis

1. Study Group Selection:
    *   A trauma-only study group (`study_db_trauma`) is created by filtering the original study database (`study_db`) to include only trauma cases.
2. Demographics Data Preparation:
    *   Unique entries of trauma study IDs are selected from the trauma-only database (`study_db_trauma`).
    *   Non-trauma patients are identified by subtracting trauma patients from the original study group.
3. ICD Code Processing:
    *   Injury and mechanism codes are combined, and specific codes are removed based on predefined sets (`ts`, `ys`, `us`, `xs`).
    *   Trauma identification is performed using injury and mechanism codes, and a new dataset (`trauma1`) is created.
4. Demographic Variables Selection:
    *   Relevant demographic variables are selected, including age, sex, ASA (American Society of Anesthesiologists) score, specialty, and emergency priority.
5. Demographic Summary Statistics:
    *   Summary statistics and histograms are generated for age.
    *   Frequency tables are created for ASA score, trauma status, sex, and emergency priority.
6. Specialty Data Processing:
    *   Specialty names are standardized, and a frequency table is generated.
7. Urgent Specialty Analysis:
    *   A list of urgent specialty frequencies is created based on urgent procedures identified earlier.
8. Results Export:
    *   Lists of specialty frequencies for urgent and entire study groups are saved to an Excel workbook (`specialty_lists.xlsx`).

#### Regression Analysis for Trauma Cohort

1. Procedure Selection for Regression:
    *   Procedures with a minimum number of raters and those with both male and female patients are identified.
2. Multilevel Logistic Regression Models:
    *   Several logistic regression models are fitted, considering different fixed and random effects.
    *   Null model is assessed for Intraclass Correlation Coefficient (ICC).
    *   Models are compared using ANOVA.
3. Odds Ratios Calculation:
    *   Odds ratios with confidence intervals are calculated and presented for significant predictors.

### Regression Analysis for Entire Cohort

1. ICD Code Processing for Entire Cohort:
    *   Similar ICD code processing is performed for the entire cohort (`study_db_all`).
2. Procedure Selection for Regression (Entire Cohort):
    *   Procedures with a minimum number of raters are identified.
3. Multilevel Logistic Regression Models (Entire Cohort):
    *   Similar steps are followed as in the trauma cohort analysis.
4. Odds Ratios Calculation (Entire Cohort):
    *   Similar odds ratios and confidence interval calculations are performed.

  

#### Threshold Analysis

1. A function named `table_of_n` is defined to calculate and create a table of procedure counts based on specified agreement and consensus thresholds.
2. Two vectors (`a_th` for agreement thresholds and `c_th` for consensus thresholds) are defined.
3. The `map2` function is used to apply the `table_of_n` function to all combinations of agreement and consensus thresholds, and the results are combined into a table (`table_of_counts`).

#### Consensus Analysis and Visualization

1. Urgency levels (`<1hr`, `<4hrs`, `<8hrs`, `<24hrs`, `<72hrs`) are defined.
2. Different consensus thresholds are evaluated (0.6, 0.65, 0.7, 0.75), and procedures meeting the urgent surgery criteria are filtered accordingly.
3. The results are combined into a data frame (`consensus_plots`) and visualized using density plots and jitter plots for each urgency level.
4. Two plots are generated: one displaying the distribution of proportion of responses for each urgency level by consensus cutoff, and another showing the distribution for each urgency level, with each point representing a surgical procedure.
