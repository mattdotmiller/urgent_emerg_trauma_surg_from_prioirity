# you may need to install the pacman package
#these packaged are a generic list I install for any session, not all are used
library(pacman)
p_load(tidyverse, checkmate, stringr, lubridate, gdata, readxl, gmodels, naniar, openxlsx, ggpubr, janitor, skimr, REDCapR, hablar, mefa)
p_load(rebus, rstatix, digest)
p_load(agrmt)
p_load(lme4, optimx)


# get and rename study data ----------------------------------------------------

study_db_all <- study_db #if moving straight on from script 1

study_db_all <- readRDS("study_db_all.RDS") #assumes it is in the working directory

# download ACHI codes -----------------------------------------------------

achi_db <- path to your table of procedure codes

# download ICD-1-_AM codes -----------------------------------------------------

icd_db <- path to your table of ICD-10 diagnosis codes

# codes to remove ---------------------------------------------------------
#these codes are then removed
#95550-01 to 95550-16 = generalised allied health interventions, 92062-00 = administration of blood and blood products, 13882-01 = venitaltory support
#13882-02 = block sympathetic nerve, 60503-00 = fluroscopy, 18270-00 = Administration of anaesthetic agent around femoral nerve,13706-02 = administration red cells
#13706-05 = Administration of gamma globulin
#18274-01  = Administration of anaesthetic agent around paravertebral thoracic nerve, 90022-00 = Administration of anaesthetic agent around other peripheral nerve
#92209-01 = Management of noninvasive ventilatory support, more than 24 but less than 96 hours, 92209-00 = Management of noninvasive ventilatory support, 24 hours or less.
#13882-02 = Management of continuous ventilatory support, 96 hours or more, 92209-02 = Management of noninvasive ventilatory support, 96 hours or more
#13882-02 = Management of continuous ventilatory support, more than 24 hours but less than 96 hours
#36800-00 = bladder catheterisation

# codes from blocks 1820:1923 ("not elsewhere classified"), 1786:1800 (oncology procedures),  imaging 1940:2016

other_codes <- c("95550-01", "95550-02", "95550-03", "95550-04", "95550-05", "95550-06", "95550-07", "95550-08", "13706-02",
                 "95550-09", "95550-10", "95550-11", "95550-12", "95550-13", "95550-14", "95550-15", "95550-16",
                 "92062-00", "13882-01", "13882-02", "60503-00" , "18270-00" ,
                 "18274-01"  , "90022-00", "92209-01", "92209-00", "13882-02" , "92209-02",
                 "13882-02", "13706-05", "36800-00")

# filter out procedures related to the above blocks and procedures
codes_to_remove1 <- achi_db %>%
  filter(str_detect(block_text, "anaesthesia|Airway management|Ventilatory support|
                    ventilatory support|Vascular access device"))

blocks_nec <- 1820:1923 #blocks not elsewhere classified
imaging_blocks <- 1940:2016
onc_blocks <- 1786:1800

blocks_to_filter <- c(blocks_nec, imaging_blocks, onc_blocks) #combine to a single block
rm(blocks_nec, imaging_blocks, onc_blocks) #remove the components

codes_to_remove2 <- achi_db %>%
  filter(block_number %in% blocks_to_filter)

codes_to_remove1 <- as.vector(unique(codes_to_remove1$achi_proc_code))
codes_to_remove2 <- as.vector(unique(codes_to_remove2$achi_proc_code))
codes_to_remove <- c(codes_to_remove1, codes_to_remove2, other_codes) #combine to a single block

rm(codes_to_remove1, codes_to_remove2)#remove the components

#replaced the codes to remoe with an NA
study_db_all <- study_db_all %>%
  mutate(across(matches("_procedure_|coded_primary_procedure"), ~ replace(., . %in% codes_to_remove, NA)))



# create trauma only study_db ---------------------------------------------

# rather than list the codes to include, we instead list the codes to remove as this is easier
ts <- c(36:50, 51:65, 74, 78, 80:88, 90:98)
ys <- c(06, 07, 10:19, 36, 37, 40:98)
us <- c(04, 06, 07, 78:92)
xs <- c(40:49)
#xs_poisoning <- c(60:69)

icds_to_remove <- c(str_c("t", ts, sep = ""), str_c("y", ys, sep= ""), str_c("u", us, sep= ""), str_c("x", xs, sep= ""))

#this script takes all of the diagnosis codes and looks for trauma codes then classifies the episode of care. 
trauma1 <- study_db %>%
  select(study_id, contains("diagnosis"))  %>%
  mutate(across(contains("diagnosis"), .fns =tolower)) %>%
  pivot_longer(-study_id, names_to = "diagnosis_code", names_prefix = "diagnosis_code", values_to = "code") %>%
  filter(code !="") %>%
  mutate(short_idc = str_extract(string = code, pattern = START %R% ANY_CHAR %R% DGT %R% DGT))%>%
  filter(!short_idc %in% icds_to_remove) %>%
  mutate(trauma_by_inj = if_else(str_detect(string = code, pattern = START %R% or("s","t")), TRUE, FALSE, FALSE))%>%
  mutate(trauma_by_mech = if_else(str_detect(string = code, pattern = START %R% or("u", "v", "w", "x")), TRUE, FALSE, FALSE))%>%
  mutate(trauma = if_else(trauma_by_mech== TRUE| trauma_by_inj == TRUE, TRUE, FALSE, FALSE)) %>%
  distinct(study_id, .keep_all = TRUE) %>%
  select(study_id, trauma) %>%
  filter(trauma == TRUE)

#extract the trauma study IDs
trauma_study_ids <- as.vector(trauma1$study_id)

#filter the study database to only those with a trauma ID
#note that the study ID comes from the patient and admission date so subsequent
#admissions for the same patient but not trauma should not be included
study_db_trauma <- study_db_all %>%
  filter(study_id %in% trauma_study_ids) %>%
  mutate(trauma_pt = TRUE)

# define the booked procedures ---------------------------------------------------
proc_to_check <- study_db_trauma %>%
  select(study_id, planned_procedure, coded_primary_procedure, contains("_procedure_"), trauma_pt) %>%
  pivot_longer(cols = c(coded_primary_procedure, contains("_procedure_")),
                        names_to = "proc_num", values_to = "achi_proc_code") %>%
  filter(!is.na(achi_proc_code)) %>%
  mutate(primary_proc = if_else(proc_num == "coded_primary_procedure", TRUE, FALSE, FALSE)) %>%
  select(-proc_num) %>%
  mutate(correct = NA_character_)

#tack on ACHI text
achi_text <- achi_db %>% select(achi_proc_code, procedure_text)

proc_to_check <- proc_to_check %>%
  left_join(achi_text, by = c("achi_proc_code"))

write.xlsx(proc_to_check, "table_of_proc.xlsx")

#you now need to fill in the correct column and save it as table_of_proc.xlsx

#bring in the labeled correct
table_of_proc <- read.xlsx("table_of_proc.xlsx") %>%
  select(study_id, achi_proc_code, correct, primary_proc) 

study_db_trauma <- study_db_trauma %>%
  pivot_longer(cols = c(coded_primary_procedure, starts_with("additional_procedure")), names_to = "proc_no",
               values_to = "achi_proc_code") %>%
  filter(!is.na(achi_proc_code)) %>%
  left_join(table_of_proc, by = c("study_id", "achi_proc_code")) %>%
rename(booked_procedure = achi_proc_code)%>%
  mutate(site = sample(1000:9999, 1)) %>%
  mutate(study_id_unique = str_c(study_id, row_number())) %>%
  relocate(study_id_unique)


#some summary information about the procedures
study_db_trauma %>%
  filter(trauma_pt == TRUE) %>%
  freq_table(primary_proc)

study_db_trauma %>%
  filter(trauma_pt == TRUE) %>%
  freq_table(primary_proc, correct)

study_db_trauma <- study_db %>%
  filter(trauma_pt == TRUE) %>%
  filter(primary_proc == TRUE & correct == "no")

pri_proc_wrong <- pri_proc_wrong$study_id_full

study_db_trauma %>%
  filter(study_id_full %in% pri_proc_wrong) %>%
  filter(trauma_pt == TRUE) %>%
  filter(primary_proc == FALSE) %>%
  group_by(study_id_full) %>%
  mutate(pri_capt = any(correct == "yes")) %>%
  slice_head(n=1) %>%
  ungroup() %>%
  freq_table(pri_capt)




study_db_trauma <- study_db_trauma %>%
  filter(trauma_pt == TRUE) %>%
  filter(correct == "yes") %>%
  distinct(study_id, record_id, booked_procedure, .keep_all = TRUE)

study_db_trauma %>%
  distinct(study_id) %>%
  nrow()

study_db_trauma %>%
  distinct(booked_procedure) %>%
  nrow()

study_db_trauma %>%
  distinct(procedural_consultant) %>%
  nrow()

# FUNCTION get list of procedures  -----------------------------------------
#this function takes the data and creates the table of procedures and urgency 
get_list_of_procedures <- function(study_db = "study_db_trauma", agreement_threshold, consensus_threshold, 
                                   urgency = 4, age_lower = NA_real_, age_upper = NA_real_, 
                                   show_figures_tables = FALSE, cohort = "all patients") {
  
  #set the list
  
  
  
  #define the main variables from the function arguement
  surgi_db_priority <- study_db
  agreement_threshold <- agreement_threshold
  consensus_threshold <- consensus_threshold
  age_lower <- age_lower
  age_upper <- age_upper
  urgent_value <- urgency
  
  #this is just to test the funciton during writing
  #surgi_db_priority <- study_db_trauma
  #agreement_threshold <- 75
  #consensus_threshold <- 0.7
  #age_lower <- 0
  #age_upper <- 110
  #  urgent_value <- 4
  #  show_figures_tables <- TRUE
  
  #make sure factors are of the class:factor
  surgi_db_priority <- surgi_db_priority %>%
    convert(fct(booked_procedure, emerg_priority, emerg_priority_collapse))
  
  
  #if no age entered, set as the max and min of the dataset
  if (is.na(age_lower)) {
    age_lower <- min(surgi_db_priority$age, na.rm = TRUE)
  }
  
  if (is.na(age_upper)) {
    age_upper <- max(surgi_db_priority$age, na.rm = TRUE)
  }
  
  #print a table of the admission to booking period, should be 100% same day
  table_1 <- surgi_db_priority %>% freq_table(admit_to_book_pd)
  
  if(show_figures_tables == TRUE) {
    
    writeLines("Table of number of cases and time from admission to booking the case. \nShould be 100% the same day")
    print(table_1)
    
  }
  #incase the dataset has longer booking periods, filter to the same day only
  #also filter to ages 
  surgi_db_priority <- surgi_db_priority %>%
    filter(admit_to_book_pd == "same day") %>%
    filter(age >= age_lower, age < age_upper)
  
  # how many individual procedures and booking proceduralists and patients?
  individ_pts <- as.vector(unique(surgi_db_priority$study_id))
  
  if(show_figures_tables == TRUE) {
    writeLines(str_c("Number of unique primary procedures = ", length(unique(surgi_db_priority$booked_procedure))))
    writeLines(str_c("Number of unique booking proceduralists = ", length(unique(surgi_db_priority$procedural_consultant))))
    writeLines(str_c("Number of individual patients = ", length(unique(surgi_db_priority$study_id))))
  }
  # categories are : <=15minutes-Immediate life threatening ; <=1hr-Life threatening conditions ;
  # <=24hrs-Non critical non-emergent; <=4hrs-Organ or limb threatening; <=72hrsCannot discharge before procedure; <=8hrs-Non-critical but emergent 
  
  #summarise the number of raters (to be added in later)
  surgi_db_raters <- surgi_db_priority %>%
    select(booked_procedure, procedural_consultant, age) %>%
    group_by(booked_procedure) %>%
    summarise(num_of_raters = n(),
              num_of_unique_raters = n_distinct(procedural_consultant),
              age = median(age)) %>%
    ungroup() %>%
    arrange(desc(num_of_unique_raters))
  
  #summarise further for results
  
  if(show_figures_tables == TRUE) {
    writeLines("Overview of number of raters")
    print(surgi_db_raters)
  }
  #summarise further for results
  surgi_db_raters_summary <- surgi_db_raters %>%
    select(num_of_unique_raters) %>%
    convert(fct(num_of_unique_raters)) %>%
    group_by(num_of_unique_raters) %>%
    summarise(count_of_num_unique_raters = n()) %>%
    arrange(desc(count_of_num_unique_raters))
  
  #draw as a plot
  Figure_raters <- surgi_db_raters_summary %>%
    ggplot(aes(x=num_of_unique_raters, y=count_of_num_unique_raters)) + geom_point() + 
    scale_y_continuous(limits = c(1, 500), breaks = c(1, seq(25, 500, by = 25))) +
    labs(y = "Number of procedures", x= "Number of unique surgeons providing an urgency",
         title = "Number of unique surgeons for each booked surgical procedure") +
    theme_bw()
  
  if(show_figures_tables == TRUE) {
    print(Figure_raters)
    print(surgi_db_raters_summary)
  }
  # create a table of all urgency categories all levels
  
  surgi_db_all_rated <- surgi_db_priority %>%
    select(booked_procedure, emerg_priority_collapse) %>%
    rstatix::freq_table(booked_procedure,emerg_priority_collapse)
  
  
  #count the number of procedures
  surgi_db_proc_n <- surgi_db_priority %>%
    select(booked_procedure, emerg_priority_collapse) %>%
    rstatix::freq_table(booked_procedure,emerg_priority_collapse) %>%
    group_by(booked_procedure) %>%
    summarise(proc_n = sum(n))
  
  #table the consultants and procedures
  surgi_db_cons_rated <- surgi_db_priority %>%
    select(booked_procedure, procedural_consultant) %>%
    rstatix::freq_table(booked_procedure, procedural_consultant)
  
  # Define the columns to check
  columns_to_check <- c('<4hrs', '<8hrs', '<24hrs', '<72hrs')
  
  #calculate agreement and consensus; note the agreement looks at agreement ebtween the 5 levels
  #whereas the agreement used in this study is 1 and 4 hours (so really 4 levels)
  surgi_db_all_rated_agreement <- surgi_db_priority %>%
    select(booked_procedure, emerg_priority_collapse) %>%
    mutate(emerg_priority_collapse = if_else(emerg_priority_collapse == "<1hr", "<4hrs",
                                             emerg_priority_collapse, emerg_priority_collapse)) %>%
    count(booked_procedure, emerg_priority_collapse) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = n) %>%
    mutate_if(is.numeric , replace_na, replace = 0) 
  
  cols_present <- names(surgi_db_all_rated_agreement)
  cols_present<- cols_present[cols_present != "booked_procedure"]
  cols_to_add <- setdiff(columns_to_check,cols_present)
  if(length(cols_to_add) >0) {
    new_cols_df <- as.data.frame(matrix("0", nrow = nrow(surgi_db_all_rated), ncol = length(cols_to_add)))
    names(new_cols_df) <- cols_to_add
    
    
    
  
  surgi_db_all_rated_agreement <- bind_cols(surgi_db_all_rated_agreement, new_cols_df) 
  }
  
  surgi_db_all_rated_agreement <- surgi_db_all_rated_agreement %>%
    convert(num(`<4hrs`,`<8hrs`,`<24hrs` ,`<72hrs`)) %>%
    pivot_longer(cols = c(-booked_procedure), names_to = "emerg_priority_collapse", values_to = "n") %>%
    group_by(booked_procedure) %>%
    summarise(Agreement=agreement(table(emerg_priority_collapse, n, useNA = "always")),
              Consensus = consensus(table(emerg_priority_collapse, n, useNA = "always"))) %>%
    ungroup()
  
  #combine the ratings counts and the agreement and consensus tables
  surgi_db_all_rated_n <- surgi_db_all_rated %>%
    select(-prop) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = n)%>%
    replace(is.na(.), 0) %>%
    left_join(surgi_db_all_rated_agreement, by = "booked_procedure")
  
  #change the counts to proportion
  
  #5 point likert urgency levels (<=1hr-Life threatening, <4 hours, <=8hrs-Non-critical but emergent, <=24hrs-Non critical, non-emergent, <=72hrsCannot discharge before procedure

  
  surgi_db_all_rated_prop <- surgi_db_all_rated %>%
    arrange(desc(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = prop) 
  
  columns_to_check <- c('<1hr','<4hrs', '<8hrs', '<24hrs', '<72hrs')
  cols_present <- names(surgi_db_all_rated_prop)
  cols_present<- cols_present[cols_present != "booked_procedure"]
  cols_to_add <- setdiff(columns_to_check,cols_present)
  if(length(cols_to_add) >0) {
    new_cols_df <- as.data.frame(matrix("0", nrow = nrow(surgi_db_all_rated), ncol = length(cols_to_add)))
    names(new_cols_df) <- cols_to_add
  
  

  surgi_db_all_rated_prop <- surgi_db_all_rated_prop %>%
    bind_cols(new_cols_df) 
  }
  
  surgi_db_all_rated_prop <- surgi_db_all_rated_prop %>%
    replace(is.na(.), 0) %>%
    left_join(surgi_db_all_rated_agreement, by = "booked_procedure") %>%
    convert(num(`<1hr`,`<4hrs`,`<8hrs`,`<24hrs` ,`<72hrs`)) %>%
    relocate(booked_procedure, `<1hr`,`<4hrs`,`<8hrs`,`<24hrs` ,`<72hrs`,Agreement, Consensus) %>%
    mutate(urgent_total_pcnt = case_when(
      urgent_value == 1 ~ `<1hr`,
      urgent_value == 4 ~ `<1hr` + `<4hrs`,
      urgent_value == 8 ~ `<1hr` + `<4hrs` + `<8hrs`,
      urgent_value == 24 ~ `<1hr` + `<4hrs` + `<8hrs` + `<24hrs`)) %>%
    mutate(urgent_surgery_by_agmnt = if_else(urgent_total_pcnt >= agreement_threshold, TRUE, FALSE, FALSE))%>%
    mutate(urgent_surgery_by_cnsus = if_else(urgent_total_pcnt < agreement_threshold & urgent_total_pcnt > 50 & Consensus < consensus_threshold, TRUE, FALSE, FALSE)) %>%
    left_join(surgi_db_proc_n, by = "booked_procedure")
   #calculate 95% CI and put into separate dataframe to be added in at the very end
  calc_ci <- surgi_db_all_rated_prop %>%
    select(booked_procedure, urgent_total_pcnt, proc_n) %>%
    mutate(n_urgent = (urgent_total_pcnt/100)*proc_n) %>%
    convert(int(n_urgent, proc_n))
  
  calculate_ci <- function(success, sample_size) {
    result <- binom.test(success, sample_size)
    ci <- as_tibble(matrix(result$conf.int, nrow = 1, dimnames = list(NULL, c("ci_lower", "ci_upper"))))
    return(ci)
  }
  
  # Use purrr::pmap_dfr to apply the function row-wise and bind the results into a new dataframe
  calc_ci2 <- calc_ci %>%
    mutate(ci = pmap_dfr(list(n_urgent, proc_n), calculate_ci)) 
  
  calc_ci2$ci_l <- calc_ci2$ci$ci_lower
  
  calc_ci3 <- calc_ci %>%
    mutate(ci = pmap_dfr(list(n_urgent, proc_n), calculate_ci)) 
  
  calc_ci3$ci_u <- calc_ci3$ci$ci_upper
  
  calc_ci3 <- calc_ci3 %>%
    select(booked_procedure, ci_u) %>%
    right_join(calc_ci2, by = "booked_procedure") %>%
    select(booked_procedure, ci_l, ci_u) %>%
    mutate(ci_l = round(ci_l*100, digits = 0),
           ci_u = round(ci_u*100, digits = 0)) %>%
    mutate(urgent_total_pcnt_CI = str_c(ci_l, ci_u, sep = " to ")) %>%
    select(booked_procedure, urgent_total_pcnt_CI)
  
  rm(calc_ci, calc_ci2)
  
  table_1 <- surgi_db_all_rated_prop %>% freq_table(urgent_surgery_by_agmnt, urgent_surgery_by_cnsus)
  
  if(show_figures_tables == TRUE) {
    print("Cross Table of Number of cases classified as urgent by agreement or consensus")
    print(table_1)
    
  }
  
  urgency_levels <- c("<1hr", "<4hrs", "<8hrs", "<24hrs", "<72hrs")
  
  figure_urgency_dist <- surgi_db_all_rated_prop %>%
    select(booked_procedure:`<72hrs`) %>%
    pivot_longer(col = c(-booked_procedure), names_to = "urgency", values_to = "pcnt") %>%
    mutate(urgency = factor(urgency, levels = urgency_levels)) %>%
    ggplot(aes(x = urgency, y = pcnt)) + geom_boxplot(width = 0.5) +
    theme_bw() +
    labs(title = "Box plots of the percentage of urgent categories \nacross all booked procedures", 
         x = "Booking urgency categories", y = "Percent of times an urgency category was used \nfor a procedure")
  
  figure_urgency_dist
  
  table_urgency_dist <- surgi_db_all_rated_prop %>%
    select(booked_procedure:`<72hrs`) %>%
    pivot_longer(col = c(-booked_procedure), names_to = "urgency", values_to = "pcnt") %>%
    group_by(urgency) %>%
    summarise(median_pct = median(pcnt),
              IQR_pct = IQR(pcnt),
              one_quantile_pcnt = quantile(pcnt, probs = 0.25),
              three_quantile_pcnt = quantile(pcnt, probs = 0.75))
  
  if(show_figures_tables == TRUE) {
    print("summary figure and table of the distribution of urgency levels across the entire datase")
    print(table_urgency_dist)
    print(figure_urgency_dist)
  }
  # is the patient trauma or not --------------------------------------------
  
  
  
  if(cohort == "trauma") {
    surgi_db_all_rated_prop <- surgi_db_all_rated_prop %>%
      mutate(trauma = TRUE) 
  }
  
  
  # add in if the procedure is in the primary procedures list ---------------
  primary_proc_vec <- surgi_db_priority %>%
    filter(primary_proc == TRUE)
  
  primary_proc_vec <- as.vector(unique(primary_proc_vec$booked_procedure))
  
  #adding in the CI at the end too
  surgi_db_all_rated_prop <- surgi_db_all_rated_prop %>%
    mutate(primary_proc = if_else(booked_procedure %in% primary_proc_vec, TRUE, FALSE, FALSE)) %>%
    left_join(calc_ci3, by = "booked_procedure") %>%
    relocate(urgent_total_pcnt_CI, .after = urgent_total_pcnt)
  
  
  table_trauma <- surgi_db_all_rated_prop %>%
    freq_table(trauma)
  
  table_primary_proc <- surgi_db_all_rated_prop %>%
    freq_table(primary_proc)
  
  
  if(show_figures_tables == TRUE) {
    print("Summary information regarding proportion of trauma patients, primary procedures")
    print(table_trauma)
    print(table_primary_proc)
    
  }
  
  
  
  
  #return the final dataframe
  surgi_db_all_rated_prop <<- surgi_db_all_rated_prop
  
}



# create tables -----------------------------------------------------------

# complete list -----------------------------------------------------------

#create the complete list
#this runs the function
full_list <- get_list_of_procedures(study_db = study_db_trauma, 
                                    agreement_threshold = 75, 
                                    consensus_threshold = .7, 
                                    urgency = 4,
                                    show_figures_tables = TRUE, 
                                    cohort = "trauma") %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) 



#urgent by agreement
full_list_urgent <- full_list %>%
  filter(urgent_surgery_by_agmnt == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)


#urgent by consensus only ones
full_list_urgent_consensus <- full_list %>%
  filter(urgent_surgery_by_cnsus == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)



#how many are trauma only, how many are primary procedures (not free text)
#for agreement
full_list_urgent %>%
  freq_table(trauma)

full_list_urgent %>%
  freq_table(primary_proc)
#for consensus
full_list_urgent_consensus %>%
  freq_table(trauma)

full_list_urgent_consensus %>%
  freq_table(primary_proc)


# age group lists ---------------------------------------------------------


paeds_list <- get_list_of_procedures(study_db = study_db_trauma, 
                                     agreement_threshold = 75, 
                                     consensus_threshold = .7, 
                                     age_upper = 16, 
                                     cohort = "trauma") %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) 

adult_list <- get_list_of_procedures(study_db = study_db_trauma, 
                                     agreement_threshold = 75, 
                                     consensus_threshold = .7, 
                                     age_lower = 16, 
                                     age_upper = 65, 
                                     cohort = "trauma") %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) 

elderly_list <- get_list_of_procedures(study_db = study_db_trauma,
                                       agreement_threshold = 75, 
                                       consensus_threshold = .7, 
                                       age_lower = 65, 
                                       cohort = "trauma") %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) 



elderly_list_urgent <- elderly_list %>%
  filter(urgent_surgery_by_agmnt == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

paeds_list_urgent <- paeds_list %>%
  filter(urgent_surgery_by_agmnt == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

adult_list_urgent <- adult_list %>%
  filter(urgent_surgery_by_agmnt == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

elderly_list_consensus <-  elderly_list %>%
  filter(urgent_surgery_by_cnsus == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

paeds_list_consensus <- paeds_list %>%
  filter(urgent_surgery_by_cnsus == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

adult_list_consensus <- adult_list %>%
  filter(urgent_surgery_by_cnsus == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

#find procedures differ for age groups

diff_full_elderly <- anti_join(full_list_urgent, elderly_list_urgent, by = "achi_code") %>%
  mutate(procedure_list = "not specific to elderly")

diff_elderly_full <- anti_join(elderly_list_urgent, full_list_urgent, by = "achi_code")%>%
  mutate(procedure_list = "elderly patients only") %>%
  bind_rows(diff_full_elderly)

rm(diff_full_elderly)

diff_full_paeds <- anti_join(full_list_urgent, paeds_list_urgent, by = "achi_code") %>%
  mutate(procedure_list = "not specific to paediatrics")

diff_paeds_full <- anti_join(paeds_list_urgent, full_list_urgent, by = "achi_code")%>%
  mutate(procedure_list = "paediatric patients only") %>%
  bind_rows(diff_full_paeds)

rm(diff_full_paeds)

diff_full_adult <- anti_join(full_list_urgent, adult_list_urgent, by = "achi_code") %>%
  mutate(procedure_list = "not specific to adults")

diff_adult_full <- anti_join(adult_list_urgent, full_list_urgent, by = "achi_code")%>%
  mutate(procedure_list = "adult patients only") %>%
  bind_rows(diff_full_adult)

rm(diff_full_adult)

diff_adult_full %>%
  freq_table(procedure_list, trauma)

diff_adult_full %>%
  freq_table(procedure_list, primary_proc)

diff_elderly_full %>%
  freq_table(procedure_list, trauma)

diff_elderly_full %>%
  freq_table(procedure_list, primary_proc)

diff_paeds_full %>%
  freq_table(procedure_list, trauma)

diff_paeds_full %>%
  freq_table(procedure_list, primary_proc)


#write it all to an xls workbook

workbooks <- list("full list by agreement" = full_list_urgent,
                  "addiional proc by consensus" = full_list_urgent_consensus,
                  "adult list by agreement" = adult_list_urgent,
                  "adult addit proc by consensus" = adult_list_consensus,
                  "paeds list by agreement" = paeds_list_urgent,
                  "paeds addit proc by consensus" = paeds_list_consensus,
                  "elderly list by agreement" = elderly_list_urgent,
                  "elderly addit proc by consensus" = elderly_list_consensus,
                  "diff proc bw adult and full" = diff_adult_full,
                  "diff proc bw elderly and full" = diff_elderly_full,
                  "diff proc bw paeds and full" = diff_paeds_full)


write.xlsx(workbooks, file="urgent_surgery_lists.xlsx",  rowNames=FALSE)


