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

achi_db <- #path to your table of procedure codes

# download ICD-1-_AM codes -----------------------------------------------------

icd_db <- #path to your table of procedure codes

# codes to remove ---------------------------------------------------------
#these codes are then removed 

# filter out procedures related to the anaesthetic and 36800-00 = bladder catheterisation
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
codes_to_remove <- c(codes_to_remove1, codes_to_remove2, "36800-00") #combine to a single block

rm(codes_to_remove1, codes_to_remove2)#remove the components

# first step is to get primary procedure ---------------------------------------------------
# this is the first part of the primary outcome
study_db_coded_primary <- study_db_all %>%
  select(study_id, achi_proc_code = coded_primary_procedure) 

# get study labels for procedures that weren't the primary procedure-------------------

achi_text <- achi_db %>%
  select(achi_proc_code, procedure_text, block_text)

code_to_text <- study_db_all %>%
  select(study_id, episode_number, contains("procedure"), admit_dt_tm) %>% #extract all of the secondary procedure codes
  select(-coded_primary_procedure) %>% #this has already been extracted
  pivot_longer(cols = c(-study_id, -planned_procedure, -actual_procedure, -episode_number,
                        -primary_procedure, -procedure_date, -admit_dt_tm), names_to = "column_n", values_to = "achi_proc_code") %>%
  filter(!is.na(achi_proc_code)) %>%
  left_join(achi_text, by = "achi_proc_code") %>% #put the text next to the code
  select(study_id, primary_procedure, planned_procedure, procedure_text, block_text, achi_proc_code) #simplify the dataframe

p_load(tm)
vec<-tm::stopwords("english")
vec <- c(vec, "and", "of") #these werent included in the stopwords list, and needed adding

# the original study had the booked procedure recorded in two places "planned procedure"
# and "primary procedure". One was free-text, the other appears to have been a drop-down
#as a result we needed to match on both free-text descriptions
planned_achi <- code_to_text %>% 
  select(-primary_procedure) %>%
  mutate(planned_procedure = str_replace_all(planned_procedure, "[^[:alnum:]]", " "), #remove punctuation
         planned_procedure = tolower(planned_procedure), #make all lower case
         planned_procedure = str_replace_all(planned_procedure, pattern = paste0('\\b',vec, '\\b', 
                                                                                 collapse = '|'), " "), #remove stop words
         procedure_text = tolower(procedure_text), #this comes from the ACHI table, and didn't need cleaning
         row_n = row_number()) %>%
  group_by(row_n) %>%
  mutate(planned_block = map2(strsplit(planned_procedure, " "), #this is where we match on common words
                              strsplit(procedure_text, " "),  
                              intersect))%>%
  ungroup() %>%
  unnest_wider(planned_block, names_sep = "_") %>% #spread out to single episodes of care
  unite("planned_block", starts_with("planned_block"), sep = ",", na.rm = TRUE) %>%
  select(-row_n) %>%
  filter(planned_block != "") %>% #get rid of rows without a match
  select(study_id, achi_proc_code)

#this repeats the above, but for the other column we had
primary_achi <- code_to_text %>% 
  select(-planned_procedure) %>%
  mutate(primary_procedure = str_replace_all(primary_procedure, "[^[:alnum:]]", " "),
         primary_procedure = tolower(primary_procedure),
         primary_procedure = str_replace_all(primary_procedure, pattern = paste0('\\b',vec, '\\b', 
                                                                                 collapse = '|'), " "),
         procedure_text = tolower(procedure_text),
         row_n = row_number()) %>%
  group_by(row_n) %>%
  mutate(primary_block = map2(strsplit(primary_procedure, " "),
                              strsplit(procedure_text, " "),  
                              intersect)) %>%
  ungroup() %>%
  unnest_wider(primary_block, names_sep = "_")%>%
  unite("primary_block", starts_with("primary_block"), sep = ",", na.rm = TRUE) %>%
  select(-row_n) %>%
  filter(primary_block != "") %>%
  select(study_id, achi_proc_code)

# create achi coded database
achi_study_db <- study_db_coded_primary %>%
  bind_rows(planned_achi) %>%
  bind_rows(primary_achi) %>% #if you have only one booked procedure column, one of these can be dropped
  distinct() %>%
  rename(booked_procedure = achi_proc_code)


#add back to study_db
#remove the codes to remove
study_db <- study_db_all %>%
  left_join(achi_study_db, by = "study_id", relationship = "many-to-many") %>%
  distinct(study_id, booked_procedure, .keep_all = TRUE) %>%
  filter(!booked_procedure %in% codes_to_remove)

#tidy up the workspace
rm(code_to_text, achi_study_db)

#count the number of additions
study_db %>%
  group_by(study_id) %>%
  mutate(num_of_proc = as.numeric(max(row_number()))) %>%
  slice_max(num_of_proc)%>%
  select(study_id, num_of_proc)%>%
  ungroup() %>%
  count(num_of_proc)

sum(is.na(study_db$booked_procedure))

#final tidyup
study_db <- study_db %>%
  filter(!is.na(booked_procedure))

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
study_db_trauma <- study_db %>%
  filter(study_id %in% trauma_study_ids)

#number of procedures
nrow(study_db)
nrow(study_db_trauma)


# get list of procedures FUNCTION -----------------------------------------
#this function takes the data and creates the table of procedures and urgency 
get_list_of_procedures <- function(study_db, agreement_threshold, consensus_threshold, 
                                   urgency = 4, age_lower = NA_real_, age_upper = NA_real_, 
                                   show_figures_tables = FALSE, cohort = "all patients") {
  
  #set the list
  
  
  
  #define the main variables from the function arguement
  surgidb_priority <- study_db
  agreement_threshold <- agreement_threshold
  consensus_threshold <- consensus_threshold
  age_lower <- age_lower
  age_upper <- age_upper
  urgent_value <- urgency
  
  #this is just to test the funciton during writing
  #surgidb_priority <- study_db_trauma
  #agreement_threshold <- 75
  #consensus_threshold <- 0.7
  #age_lower <- 0
  #age_upper <- 110
  
  #make sure factors are of the class:factor
  surgidb_priority <- surgidb_priority %>%
    convert(fct(booked_procedure, emerg_priority, emerg_priority_collapse))
  
  
  #if no age entered, set as the max and min of the dataset
  if (is.na(age_lower)) {
    age_lower <- min(surgidb_priority$age, na.rm = TRUE)
  }
  
  if (is.na(age_upper)) {
    age_upper <- max(surgidb_priority$age, na.rm = TRUE)
  }
  
  #print a table of the admission to booking period, should be 100% same day
  table_1 <- surgidb_priority %>% freq_table(admit_to_book_pd)
  
  if(show_figures_tables == TRUE) {
    
    writeLines("Table of number of cases and time from admission to booking the case. \nShould be 100% the same day")
    print(table_1)
    
  }
  #incase the dataset has longer booking periods, filter to the same day only
  #also filter to ages if specified
  surgidb_priority <- surgidb_priority %>%
    filter(admit_to_book_pd == "same day") %>%
    filter(age >= age_lower, age < age_upper)
  
  # how many individual procedures and booking proceduralists and patients?
  individ_pts <- as.vector(unique(surgidb_priority$study_id))
  
  if(show_figures_tables == TRUE) {
    writeLines(str_c("Number of unique primary procedures = ", length(unique(surgidb_priority$booked_procedure))))
    writeLines(str_c("Number of unique booking proceduralists = ", length(unique(surgidb_priority$procedural_consultant))))
    writeLines(str_c("Number of individual patients = ", length(unique(surgidb_priority$study_id))))
  }
  # categories are : <=15minutes-Immediate life threatening ; <=1hr-Life threatening conditions ;
  # <=24hrs-Non critical non-emergent; <=4hrs-Organ or limb threatening; <=72hrsCannot discharge before procedure; <=8hrs-Non-critical but emergent 
  
  #summarise the number of raters (to be added in later)
  surgidb_raters <- surgidb_priority %>%
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
    print(surgidb_raters)
  }
  #summarise further for results
  surgidb_raters_summary <- surgidb_raters %>%
    select(num_of_unique_raters) %>%
    convert(fct(num_of_unique_raters)) %>%
    group_by(num_of_unique_raters) %>%
    summarise(count_of_num_unique_raters = n()) %>%
    arrange(desc(count_of_num_unique_raters))
  
  #draw as a plot
  Figure_raters <- surgidb_raters_summary %>%
    ggplot(aes(x=num_of_unique_raters, y=count_of_num_unique_raters)) + geom_point() + 
    scale_y_continuous(limits = c(1, 500), breaks = c(1, seq(25, 500, by = 25))) +
    labs(y = "Number of procedures", x= "Number of unique surgeons providing an urgency",
         title = "Number of unique surgeons for each booked surgical procedure") +
    theme_bw()
  
  if(show_figures_tables == TRUE) {
    print(Figure_raters)
    print(surgidb_raters_summary)
  }
  # create a table of all urgency categories all levels
  
  surgidb_all_rated <- surgidb_priority %>%
    select(booked_procedure, emerg_priority_collapse) %>%
    rstatix::freq_table(booked_procedure,emerg_priority_collapse)
  
  
  #count the number of procedures
  surgidb_proc_n <- surgidb_priority %>%
    select(booked_procedure, emerg_priority_collapse) %>%
    rstatix::freq_table(booked_procedure,emerg_priority_collapse) %>%
    group_by(booked_procedure) %>%
    summarise(proc_n = sum(n))
  
  #table the consultants and procedures
  surgidb_cons_rated <- surgidb_priority %>%
    select(booked_procedure, procedural_consultant) %>%
    rstatix::freq_table(booked_procedure, procedural_consultant)
  
  #calculate agreement and consensus; note the agreement looks at agreement between the 5 levels
  #whereas the agreement used in this study is 1 and 4 hours (so really 4 levels)
  surgidb_all_rated_agreement <- surgidb_priority %>%
    select(booked_procedure, emerg_priority_collapse) %>%
    mutate(emerg_priority_collapse = if_else(emerg_priority_collapse == "<1hr", "<4hrs",
                                             emerg_priority_collapse, emerg_priority_collapse)) %>%
    count(booked_procedure, emerg_priority_collapse) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = n) %>%
    mutate_if(is.numeric , replace_na, replace = 0) %>%
    convert(num(`<4hrs`,`<8hrs`,`<24hrs` ,`<72hrs`)) %>%
    pivot_longer(cols = c(-booked_procedure), names_to = "emerg_priority_collapse", values_to = "n") %>%
    group_by(booked_procedure) %>%
    summarise(Agreement=agreement(table(emerg_priority_collapse, n, useNA = "always")),
              Consensus = consensus(table(emerg_priority_collapse, n, useNA = "always"))) %>%
    ungroup()
  
  #combine the ratings counts and the agreement and consensus tables
  surgidb_all_rated_n <- surgidb_all_rated %>%
    select(-prop) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = n)%>%
    replace(is.na(.), 0) %>%
    left_join(surgidb_all_rated_agreement, by = "booked_procedure")
  
  #change the counts to proportionn
  
  #5 point likert urgency levels (<=1hr-Life threatening, <4 hours, <=8hrs-Non-critical but emergent, <=24hrs-Non critical, non-emergent, <=72hrsCannot discharge before procedure
  
  surgidb_all_rated_prop <- surgidb_all_rated %>%
    arrange(desc(n)) %>%
    select(-n) %>%
    pivot_wider(names_from = emerg_priority_collapse, values_from = prop) %>%
    replace(is.na(.), 0) %>%
    left_join(surgidb_all_rated_agreement, by = "booked_procedure") %>%
    relocate(booked_procedure, `<1hr`,`<4hrs`,`<8hrs`,`<24hrs` ,`<72hrs`,Agreement, Consensus) %>%
    mutate(urgent_total_pcnt = case_when(
      urgent_value == 1 ~ `<1hr`,
      urgent_value == 4 ~ `<1hr` + `<4hrs`,
      urgent_value == 8 ~ `<1hr` + `<4hrs` + `<8hrs`,
      urgent_value == 24 ~ `<1hr` + `<4hrs` + `<8hrs` + `<24hrs`)) %>%
    mutate(urgent_surgery_by_agmnt = if_else(urgent_total_pcnt >= agreement_threshold, TRUE, FALSE, FALSE))%>%
    mutate(urgent_surgery_by_cnsus = if_else(urgent_total_pcnt < agreement_threshold & urgent_total_pcnt > 50 & Consensus < consensus_threshold, TRUE, FALSE, FALSE)) %>%
    left_join(surgidb_proc_n, by = "booked_procedure")
  #calculate 95% CI and put into separate dataframe to be added in at the very end
  calc_ci <- surgidb_all_rated_prop %>%
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
  
  table_1 <- surgidb_all_rated_prop %>% freq_table(urgent_surgery_by_agmnt, urgent_surgery_by_cnsus)
  
  if(show_figures_tables == TRUE) {
    print("Cross Table of Number of cases classified as urgent by agreement or consensus")
    print(table_1)
    
  }
  
  urgency_levels <- c("<1hr", "<4hrs", "<8hrs", "<24hrs", "<72hrs")
  
  figure_urgency_dist <- surgidb_all_rated_prop %>%
    select(booked_procedure:`<72hrs`) %>%
    pivot_longer(col = c(-booked_procedure), names_to = "urgency", values_to = "pcnt") %>%
    mutate(urgency = factor(urgency, levels = urgency_levels)) %>%
    ggplot(aes(x = urgency, y = pcnt)) + geom_boxplot(width = 0.5) +
    theme_bw() +
    labs(title = "Box plots of the percentage of urgent categories \nacross all booked procedures", 
         x = "Booking urgency categories", y = "Percent of times an urgency category was used \nfor a procedure")
  
  figure_urgency_dist
  
  table_urgency_dist <- surgidb_all_rated_prop %>%
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
  #only run if you are using the entire cohort. essentially same code as lines 146-171
  if(cohort == "all patients") {
    
    ts <- c(36:50, 51:65, 74, 78, 80:88, 90:98)
    ys <- c(06, 07, 10:19, 36, 37, 40:98)
    us <- c(04, 06, 07, 78:92)
    xs <- c(40:49)
    #xs_poisoning <- c(60:69)
    
    
    icds_to_remove <- c(str_c("t", ts, sep = ""), str_c("y", ys, sep= ""), str_c("u", us, sep= ""), str_c("x", xs, sep= ""))

    
    
    trauma1 <- surgidb_priority %>%
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
    
    trauma_study_ids <- as.vector(trauma1$study_id)
    
    procedures_trauma <- surgidb_priority %>%
      filter(study_id %in% trauma_study_ids) %>%
      select(booked_procedure) %>%
      mutate(trauma = TRUE) %>%
      distinct()
    
    surgidb_all_rated_prop <- surgidb_all_rated_prop %>%
      left_join(procedures_trauma, by = "booked_procedure") %>%
      replace_na(list(trauma = FALSE))
    
  } 
  
  if(cohort == "trauma") {
    surgidb_all_rated_prop <- surgidb_all_rated_prop %>%
      mutate(trauma = TRUE) 
  }
  
  
  # add in if the procedure is in the primary procedures list ---------------
  primary_proc <- as.vector(unique(study_db_coded_primary$achi_proc_code))
  
  #adding in the CI at the end too
  surgidb_all_rated_prop <- surgidb_all_rated_prop %>%
    mutate(primary_proc = if_else(booked_procedure %in% primary_proc, TRUE, FALSE, FALSE)) %>%
    left_join(calc_ci3, by = "booked_procedure") %>%
    relocate(urgent_total_pcnt_CI, .after = urgent_total_pcnt)
  
  
  table_trauma <- surgidb_all_rated_prop %>%
    freq_table(trauma)
  
  table_primary_proc <- surgidb_all_rated_prop %>%
    freq_table(primary_proc)
  
  
  
  if(show_figures_tables == TRUE) {
    print("Summary information regarding proportion of trauma patients, primary procedures")
    print(table_trauma)
    print(table_primary_proc)
    
  }

  #return the final dataframe
  surgidb_all_rated_prop <<- surgidb_all_rated_prop
  
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

# gender lists ------------------------------------------------------------


trauma_m <- study_db_trauma %>%
  filter(sex == "M")

male_list <- get_list_of_procedures(study_db = trauma_m, 
                                    agreement_threshold = 75, 
                                    consensus_threshold = .7, 
                                    urgency = 4,
                                    show_figures_tables = TRUE, 
                                    cohort = "trauma") %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) 

trauma_f <- study_db_trauma %>%
  filter(sex == "F")

female_list <- get_list_of_procedures(study_db = trauma_f, 
                                      agreement_threshold = 75, 
                                      consensus_threshold = .7, 
                                      urgency = 4,
                                      show_figures_tables = TRUE, 
                                      cohort = "trauma") %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) 


male_list_urgent <- male_list %>%
  filter(urgent_surgery_by_agmnt == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)

female_list_urgent <- female_list %>%
  filter(urgent_surgery_by_agmnt == TRUE) %>%
  select(achi_code = booked_procedure, procedure_text, block_text, trauma, primary_proc, urgent_total_pcnt, urgent_total_pcnt_CI)


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


