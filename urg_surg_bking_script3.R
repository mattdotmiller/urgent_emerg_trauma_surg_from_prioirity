#the dataframes all come from script 2
p_load(vtable)
# demographics -------------------------------------------------------------
#first get the study group of interest
#trauma patients
study_demographics <- study_db_trauma %>%
  distinct(study_id, .keep_all = T)


study_db_demographics <- study_demographics 


#non-trauma patients
study_demographics <- study_db %>%
  anti_join(study_demographics, by = "study_id")%>%
  distinct(study_id, .keep_all = T)

ts <- c(36:50, 51:65, 74, 78, 80:88, 90:98)
ys <- c(06, 07, 10:19, 36, 37, 40:98)
us <- c(04, 06, 07, 78:92)
xs <- c(40:49)
#xs_poisoning <- c(60:69)


icds_to_remove <- c(str_c("t", ts, sep = ""), str_c("y", ys, sep= ""), str_c("u", us, sep= ""), str_c("x", xs, sep= ""))
#write.xlsx(icd_codes, "icds_removed.xlsx")
#combines injury and mechanism codes

trauma1 <- study_demographics %>%
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

procedures_trauma <- study_demographics %>%
  filter(study_id %in% trauma_study_ids) %>%
  select(booked_procedure) %>%
  mutate(trauma = TRUE) %>%
  distinct()

# demographics start here
study_db_demographics <- study_demographics %>%
  left_join(procedures_trauma, by = "booked_procedure") %>%
  select(study_id, age, trauma, sex, asa, specialty,emerg_priority_collapse ) %>%
  replace_na(list(trauma = FALSE))


summary_stats <-  c('notNA(x)', 'propNA(x)', 'mean(x)','sd(x)', 'median(x)', 'IQR(x)','pctile(x)[25]', 'pctile(x)[75]','min(x)', 'max(x)') 
summary_stats_names =c('N', 'missing', 'mean','sd', 'median', 'IQR','25 centile', '75 centile','min', 'max')

study_db_demographics %>%
  st(vars = c('age'), 
     add.median = TRUE,
     summ=summary_stats,
     summ.names = summary_stats_names)

hist(study_db_demographics$age)


study_db_demographics %>%
  freq_table(asa)

study_db_demographics %>%
  freq_table(trauma)

study_db_demographics %>%
  freq_table(sex)

study_db_demographics %>%
  freq_table(emerg_priority_collapse)

study_db_specialty <- study_db_demographics %>%
  mutate(specialty = str_replace(specialty, " SN", "")) %>%
  mutate(specialty = case_when (
    specialty %in% c("Obs/Gynae", "Gynaecology") ~ "Obs/Gynae",
    specialty %in% c("Colorectal", "Upper GI", "Breast/Endo", "Liver Surgery", "Paediatric") ~ "General Surgery",
    TRUE ~ specialty
  )) %>%
  freq_table(specialty) %>%
  arrange(desc(prop)) 


#specialty - needs a bit more processing because of duplicate teams from ACHI code
specialty_for_code <- study_db_trauma %>%
  select(booked_procedure, primary_procedure, specialty) %>%
  distinct(booked_procedure, specialty)


urgent_achi_codes <- as.vector(full_list_urgent$achi_code)

urgent_specialty_list <- specialty_for_code %>% filter(booked_procedure %in% urgent_achi_codes) %>%
  left_join(achi_text, by = c("booked_procedure" = "achi_proc_code")) %>%
  mutate(specialty = str_replace(specialty, " SN", "")) %>%
  mutate(specialty = case_when (
    specialty %in% c("Obs/Gynae", "Gynaecology") ~ "Obs/Gynae",
    specialty %in% c("Colorectal", "Upper GI", "Breast/Endo", "Liver Surgery", "Paediatric") ~ "General Surgery",
    TRUE ~ specialty
  )) %>%
  freq_table(specialty) %>%
  arrange(desc(prop)) 


#write it all to an xls workbook

workbooks <- list("urgent speciality list" = urgent_specialty_list,
                  "study db specialty list" = study_db_specialty)


write.xlsx(workbooks, file="specialty_lists.xlsx",  rowNames=FALSE)

# regression analysis for trauma cohort only ------------------------------


#get procedures with min number of rates
num_of_raters <- study_db_trauma %>% #separate section below if you want all patients, subsetted by trauma
  select(booked_procedure, procedural_consultant) %>%
  group_by(booked_procedure) %>%
  summarise(num_of_raters = n(),
            num_of_unique_raters = n_distinct(procedural_consultant)) %>%
  ungroup() %>%
  arrange(desc(num_of_unique_raters)) %>%
  filter(num_of_raters >= 8)

raters_over_limit <- unique(as.vector(num_of_raters$booked_procedure))

#get procedures with at least 1 male and 1 female
num_of_genders <- study_db_trauma %>%
  select(booked_procedure, sex) %>%
  group_by(booked_procedure) %>%
  summarise(
    male_count = sum(sex == "M"),
    female_count = sum(sex == "F")) %>%
  ungroup() %>%
  filter(male_count >=1 & female_count >=1)

gender_over_limit <- unique(as.vector(num_of_genders$booked_procedure))

common_elements <- intersect(gender_over_limit, raters_over_limit)


u_model <- study_db_trauma %>%
  dplyr::select(booked_procedure, sex, emerg_priority_collapse, age, procedural_consultant, study_id) %>%
  #filter(booked_procedure %in% raters_over_limit) %>% #add this and remove the next line if you arent interested in gender
  filter(booked_procedure %in% common_elements) %>%
  mutate(urgent_yn = if_else(emerg_priority_collapse %in% c("<1hr","<4hrs"), "urgent", "not urgent", NA_character_)) %>%
  mutate(age_gp = case_when(
    age <16 ~ "paeds",
    age >= 16 & age <65 ~ "adult",
    age >=65 ~ "elderly")) %>%
  convert(fct(urgent_yn, age_gp)) %>%
  mutate(urgent_yn=fct_relevel(urgent_yn,c("urgent","not urgent")))

#whats the sample size?
nrow(u_model)

#multilevel model needed?

#calculate the ICC
ICC.Model<-function(Model.Name) {
  tau.Null<-as.numeric(lapply(summary(Model.Name)$varcor, diag))
  sigma.Null <- as.numeric(attr(summary(Model.Name)$varcor, "sc")^2)
  ICC.Null <- tau.Null/(tau.Null+sigma.Null)
  return(ICC.Null)
}

#null model
m_null <- glmer(urgent_yn ~ 1 + (1| booked_procedure), data = u_model, family = binomial, 
                control = glmerControl(
                  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
ICC.Model(m_null)

#run the multilevel models

m1 <- glmer(urgent_yn ~ sex+age_gp + 
              (1| booked_procedure), data = u_model, family = binomial, 
            control = glmerControl(
              optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

#add random slope
m2 <- glmer(urgent_yn ~ sex+ age_gp + 
              (1-booked_procedure| booked_procedure), data = u_model, family = binomial, 
            control = glmerControl(
              optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

#model with an interection term
m1_interaction<- glmer(urgent_yn ~ sex*age_gp + 
                         (1| booked_procedure), data = u_model, family = binomial, 
                       control = glmerControl(
                         optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

anova(m1, m2) #no difference. drop m2
anova(m1, m_null) #p less than 0.001 so we will run M1

#m1 <- m1_interaction # idf you want the summary stats for the interaction model
summary(m1)

# Extract odds ratios and confidence intervals
se <- sqrt(diag(vcov(m1)))

df_tab <- data.frame(
  OddsRatio = exp(fixef(m1)),
  LL = exp(fixef(m1) - 1.96 * se),
  UL = exp(fixef(m1) + 1.96 * se))  

df_tab <- df_tab %>%
  rownames_to_column(var = "Variable")%>%
  mutate_if(is.numeric, function(x) round(x, 2))

print(df_tab)

# regression analysis whole cohort (not used)  ----------------------------


ts <- c(36:50, 51:65, 74, 78, 80:88, 90:98)
ys <- c(06, 07, 10:19, 36, 37, 40:98)
us <- c(04, 06, 07, 78:92)
xs <- c(40:49)
#xs_poisoning <- c(60:69)


icds_to_remove <- c(str_c("t", ts, sep = ""), str_c("y", ys, sep= ""), str_c("u", us, sep= ""), str_c("x", xs, sep= ""))

#combines injury and mechanism codes

trauma1 <- study_db_all %>%
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

num_of_raters <- study_db %>%
  select(booked_procedure, procedural_consultant) %>%
  group_by(booked_procedure) %>%
  summarise(num_of_raters = n(),
            num_of_unique_raters = n_distinct(procedural_consultant)) %>%
  ungroup() %>%
  arrange(desc(num_of_unique_raters)) %>%
  filter(num_of_raters >= 20)

raters_over_limit <- unique(as.vector(num_of_raters$booked_procedure))

u_model <- study_db %>%
  dplyr::select(booked_procedure, sex, emerg_priority_collapse, age, spec_cons, study_id, asa, specialty) %>%
  filter(booked_procedure %in% raters_over_limit) %>%
  mutate(urgent_yn = if_else(emerg_priority_collapse %in% c("<1hr","<4hrs"), "urgent", "not urgent", NA_character_)) %>%
  mutate(age_gp = case_when(
    age <16 ~ "paeds",
    age >= 16 & age <65 ~ "adult",
    age >=65 ~ "elderly")) %>%
  convert(fct(urgent_yn, age_gp)) %>%
  mutate(urgent_yn=fct_relevel(urgent_yn,c("urgent","not urgent"))) %>%
  mutate(trauma = if_else(study_id %in% trauma_study_ids, TRUE, FALSE, FALSE))

#whats the sample size?
nrow(u_model)

#multilevel model needed?

#calculate the ICC
ICC.Model<-function(Model.Name) {
  tau.Null<-as.numeric(lapply(summary(Model.Name)$varcor, diag))
  sigma.Null <- as.numeric(attr(summary(Model.Name)$varcor, "sc")^2)
  ICC.Null <- tau.Null/(tau.Null+sigma.Null)
  return(ICC.Null)
}

#null model
m_null <- glmer(urgent_yn ~ 1 + (1| booked_procedure), data = u_model, family = binomial, 
                control = glmerControl(
                  optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))
ICC.Model(m_null)

#run the multilevel models

m1 <- glmer(urgent_yn ~ sex+age_gp + trauma +
              (1| booked_procedure), data = u_model, family = binomial, 
            control = glmerControl(
              optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

#add random slope
m2 <- glmer(urgent_yn ~ sex+ age_gp + trauma +
              (1-booked_procedure| booked_procedure), data = u_model, family = binomial, 
            control = glmerControl(
              optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

#model with an interection term
m1_interaction<- glmer(urgent_yn ~ sex*age_gp + trauma +
                         (1| booked_procedure), data = u_model, family = binomial, 
                       control = glmerControl(
                         optimizer ='optimx', optCtrl=list(method='L-BFGS-B')))

anova(m1, m2) #no difference. drop m2
anova(m1, m_null) #p less than 0.001 so we will run M1

#m1 <- m1_interaction # idf you want the summary stats for the interaction model
summary(m1)

# Extract odds ratios and confidence intervals
se <- sqrt(diag(vcov(m1)))

df_tab <- data.frame(
  OddsRatio = exp(fixef(m1)),
  LL = exp(fixef(m1) - 1.96 * se),
  UL = exp(fixef(m1) + 1.96 * se))  

df_tab <- df_tab %>%
  rownames_to_column(var = "Variable")%>%
  mutate_if(is.numeric, function(x) round(x, 2))

print(df_tab)



# some summary information to help decide on cut-offs first --------------------------------------------------------

# number of procedures classified by thresholds of consensus and agreement -------------------------------------------------------------------
# number of procedures classified by thresholds of consensus -------------------------------------------------------------------

# This function generates a table of counts for procedures based on agreement and consensus thresholds
table_of_n <- function(x, y) {
  a_th <- x
  c_th <- y
  
  # Uncomment these lines if you want to use default threshold values
  # a_th <- 75
  # c_th <- 0.5
  
  # Call a function (get_list_of_procedures) to get a result
  result <- get_list_of_procedures(study_db = study_db_trauma, agreement_threshold = a_th, consensus_threshold = c_th, cohort = "trauma") 
  
  # Create a frequency table based on specific columns
  result_table <- result %>% freq_table(urgent_surgery_by_agmnt, urgent_surgery_by_cnsus)
  
  # Extract counts for urgent surgery based on agreement and consensus
  n_by_agreement <- result_table %>% filter(urgent_surgery_by_agmnt == TRUE) %>% select(n) %>% pull()
  n_by_consensus <- result_table %>% filter(urgent_surgery_by_cnsus == TRUE) %>% select(n) %>% pull()
  
  # Handle cases where the counts are empty
  if (is_empty(n_by_consensus)) {
    n_by_consensus <- 0 
  }
  
  if (is_empty(n_by_agreement)) {
    n_by_agreement <- 0 
  }
  
  # Create a data frame with threshold values and counts
  table_of_n <<- data.frame(a_th, c_th, n_by_agreement, n_by_consensus)
}

# Define threshold values for agreement and consensus
a_th <- c(75, 80, 90)
c_th <- c(0.6, 0.65, 0.7, 0.9)

# Generate combinations of threshold values
combinations <- expand.grid(a_th = a_th, c_th = c_th)

# Apply the table_of_n function to each combination of thresholds
table_of_counts <- map2(combinations$a_th, combinations$c_th, table_of_n)
table_of_counts <- reduce(table_of_counts, rbind.data.frame)

# Print the resulting table of counts
print(table_of_counts)

# plots of consensus -------------------------------------------------------------------
# Define urgency levels
urgency_levels <- c("<1hr", "<4hrs", "<8hrs", "<24hrs", "<72hrs")

# Plot a histogram of consensus to see the distribution
# (The code for this part is commented out)

# Generate subsets of data based on different consensus thresholds
consensus_7 <- get_list_of_procedures(study_db = study_db_trauma, agreement_threshold = 80, 
                                      consensus_threshold = 0.7, cohort = "trauma") %>%
  filter(urgent_surgery_by_cnsus == TRUE) %>%
  mutate(cons_cutoff = 0.7)

# Repeat for other consensus thresholds
# (consensus_75, consensus_65, consensus_6)

# Combine the subsets into a single data frame
consensus_plots <- consensus_7 %>%
  bind_rows(consensus_75, consensus_65, consensus_6) %>%
  convert(fct(cons_cutoff))

# Define labels for the plots
cons_labels <- c("Consensus = 0.6", "Consensus = 0.65", "Consensus = 0.7", "Consensus = 0.75")
names(cons_labels) <- c("0.6", "0.65", "0.7", "0.75")

# Density plot
consensus_plots %>%
  select(booked_procedure, cons_cutoff, `<1hr`, `<4hrs`, `<8hrs`, `<24hrs`, `<72hrs`) %>%
  pivot_longer(col = c(-booked_procedure, -cons_cutoff), names_to = "urgency", values_to = "pcnt") %>%
  mutate(urgency = factor(urgency, levels = urgency_levels)) %>%
  ggplot(aes(x = pcnt, colour = urgency)) + geom_density() +
  theme_bw() +
  facet_wrap(~cons_cutoff, labeller = labeller(cons_cutoff = cons_labels)) +
  labs(title = "Distribution of proportion of responses for each urgency level \nby consensus cut-off",
       x = "Percent of responses", y = "Density")

# Scatter plot
consensus_plots %>%
  select(booked_procedure, cons_cutoff, `<1hr`, `<4hrs`, `<8hrs`, `<24hrs`, `<72hrs`) %>%
  pivot_longer(col = c(-booked_procedure, -cons_cutoff), names_to = "urgency", values_to = "pcnt") %>%
  mutate(urgency = factor(urgency, levels = urgency_levels)) %>%
  ggplot(aes(x = urgency, y = pcnt)) + geom_jitter(width = 0.1, height = 0.1) +
  theme_bw() +
  facet_wrap(~cons_cutoff, labeller = labeller(cons_cutoff = cons_labels)) +
  labs(title = "Distribution of proportion of responses for each urgency level \nby consensus cut-off",
       x = "Urgency level", y = "Percent of responses", subtitle = "Each point represents a surgical procedure")

