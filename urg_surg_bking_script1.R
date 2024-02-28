# you may need to install the pacman package
#these packaged are a generic list I install for any session, not all are used

library(pacman)
p_load(tidyverse, checkmate, stringr, lubridate, gdata, readxl, gmodels, naniar, openxlsx, ggpubr, janitor, skimr, REDCapR, hablar, mefa)
p_load(rebus, rstatix, digest)
p_load(agrmt)

#needs this vector to recreate the study ID
id_chrs <- c(0:9, letters)
#define the collapsed urgency levels
urgency_levels_collapsed <- c("<1hr","<4hrs", "<8hrs", "<24hrs","<72hrs")

#bring in your study databaase, assumes it is in the working directory

study_db <- readxl("study_db.xlsx")

study_db <- study_db %>%
  mutate(admit_to_book_pd = as.numeric(difftime(emerg_book_dt_tm, admit_dt_tm), units="hours"))%>%
  mutate(admit_to_book_pd = case_when(
    admit_to_book_pd <= 24 ~ "same day",
    admit_to_book_pd > 24 & admit_to_book_pd <= 168 ~ "same week",
    admit_to_book_pd > 168 ~ "over 1 week"))

# print table of all booking periods.
study_db %>% freq_table(admit_to_book_pd)

#reduce the study database to only the first procedure that happened within 24hours of admission
#collapse the emergency priorities

study_db <- study_db %>%
  filter(admit_to_book_pd == "same day")  %>%
  group_by(study_id, age, admit_dt_tm) %>% #this bit shrinks to to only the first case on admission
  arrange(case_date) %>%
  slice_head(n = 1) %>%
  ungroup() %>%
  mutate(emerg_priority_collapse = case_when(
    emerg_priority %in% c("<=15minutes-Immediate life threatening", "A-1hr Life threatening incl obstetric", "<=1hr-Life threatening conditions",
                          "B-2hrs Highly critical organ/limb threat") ~ "<1hr",
    emerg_priority %in% c( "<=4hrs-Organ or limb threatening", "C-4hrs Critical") ~ "<4hrs",
    emerg_priority %in% c("<=8hrs-Non-critical but emergent" , "D-8hrs Urgent") ~ "<8hrs",
    emerg_priority %in% c("<=24hrs-Non critical, non-emergent", "E-24hrs Semi-Urgent") ~ "<24hrs",
    emerg_priority %in% c("<=72hrsCannot discharge before procedure", "F-72hrs Non-Urgent") ~ "<72hrs")) %>%
  mutate(emerg_priority_collapse = factor(emerg_priority_collapse, levels = urgency_levels_collapsed)) %>%
  rowwise() %>%
  mutate(procedural_consultant =  digest(procedural_consultant, algo = "md5")) %>%
  ungroup()

saveRDS(study_db, "study_db_all.RDS") #if not moving directly on to script 2
