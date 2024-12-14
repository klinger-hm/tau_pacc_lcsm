library(dplyr)
library(ggplot2)
library(flextable)
library(gtsummary)
library(FSA)
library(stringr)
library(tidyr)
library(vctrs)

load("~/Dropbox (Partners HealthCare)/SEM projects/project_v2/HABS Data/dcpull_longpacctaupib_112624.RData")

wide_data <- data %>%
  group_by(SubjIDshort) %>%
  mutate(bl_DX = dplyr::first(na.omit(HABS_DX)),
         bl_Age = dplyr::first(na.omit(NP_Age)),
         bl_TauAge = dplyr::first(na.omit(TAU_Age_j1)),
         bl_PIBGroup = dplyr::first(na.omit(PIB_FS_DVR_Group)),
         StudyArc = str_replace(StudyArc, "HABL", "HAB"),
         StudyArc_j1 = str_replace(StudyArc_j1, "HABL", "HAB"),
         tau_label = str_replace(StudyArc_j1, "HAB_", "TAU_"),
         pacc_label = str_replace(StudyArc, "HAB_", "PACC_")) %>%
  pivot_wider(names_from = tau_label, values_from = TAU_FS_SUVR_entorhinal_bh) %>%
  select(-c("NA")) %>%
  pivot_wider(names_from = pacc_label, values_from = NP_PACC_PACC5) %>%
  select(SubjIDshort, SEX, YrsEd, E4_Status, Race, Ethnicity, bl_DX, bl_PIBGroup,
         bl_Age, PACC_1.0, PACC_2.0, PACC_3.0, PACC_4.0, PACC_5.0, PACC_6.0, 
         PACC_7.0, PACC_8.0, PACC_9.0, PACC_10.0, PACC_11.0, PACC_12.0, 
         bl_TauAge, TAU_1.0, TAU_2.0, TAU_3.0, TAU_4.0, TAU_5.0, TAU_6.0, TAU_9.0, TAU_12.0) %>%
  filter(bl_DX == "CN") %>%
  group_by(SubjIDshort) %>%
  fill(contains("TAU_") | contains("PACC_"), .direction = "updown") %>%
  distinct()


wide_data_years <- data %>%
  group_by(SubjIDshort) %>%
  mutate(bl_Date = dplyr::first(na.omit(NP_SessionDate)),
         bl_DX = dplyr::first(na.omit(HABS_DX)),
         bl_Age = dplyr::first(na.omit(NP_Age)),
         bl_TauAge = dplyr::first(na.omit(TAU_Age_j1)),
         bl_PIBGroup = dplyr::first(na.omit(PIB_FS_DVR_Group)),
         StudyArc = str_replace(StudyArc, "HABL", "HAB"),
         StudyArc_j1 = str_replace(StudyArc_j1, "HABL", "HAB"),
         pacc_time = as.numeric((NP_SessionDate - bl_Date)/365.25),
         pacc_time_round = round(pacc_time),
         tau_time = as.numeric((TAU_SessionDate_j1 - bl_Date)/365.25),
         tau_time_round = round(tau_time),
         tau_label = as.numeric(str_replace(StudyArc_j1, "HAB_", "")) - 1,
         pacc_label = as.numeric(str_replace(StudyArc, "HAB_", "")) - 1,
         pacc_compare = case_when(
           pacc_time_round == pacc_label ~ 0,
           pacc_time_round != pacc_label ~ 1,
           TRUE ~ NA),
         tau_compare = case_when(
           tau_time_round == tau_label ~ 0,
           tau_time_round != tau_label ~ 1,
           TRUE ~ NA),
         pacc_time_diff = pacc_time - lag(pacc_time),
         tau_time_fill = vec_fill_missing(tau_time, "down"),
         tau_time_diff = tau_time - lag(tau_time_fill)) #%>%
 # pivot_wider(names_from = tau_label, values_from = TAU_FS_SUVR_entorhinal_bh) %>%
 #  select(-c("NA")) %>%
 #  pivot_wider(names_from = pacc_label, values_from = NP_PACC_PACC5) %>%
 #  select(SubjIDshort, SEX, YrsEd, E4_Status, Race, Ethnicity, bl_DX, bl_PIBGroup,
 #         bl_Age, PACC_1.0, PACC_2.0, PACC_3.0, PACC_4.0, PACC_5.0, PACC_6.0, 
 #         PACC_7.0, PACC_8.0, PACC_9.0, PACC_10.0, PACC_11.0, PACC_12.0, 
 #         bl_TauAge, TAU_1.0, TAU_2.0, TAU_3.0, TAU_4.0, TAU_5.0, TAU_6.0, TAU_9.0, TAU_12.0) %>%
 #  filter(bl_DX == "CN") %>%
 #  group_by(SubjIDshort) %>%
 #  fill(contains("TAU_") | contains("PACC_"), .direction = "updown") %>%
 #  distinct()

summary(wide_data_years$pacc_time_diff); sd(wide_data_years$pacc_time_diff, na.rm = T)
summary(wide_data_years$tau_time_diff); sd(wide_data_years$tau_time_diff, na.rm = T)

table(wide_data_years$pacc_compare)
table(wide_data_years$tau_compare)


test_pacc <- wide_data_years %>%
  group_by(SubjIDshort, pacc_time_round) %>%
  tally()

nrow(subset(test_pacc, n > 1))


test_tau <- wide_data_years %>%
  filter(!is.na(tau_time)) %>%
  group_by(SubjIDshort, tau_time_round) %>%
  tally()

nrow(subset(test_tau, n > 1))


# data <- data %>%
#   group_by(SubjIDshort) %>%
#   add_tally(name = "pacc_tp", wt = !is.na(NP_PACC_PACC5)) %>%
#   add_tally(name = "tau_tp", wt = !is.na(TAU_FS_SUVR_entorhinal_bh)) %>%
#   mutate(
#     bl_DX = dplyr::first(na.omit(HABS_DX)),
#     bl_PACC = dplyr::first(na.omit(NP_PACC_PACC5)),
#     bl_ECTAU = dplyr::first(na.omit(TAU_FS_SUVR_entorhinal_bh)),
#     bl_ITTAU = dplyr::first(na.omit(TAU_FS_SUVR_inferiortemporal_bh)),
#     bl_PIBGroup = dplyr::first(na.omit(PIB_FS_DVR_Group)),
#     bl_TAUStudyArc = dplyr::first(na.omit(StudyArc_j1)),
#     pacc_time = as.numeric((NP_SessionDate - dplyr::first(na.omit(NP_SessionDate)))/365.25),
#     tau_time = as.numeric((TAU_SessionDate_j1 - dplyr::first(na.omit(TAU_SessionDate_j1)))/365.25),
#     pacc_visit = row_number(),
#     tau_visit = replace(row_number() - cumsum(is.na(tau_time)), is.na(tau_time), NA),
#     pacc_label = str_replace(StudyArc, "L", ""),
#     tau_label = str_replace(StudyArc_j1, "L", "")) %>%
#   filter(bl_DX == "CN") %>%
#   filter(tau_tp > 0)
# 
# data$pacc_label <- factor(data$pacc_label, levels = c("HAB_1.0", "HAB_2.0", "HAB_3.0", "HAB_4.0", "HAB_5.0", "HAB_6.0", 
#                                                       "HAB_7.0", "HAB_8.0", "HAB_9.0", "HAB_10.0", "HAB_11.0", "HAB_12.0", "HAB_13.0", "HAB_14.0"))
# 
# data$tau_label <- factor(data$tau_label, levels = c("HAB_1.0", "HAB_2.0", "HAB_3.0", "HAB_4.0", "HAB_5.0", "HAB_6.0", 
#                                                     "HAB_9.0", "HAB_12.0"))
# 
# baseline <- data %>%
#   group_by(SubjIDshort) %>%
#   arrange(SubjIDshort, NP_SessionDate) %>%
#   slice(1)
# 
# tau_baseline <- data %>%
#   filter(!is.na(TAU_FS_SUVR_entorhinal_bh)) %>%
#   group_by(SubjIDshort) %>%
#   arrange(SubjIDshort, TAU_SessionDate_j1) %>%
#   slice(1)
# 
# tau_visits <- baseline %>%
#   group_by(tau_tp) %>%
#   tally(name = "unique_ppts") %>%
#   mutate(total_ppts = rcumsum(unique_ppts))
# 
# pacc_visits <- baseline %>%
#   group_by(pacc_tp) %>%
#   tally(name = "unique_ppts") %>%
#   mutate(total_ppts = rcumsum(unique_ppts))
# 
# long_data <- data %>%
#   pivot_longer(c(pacc_label, tau_label), names_to = "Measure", values_to = "StudyArc_combo")
# 
# data <- data %>%
#   group_by(SubjIDshort) %>%
#   mutate(pacc_studyarcs = paste0(StudyArc))
