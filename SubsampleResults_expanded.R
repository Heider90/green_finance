#Goal of this script is to create ordered likert scale DFs including rowsums and import them into excel

#Goal of this script is to create summary statistics and plots of various question of the 
#green finance modul of the oenb survey 2022
options(scipen = 999)
rm(list = ls())
Sys.setlocale("LC_ALL","English")  
library(tidyverse)
library(readxl)
library(data.table)
library(tidylog)
library(haven)
library(foreign)
library(survey)
library(srvyr)

survey_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")

####################################################################################################
####################################################################################################
#I create different survey subsets based on education level, gender, income level and savings level:
####################################################################################################
####################################################################################################
#Females 
wgt_srv_fem <- survey_wgt %>% 
  filter(female01 == 1)
#743 out of 1431 individuals --> 52% of the sample 

#Individuals who at least have Matura
wgt_srv_edu1 <- survey_wgt %>%
  filter(edu4 %in% c("high sec edu", "tertiary education"))
#836 out of 1431 individuals -> 58% of the sample

#Individuals who have univerity education
wgt_srv_edu2 <- survey_wgt %>%
  filter(edu4 %in% c("tertiary education"))
#230 out of 1431 individuals -> 16% of the sample
#females

#Individuals with an income of over 2k net
wgt_srv_inc <- survey_wgt %>%
  filter(pinc %in% c("1950-3000", "3000+"))
#583 out of 1431 individuals -> 41% of the sample 

#Individuals with an income of over 3k net
wgt_srv_inc2 <- survey_wgt %>%
  filter(pinc %in% c("3000+"))
#181 out of 1431 individuals -> 13% of the sample 

#Individuals who can save 300 euros or more
wgt_srv_saver <- survey_wgt %>%
  filter(bf19a_fct %in% c("bis 300", "bis 500", "bis 1000", "mehr als 1000"))
#413 of 1431 observations -> 29% of the sample




# Calculate the sum of rows 3 and 4 and store it in a new row
# Reorder the data frame based on desired values in the first column
desired_order <- c("Keine Angabe",
                   "Trifft nicht zu",
                   "Weiss nicht",
                   "Stimme eher nicht zu",
                   "Stimme ueberhaupt nicht zu",
                   "Stimme nicht zu",
                   "Weder noch",
                   "Stimme vollkommen zu",
                   "Stimmer eher zu",
                   "Stimme zu")








################################################################################
#######LIKERT REORDERING########################################################
#21a_1

sum_row_stimmezu <- f21a_1_all %>%
  filter(bf21a_1 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- f21a_1_all %>%
  filter(bf21a_1 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
f21a_1_all_new <- f21a_1_all %>%
  add_row(bf21a_1 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(f21a_1_all)) %>% 
  add_row(bf21a_1 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(f21a_1_all)) %>%
  mutate(bf21a_1 = factor(bf21a_1, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21a_1)


write.csv(f21a_1_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_1_all_new.csv", row.names = FALSE)

################################################################################
#21a_2
sum_row_stimmezu <- bf21a_2_all %>%
  filter(bf21a_2 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21a_2_all %>%
  filter(bf21a_2 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
f21a_2_all_new <- bf21a_2_all %>%
  add_row(bf21a_2 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21a_2_all)) %>% 
  add_row(bf21a_2 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21a_2_all)) %>%
  mutate(bf21a_2 = factor(bf21a_2, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21a_2)


#bf21a
write.csv(f21a_2_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_2_all_new.csv", row.names = FALSE)

################################################################################
#21a_3
sum_row_stimmezu <- bf21a_3_all %>%
  filter(bf21a_3 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21a_3_all %>%
  filter(bf21a_3 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
f21a_3_all_new <- bf21a_3_all %>%
  add_row(bf21a_3 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21a_3_all)) %>% 
  add_row(bf21a_3 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21a_3_all)) %>%
  mutate(bf21a_3 = factor(bf21a_3, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21a_3)


#bf21a
write.csv(f21a_3_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_3_all_new.csv", row.names = FALSE)

################################################################################
#21a_4
sum_row_stimmezu <- bf21a_4_all %>%
  filter(bf21a_4 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21a_4_all %>%
  filter(bf21a_4 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
f21a_4_all_new <- bf21a_4_all %>%
  add_row(bf21a_4 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21a_4_all)) %>% 
  add_row(bf21a_4 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21a_4_all)) %>%
  mutate(bf21a_4 = factor(bf21a_4, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21a_4)


#bf21a
write.csv(f21a_4_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_4_all_new.csv", row.names = FALSE)


################################################################################
#21a_5
sum_row_stimmezu <- bf21a_5_all %>%
  filter(bf21a_5 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21a_5_all %>%
  filter(bf21a_5 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
f21a_5_all_new <- bf21a_5_all %>%
  add_row(bf21a_5 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21a_5_all)) %>% 
  add_row(bf21a_5 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21a_5_all)) %>%
  mutate(bf21a_5 = factor(bf21a_5, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21a_5)


#bf21a
write.csv(f21a_5_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_5_all_new.csv", row.names = FALSE)

################################################################################
#21a_6
sum_row_stimmezu <- bf21a_6_all %>%
  filter(bf21a_6 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21a_6_all %>%
  filter(bf21a_6 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
f21a_6_all_new <- bf21a_6_all %>%
  add_row(bf21a_6 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21a_2_all)) %>% 
  add_row(bf21a_6 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21a_2_all)) %>%
  mutate(bf21a_6 = factor(bf21a_6, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21a_6)


#bf21a
write.csv(f21a_6_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_6_all_new.csv", row.names = FALSE)

################################################################################
#21b_1
sum_row_stimmezu <- bf21b_1_all %>%
  filter(bf21b_1 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_1_all %>%
  filter(bf21b_1 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_1_all_new <- bf21b_1_all %>%
  add_row(bf21b_1 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_1_all)) %>% 
  add_row(bf21b_1 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_1_all)) %>%
  mutate(bf21b_1 = factor(bf21b_1, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_1)


#21b_1
write.csv(bf21b_1_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_1_all_new.csv", row.names = FALSE)

################################################################################
#21b_2
sum_row_stimmezu <- bf21b_2_all %>%
  filter(bf21b_2 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_2_all %>%
  filter(bf21b_2 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_2_all_new <- bf21b_2_all %>%
  add_row(bf21b_2 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_2_all)) %>% 
  add_row(bf21b_2 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_2_all)) %>%
  mutate(bf21b_2 = factor(bf21b_2, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_2)


#bf21b_2
write.csv(bf21b_2_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_2_all_new.csv", row.names = FALSE)


################################################################################
#21b_3
sum_row_stimmezu <- bf21b_3_all %>%
  filter(bf21b_3 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_3_all %>%
  filter(bf21b_3 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_3_all_new <- bf21b_3_all %>%
  add_row(bf21b_3 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_3_all)) %>% 
  add_row(bf21b_3 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_3_all)) %>%
  mutate(bf21b_3 = factor(bf21b_3, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_3)


#bf21a
write.csv(bf21b_3_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_3_all_new.csv", row.names = FALSE)

################################################################################
#21b_4
sum_row_stimmezu <- bf21b_4_all %>%
  filter(bf21b_4 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_4_all %>%
  filter(bf21b_4 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_4_all_new <- bf21b_4_all %>%
  add_row(bf21b_4 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_4_all)) %>% 
  add_row(bf21b_4 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_4_all)) %>%
  mutate(bf21b_4 = factor(bf21b_4, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_4)


#bf21a
write.csv(bf21b_4_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_4_all_new.csv", row.names = FALSE)


################################################################################
#21b_5
sum_row_stimmezu <- bf21b_5_all %>%
  filter(bf21b_5 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_5_all %>%
  filter(bf21b_5 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_5_all_new <- bf21b_5_all %>%
  add_row(bf21b_5 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_5_all)) %>% 
  add_row(bf21b_5 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_5_all)) %>%
  mutate(bf21b_5 = factor(bf21b_5, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_5)


#bf21a
write.csv(bf21b_5_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_5_all_new.csv", row.names = FALSE)

################################################################################
#21b_6
sum_row_stimmezu <- bf21b_6_all %>%
  filter(bf21b_6 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_6_all %>%
  filter(bf21b_6 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_6_all_new <- bf21b_6_all %>%
  add_row(bf21b_6 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_6_all)) %>% 
  add_row(bf21b_6 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_6_all)) %>%
  mutate(bf21b_6 = factor(bf21b_6, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_6)


#bf21a
write.csv(bf21b_6_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_6_all_new.csv", row.names = FALSE)

################################################################################
#21b_7
sum_row_stimmezu <- bf21b_7_all %>%
  filter(bf21b_7 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_7_all %>%
  filter(bf21b_7 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_7_all_new <- bf21b_7_all %>%
  add_row(bf21b_7 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_7_all)) %>% 
  add_row(bf21b_7 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_7_all)) %>%
  mutate(bf21b_7 = factor(bf21b_7, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_7)


#bf21a
write.csv(bf21b_7_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_7_all_new.csv", row.names = FALSE)

################################################################################
#21b_8
sum_row_stimmezu <- bf21b_8_all %>%
  filter(bf21b_8 %in% c("Stimme vollkommen zu", "Stimmer eher zu")) %>%
  summarise(across(where(is.numeric), sum))


sum_row_stimmenicht <- bf21b_8_all %>%
  filter(bf21b_8 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu")) %>%
  summarise(across(where(is.numeric), sum))

# Add a new row for the sum of rows 3 and 4 to the original data frame
bf21b_8_all_new <- bf21b_8_all %>%
  add_row(bf21b_8 = "Stimme zu", !!!sum_row_stimmezu, .after = nrow(bf21b_8_all)) %>% 
  add_row(bf21b_8 = "Stimme nicht zu", !!!sum_row_stimmenicht, .after = nrow(bf21b_8_all)) %>%
  mutate(bf21b_8 = factor(bf21b_8, levels = desired_order, ordered = TRUE)) %>% 
  arrange(bf21b_8)


#bf21a
write.csv(bf21b_8_all_new, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_8_all_new.csv", row.names = FALSE)
