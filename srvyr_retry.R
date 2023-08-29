
options(scipen = 999)
rm(list = ls())
Sys.setlocale("LC_ALL","English")  
library(tidyverse)
library(readxl)
library(data.table)
library(countrycode)
library(tidylog)
library(haven)
library(foreign)
library(survey)
library(srvyr)

#Load Survey Data
survey <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")
#OENB survey weighted


#Recode Anteil der ö. Bevölkerung, die in 2/5/10/15 JAHREN direkt von Umweltkatastrophen betroffen ist
#as fct with categories 0-4%, 5-14%, 15-50%, >50%, weiß nicht, keine Angabe 
survey <- survey %>%
  mutate(bf7_1fct = cut(bf7_1,
                        breaks = c(-Inf, -99, -97, 4, 14, 50, Inf),
                        labels = c("keine Angabe", "Weiss nicht", "0-4%", "5-14%", "15-50%", ">50%"),
                        include.lowest = TRUE),
         bf7_2fct = cut(bf7_2,
                        breaks = c(-Inf, -99, -97, 4, 14, 50, Inf),
                        labels = c("keine Angabe", "Weiss nicht", "0-4%", "5-14%", "15-50%", ">50%"),
                        include.lowest = TRUE),
         bf7_3fct = cut(bf7_3,
                        breaks = c(-Inf, -99, -97, 4, 14, 50, Inf),
                        labels = c("keine Angabe", "Weiss nicht", "0-4%", "5-14%", "15-50%", ">50%"),
                        include.lowest = TRUE),
         bf7_4fct = cut(bf7_4,
                        breaks = c(-Inf, -99, -97, 4, 14, 50, Inf),
                        labels = c("keine Angabe", "Weiss nicht", "0-4%", "5-14%", "15-50%", ">50%"),
                        include.lowest = TRUE))


#Recode Savings as fct variable with values 100, 200, 300, 500, 1000, more than 1000
survey$bf19a_fct <- cut(survey$bf19a, 
                        breaks = c(-Inf, -99, -97, 100, 200,300, 500, 1000, Inf),
                        labels = c("keine Angabe", "Weiss nicht", "bis 100", "bis 200", "bis 300", "bis 500", "bis 1000", "mehr als 1000"),
                        include.lowest = TRUE)


#Recode Answers of Der Klimawandel wird meine finanzielle Situation in 2/5/10/15 Jahren..


survey <- survey %>%
  mutate(across(c(bf8_1, bf8_2, bf8_3, bf8_4), ~recode(as.character(.),
                                                       "-99" = "Keine Angabe",
                                                       "-98" = "Trifft nicht zu",
                                                       "-97" = "Weiss nicht",
                                                       "1" = "verschlechtern",
                                                       "2" = "eher verschlechtern",
                                                       "3" = "Weder noch",
                                                       "4" = "eher verbessern",
                                                       "5" = "verbessern")))

survey <- survey %>%
  mutate(across(c(bf21a_1,bf21a_2,bf21a_3,bf21a_4,
                  bf21a_5,bf21a_6,bf21b_1,bf21b_2,
                  bf21b_3,bf21b_4,bf21b_5,bf21b_6,
                  bf21b_7,bf21b_8), ~recode(as.character(.),
                                            "-99" = "Keine Angabe",
                                            "-98" = "Trifft nicht zu",
                                            "-97" = "Weiss nicht",
                                            "1" = "Stimme vollkommen zu",
                                            "2" = "Stimmer eher zu",
                                            "3" = "Weder noch",
                                            "4" = "Stimme eher nicht zu",
                                            "5" = "Stimme ueberhaupt nicht zu")))

#Reduce DF to Columns needed
survey_wgt <- survey %>% 
  select(age,agecat,female01,edu4,msize,jobstatus,pinc,bf19a, bf19a_fct,bf7_1,bf7_2,bf7_3,bf7_4,
         bf7_1fct, bf7_2fct, bf7_3fct, bf7_4fct, 
         bf8_1, bf8_2, bf8_3, bf8_4,
         bf21a_1,bf21a_2,bf21a_3,bf21a_4,
         bf21a_5,bf21a_6,bf21b_1,bf21b_2,
         bf21b_3,bf21b_4,bf21b_5,bf21b_6,
         bf21b_7,bf21b_8, weight)




#Recode to weighted survey
survey_wgt <- as_survey_design(survey, weights = weight)



#SAVE AS WEIGHTED SURVEY
# Save as R file
saveRDS(survey_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")


summary_bf8_1 <- survey_wgt %>%
  survey_count(bf8_1) %>% 
  mutate(Percentage = round(n / sum(n) * 100))


summary_bf8_2 <- survey_wgt %>%
  survey_count(bf8_2) %>% 
  mutate(Percentage = round(n / sum(n) * 100))



