#Green FInance
#Goal of this script: analyse climate related question from a generational e.g. young/old pov in order to point out
#differences and similarities 
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

#Load Survey Data
survey <- read_dta("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/60011055b_externv_3.dta")



###################################################################################################
###################################################################################################
###################################################################################################
#DATA WRANGLING: 
#RECODE EXISTING VARIABLES



# Convert variable names to lowercase
names(survey) <- tolower(names(survey))

# Create socio-economic characteristics
# Age
survey$age <- survey$alterxp1
survey$sqage <- survey$age^2

# Age category
survey$agecat <- cut(survey$age, breaks = c(15, 29, 44, 59, 79, 150),
                     labels = c("15-29 years", "30-44 years", "45-59 years", "60-79 years", "80+ years"))


# Female
survey$female01 <- ifelse(survey$geschlxp1 == 2, 1, 0)


# Education
survey <- survey %>%
  mutate(edu4 = case_when(
    educxp1 %in% 1:2 ~ "primary education",
    educxp1 == 3 ~ "Low sec edu",
    educxp1 %in% 4:9 ~ "high sec edu",
    educxp1 %in% 10:12 ~ "tertiary education",
    TRUE ~ as.character(educxp1)
  )) %>%
  mutate(edu4 = factor(edu4))


# Municipality size
#Andreas Kategorien: 0-5k, 5k-1Mio, 1Mio+
survey <- survey %>%
  mutate(msize = case_when(
    ogr %in% 1:3 ~ 1,
    ogr %in% 4:7 ~ 2,
    ogr == 8 ~ 3
  )) %>%
  mutate(msize = factor(msize,
                        levels = c(1, 2, 3),
                        labels = c("0-5000","5000-1Mio", "1 Mio+")))


# Job status
survey <- survey %>%
  mutate(jobstatus = case_when(
    taetxp1 == 1 ~ "fulltime",
    taetxp1 == 13 ~ "Fulltime",
    taetxp1 %in% 14:15 ~ "Parttime",
    taetxp1 == 5 ~ "unemployed",
    taetxp1 == 6 ~ "retired",
    TRUE ~ "Other"
  ))



#Income 
survey <- survey %>%
  mutate(pinc = case_when(
    zpnett %in% 1:4 ~ "0-900",
    zpnett %in% 5:7 ~ "900-1350",
    zpnett %in% 8:9 ~ "1350-1650",
    zpnett %in% 10:11 ~ "1650-1950",
    zpnett %in% 12:16 ~ "1950-3000",
    zpnett %in% 17:24 ~ "3000+",
    zpnett %in% c(25, 98) ~ "kein Einkommen",
    zpnett %in% c(26, 99) ~ "weiss nicht"
  )) %>% 
  mutate(pinc_d = as.factor(pinc))
###################################################################################################
#Create new Variables


# Generate other variables
# Car to work
survey$cartowork01 <- ifelse(survey$bf1 == 1, TRUE, NA)


#Recode Anteil der ˆ. Bevˆlkerung, die in 2/5/10/15 JAHREN direkt von Umweltkatastrophen betroffen ist
#as fct with categories 0-4%, 5-14%, 15-50%, >50%, weiﬂ nicht, keine Angabe 
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


#Recode to weighted survey
survey_wgt <- as_survey_design(survey, weights = weight)



###################################################################################################
###################################################################################################
###################################################################################################

#SAVE Data
# Save as CSV
write.csv(survey, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.csv", row.names = FALSE)


# Save as R file
#Full Survey
saveRDS(survey, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")
#Weighted Survey
saveRDS(survey_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")
