#Regression 
#Goal of this Script: Recode Variables, create weighted survey dataset and do regressions
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
library(forcats)
library(stargazer)
library(margins)
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
###################################################################################################
###################################################################################################
#Create new Variables


# Generate other variables
# Car to work
survey$cartowork01 <- ifelse(survey$bf1 == 1, TRUE, NA)


#Recode Anteil der Ã¶. BevÃ¶lkerung, die in 2/5/10/15 JAHREN direkt von Umweltkatastrophen betroffen ist
#as fct with categories 0-4%, 5-14%, 15-50%, >50%, weiÃ nicht, keine Angabe 
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


#Recode F24 as proxy for savings e.g. 100€, 500€, 1000€, 2000€, 5000€
survey <- survey %>%
  mutate(dummy_bf24_1 = case_when(
    as.character(bf24_1) == "1" ~ 100,
    TRUE ~ 0
  ),
  dummy_bf24_2 = case_when(
    as.character(bf24_2) == "1" ~ 500,
    TRUE ~ 0
  ),
  dummy_bf24_3 = case_when(
    as.character(bf24_3) == "1" ~ 1000,
    TRUE ~ 0
  ),
  dummy_bf24_4 = case_when(
    as.character(bf24_4) == "1" ~ 2000,
    TRUE ~ 0
  ),
  dummy_bf24_5 = case_when(
    as.character(bf24_5) == "1" ~ 5000,
    TRUE ~ 0
  ))




survey <- survey %>%
  rowwise() %>%
  mutate(savings = max(dummy_bf24_1, dummy_bf24_2, dummy_bf24_3, dummy_bf24_4, dummy_bf24_5),
         savings_fct = factor(savings, levels = c(0, 100, 500, 1000, 2000, 5000)))


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


#Create Green Indeces
#Opinion_Index where the following answers are coded as 1(e.g. green)
#F21a_1 -> "Stimme eher nicht zu", "Stimme ueberhaupt nicht zu"
survey$dummy_f21a1 <- ifelse(survey$bf21a_1 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu"), 1, 0)
#F21a_2 -> "Stimme vollkommen zu", "Stimmer eher zu"
survey$dummy_f21a2 <- ifelse(survey$bf21a_2 %in% c("Stimme vollkommen zu", "Stimmer eher zu"), 1, 0)
#F21a_3 -> "Stimme eher nicht zu", "Stimme ueberhaupt nicht zu"
survey$dummy_f21a3 <- ifelse(survey$bf21a_3 %in% c("Stimme eher nicht zu", "Stimme ueberhaupt nicht zu"), 1, 0)
#F21a_4 -> "Stimme vollkommen zu", "Stimmer eher zu"
survey$dummy_f21a4 <- ifelse(survey$bf21a_4 %in% c("Stimme vollkommen zu", "Stimmer eher zu"), 1, 0)
#F21a_5 -> "Stimme vollkommen zu", "Stimmer eher zu"
survey$dummy_f21a5 <- ifelse(survey$bf21a_5 %in% c("Stimme vollkommen zu", "Stimmer eher zu"), 1, 0)




#Attitude-Index where the following answers are coded as 1(e.g. green)
#F21b_1 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_2 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_3 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_4 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_5 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_6 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_7 -> "Stimme vollkommen zu", "Stimmer eher zu"
#F21b_8 -> "Stimme vollkommen zu", "Stimmer eher zu"

# Columns bf21b_1 to bf21b_8
for (i in 1:8) {
  col_name <- paste0("bf21b_", i)
  dummy_name <- paste0("dummy_", col_name)
  survey[[dummy_name]] <- ifelse(survey[[col_name]] %in% c("Stimme vollkommen zu", "Stimmer eher zu"), 1, 0)
}






#Create Index for financial literacy based on questions 23_1 to 23_2
survey$dummy_f23_1 <- ifelse(survey$bf23_1 %in% c(1), 1, 0)
survey$dummy_f23_2 <- ifelse(survey$bf23_2 %in% c(1), 1, 0)
survey$dummy_f23_3 <- ifelse(survey$bf23_3 %in% c(1), 1, 0)

#Now i create the indeces columns
survey <- survey %>% 
  mutate(index_opinion = dummy_f21a1 + dummy_f21a2 + dummy_f21a3 + dummy_f21a4 + dummy_f21a5,
         index_attitude = dummy_bf21b_1 + dummy_bf21b_2 +dummy_bf21b_3 +dummy_bf21b_4 +dummy_bf21b_5 +
           dummy_bf21b_6 +dummy_bf21b_7 + dummy_bf21b_8,
         index_tot = index_opinion + index_attitude,
         fin_lit = dummy_f23_1 + dummy_f23_2 + dummy_f23_3)



###################################################################################################
###################################################################################################
###################################################################################################
#Create Regression Table

survey_reg <- survey %>% 
  filter(!(pinc_d %in% c("weiss nicht"))) %>%
  mutate(pinc_d = fct_relevel(pinc_d,
                              "kein Einkommen", "0-900", "900-1350",
                              "1350-1650", "1650-1950", "1950-3000",
                              "3000+"),
         edu4 = fct_relevel(edu4, "primary education", "Low sec edu",
                            "high sec edu", "tertiary education"),
         female01 = factor(female01, levels = c(0, 1), labels = c("Male", "Female")),
         fin_lit = factor(fin_lit, levels = c(0,1,2,3), labels = c("none","low", "medium", "high"))) %>% 
  select(alterxp1, edu4, pinc_d, jobstatus, female01, msize,fin_lit,savings_fct, index_opinion,
         index_attitude, index_tot, weight) %>% 
  rename(age = alterxp1)

#Create Weighted Regression Frame
srv_reg_wgt <- as_survey_design(survey_reg, weights = weight)
survey_wgt <- as_survey_design(survey, weights = weight)

###################################################################################################
###################################################################################################
###################################################################################################
#REGRESSIONS
#Model 1: Total index
model1_wgt1 <- svyglm(index_tot ~ age + I(age^2) + pinc_d + edu4 + female01 + fin_lit + msize, design  = srv_reg_wgt)
#Model2: Meinungs Index
model2_wgt2 <- svyglm(index_opinion ~ age + I(age^2) + pinc_d + edu4 + female01 + fin_lit + msize, design = srv_reg_wgt)

#Model 3: Haltungs Index
model3_wgt3 <- svyglm(index_attitude ~ age + I(age^2) + pinc_d + edu4 +female01 + fin_lit + msize, design = srv_reg_wgt)



stargazer(model1_wgt1, model2_wgt2, model3_wgt3,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 1", "Model 2", "Model 3"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")


#Excluding financial literacy
#Model 1: Total index
model3_wgt1 <- svyglm(index_tot ~ age + I(age^2) + pinc_d + edu4 + female01  + msize, design  = srv_reg_wgt)
#Model2: Meinungs Index
model4_wgt2 <- svyglm(index_opinion ~ age + I(age^2) + pinc_d + edu4 + female01  + msize, design = srv_reg_wgt)

#Model 3: Haltungs Index
model5_wgt3 <- svyglm(index_attitude ~ age + I(age^2) + pinc_d + edu4 +female01  + msize, design = srv_reg_wgt)



stargazer(model3_wgt1, model4_wgt2, model5_wgt3,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 4", "Model 5", "Model 6"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")


#Excluding financial literacy & msize
#Model 1: Total index
model6_wgt1 <- svyglm(index_tot ~ age + I(age^2) + pinc_d + edu4 + female01 , design  = srv_reg_wgt)
#Model2: Meinungs Index
model7_wgt2 <- svyglm(index_opinion ~ age + I(age^2) + pinc_d + edu4 + female01 , design = srv_reg_wgt)

#Model 3: Haltungs Index
model8_wgt3 <- svyglm(index_attitude ~ age + I(age^2) + pinc_d + edu4 +female01 , design = srv_reg_wgt)



stargazer(model6_wgt1, model7_wgt2, model8_wgt3,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 7", "Model 8", "Model 9"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")

#Excluding financial literacy & msize & age
model9_wgt1 <- svyglm(index_tot ~  pinc_d + edu4 + female01 , design  = srv_reg_wgt)
#Model2: Meinungs Index
model10_wgt2 <- svyglm(index_opinion ~  pinc_d + edu4 + female01 , design = srv_reg_wgt)

#Model 3: Haltungs Index
model11_wgt3 <- svyglm(index_attitude ~  pinc_d + edu4 +female01 , design = srv_reg_wgt)



stargazer(model9_wgt1, model10_wgt2, model10_wgt2,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 7", "Model 8", "Model 9"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")



#Excluding education
#Model 1: Total index
model12_wgt1 <- svyglm(index_tot ~ age + I(age^2) + pinc_d  + female01 + fin_lit + msize, design  = srv_reg_wgt)
#Model2: Meinungs Index
model13_wgt2 <- svyglm(index_opinion ~ age + I(age^2) + pinc_d  + female01 + fin_lit + msize, design = srv_reg_wgt)

#Model 3: Haltungs Index
model14_wgt3 <- svyglm(index_attitude ~ age + I(age^2) + pinc_d  +female01 + fin_lit + msize, design = srv_reg_wgt)



stargazer(model12_wgt1, model13_wgt2, model14_wgt3,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 1", "Model 2", "Model 3"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")



#Excluding Income 
#Model 1: Total index
model15_wgt1 <- svyglm(index_tot ~ age + I(age^2) + edu4  + female01 + fin_lit + msize, design  = srv_reg_wgt)
#Model2: Meinungs Index
model16_wgt2 <- svyglm(index_opinion ~ age + I(age^2) + edu4  + female01 + fin_lit + msize, design = srv_reg_wgt)

#Model 3: Haltungs Index
model17_wgt3 <- svyglm(index_attitude ~ age + I(age^2) + edu4  +female01 + fin_lit + msize, design = srv_reg_wgt)



stargazer(model15_wgt1, model16_wgt2, model17_wgt3,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 1", "Model 2", "Model 3"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")



#Regression including Savings
#REGRESSIONS
#Model 1: Total index
model18_wgt1 <- svyglm(index_tot ~ age + I(age^2) + pinc_d + edu4 + savings_fct +  female01 + fin_lit + msize, design  = srv_reg_wgt)
#Model2: Meinungs Index
model19_wgt2 <- svyglm(index_opinion ~ age + I(age^2) + pinc_d + edu4 + savings_fct + female01 + fin_lit + msize, design = srv_reg_wgt)

#Model 3: Haltungs Index
mode20_wgt3 <- svyglm(index_attitude ~ age + I(age^2) + pinc_d + edu4 + savings_fct +female01 + fin_lit + msize, design = srv_reg_wgt)



stargazer(model18_wgt1, model19_wgt2, mode20_wgt3,
          type = "text",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 1", "Model 2", "Model 3"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")


###################################################################################################
###################################################################################################
###################################################################################################
#Visualization