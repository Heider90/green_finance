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
library(forcats)

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


#Recode F24 as proxy for savings e.g. 100???, 500???, 1000???, 2000???, 5000???
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


#Create savings variable based on the savings dummy 
survey <- survey %>%
  rowwise() %>%
  mutate(savings = max(dummy_bf24_1, dummy_bf24_2, dummy_bf24_3, dummy_bf24_4, dummy_bf24_5))

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



#Now i create the indeces columns
survey <- survey %>% 
  mutate(index_opinion = dummy_f21a1 + dummy_f21a2 + dummy_f21a3 + dummy_f21a4 + dummy_f21a5,
         index_attitude = dummy_bf21b_1 + dummy_bf21b_2 +dummy_bf21b_3 +dummy_bf21b_4 +dummy_bf21b_5 +
           dummy_bf21b_6 +dummy_bf21b_7 + dummy_bf21b_8,
         index_tot = index_opinion + index_attitude)



#Create Regression Table

survey_reg <- survey %>% 
  filter(!(pinc_d %in% c("weiss nicht"))) %>%
  mutate(pinc_d = fct_relevel(pinc_d,
                              "kein Einkommen", "0-900", "900-1350",
                              "1350-1650", "1650-1950", "1950-3000",
                              "3000+"),
         edu4 = fct_relevel(edu4, "primary education", "Low sec edu",
                            "high sec edu", "tertiary education"),
         female01 = factor(female01, levels = c(0, 1), labels = c("Male", "Female"))) %>% 
  select(alterxp1, edu4, pinc_d, jobstatus, female01, msize, index_opinion,
         index_attitude, index_tot, weight) %>% 
  rename(age = alterxp1)



##########################################################################
#REGRESSIONS

#Model 1: total Index regressed
model1 <- lm(index_tot ~ age + I(age^2) + pinc_d + edu4 + female01 + msize, data = survey_reg)

#Model2: Meinungs Index
model2 <- lm(index_opinion ~ age + I(age^2) + pinc_d + edu4 + female01 + msize, data = survey_reg)

#Model 3: Haltungs Index
model3 <- lm(index_attitude ~ age + I(age^2) + pinc_d + edu4 +female01 + msize, data = survey_reg)

# View the summary of the regression model
summary(model1)
summary(model2)
summary(model3)

##########################################################################
#WEIGHTED REGRESSIONS

#Weighted DATA Frame
srv_reg_wgt <- as_survey_design(survey_reg, weights = weight)


model1_wgt <- lm(index_tot ~ age + I(age^2) + pinc_d + edu4 + female01 + msize, data = srv_reg_wgt)

#Model2: Meinungs Index
model2_wgt <- lm(index_opinion ~ age + I(age^2) + pinc_d + edu4 + female01 + msize, data = srv_reg_wgt)

#Model 3: Haltungs Index
model3_wgt <- lm(index_attitude ~ age + I(age^2) + pinc_d + edu4 +female01 + msize, data = srv_reg_wgt)

# View the summary of the regression model
summary(model1_wgt)
summary(model2_wgt)
summary(model3_wgt)


#LATEX TABLES
#STARGAZER ADAPTED FE
stargazer(model1, type = "latex",
          title = "Total Index")

stargazer(model2, type = "latex",
          title = "Meinungs Index")


stargazer(model3, type = "latex",
          title = "Haltungs Index")


#Create Latex Code
#Pooled OLS Models
stargazer(model1_wgt, model2_wgt, model3_wgt,
          type = "latex",
          title = "Weighted Regression Tables",
          align = TRUE, 
          column.labels = c("Model 1", "Model 2", "Model 3"),
          omit.stat = c("f", "ser"),
          float = FALSE,
          table.placement =  "ht")

#Descriptive Plots
hist(survey_reg$index_opinion, breaks = 6, col = "blue", main = "Histogram of Meinungs Index", xlab = "Index Opinion")
ind_op <- data.frame(table(survey_reg$index_opinion)) 

hist(survey_reg$index_attitude, breaks = 8, col = "blue", main = "Histogram of Haltungs Index", xlab = "Index Attitude")
table(survey_reg$index_attitude)
ind_att <- data.frame(table(survey_reg$index_attitude)) 

hist(survey_reg$index_tot, breaks = 14, col = "blue", main = "Histogram of Total Index", xlab = "Total Index")
table(survey_reg$index_tot)
ind_tot <- data.frame(table(survey_reg$index_tot)) 

###################################################################################################
###################################################################################################
#Create Subsamples for: 
#ALL

#Woman
srv_fem <- survey %>% 
  filter(female01 == 1)

#EDU1: Matura oder höher
srv_edu1 <- survey %>% 
  filter(edu4 %in% c("high sec edu", "tertiary education"))


str(survey_reg)

#EDU2 Uni oder höher
srv_edu2 <- survey %>% 
  filter(edu4 %in% c("tertiary education"))

#Einkommen: 1950+
srv_inc1 <- survey %>%
  filter(pinc %in% c("1950-3000", "3000+"))

#Einkommen: 3000+
srv_inc2 <- survey %>%
  filter(pinc %in% c("3000+"))

#Sparer: 300+ pro Monat 
srv_sav <- survey %>%
  filter(bf19a_fct %in% c("bis 300", "bis 500", "bis 1000", "mehr als 1000"))

#Climate Pessimists: Klimawandel wird meine Situation in den 10 n�chsten 10 Jahren (eher) verschlechtern
srv_climpes <- survey %>% 
  filter(bf8_3 %in% c("eher verschlechtern", "verschlechtern"))


#Unter 44
srv_age <- survey %>% 
  filter(agecat %in% c("15-29 years", "30-44 years"))

#rural 
srv_rural <- survey %>% 
  filter(msize == "0-5000")

#employed 
srv_emp <- survey %>% 
  filter(jobstatus %in% c("Fulltime", "Parttime"))



survey <- survey %>%
  mutate(jobstatus = case_when(
    taetxp1 == 1 ~ "fulltime",
    taetxp1 == 13 ~ "Fulltime",
    taetxp1 %in% 14:15 ~ "Parttime",
    taetxp1 == 5 ~ "unemployed",
    taetxp1 == 6 ~ "retired",
    TRUE ~ "Other"
  ))


###################################################################################################
###################################################################################################
#Transform Data into survey data 
survey_wgt <- as_survey_design(survey, weights = weight)
srv_fem_wgt <- as_survey_design(srv_fem, weights = weight)
srv_edu1_wgt <- as_survey_design(srv_edu1, weights = weight)
srv_edu2_wgt <- as_survey_design(srv_edu2, weights = weight)
srv_inc1_wgt <- as_survey_design(srv_inc1, weights = weight)
srv_inc2_wgt <- as_survey_design(srv_inc2, weights = weight)
srv_sav_wgt <- as_survey_design(srv_sav, weights = weight)
srv_climpes_wgt <- as_survey_design(srv_climpes, weights = weight)
srv_rural_wgt <- as_survey_design(srv_rural, weights = weight)
srv_age_wgt <- as_survey_design(srv_age, weights = weight)
srv_emp_wgt <- as_survey_design(srv_emp, weights = weight)


###################################################################################################
###################################################################################################
#Save DATA
# Save as R file
#Full Survey
saveRDS(survey, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")
saveRDS(survey_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")
saveRDS(srv_fem_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_fem_wgt.rds")
saveRDS(srv_edu1_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_edu1_wgt.rds")
saveRDS(srv_edu2_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_edu2_wgt.rds")
saveRDS(srv_inc1_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_inc1_wgt.rds")
saveRDS(srv_inc2_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_inc2_wgt.rds")
saveRDS(srv_sav_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_sav_wgt.rds")
saveRDS(srv_climpes_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_climpes_wgt.rds")
saveRDS(srv_rural_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_inc2_wgt.rds")
saveRDS(srv_age_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_sav_wgt.rds")
saveRDS(srv_emp_wgt, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_climpes_wgt.rds")


#Save Index Data
write.csv(ind_att, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/ind_att.csv", row.names = FALSE)
write.csv(ind_op, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/ind_op.csv", row.names = FALSE)
write.csv(ind_tot, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/ind_tot.csv", row.names = FALSE)
