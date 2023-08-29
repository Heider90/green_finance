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

# Read and assign the RDS files to their respective data frames
survey <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")
survey_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")
srv_fem_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_fem_wgt.rds")
srv_edu1_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_edu1_wgt.rds")
srv_edu2_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_edu2_wgt.rds")
srv_inc1_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_inc1_wgt.rds")
srv_inc2_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_inc2_wgt.rds")
srv_sav_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/srv_sav_wgt.rds")

###############################################################################################################
###############################################################################################################
#Function to create results that can be compared to IFES Slides
calculate_summary <- function(data, column, label) {
  data %>%
    survey_count({{ column }}) %>%
    mutate({{ label }} := round(n / sum(n) * 100)) %>%
    select({{ column }}, {{ label }})
}

#Der Finanzsektor hat eine besondere Verantwortung für eine klimaneutrale Wirtschaft(bf21a_4)
f21a_4_all <- calculate_summary(survey_wgt, bf21a_4, all)





#Banken/Finanzdienstleister versuchen durch umweltfreundliches/soziales Image nur mehr Gewinn zu machen(bf21a_6)
f21a_6_all <- calculate_summary(survey_wgt, bf21a_6, all)


#Ökologisch nachhaltige Unternehmen sind langfristig ertragreicher(bf21a_2)
f21a_2_all <- calculate_summary(survey_wgt, bf21a_2, all)


#Der Klimawandel ist ein finanzielles Risiko für den Finanzsektor (bf21a_5)
f21a_5_all <- calculate_summary(survey_wgt, bf21a_5, all)
