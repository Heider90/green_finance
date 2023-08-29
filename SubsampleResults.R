

survey_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")


calculate_summary <- function(data, column, label) {
  data %>%
    survey_count({{ column }}) %>%
    mutate({{ label }} := round(n / sum(n) * 100)) %>%
    select({{ column }}, {{ label }})
}


#bf8_1: Der Finanzsektor wird meine finanzielle Situation in den n�chsten 2/5/10/15 Jahren verschlechtern
f8_1 <- calculate_summary(survey_wgt, bf8_1, all) %>% 
  rename(answers = bf8_1, 
         q1 = all)
f8_2 <- calculate_summary(survey_wgt, bf8_2, all) %>% 
  rename(answers = bf8_2, 
         q2 = all)
f8_3 <- calculate_summary(survey_wgt, bf8_3, all) %>% 
  rename(answers = bf8_3, 
         q3 = all)
f8_4 <- calculate_summary(survey_wgt, bf8_4, all) %>% 
  rename(answers = bf8_4, 
         q4 = all)

f8_list <- list(f8_1,f8_2, f8_3, f8_4)
f8_all <- reduce(f8_list, full_join, by = "answers")

#bf21a_1: Der Finanzsektor hat eine besondere Verantwortung für eine klimaneutrale Wirtschaft
#All
f21a_all <- calculate_summary(survey_wgt, bf21a_1, all)
#Fem
f21a_fem <- calculate_summary(wgt_srv_fem, bf21a_1, female)
#Edu1
f21a_edu1 <- calculate_summary(wgt_srv_edu1, bf21a_1, edu1)
#Edu2
f21a_edu2 <- calculate_summary(wgt_srv_edu2, bf21a_1, edu2)
#Inc_2k
f21a_inc <- calculate_summary(wgt_srv_inc, bf21a_1, inc_2k)
#inc_3k
f21a_inc2 <- calculate_summary(wgt_srv_inc2, bf21a_1, inc_3k)
#saver
f21a_sav <- calculate_summary(wgt_srv_saver, bf21a_1, saver)
#climate pesimist
f21a_climpes <- calculate_summary(srv_climpes_wgt, bf21a_1, climpes)
#Age
f21a_age <- calculate_summary(srv_age_wgt, bf21a_1, age)
#Rural
f21a_rural <- calculate_summary(srv_rural_wgt, bf21a_1, rural)
#Employed
f21a_emp <- calculate_summary(srv_emp_wgt, bf21a_1, emp)

#List together and reduce to 1 DF
f21a_1_list <- list(f21a_all,f21a_fem, f21a_edu1, f21a_edu2, f21a_inc,f21a_inc2, f21a_sav,f21a_climpes,
                    f21a_age,f21a_rural, f21a_emp)
f21a_1_all <- reduce(f21a_1_list, full_join, by = "bf21a_1")



#bf21a_2: Banken und andere Finanzdienstleister versuchen durch umweltfreundliches oder soziales
#Image nur mehr Gewinn zu machen 
#All
bf21a_2_all <- calculate_summary(survey_wgt, bf21a_2, all)
#Fem
bf21a_2_fem <- calculate_summary(wgt_srv_fem, bf21a_2, female)
#Edu1
bf21a_2_edu1 <- calculate_summary(wgt_srv_edu1, bf21a_2, edu1)
#Edu2
bf21a_2_edu2 <- calculate_summary(wgt_srv_edu2, bf21a_2, edu2)
#Inc_2k
bf21a_2_inc <- calculate_summary(wgt_srv_inc, bf21a_2, inc_2k)
#inc_3k
bf21a_2_inc2 <- calculate_summary(wgt_srv_inc2, bf21a_2, inc_3k)
#saver
bf21a_2_sav <- calculate_summary(wgt_srv_saver, bf21a_2, saver)
#climate pesimist
bf21a_2_climpes <- calculate_summary(srv_climpes_wgt, bf21a_2, climpes)
#Age
bf21a_2_age <- calculate_summary(srv_age_wgt, bf21a_2, age)
#Rural
bf21a_2_rural <- calculate_summary(srv_rural_wgt, bf21a_2, rural)
#Employed
bf21a_2_emp <- calculate_summary(srv_emp_wgt, bf21a_2, emp)

#List together and reduce to 1 DF
bf21a_2_list <- list(bf21a_2_all,bf21a_2_fem,bf21a_2_edu1,bf21a_2_edu2,bf21a_2_inc,
                     bf21a_2_inc2,bf21a_2_sav,bf21a_2_climpes, bf21a_2_age, bf21a_2_rural, bf21a_2_emp)
bf21a_2_all <- reduce(bf21a_2_list, full_join, by = "bf21a_2")



#bf21a_3: Ökologisch nachhaltige Unternehmen sind langfristig ertragreicher
#All
bf21a_3_all <- calculate_summary(survey_wgt, bf21a_3, all)
#Fem
bf21a_3_fem <- calculate_summary(wgt_srv_fem, bf21a_3, female)
#Edu1
bf21a_3_edu1 <- calculate_summary(wgt_srv_edu1, bf21a_3, edu1)
#Edu2
bf21a_3_edu2 <- calculate_summary(wgt_srv_edu2, bf21a_3, edu2)
#Inc_2k
bf21a_3_inc <- calculate_summary(wgt_srv_inc, bf21a_3, inc_2k)
#inc_3k
bf21a_3_inc2 <- calculate_summary(wgt_srv_inc2, bf21a_3, inc_3k)
#saver
bf21a_3_sav <- calculate_summary(wgt_srv_saver, bf21a_3, saver)
#climate pesimist
bf21a_3_climpes <- calculate_summary(srv_climpes_wgt, bf21a_3, climpes)
#Age
bf21a_3_age <- calculate_summary(srv_age_wgt, bf21a_3, age)
#Rural
bf21a_3_rural <- calculate_summary(srv_rural_wgt, bf21a_3, rural)
#Employed
bf21a_3_emp <- calculate_summary(srv_emp_wgt, bf21a_3, emp)

#List together and reduce to 1 DF
bf21a_3_list <- list(bf21a_3_all,bf21a_3_fem,bf21a_3_edu1,bf21a_3_edu2,bf21a_3_inc,
                     bf21a_3_inc2,bf21a_3_sav, bf21a_3_climpes, bf21a_3_age, bf21a_3_rural,
                     bf21a_3_emp)
bf21a_3_all <- reduce(bf21a_3_list, full_join, by = "bf21a_3")



#bf21a_4: Der Klimawandel ist ein finanzielles Risiko für den Finanzsektor 
#All
bf21a_4_all <- calculate_summary(survey_wgt, bf21a_4, all)
#Fem
bf21a_4_fem <- calculate_summary(wgt_srv_fem, bf21a_4, female)
#Edu1
bf21a_4_edu1 <- calculate_summary(wgt_srv_edu1, bf21a_4, edu1)
#Edu2
bf21a_4_edu2 <- calculate_summary(wgt_srv_edu2, bf21a_4, edu2)
#Inc_2k
bf21a_4_inc <- calculate_summary(wgt_srv_inc, bf21a_4, inc_2k)
#inc_3k
bf21a_4_inc2 <- calculate_summary(wgt_srv_inc2, bf21a_4, inc_3k)
#saver
bf21a_4_sav <- calculate_summary(wgt_srv_saver, bf21a_4, saver)
#climate pesimist
bf21a_4_climpes <- calculate_summary(srv_climpes_wgt, bf21a_4, climpes)
#Age
bf21a_4_age <- calculate_summary(srv_age_wgt, bf21a_4, age)
#Rural
bf21a_4_rural <- calculate_summary(srv_rural_wgt, bf21a_4, rural)
#Employed
bf21a_4_emp <- calculate_summary(srv_emp_wgt, bf21a_4, emp)

#List together and reduce to 1 DF
bf21a_4_list <- list(bf21a_4_all,bf21a_4_fem,bf21a_4_edu1, bf21a_4_edu2, bf21a_4_inc,
                     bf21a_4_inc2, bf21a_4_sav, bf21a_4_climpes, bf21a_4_age, bf21a_4_rural,
                     bf21a_4_emp)
bf21a_4_all <- reduce(bf21a_4_list, full_join, by = "bf21a_4")


#bf21a_5: Die Einhaltung von Umweltstandards sind für Unternehmen vor allem Kostenfaktoren
#die den Ertrag mindern
#All
bf21a_5_all <- calculate_summary(survey_wgt, bf21a_5, all)
#Fem
bf21a_5_fem <- calculate_summary(wgt_srv_fem, bf21a_5, female)
#Edu1
bf21a_5_edu1 <- calculate_summary(wgt_srv_edu1, bf21a_5, edu1)
#Edu2
bf21a_5_edu2 <- calculate_summary(wgt_srv_edu2, bf21a_5, edu2)
#Inc_2k
bf21a_5_inc <- calculate_summary(wgt_srv_inc, bf21a_5, inc_2k)
#inc_3k
bf21a_5_inc2 <- calculate_summary(wgt_srv_inc2, bf21a_5, inc_3k)
#bf21a_5
bf21a_5_sav <- calculate_summary(wgt_srv_saver, bf21a_5, saver)
#climate pesimist
bf21a_5_sav_climpes <- calculate_summary(srv_climpes_wgt, bf21a_5, climpes)
#Age
bf21a_5_age <- calculate_summary(srv_age_wgt, bf21a_5, age)
#Rural
bf21a_5_rural <- calculate_summary(srv_rural_wgt, bf21a_5, rural)
#Employed
bf21a_5_emp <- calculate_summary(srv_emp_wgt, bf21a_5, emp)

#List together and reduce to 1 DF
bf21a_5_list <- list(bf21a_5_all,bf21a_5_fem,bf21a_5_edu1, bf21a_5_edu2, bf21a_5_inc,
                     bf21a_5_inc2, bf21a_5_sav, bf21a_5_sav_climpes, bf21a_5_age, bf21a_5_rural,
                     bf21a_5_emp)
bf21a_5_all <- reduce(bf21a_5_list, full_join, by = "bf21a_5")


#bf21a_5: Geldgeber sollen in Unternehmen investieren die Gewinne machen, auch wenn sie damit die Umwelt
#belasten
#All
bf21a_6_all <- calculate_summary(survey_wgt, bf21a_6, all)
#Fem
bf21a_6_fem <- calculate_summary(wgt_srv_fem, bf21a_6, female)
#Edu1
bf21a_6_edu1 <- calculate_summary(wgt_srv_edu1, bf21a_6, edu1)
#Edu2
bf21a_6_edu2 <- calculate_summary(wgt_srv_edu2, bf21a_6, edu2)
#Inc_2k
bf21a_6_inc <- calculate_summary(wgt_srv_inc, bf21a_6, inc_2k)
#inc_3k
bf21a_6_inc2 <- calculate_summary(wgt_srv_inc2, bf21a_6, inc_3k)
#bf21a_5
bf21a_6_sav <- calculate_summary(wgt_srv_saver, bf21a_6, saver)
#climate pesimist
bf21a_6_sav_climpes <- calculate_summary(srv_climpes_wgt, bf21a_6, climpes)
#Age
bf21a_6_age <- calculate_summary(srv_age_wgt, bf21a_6, age)
#Rural
bf21a_6_rural <- calculate_summary(srv_rural_wgt, bf21a_6, rural)
#Employed
bf21a_6_emp <- calculate_summary(srv_emp_wgt, bf21a_6, emp)

#List together and reduce to 1 DF
bf21a_6_list <- list(bf21a_6_all,bf21a_6_fem,bf21a_6_edu1, bf21a_6_edu2, bf21a_6_inc,
                     bf21a_6_inc2, bf21a_6_sav, bf21a_6_sav_climpes, bf21a_6_age, bf21a_6_rural, 
                     bf21a_6_emp)
bf21a_6_all <- reduce(bf21a_6_list, full_join, by = "bf21a_6")


#bf21b_1: Mir ist wichtig das meine Versicherung schrittweise aus Investments in Kohleförderung und
#Kohlekraft aussteit
#All
bf21b_1_all <- calculate_summary(survey_wgt, bf21b_1, all)
#Fem
bf21b_1_fem <- calculate_summary(wgt_srv_fem, bf21b_1, female)
#Edu1
bf21b_1_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_1, edu1)
#Edu2
bf21b_1_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_1, edu2)
#Inc_2k
bf21b_1_inc <- calculate_summary(wgt_srv_inc, bf21b_1, inc_2k)
#inc_3k
bf21b_1_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_1, inc_3k)
#bf21a_5
bf21b_1_sav <- calculate_summary(wgt_srv_saver, bf21b_1, saver)
#climate pesimist
bf21b_1_climpes <- calculate_summary(srv_climpes_wgt, bf21b_1, climpes)
#Age
bf21b_1_age <- calculate_summary(srv_age_wgt, bf21b_1, age)
#Rural
bf21b_1_rural <- calculate_summary(srv_rural_wgt, bf21b_1, rural)
#Employed
bf21b_1_emp <- calculate_summary(srv_emp_wgt, bf21b_1, emp)

#List together and reduce to 1 DF
bf21b_1_list <- list(bf21b_1_all,bf21b_1_fem,bf21b_1_edu1, bf21b_1_edu2, bf21b_1_inc,
                     bf21b_1_inc2, bf21b_1_sav, bf21b_1_climpes, bf21b_1_age, bf21b_1_rural, bf21b_1_emp)
bf21b_1_all <- reduce(bf21b_1_list, full_join, by = "bf21b_1")


#bf21b_2: Mir ist es wichtig, dass meine Bank bis Mitte des Jahrhunderts klimaneutral wirtschaftet 
bf21b_2_all <- calculate_summary(survey_wgt, bf21b_2, all)
#Fem
bf21b_2_fem <- calculate_summary(wgt_srv_fem, bf21b_2, female)
#Edu1
bf21b_2_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_2, edu1)
#Edu2
bf21b_2_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_2, edu2)
#Inc_2k
bf21b_2_inc <- calculate_summary(wgt_srv_inc, bf21b_2, inc_2k)
#inc_3k
bf21b_2_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_2, inc_3k)
#bf21a_5
bf21b_2_sav <- calculate_summary(wgt_srv_saver, bf21b_2, saver)
#climate pesimist
bf21b_2_climpes <- calculate_summary(srv_climpes_wgt, bf21b_2, climpes)
#Age
bf21b_2_age <- calculate_summary(srv_age_wgt, bf21b_2, age)
#Rural
bf21b_2_rural <- calculate_summary(srv_rural_wgt, bf21b_2, rural)
#Employed
bf21b_2_emp <- calculate_summary(srv_emp_wgt, bf21b_2, emp)

#List together and reduce to 1 DF
bf21b_2_list <- list(bf21b_2_all,bf21b_2_fem,bf21b_2_edu1, bf21b_2_edu2, bf21b_2_inc,
                     bf21b_2_inc2, bf21b_2_sav, bf21b_2_climpes, bf21b_2_age, bf21b_2_rural, bf21b_2_emp)
bf21b_2_all <- reduce(bf21b_2_list, full_join, by = "bf21b_2")

#bf21b_3: Mir ist es wichtig, dass mein veranlagtes Geld nicht in fossile Energieträger investiert wird
bf21b_3_all <- calculate_summary(survey_wgt, bf21b_3, all)
#Fem
bf21b_3_fem <- calculate_summary(wgt_srv_fem, bf21b_3, female)
#Edu1
bf21b_3_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_3, edu1)
#Edu2
bf21b_3_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_3, edu2)
#Inc_2k
bf21b_3_inc <- calculate_summary(wgt_srv_inc, bf21b_3, inc_2k)
#inc_3k
bf21b_3_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_3, inc_3k)
#bf21a_5
bf21b_3_sav <- calculate_summary(wgt_srv_saver, bf21b_3, saver)
#climate pesimist
bf21b_3_climpes <- calculate_summary(srv_climpes_wgt, bf21b_3, climpes)
#Age
bf21b_3_age <- calculate_summary(srv_age_wgt, bf21b_3, age)
#Rural
bf21b_3_rural <- calculate_summary(srv_rural_wgt, bf21b_3, rural)
#Employed
bf21b_3_emp <- calculate_summary(srv_emp_wgt, bf21b_3, emp)

#List together and reduce to 1 DF
bf21b_3_list <- list(bf21b_3_all,bf21b_3_fem,bf21b_3_edu1, bf21b_3_edu2, bf21b_3_inc,
                     bf21b_3_inc2, bf21b_3_sav, bf21b_3_climpes, bf21b_3_age, bf21b_3_rural, 
                     bf21b_3_emp)
bf21b_3_all <- reduce(bf21b_3_list, full_join, by = "bf21b_3")

#bf21b_4: Ich bevorzuge Finanzunternehmen die einen klaren ethischen umweltfreundlichen Standpunkt
#vertreten
bf21b_4_all <- calculate_summary(survey_wgt, bf21b_4, all)
#Fem
bf21b_4_fem <- calculate_summary(wgt_srv_fem, bf21b_4, female)
#Edu1
bf21b_4_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_4, edu1)
#Edu2
bf21b_4_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_4, edu2)
#Inc_2k
bf21b_4_inc <- calculate_summary(wgt_srv_inc, bf21b_4, inc_2k)
#inc_3k
bf21b_4_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_4, inc_3k)
#bf21a_5
bf21b_4_sav <- calculate_summary(wgt_srv_saver, bf21b_4, saver)
#climate pesimist
bf21b_4_climpes <- calculate_summary(srv_climpes_wgt, bf21b_4, climpes)
#Age
bf21b_4_age <- calculate_summary(srv_age_wgt, bf21b_4, age)
#Rural
bf21b_4_rural <- calculate_summary(srv_rural_wgt, bf21b_4, rural)
#Employed
bf21b_4_emp <- calculate_summary(srv_emp_wgt, bf21b_4, emp)

#List together and reduce to 1 DF
bf21b_4_list <- list(bf21b_4_all,bf21b_4_fem,bf21b_4_edu1, bf21b_4_edu2, bf21b_4_inc,
                     bf21b_4_inc2, bf21b_4_sav, bf21b_4_climpes, bf21b_4_age, bf21b_4_rural, bf21b_4_emp)
bf21b_4_all <- reduce(bf21b_4_list, full_join, by = "bf21b_4")

#bf21b_5: Bevor ich mich für ein Finanzprodukt entscheide, würde ich mehrere Alternativen in Betracht
#ziehen, um sicherzustellen dass ethische,soziale und Umwelt-Kriterien erfüllt sind
bf21b_5_all <- calculate_summary(survey_wgt, bf21b_5, all)
#Fem
bf21b_5_fem <- calculate_summary(wgt_srv_fem, bf21b_5, female)
#Edu1
bf21b_5_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_5, edu1)
#Edu2
bf21b_5_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_5, edu2)
#Inc_2k
bf21b_5_inc <- calculate_summary(wgt_srv_inc, bf21b_5, inc_2k)
#inc_3k
bf21b_5_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_5, inc_3k)
#bf21a_5
bf21b_5_sav <- calculate_summary(wgt_srv_saver, bf21b_5, saver)
#climate pesimist
bf21b_5_climpes <- calculate_summary(srv_climpes_wgt, bf21b_5, climpes)
#Age
bf21b_5_age <- calculate_summary(srv_age_wgt, bf21b_5, age)
#Rural
bf21b_5_rural <- calculate_summary(srv_rural_wgt, bf21b_5, rural)
#Employed
bf21b_5_emp <- calculate_summary(srv_emp_wgt, bf21b_5, emp)

#List together and reduce to 1 DF
bf21b_5_list <- list(bf21b_5_all,bf21b_5_fem,bf21b_5_edu1, bf21b_5_edu2, bf21b_5_inc,
                     bf21b_5_inc2, bf21b_5_sav, bf21b_5_climpes, bf21b_5_age, bf21b_5_rural, bf21b_5_emp)
bf21b_5_all <- reduce(bf21b_5_list, full_join, by = "bf21b_5")

#bf21b_6: Ich möchte wissen, ob mein vernlagtes Geld zum Klima- und Umweltschutz beiträgt 
bf21b_6_all <- calculate_summary(survey_wgt, bf21b_6, all)
#Fem
bf21b_6_fem <- calculate_summary(wgt_srv_fem, bf21b_6, female)
#Edu1
bf21b_6_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_6, edu1)
#Edu2
bf21b_6_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_6, edu2)
#Inc_2k
bf21b_6_inc <- calculate_summary(wgt_srv_inc, bf21b_6, inc_2k)
#inc_3k
bf21b_6_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_6, inc_3k)
#bf21a_5
bf21b_6_sav <- calculate_summary(wgt_srv_saver, bf21b_6, saver)
#climate pesimist
bf21b_6_climpes <- calculate_summary(srv_climpes_wgt, bf21b_6, climpes)
#Age
bf21b_6_age <- calculate_summary(srv_age_wgt, bf21b_6, age)
#Rural
bf21b_6_rural <- calculate_summary(srv_rural_wgt, bf21b_6, rural)
#Employed
bf21b_6_emp <- calculate_summary(srv_emp_wgt, bf21b_6, emp)

#List together and reduce to 1 DF
bf21b_6_list <- list(bf21b_6_all, bf21b_6_fem, bf21b_6_edu1, bf21b_6_edu2, bf21b_6_inc,
                     bf21b_6_inc2, bf21b_6_sav, bf21b_6_climpes, bf21b_6_age, bf21b_6_rural, bf21b_6_emp)
bf21b_6_all <- reduce(bf21b_6_list, full_join, by = "bf21b_6")


#bf21b_7: Ich habe mich bereits für eine/mehrere Finanzprodukte entschieden die aktiv zum
#Klimaschutz beitragen
bf21b_7_all <- calculate_summary(survey_wgt, bf21b_7, all)
#Fem
bf21b_7_fem <- calculate_summary(wgt_srv_fem, bf21b_7, female)
#Edu1
bf21b_7_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_7, edu1)
#Edu2
bf21b_7_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_7, edu2)
#Inc_2k
bf21b_7_inc <- calculate_summary(wgt_srv_inc, bf21b_7, inc_2k)
#inc_3k
bf21b_7_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_7, inc_3k)
#bf21a_5
bf21b_7_sav <- calculate_summary(wgt_srv_saver, bf21b_7, saver)
#climate pesimist
bf21b_7_climpes <- calculate_summary(srv_climpes_wgt, bf21b_7, climpes)
#Age
bf21b_7_age <- calculate_summary(srv_age_wgt, bf21b_7, age)
#Rural
bf21b_7_rural <- calculate_summary(srv_rural_wgt, bf21b_7, rural)
#Employed
bf21b_7_emp <- calculate_summary(srv_emp_wgt, bf21b_7, emp)

#List together and reduce to 1 DF
bf21b_7_list <- list(bf21b_7_all, bf21b_7_fem, bf21b_7_edu1, bf21b_7_edu2, bf21b_7_inc,
                     bf21b_7_inc2, bf21b_7_sav, bf21b_7_climpes, bf21b_7_age, bf21b_7_rural, bf21b_7_emp)
bf21b_7_all <- reduce(bf21b_7_list, full_join, by = "bf21b_7")


#bf21b_8: Ich bin bereit auf Teile meines Zinsertrags zu verzichten wenn mein Geld dafür in nachhaltige/
#umweltfreundliche/humanitäre Projekte investiert wird 
bf21b_8_all <- calculate_summary(survey_wgt, bf21b_8, all)
#Fem
bf21b_8_fem <- calculate_summary(wgt_srv_fem, bf21b_8, female)
#Edu1
bf21b_8_edu1 <- calculate_summary(wgt_srv_edu1, bf21b_8, edu1)
#Edu2
bf21b_8_edu2 <- calculate_summary(wgt_srv_edu2, bf21b_8, edu2)
#Inc_2k
bf21b_8_inc <- calculate_summary(wgt_srv_inc, bf21b_8, inc_2k)
#inc_3k
bf21b_8_inc2 <- calculate_summary(wgt_srv_inc2, bf21b_8, inc_3k)
#bf21a_5
bf21b_8_sav <- calculate_summary(wgt_srv_saver, bf21b_8, saver)
#climate pesimist
bf21b_8_climpes <- calculate_summary(srv_climpes_wgt, bf21b_8, climpes)
#Age
bf21b_8_age <- calculate_summary(srv_age_wgt, bf21b_8, age)
#Rural
bf21b_8_rural <- calculate_summary(srv_rural_wgt, bf21b_8, rural)
#Employed
bf21b_8_emp <- calculate_summary(srv_emp_wgt, bf21b_8, emp)


#List together and reduce to 1 DF
bf21b_8_list <- list(bf21b_8_all, bf21b_8_fem, bf21b_8_edu1, bf21b_8_edu2, bf21b_8_inc,
                     bf21b_8_inc2, bf21b_8_sav, bf21b_8_climpes, bf21b_8_age, bf21b_8_rural, bf21b_8_emp)
bf21b_8_all <- reduce(bf21b_8_list, full_join, by = "bf21b_8")






#SAVE as CSV

#bf21a
write.csv(f21a_1_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f21a_1_all.csv", row.names = FALSE)
write.csv(bf21a_2_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21a_2_all.csv", row.names = FALSE)
write.csv(bf21a_3_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21a_3_all.csv", row.names = FALSE)
write.csv(bf21a_4_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21a_4_all.csv", row.names = FALSE)
write.csv(bf21a_5_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21a_5_all.csv", row.names = FALSE)
write.csv(bf21a_6_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21a_6_all.csv", row.names = FALSE)

#bf21b
write.csv(bf21b_1_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_1_all.csv", row.names = FALSE)
write.csv(bf21b_2_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_2_all.csv", row.names = FALSE)
write.csv(bf21b_3_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_3_all.csv", row.names = FALSE)
write.csv(bf21b_4_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_4_all.csv", row.names = FALSE)
write.csv(bf21b_5_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_5_all.csv", row.names = FALSE)
write.csv(bf21b_6_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_6_all.csv", row.names = FALSE)
write.csv(bf21b_7_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_7_all.csv", row.names = FALSE)
write.csv(bf21b_8_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/bf21b_8_all.csv", row.names = FALSE)


#bf8
write.csv(f8_all, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/f8_all.csv", row.names = FALSE)


