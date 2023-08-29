

survey <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")







compute_pcts <- function(data, variable, question) {
  result <- table(data[[variable]]) %>%
    as.data.frame() %>%
    rename(Answers = Var1) %>%
    mutate(Answers = recode(Answers, 
                            "-99" = "Keine Angabe",
                            "-98" = "Trifft nicht zu",
                            "-97" = "Weiss nicht",
                            "1" = "Stimme vollkommen zu",
                            "2" = "Stimmer eher zu",
                            "3" = "Weder noch",
                            "4" = "Stimme eher nicht zu",
                            "5" = "Stimme ueberhaupt nicht zu"),
           pct = Freq / sum(Freq),
           question = question)
  
  return(result)
}





#Plot horizontal stacked bar plot incl percentage
plot1_func <- function(data) {
  
  
  ggplot(data, aes(fill = Answers, y = Freq, x = question)) + 
    geom_bar(position = "fill", stat = "identity") +
    geom_text(aes(label = paste0(round(pct * 100))), position = position_fill(vjust = 0.5),
              color = ifelse(data$Answers == "Keine Angabe", "white", "black"),
              fontface = "bold") + 
    coord_flip() +
    scale_fill_manual(values = c("Keine Angabe" = "black",
                                 "Trifft nicht zu" = "darkgrey",
                                 "Weiss nicht" = "lightgrey",
                                 "Stimme vollkommen zu" = "darkgreen",
                                 "Stimmer eher zu" = "lightgreen",
                                 "Weder noch" = "beige",
                                 "Stimme eher nicht zu" = "pink",
                                 "Stimme ueberhaupt nicht zu" = "red"))
  
}

# Example usage:
#Do it for bf8_4 and create 1 chart
bf8_1 <- compute_pcts(data = survey, variable = "bf8_1", question = "q1")
bf8_2 <- compute_pcts(data = survey, variable = "bf8_2", question = "q2")
bf8_3 <- compute_pcts(data = survey, variable = "bf8_3", question = "q3")
bf8_4 <- compute_pcts(data = survey, variable = "bf8_4", question = "q4")


bf8 <- rbind(bf8_1, bf8_2, bf8_3, bf8_4) 


bf8_plot <- plot1_func(bf8)
bf8_plot



#Redo bf8 now one time with low and one time with highedu 
survey_he <- survey %>% 
  filter(edu4 == "high sec edu")



bf8_1_he <- compute_pcts(data = survey_he, variable = "bf8_1", question = "q1")
bf8_2_he <- compute_pcts(data = survey_he, variable = "bf8_2", question = "q2")
bf8_3_he <- compute_pcts(data = survey_he, variable = "bf8_3", question = "q3")
bf8_4_he <- compute_pcts(data = survey_he, variable = "bf8_4", question = "q4")


bf8_he <- rbind(bf8_1_he, bf8_2_he, bf8_3_he, bf8_4_he) 


bf8_he_plot <- plot1_func(bf8_he)
bf8_he_plot



#Redo it with territiary education
survey_te <- survey %>% 
  filter(edu4 == "tertiary education")



bf8_1_te <- compute_pcts(data = survey_te, variable = "bf8_1", question = "q1")
bf8_2_te <- compute_pcts(data = survey_te, variable = "bf8_2", question = "q2")
bf8_3_te <- compute_pcts(data = survey_te, variable = "bf8_3", question = "q3")
bf8_4_te <- compute_pcts(data = survey_te, variable = "bf8_4", question = "q4")


bf8_te <- rbind(bf8_1_te, bf8_2_te, bf8_3_te, bf8_4_te) 


bf8_te_plot <- plot1_func(bf8_te)
bf8_te_plot



#Redo it with lower edu
survey_le <- survey %>% 
  filter(edu4 == "Low sec edu" | edu4 == "primary education")


bf8_1_le <- compute_pcts(data = survey_le, variable = "bf8_1", question = "q1")
bf8_2_le <- compute_pcts(data = survey_le, variable = "bf8_2", question = "q2")
bf8_3_le <- compute_pcts(data = survey_le, variable = "bf8_3", question = "q3")
bf8_4_le <- compute_pcts(data = survey_le, variable = "bf8_4", question = "q4")


bf8_le <- rbind(bf8_1_le, bf8_2_le, bf8_3_le, bf8_4_le) 


bf8_le_plot <- plot1_func(bf8_te)
bf8_le_plot



##F21a_1:Geldgeber sollen in Unternehmen investieren, die Gewinne machen, 
#auch wenn sie damit die Umwelt belasten.
#F21a_2: Ökologisch nachhaltige Unternehmen sind langfristig ertragreicher.
#F21a_3: Die Einhaltung von Umweltstandards sind für Unternehmen vor allem Kostenfaktoren,
#die den Ertrag mindern.
#F21a_4: Der Finanzsektor hat eine besondere Verantwortung für eine klimaneutrale Wirtschaft. 
#F21a_5: Der Klimawandel ist ein finanzielles Risiko für den Finanzsektor.
#F21a_6: Banken und andere Finanzdienstleister versuchen durch umweltfreundliches
#oder soziales Image nur mehr Gewinn zu machen („Greenwashing“).
f21a_1 <- compute_pcts(data = survey, variable = "bf21a_1", question = "f21a_1")
f21a_2 <- compute_pcts(data = survey, variable = "bf21a_2", question = "f21a_2")
f21a_3 <- compute_pcts(data = survey, variable = "bf21a_3", question = "f21a_3")
f21a_4 <- compute_pcts(data = survey, variable = "bf21a_4", question = "f21a_4")
f21a_5 <- compute_pcts(data = survey, variable = "bf21a_5", question = "f21a_5")
f21a_6 <- compute_pcts(data = survey, variable = "bf21a_6", question = "f21a_6")


f21a <- rbind(f21a_1,f21a_2,f21a_3,f21a_4,f21a_5,f21a_6)

#Plot
f21a_plot <- plot1_func(f21a)
f21a_plot




#Save as csv

#F8.1
write.csv(bf8_1, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)

#F8.2
write.csv(bf8_2, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)

#F8.3
write.csv(bf8_3, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)

#F8.4
write.csv(bf8_4, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)


#F8.4 HE
write.csv(bf8_4_he, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)

#F8.4 LE
write.csv(bf8_4_le, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)

#F8.4 TE
write.csv(bf8_4_te, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)

#f21a
write.csv(f21a, file = "G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/Data/output/bf8_1.csv", row.names = FALSE)