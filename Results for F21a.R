#Visualize Results for F21a(6 Questions) and F21b(8 Questions)


#Load unweighted and weighted survey
survey <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_KARIEM.rds")
survey_wgt <- readRDS("G:/REFIE/Externe/Kariem/OeNB Barometer/Analysis/survey_wgt.rds")


#Function to create likert scale result
calculate_summary <- function(data, column, label) {
  data %>%
    survey_count({{ column }}) %>%
    mutate({{ label }} := round(n / sum(n) * 100)) %>%
    select({{ column }}, {{ label }})
}

# Reorder the data frame based on desired values in the first column
desired_order <- c("Stimme vollkommen zu",
                   "Stimmer eher zu",
                   "Weder noch",
                   "Stimme eher nicht zu",
                   "Stimme ueberhaupt nicht zu",
                   "Keine Angabe",
                   "Trifft nicht zu",
                   "Weiss nicht")


desired_order2 <- c("Weiss nicht",
                    "Trifft nicht zu",
                    "Keine Angabe",
                    "Stimme ueberhaupt nicht zu",
                    "Stimme eher nicht zu",
                    "Weder noch",
                    "Stimmer eher zu",
                    "Stimme vollkommen zu")

#Caculate Answers

#F21a
f21a_1 <- calculate_summary(survey_wgt, bf21a_1, all) %>% 
  rename(answers = bf21a_1) %>% 
  mutate(question = "q1")

f21a_2 <- calculate_summary(survey_wgt, bf21a_2, all)%>% 
  rename(answers = bf21a_2) %>% 
  mutate(question = "q4")

f21a_3 <- calculate_summary(survey_wgt, bf21a_3, all)%>% 
  rename(answers = bf21a_3) %>% 
  mutate(question = "q2")

f21a_4 <- calculate_summary(survey_wgt, bf21a_4, all)%>% 
  rename(answers = bf21a_4) %>% 
  mutate(question = "q6")

f21a_5 <- calculate_summary(survey_wgt, bf21a_5, all)%>% 
  rename(answers = bf21a_5) %>% 
  mutate(question = "q3")

f21a_6 <- calculate_summary(survey_wgt, bf21a_6, all)%>% 
  rename(answers = bf21a_6) %>% 
  mutate(question = "q5")


#List together and reduce to 1 DF
f21a <- rbind(f21a_1, f21a_2, f21a_3, f21a_4, f21a_5,f21a_6)



#F21b
f21b_1 <- calculate_summary(survey_wgt, bf21b_1, all)%>% 
  rename(answers = bf21b_1) %>% 
  mutate(question = "q5")

f21b_2 <- calculate_summary(survey_wgt, bf21b_2, all)%>% 
  rename(answers = bf21b_2) %>% 
  mutate(question = "q4")

f21b_3 <- calculate_summary(survey_wgt, bf21b_3, all)%>% 
  rename(answers = bf21b_3) %>% 
  mutate(question = "q2")

f21b_4 <- calculate_summary(survey_wgt, bf21b_4, all)%>% 
  rename(answers = bf21b_4) %>% 
  mutate(question = "q3")

f21b_5 <- calculate_summary(survey_wgt, bf21b_5, all)%>% 
  rename(answers = bf21b_5) %>% 
  mutate(question = "q6")

f21b_6 <- calculate_summary(survey_wgt, bf21b_6, all) %>% 
  rename(answers = bf21b_6) %>% 
  mutate(question = "q7")

f21b_7 <- calculate_summary(survey_wgt, bf21b_7, all) %>% 
  rename(answers = bf21b_7) %>% 
  mutate(question = "q8")

f21b_8 <- calculate_summary(survey_wgt, bf21b_8, all)%>% 
  rename(answers = bf21b_8) %>% 
  mutate(question = "q1")

f21b <- rbind(f21b_1, f21b_2, f21b_3, f21b_4, f21b_5,f21b_6, f21b_7, f21b_8)







                   # levels = c("Stimme vollkommen zu", "Stimmer eher zu", "Weder noch",
                               #"Stimme eher nicht zu","Stimme ueberhaupt nicht zu", "Trifft nicht zu",
                              # "Weiss nicht", "Keine Angabe")),
# Reorder the questions column based on desired_order
f21a$answers <- factor(f21a$answers, levels = desired_order2)
# Reorder the questions column based on desired_order
f21b$answers <- factor(f21b$answers, levels = desired_order2)


#Likert colors
likert_colors <- c("Keine Angabe" = "lightgrey",
                   "Stimme eher nicht zu" = "pink",
                   "Stimme ueberhaupt nicht zu" = "red",
                   "Stimme vollkommen zu" = "darkgreen",
                   "Stimmer eher zu" = "lightgreen",
                   "Trifft nicht zu" = "grey",
                   "Weder noch" = "beige",
                   "Weiss nicht" = "darkgrey")


#PLOTS
f21a_plot <- ggplot(f21a, aes(fill = answers, y = all, x = question)) +
  geom_bar(position = "fill", stat = "identity", color = "black", size = 0.2) +
  geom_text(aes(label = all, fontface = "bold"), position = position_fill(vjust = 0.5), color = "black", size = 3) +
  coord_flip() +
  scale_fill_manual(values = likert_colors) +
  labs(x = "Subsample", y = "Value", fill = "Answer") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1),
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        legend.position = "top") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = percent(c(0, 0.25, 0.5, 0.75, 1))
  )

f21a_plot



f21b_plot <- ggplot(f21b, aes(fill = answers, y = all, x = question)) +
  geom_bar(position = "fill", stat = "identity", color = "black", size = 0.2) +
  geom_text(aes(label = all, fontface = "bold"), position = position_fill(vjust = 0.5), color = "black", size = 3) +
  coord_flip() +
  scale_fill_manual(values = likert_colors) +
  labs(x = "Subsample", y = "Value", fill = "Answer") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.line = element_line(size = 1),
        legend.key.size = unit(0.5, "cm"),
        axis.text.y = element_text(face = "bold"),
        axis.text.x = element_text(face = "bold"),
        legend.position = "top") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = percent(c(0, 0.25, 0.5, 0.75, 1))
  )

f21b_plot












