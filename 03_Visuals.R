


ggplot(data = survey, aes(x = bf8_1, fill = bf8_1)) +
  geom_bar() +
  labs(x = "Answer", y = "Count", title = "Responses to Question bf8_1")



ggplot(data = survey, aes(x = bf8_1, fill = bf8_1_category)) +
  geom_bar(position = "stack") +
  labs(x = "Answer", y = "Count", title = "Responses to Question bf8_1")

survey_birgit <- survey_birgit %>%
  mutate(bf8_1_category = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"
  ))




test <- survey_birgit %>%
  count(bf8_1) %>%
  mutate(bf8_1_pct = n / sum(n), 1) %>% 
  recode(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))


klima_1 <- survey_birgit %>%
  count(bf8_1) %>%
  mutate(bf8_1_pct = round(n / sum(n), 2)) %>% 
  mutate(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))



klima_1_jung <- survey_birgit %>%
  filter(agecat == "15-29 years") %>% 
  count(bf8_1) %>%
  mutate(bf8_1_pct = round(n / sum(n), 2)) %>% 
  mutate(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))


klima_2_jung <- survey_birgit %>%
  filter(agecat == "30-44 years") %>% 
  count(bf8_1) %>%
  mutate(bf8_1_pct = round(n / sum(n), 2)) %>% 
  mutate(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))


klima_2_mittel <- survey_birgit %>%
  filter(agecat == "45-59 years") %>% 
  count(bf8_1) %>%
  mutate(bf8_1_pct = round(n / sum(n), 2)) %>% 
  mutate(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))


klima_2_alt <- survey_birgit %>%
  filter(agecat == "60-79 years") %>% 
  count(bf8_1) %>%
  mutate(bf8_1_pct = round(n / sum(n), 2)) %>% 
  mutate(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))

klima_2_alt2 <- survey_birgit %>%
  filter(agecat == "80+ years") %>% 
  count(bf8_1) %>%
  mutate(bf8_1_pct = round(n / sum(n), 2)) %>% 
  mutate(bf8_1 = case_when(
    bf8_1 == -99 ~ "No Answer",
    bf8_1 == -97 ~ "Don't Know",
    bf8_1 == 1 ~ "Worse",
    bf8_1 == 2 ~ "Probably Worse",
    bf8_1 == 3 ~ "Neither",
    bf8_1 == 4 ~ "Probably Better",
    bf8_1 == 5 ~ "Better",
    TRUE ~ "Other"))





klima_jung <- survey_birgit %>% 
  filter(age == "15-29 years")




ggplot(test1) +
  geom_bar(aes(x = factor(1), y = bf8_1_pct, fill = bf8_1_pct), stat = "identity") +
  labs(x = "", y = "Percentage", title = "Stacked Bar Plot") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()



library(ggplot2)

ggplot(test1) +
  geom_bar(aes(x = factor(1), y = bf8_1_pct, fill = bf8_1_pct), stat = "identity") +
  geom_text(aes(x = factor(1), y = bf8_1_pct, label = bf8_1_pct), 
            position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "", y = "Percentage", title = "Stacked Bar Plot") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal()






ggplot(test1) +
  geom_bar(aes(x = factor(1), y = bf8_1_pct, fill = bf8_1_pct), stat = "identity") +
  geom_text(aes(x = factor(1), y = bf8_1_pct, label = bf8_1_pct),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "", y = "Percentage", title = "Stacked Bar Plot") +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      breaks = test1$bf8_1_pct,
                      labels = test1$bf8_1) +
  theme_minimal()




library(ggplot2)

ggplot(test1) +
  geom_bar(aes(x = factor(1), y = bf8_1_pct, fill = bf8_1_pct), stat = "identity") +
  geom_text(aes(x = factor(1), y = bf8_1_pct, label = bf8_1_pct),
            position = position_stack(vjust = 0.5), color = "white") +
  labs(x = "", y = "Percentage", title = "Stacked Bar Plot") +
  scale_fill_gradient(low = "lightblue", high = "darkblue",
                      breaks = test1$bf8_1_pct,
                      labels = test1$n) +
  theme_minimal()


