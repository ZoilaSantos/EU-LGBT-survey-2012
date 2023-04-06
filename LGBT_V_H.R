LGBT_V_H <- LGBT_Survey_ViolenceAndHarassment

# every country in this dataset
countries <- unique(LGBT_V_H$CountryCode)

# number of questions = 47
length(unique(LGBT_V_H$question_code))

# combined questions with their codes into a table
ques_code_lab <- data.frame(LGBT_V_H$question_code,LGBT_V_H$question_label) %>% 
  arrange(LGBT_V_H$question_code) %>% 
  unique()


#### question fa2_4: Thinking about the MOST SERIOUS physical / sexual attack or threat of violence, what happened to you?
fa2_4_bi <- LGBT_V_H %>% 
  filter(question_code == "fa2_4", subset == "Bisexual men" | subset == "Bisexual women")

# returns the countries which do NOT have data on BOTH bisexual men AND bisexual women
fa2_4_bi_check <- fa2_4_bi %>% 
  select(CountryCode,subset) %>% 
  unique() %>% 
  count(CountryCode) %>% 
  filter(n==1) %>% 
  pull(CountryCode)

# returns table with countries that have data on both bi men and bi women
fa2_4_bi_clean <- fa2_4_bi %>% 
  filter(!CountryCode %in% fa2_4_bi_check,CountryCode != "Average") %>% 
  select(CountryCode,subset,answer,percentage) %>% 
  arrange(CountryCode)

fa2_4_bi_wide <- fa2_4_bi_clean %>% 
  pivot_wider(names_from = answer,values_from = percentage) %>%
  rename("physical_attack" = "Physical attack") %>% 
  rename("sexual_attack" = "Sexual attack") %>% 
  rename("PS_attack" = "Physical and sexual attack") %>% 
  rename("P_threat" = "Threat of physical violence") %>% 
  rename("S_threat" = "Threat of sexual violence") %>% 
  rename("PS_threat" = "Threat of both physical and sexual violence") %>% 
  rename("do_not_know" = "Don`t know") %>% 
  arrange(CountryCode,subset)

t.test(physical_attack ~ subset, data = fa2_4_bi_wide)
t.test(physical_attack ~ subset, data = fa2_4_bi_wide,alternative="greater")

t.test(sexual_attack ~ subset, data = fa2_4_bi_wide)
t.test(sexual_attack ~ subset, data = fa2_4_bi_wide, alternative="less")


#### question fa2_6: MOST SERIOUS physical / sexual attack or threat of violence - Was the perpetrator alone, or was there more than one perpetrator?
fa2_6_bi <- LGBT_V_H %>% 
  filter(question_code == "fa2_6", subset == "Bisexual men" | subset == "Bisexual women")

# returns the countries which do NOT have data on BOTH bisexual men AND bisexual women
fa2_6_bi_check <- fa2_6_bi %>% 
  select(CountryCode,subset) %>% 
  unique() %>% 
  count(CountryCode) %>% 
  filter(n==1) %>% 
  pull(CountryCode)

# returns table with countries that have data on both bi men and bi women
fa2_6_bi_clean <- fa2_6_bi %>% 
  filter(!CountryCode %in% fa2_6_bi_check,CountryCode != "Average") %>% 
  select(CountryCode,subset,answer,percentage) %>% 
  rename("fa2_6_country" = "CountryCode") %>% 
  rename("fa2_6_subset" = "subset") %>% 
  rename("fa2_6_answer" = "answer") %>% 
  rename("fa2_6_percentage" = "percentage") %>% 
  arrange(fa2_6_country,fa2_6_subset)

fa2_6_bi_wide <- fa2_6_bi_clean %>% 
  pivot_wider(names_from = answer,values_from = percentage) %>%
  rename("more_perp" = "More perpetrators") %>%
  rename("alone" = "Alone") %>%
  arrange(CountryCode,subset)

t.test(alone ~ subset, data = fa2_6_bi_wide)
t.test(alone ~ subset, data = fa2_6_bi_wide, alternative="less")
t.test(more_perp ~ subset, data = fa2_6_bi_wide)
t.test(more_perp ~ subset, data = fa2_6_bi_wide, alternative="greater")


#### fa2_8: MOST SERIOUS physical / sexual attack or threat of violence - What was the gender of the perpetrator(s)?
fa2_8_bi <- LGBT_V_H %>% 
  filter(question_code == "fa2_8", subset == "Bisexual men" | subset == "Bisexual women")

# returns the countries which do NOT have data on BOTH bisexual men or only bisexual women
fa2_8_bi_check <- fa2_8_bi %>% 
  select(CountryCode,subset) %>% 
  unique() %>% 
  count(CountryCode) %>% 
  filter(n==1) %>% 
  pull(CountryCode)

fa2_8_bi_clean <- fa2_8_bi %>% 
  filter(!CountryCode %in% fa2_8_bi_check,CountryCode != "Average") %>% 
  select(CountryCode,subset,answer,percentage) %>% 
  arrange(CountryCode)

fa2_8_bi_wide <- fa2_8_bi_clean %>% 
  pivot_wider(names_from = answer,values_from = percentage) %>%
  rename("M_and_F" = "Both male and female") %>%
  rename("do_not_know" = "Don`t know") %>%
  arrange(CountryCode,subset)

t.test(Male ~ subset,data = fa2_8_bi_wide)
t.test(Male ~ subset,data = fa2_8_bi_wide,alternative="greater")
