#+ PS 1 Coding
#+ Edgar Aguilar
#+ January 26, 2025
#+ 
#+ In this file I wil do the coding before bringing it tor Rmd.

# Install libraries
install.packages("tidyverse")
install.packages("haven")

# Load libraries
library(tidyverse)
library(haven)

# Import the Afrobarometer dataset
df <- read_sav('Congo_Round9.sav')

# convert file to csv
write_csv(df, 'output file.csv')
save(df, file = 'newdata.Rdata')

# Import CSV file
df1 <- read_csv('output file.csv')

# Getting to know the data ----
colnames(df1)

# View the first few entries
head(df1$RESPNO)
head(df1$DATEINTR)

# Summarize the column
summary(df1$RESPNO)
summary(df1$DATEINTR)

# Question 1 ----
# write down country: Congo

# To get how many respondants, Count unique values in RESPNO
length(unique(df1$RESPNO))

# When responses were collected
min(df1$DATEINTR)
max(df1$DATEINTR)

# Question 2 ----
## describe respondants: ages, male vs. female (Q100), language, 
# columns: REGION, URBRUR, Q1 (age), Q2 (language)

#Age
# Create a tibble from the summary of Age (Q1)
age_summary <- tibble(
  Statistic = c("Min", "1st Quartile", "Median", "Mean", "3rd Quartile", "Max"),
  Value = as.numeric(summary(df1$Q1))  
)

# Display the tibble
age_summary

# Gender
# Show unique values in the GENDER column -- 1 = Man 2 = Woman
unique(df1$Q100)

# Replace 1 with "Man" and 2 with "Woman", and calculate percentages
gender_tibble <- df1 %>%
  mutate(
    Q100 = case_when(
      Q100 == 1 ~ "Man",
      Q100 == 2 ~ "Woman",
    )
  ) %>%
  count(Q100, name = "Count") %>%  
  mutate(Percentage = round((Count / sum(Count)) * 100, 2), Category = "Gender")

# Display the tibble
gender_tibble

# REGION
unique(df1$REGION)

region_tibble <- df1 %>%
  mutate(
    REGION = case_when(
      REGION == 1780 ~ "Brazzaville",
      REGION == 1781 ~ "Pointe-Noire",
      REGION == 1782 ~ "Kouilou",
      REGION == 1783 ~ "Niari",
      REGION == 1784 ~ "Bouenza",
      REGION == 1785 ~ "Lekoumou",
      REGION == 1786 ~ "Pool",
      REGION == 1787 ~ "Plateaux",
      REGION == 1788 ~ "Cuvette",
      REGION == 1789 ~ "Cuvette Ouest",
      REGION == 1790 ~ "Sangha",
      REGION == 1791 ~ "Likouala ",
    )
  ) %>%
  count(REGION, name = "Count") %>%
  mutate(Percentage = round((Count/sum(Count))*100, 2), Category = "Region")

# Display the tibble
region_tibble

### 1780=Brazzaville, 1781=Pointe-Noire, 1872=Kouilou, 1783=Niari, 1784=Bouenza, 
### 1785=Lekoumou, 1786=Pool, 1787=Plateaux, 1788=Cuvette, 1789=Cuvette Ouest, 
### 1790=Sangha, 1791=Likouala 

# URBRUR-- 1 = Urban, 2 - Rural
unique(df1$URBRUR)

# Replace 1 with "Urban" and 2 with "Rural", and calculate percentages
urbrur_tibble <- df1 %>%
  mutate(
    URBRUR = case_when(
      URBRUR == 1 ~ "Urban",
      URBRUR == 2 ~ "Rural",
    )
  ) %>%
  count(URBRUR, name = "Count") %>%  
  mutate(Percentage = round((Count / sum(Count)) * 100, 2), Category = "Location") 

# Display the tibble
urbrur_tibble

# Language (Q2)
## 2=Français, 1780=Lingala, 1781=Kituba, 1782=Lari, 1783=Teke, 
## 9998=A refusé de répondre, 9999=Ne sait pas, -1=Manquant 

unique(df1$Q2)

# Replace and calculate categories
language_tibble <- df1 %>%
  mutate(
    Q2 = case_when(
      Q2 == 2 ~ "Français",
      Q2 == 1780 ~ "Lingala",
      Q2 == 1781 ~ "Kituba",
      Q2 == 1782 ~ "Lari",
      Q2 == 1783 ~ "Teke",
      Q2 == 1783 ~ "Teke",
      Q2 == 9995 ~ "Other",
      Q2 == 9998 ~ "A refusé de répondre",
      Q2 == 9999 ~ "Ne sait pas",
      Q2 == -1 ~ "Manquant ",
    )
  ) %>%
  count(Q2, name = "Count") %>%  
  mutate(Percentage = round((Count / sum(Count)) * 100, 2), Category = "Language") 

# Display the tibble
language_tibble

#### combine

# ---- Combine All Tibbles ----
summary_table <- bind_rows(
  gender_tibble,
  region_tibble,
  urbrur_tibble,
  language_tibble
)

# ---- Display the Final Table ----
# As plain tibble
summary_table


# Merge Q100, REGION, URBRUR, and Q2 into one column called 'Variable'
updated_summary_table <- summary_table %>%
  mutate(
    Variable = coalesce(as.character(Q100), as.character(REGION), as.character(URBRUR), as.character(Q2))  # Combine columns into 'Variable'
  ) %>%
  select(Category, Variable, Count, Percentage)  # Rearrange columns to the desired order

# Display the updated table
updated_summary_table

#### QUESTION 3
China_attitudes_frequency <- df1 %>%
  count(Q78A, name = "Count") %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 2),
    Response = case_when(
      Q78A == 1 ~ "Very negative",
      Q78A == 2 ~ "Somewhat negative",
      Q78A == 3 ~ "Neither positive nor negative",
      Q78A == 4 ~ "Somewhat positive",
      Q78A == 5 ~ "Very positive",
      Q78A == 8 ~ "Refused",
      Q78A == 9 ~ "Don't know",
      Q78A == -1 ~ "Missing",
    )
  ) %>%
  select(Response, Count, Percentage)

# Display the table
China_attitudes_frequency

#### QUESTION 4
USA_attitudes_frequency <- df1 %>%
  count(Q78B, name = "Count") %>%
  mutate(
    Percentage = round((Count / sum(Count)) * 100, 2),
    Response = case_when(
      Q78B == 1 ~ "Very negative",
      Q78B == 2 ~ "Somewhat negative",
      Q78B == 3 ~ "Neither positive nor negative",
      Q78B == 4 ~ "Somewhat positive",
      Q78B == 5 ~ "Very positive",
      Q78B == 8 ~ "Refused",
      Q78B == 9 ~ "Don't know",
      Q78B == -1 ~ "Missing",
    )
  ) %>%
  select(Response, Count, Percentage)

# Display the table
USA_attitudes_frequency



#### QUESTION 5

# Clean Q78A and Q78B using across
df_clean <- df1 %>%
  mutate(
    across(
      Q78A:Q78B,  
      ~ if_else(.x %in% 1:5, .x, NA)
    )
  )

# Perform the paired t-test
t_test_result <- t.test(df_clean$Q78A, df_clean$Q78B, paired = TRUE)

# Display the t-test result
t_test_result
