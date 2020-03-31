#Libraries----
library(tidyverse)
library(readr)
library(here)

#Load data----
dog_owners_survey <- read_csv(here("raw_data/dog_survey.csv"))

dim(dog_owners_survey)
#308 11

names(dog_owners_survey)

#Rename, select columns and  identify repetition----
dog_owners_survey <- dog_owners_survey %>%
  rename(title = Title) %>%
  select(- c(X10, X11))

dog_owners_survey <- dog_owners_survey %>%
  mutate(duplicate_items = duplicated(dog_owners_survey)) %>%
  filter(duplicate_items != TRUE) %>%
  select(-duplicate_items)

dog_owners_survey <- dog_owners_survey %>%
  mutate(dog_size = str_replace(dog_size, "Smallish", "S")) %>%
  mutate(dog_size = str_replace(dog_size, "large", "L")) %>%
  mutate(dog_size = str_replace(dog_size, "Medium sized", "M"))
           
dog_owners_survey <- dog_owners_survey %>%
  mutate(is_di = str_detect(dog_size, "-") |
           str_detect(dog_size, "NO") |
           str_detect(dog_size, "N/A")) %>%
  mutate(dog_size = if_else(
    is_di == TRUE, "No entry", dog_size
  ))
  
dog_owners_survey <- dog_owners_survey %>%
  mutate(dog_size = ifelse(is.na(dog_size), "No entry", dog_size))

dog_owners_survey <- dog_owners_survey %>%
  mutate(is_di = str_starts(dog_gender, "[Ff]e")) %>%
  mutate(dog_gender = if_else(
    is_di == TRUE, "F", dog_gender
  ))
 
dog_owners_survey <- dog_owners_survey %>%
  mutate(is_di = str_starts(dog_gender, "[Mm][Aa]")) %>%
  mutate(dog_gender = if_else(
    is_di == TRUE, "M", dog_gender
  )) 

dog_owners_survey <- dog_owners_survey %>%
  mutate(is_di = str_starts(dog_gender, "Do") |
           str_starts(dog_gender, "Unkown")) %>%
  mutate(dog_gender = if_else(
    is_di == TRUE, "Unknown", dog_gender
  )) 


dog_owners_survey <- dog_owners_survey %>%
  mutate(is_di = str_detect(dog_gender, "-")) %>%
  mutate(dog_gender = if_else(
    is_di == TRUE, "No entry", dog_gender
  )) 

dog_owners_survey <- dog_owners_survey %>%
  mutate(dog_gender= ifelse(is.na(dog_gender), "No entry", dog_gender))

dog_owners_survey <- dog_owners_survey %>%
    mutate(amount_spent_on_dog_food = 
             str_replace(amount_spent_on_dog_food, "about £69.28", "£69.28")) %>%
   mutate(amount_spent_on_dog_food = 
           str_replace(amount_spent_on_dog_food, 
                       "Has increased recently to about £40", "£40")) %>%
  mutate(amount_spent_on_dog_food = 
                  str_replace(amount_spent_on_dog_food, 
                              "Between £10 and £20", "£15")) %>%
  mutate(amount_spent_on_dog_food =
           str_replace(amount_spent_on_dog_food,
                       "£", "")) %>%
  mutate_at("amount_spent_on_dog_food", as.numeric) %>%
  mutate(amount_spent_on_dog_food = abs(amount_spent_on_dog_food))

dog_owners_survey <- dog_owners_survey %>%
  filter(!c(dog_age == "12+" | dog_age == "Less than 20"))


dog_owners_survey <- dog_owners_survey%>%
  select(-is_di)

dog_owners_survey <- dog_owners_survey %>%
  filter( !c(id == 119 | id == 174))

dog_owners_survey_1 <- tibble( id = c(119, 119, 174, 174, 174),
                              title = c("Mrs", "Mrs", "Mrs", "Mrs", "Mrs"),
                              first_name = c("Keith", "Keith", "Levy", "Levy", "Levy"),
                              last_name = c("Dentith", "Dentith", "Howels", "Howels", "Howels"),
                              email = c("kdentith3a@house.gov", 
                                        "kdentith3a@house.gov", 
                                        "lhowels4t@vkontakte.ru",
                                        "lhowels4t@vkontakte.ru",
                                        "lhowels4t@vkontakte.ru"), 
                             amount_spent_on_dog_food = c(2.35, 2.35, 33, 33, 34),
                             dog_size = c("S", "S", "S", "L", "L"),
                             dog_gender = c("M", "F", "M", "M", "F"),
                             dog_age = c("5", "4", "3", "3", "5")
)

dog_owners_survey <- bind_rows(dog_owners_survey, dog_owners_survey_1)

dog_owners_survey <- dog_owners_survey %>%
  mutate_at("dog_age", as.integer)

dog_owners_survey <- dog_owners_survey %>%
  mutate(email = str_replace(email, "-", "No entry")) %>%
  mutate(email = ifelse(is.na(email), "No entry", email)) %>%
  mutate(title = ifelse(is.na(title), "No entry", title))

#Export data----

write_csv(dog_owners_survey, here("clean_data/dog_survey_clean.csv"))
