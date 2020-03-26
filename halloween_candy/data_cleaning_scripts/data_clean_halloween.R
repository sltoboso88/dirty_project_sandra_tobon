
# Libraries ----
library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(readr)

# Load data----
halloween_candy_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx"))

#Exploring the data----
dim(halloween_candy_2015)

#5630 124
names(halloween_candy_2015)
#they are very long and with a lot space

#Rename, delete, and pivot some columns----

halloween_candy_2015 <- halloween_candy_2015 %>%
  pivot_longer(
    cols = starts_with("["),
    names_to = "candies_food",
    values_to = "preference_candies"
  )

#columns without information
halloween_candy_2015 <- halloween_candy_2015 %>%
  select( - `Fill in the blank: "Taylor Swift is a force for ___________"`, 
          -c(`Please estimate the degrees of separation you have from the following folks [Bruce Lee]`:`Please estimate the degrees of separation you have from the following folks [Beyoncé Knowles]`)) 
  
halloween_candy_2015 <- halloween_candy_2015 %>%
  pivot_longer(
    cols = contains("["),
    names_to = "celebrities_name",
    values_to = "degrees_separation"
  )

halloween_candy_2015 <- clean_names(halloween_candy_2015)

#this column doesn't have too many information only 883 values

halloween_candy_2015 <- halloween_candy_2015 %>%
  select(-please_leave_any_remarks_or_comments_regarding_your_choices)


#This column have 5 options for answer it so I will separate it
halloween_candy_2015 <- halloween_candy_2015 %>%
  separate(
    check_all_that_apply_i_cried_tears_of_sadness_at_the_end_of,
    c("cried_at_the_end_1", "cried_at_the_end_2", "cried_at_the_end_3",
      "cried_at_the_end_4", "cried_at_the_end_5"), 
    sep = "\\,"
  )

#now I will join the question that 
halloween_candy_2015 <- halloween_candy_2015 %>%
  pivot_longer(
    cols = c(cried_at_the_end_1, cried_at_the_end_2, cried_at_the_end_3,
             cried_at_the_end_4, cried_at_the_end_5, that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was,
             fill_in_the_blank_imitation_is_a_form_of),
    names_to = "questions_check_answer",
    values_to = "check_answer"
  )

halloween_candy_2015 <- halloween_candy_2015 %>%
  drop_na()

halloween_candy_2015 <- halloween_candy_2015 %>%
  pivot_longer(
    cols = c(guess_the_number_of_mints_in_my_hand,
             betty_or_veronica,
             what_is_your_favourite_font,
             if_you_squint_really_hard_the_words_intelligent_design_would_look_like,
             which_day_do_you_prefer_friday_or_sunday),
    names_to = "general_questions",
    values_to = "answers_general_questions"
  )

halloween_candy_2015 <- halloween_candy_2015 %>%
  rename(JOY = 
            please_list_any_items_not_included_above_that_give_you_joy,
         DESPAIR = 
           please_list_any_items_not_included_above_that_give_you_despair)

halloween_candy_2015 <- halloween_candy_2015 %>%
  pivot_longer(
    cols = c(JOY, DESPAIR),
    names_to = "joy_despair_other_candy",
    values_to =  "other_candies_names"
  )


#Clean some extra data in the rows----

pattern <- "Please estimate the degree\\(s\\) of separation you have from the following celebrities \\["

halloween_candy_2015 <- halloween_candy_2015 %>%
  mutate(celebrities_name = str_replace_all(celebrities_name, pattern, ""))

pattern <- "\\]"

halloween_candy_2015 <- halloween_candy_2015 %>%
  mutate(celebrities_name = str_replace_all(celebrities_name, pattern, ""))

halloween_candy_2015 <- halloween_candy_2015 %>%
  mutate(candies_food = str_replace_all(candies_food, pattern, ""))

pattern <- "\\["

halloween_candy_2015 <- halloween_candy_2015 %>%
  mutate(candies_food = str_replace_all(candies_food, pattern, ""))


write_csv(halloween_candy_2015, here("clean_data/halloween_candy_2015.csv"))






