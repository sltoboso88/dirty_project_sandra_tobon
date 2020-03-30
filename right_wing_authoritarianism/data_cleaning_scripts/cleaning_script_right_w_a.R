# Libraries ----
library(tidyverse)
library(readr)
library(janitor)
library(here)
# Load data ----
right_wing_au <- read_csv(here("raw_data/rwa.csv"))

dim(right_wing_au)
#9881 90

names(right_wing_au)

#Rename, delete and proccess some columns----
right_wing_au <- clean_names(right_wing_au)

right_wing_au <- right_wing_au %>%
  select(-c(q1, q2, e1, e2))

right_wing_au <- right_wing_au %>% 
  filter_at(vars(e3:e22), all_vars( . >= 0))

right_wing_au <- right_wing_au %>%
  mutate(time_a_rwas = rowSums(.[21:40])) %>%
  mutate(time_a_rwas = round((time_a_rwas/1000),0))

right_wing_au <- right_wing_au %>%
  mutate(total_time_s = rowSums(.[41:43]))

right_wing_au <- right_wing_au %>%
  select(-c(e3:vcl16),
         -c(religion:married), 
         -c(screenw:screenh), 
         -major)

right_wing_au <- right_wing_au %>%
  filter(age < 100)

right_wing_au <- right_wing_au %>%
  rename("nat_engl_spe" = engnat)

right_wing_au <- right_wing_au %>%
  pivot_longer(
    cols = c(q3:q22),
    names_to = "number_question",
    values_to = "score"
  )

right_wing_au <- right_wing_au %>%
  mutate(score = 
          ifelse(number_question %in% 
                c("q4", "q6", "q8", "q9", "q11", "q13", "q15", "q18", "q20", "q21" ), 
                abs(score - 9), score))

# Export the data clean ----
write_csv(right_wing_au, here("clean_data/right_wing_clean.csv"))
