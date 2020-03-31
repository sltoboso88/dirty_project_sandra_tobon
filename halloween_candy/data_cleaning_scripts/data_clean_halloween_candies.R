
# Libraries----
library(tidyverse)
library(janitor)
library(readxl)
library(here)
library(readr)
#Load data ----
halloween_candy_2015 <- read_excel(here("raw_data/boing-boing-candy-2015.xlsx"))

halloween_candy_2016 <- read_excel(here("raw_data/boing-boing-candy-2016.xlsx"))

halloween_candy_2017 <- read_excel(here("raw_data/boing-boing-candy-2017.xlsx"))

dim(halloween_candy_2015)
#5630 124
names(halloween_candy_2015)

dim(halloween_candy_2016)
#1259 123
names(halloween_candy_2016)

dim(halloween_candy_2017)
#2460 120
names(halloween_candy_2017)


#Rename delete and pivot some columns 2015----

halloween_candy_2015 <- halloween_candy_2015 %>%
  select(-Timestamp, 
         -c(`Please leave any remarks or comments regarding your choices.`:
            `Please estimate the degree(s) of separation you have from the following celebrities [Francis Bacon (1561 - 1626)]`),
         -c(`Which day do you prefer, Friday or Sunday?`:
            `Please estimate the degrees of separation you have from the following folks [Beyoncé Knowles]`))

halloween_candy_2015 <- halloween_candy_2015 %>%
  rename(age = `How old are you?`,
         trick_themself = `Are you going actually going trick or treating yourself?`)

halloween_candy_2015 <- halloween_candy_2015 %>%
  mutate_at("age", as.integer) %>%
  filter(!(is.na(age) | age >= 100))

halloween_candy_2015 <- halloween_candy_2015 %>%
  pivot_longer(
    cols = starts_with("["),
    names_to = "candies_food",
    values_to = "preference_candies"
  )

pattern_1 <- "\\["
pattern_2 <- "\\]"

halloween_candy_2015 <- halloween_candy_2015 %>%
  mutate(candies_food = str_replace_all(candies_food, pattern_1, "")) %>%
  mutate(candies_food = str_replace_all(candies_food, pattern_2, ""))

halloween_candy_2015 <- halloween_candy_2015 %>%
  drop_na()


gender = rep("Not ask", times = length(halloween_candy_2015$age))

country = rep("Not ask", times = length(halloween_candy_2015$age))

year = rep("2015", times = length(halloween_candy_2015$age))

halloween_candy_2015 <- 
  add_column(halloween_candy_2015, year, gender, country)

halloween_candy_2015 <- halloween_candy_2015[,c(5, 1, 6, 2, 7, 3, 4)]



#Rename delete and pivot some columns 2016----

halloween_candy_2016 <- halloween_candy_2016 %>%
  select( -`Timestamp`,
          -`Which state, province, county do you live in?`, 
          -c(`Please list any items not included above that give you JOY.`:`[York Peppermint Patties] Ignore`),
          )

halloween_candy_2016 <- halloween_candy_2016 %>%
  rename(country = `Which country do you live in?`,
         age = `How old are you?`,
         trick_themself = `Are you going actually going trick or treating yourself?`,
         gender = `Your gender:`)

halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate_at("age", as.integer)

halloween_candy_2016 <- halloween_candy_2016 %>%
  filter(!(is.na(age) | age >= 100))

halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(country = str_to_title(country))

halloween_candy_2016 <- halloween_candy_2016 %>%
  filter(!is.na(country))

pattern = "[0-9]{2}"
halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(country = str_replace(country, pattern, "USA"))

halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(is_us = str_detect(country, "United K") |
           str_detect(country, "Uk") |
           str_detect(country, "England")) %>%
  mutate(country = if_else(
    is_us == TRUE, "UK", country
  ))



halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(is_us = str_detect(country, "Never") |
           str_detect(country, "The Netherlands")) %>%
  mutate(country = if_else(
    is_us == TRUE, "Netherlands", country
  ))

halloween_candy_2016 <- halloween_candy_2016 %>%
  filter( !(country == "There Isn't One For Old Men" | 
              country == "The Republic Of Cascadia"))

halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(is_us = str_starts(country, "Us") |
           str_starts(country, "U.s") |
           str_detect(country, "Uni") |
           str_detect(country, "Eua") |
           str_detect(country, "America") |
           str_detect(country, "Merica") | 
           str_detect(country, "Murica") |
           str_detect(country, "God") |
           str_detect(country, "South")|
           str_detect(country, "The")) %>%
  mutate(country = if_else(
    is_us == TRUE, "USA", country
  ))

halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(is_us = str_detect(country, "Espa")) %>%
  mutate(country = if_else(
    is_us == TRUE, "Spain", country
  ))

halloween_candy_2016 <- halloween_candy_2016 %>%
  select(-is_us)

halloween_candy_2016 <- halloween_candy_2016 %>%
  pivot_longer(
    cols = starts_with("["),
    names_to = "candies_food",
    values_to = "preference_candies"
  )

halloween_candy_2016 <- halloween_candy_2016 %>%
  mutate(candies_food = str_replace_all(candies_food, pattern_1, "")) %>%
  mutate(candies_food = str_replace_all(candies_food, pattern_2, ""))

halloween_candy_2016 <- halloween_candy_2016 %>%
  drop_na()

year = rep("2016", times = length(halloween_candy_2016$age))

halloween_candy_2016 <- halloween_candy_2016 %>%
  add_column(year)

halloween_candy_2016 <- halloween_candy_2016[,c(7, 1, 2, 3, 4, 5, 6)]



#Rename delete and pivot some columns 2017----

halloween_candy_2017 <- halloween_candy_2017 %>%
  select(-`Internal ID`, 
         -`Q5: STATE, PROVINCE, COUNTY, ETC`,
         -c(`Q7: JOY OTHER`:`Click Coordinates (x, y)`))

halloween_candy_2017 <- halloween_candy_2017 %>%
  rename(trick_themself = `Q1: GOING OUT?`,
         gender = `Q2: GENDER`,
         age = `Q3: AGE`,
         country = `Q4: COUNTRY`)

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate_at("age", as.integer) %>%
  filter(!c(is.na(age) | age >= 100))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(country = str_replace(country, "[0-9]+", "USA")) %>%
  mutate(country = str_to_title(country)) %>%
  filter(!(is.na(country)))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(is_us = str_detect(country, "United K") |
           str_detect(country, "England") |
           str_detect(country, "U.k.") |
           str_detect(country, "Endla") |
           str_detect(country, "Uk")|
           str_detect(country, "Scotland")) %>%
  mutate(country = if_else(
    is_us == TRUE, "UK", country
  ))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(is_us = str_starts(country, "Can")) %>%
  mutate(country = if_else(
    is_us == TRUE, "Canada", country
  ))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(is_us = str_starts(country, "The Net")) %>%
  mutate(country = if_else(
    is_us == TRUE, "Netherlands", country
  ))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(is_us = str_starts(country, "Us") |
           str_starts(country, "Mur") |
           str_detect(country, "Un") |
           str_starts(country, "U.s") |
           str_detect(country, "Amer") |
           str_detect(country, "North") |
           str_detect(country, "U S") |
           str_detect(country, "Pitts") |
           str_detect(country, "Ud") |
           str_detect(country, "New") |
           str_detect(country, "Mer") |
           str_detect(country, "Trum") |
           str_starts(country, "Casc") |
           str_starts(country, "I Don") |
           str_starts(country, "Nar")
           ) %>%
  mutate(country = if_else(
    is_us == TRUE, "USA", country
  ))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(is_us = str_starts(country, "South K")) %>%
  mutate(country = if_else(
    is_us == TRUE, "Korea", country
  ))

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(is_us = str_starts(country, "Uae")) %>%
  mutate(country = if_else(
    is_us == TRUE, "UAE", country
  ))


halloween_candy_2017 <- halloween_candy_2017 %>%
  filter(!(country == "Soviet Canuckistan" | country == "Europe" |
           country == "Earth" | country == "1" ))

halloween_candy_2017 <- halloween_candy_2017 %>%
  pivot_longer(
    cols = starts_with("Q6"),
    names_to = "candies_food",
    values_to = "preference_candies"
  )

halloween_candy_2017 <- halloween_candy_2017 %>%
  select( -is_us)

pattern <- "Q6 \\| " 

halloween_candy_2017 <- halloween_candy_2017 %>%
  mutate(candies_food = str_replace_all(candies_food, pattern, ""))

year = rep("2017", times = length(halloween_candy_2017$age))

halloween_candy_2017 <- halloween_candy_2017 %>%
  add_column(year)

halloween_candy_2017 <- halloween_candy_2017[,c(7, 1, 2, 3, 4, 5, 6)]

halloween_candy_2017 <- halloween_candy_2017 %>%
  drop_na()


#Tibble 2015_2016_2017----

halloween_candy = halloween_candy_2015

halloween_candy <- halloween_candy %>%
  union(halloween_candy_2016)

halloween_candy <- halloween_candy %>%
  union(halloween_candy_2017)

halloween_candy <- halloween_candy %>%
  filter(!c(candies_food == "Cash, or other forms of legal tender" |
            candies_food == "Dental paraphenalia" |
            candies_food == "Creepy Religious comics/Chick Tracts" |
            candies_food == "Hugs (actual physical hugs)" |
            candies_food == "Bonkers (the board game)" |
            candies_food ==
             "Person of Interest Season 3 DVD Box Set (not including Disc 4 with hilarious outtakes"
           ))

write_csv(halloween_candy, here("clean_data/halloween_candy.csv"))


