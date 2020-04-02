#Libraries----
library(tidyverse)
library(readr)
library(here)

#Load Data----
cake_ingredients <- read_csv(here("raw_data/cake-ingredients-1961.csv"))

cake_code <- read_csv(here("raw_data/cake_ingredient_code.csv")) 

dim(cake_ingredients)

dim(cake_code)
#Pivot longer, joins and clean names----
cake_ingredients <- cake_ingredients %>%
  pivot_longer(
    cols = c(AE:ZH),
    names_to = "code",
    values_to = "ingredients_quantity"
  )

cake_code <- cake_code %>%
  mutate(measure = ifelse(is.na(measure), "cup", measure)) %>%
  mutate(ingredient = str_replace(ingredient, "Sour cream cup", "Sour cream"))

cake_ingredients <- cake_ingredients %>%
  drop_na()

cake_ingredients <- cake_ingredients %>%
  inner_join(cake_code, by = "code")

cake_ingredients <- cake_ingredients[, c(1, 2, 4, 5, 3)]

cake_ingredients <- cake_ingredients %>%
  rename(cake = Cake)

write_csv(cake_ingredients, here("clean_data/cake_ingredients_clean.csv"))

