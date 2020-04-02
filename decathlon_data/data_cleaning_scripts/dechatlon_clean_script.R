#Libraries ----
library(tidyverse)
library(readr)
library(here)
library(janitor)

#Load----
decathlon_data <- read_rds(here("raw_data/decathlon.rds"))

dim(decathlon_data)

names(decathlon_data)

#Clean names and convert row names to columns----
decathlon_data <- clean_names(decathlon_data)

decathlon_data <- decathlon_data %>%
  rename(race_100m = x100m, 
         race_400m = x400m,
         race_1500m = x1500m,
         hurdle_110m = x110m_hurdle)

decathlon_data <- decathlon_data %>%
  rownames_to_column("broker_name")

decathlon_data <- decathlon_data %>%
  mutate(broker_name = str_to_title(broker_name)) 

decathlon_data <- decathlon_data %>%
  mutate_at("competition", as.character)

decathlon_data <- decathlon_data %>%
  select(-high_jump, -c(hurdle_110m:race_1500m))

write_csv(decathlon_data, here("clean_data/decathlon_cleaning.csv"))

