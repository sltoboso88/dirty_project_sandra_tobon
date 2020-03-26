#library----
library(readxl)
library(janitor)
library(tidyverse)
library(openxlsx)
library(here)

# Load Data from css file----
birds_data <- read_excel("raw_data/seabirds.xls", sheet = "Bird data by record ID")

ships_data <- read_excel("raw_data/seabirds.xls", sheet = "Ship data by record ID")


# Checking the data-----
dim(birds_data) 
#49019 26
dim(ships_data) 
#12310 27
names(birds_data)
names(ships_data)
#some names are in capital letter and aren't very informative




# Rename de columns-------------------------------------------------------------------------
birds_data <- clean_names(birds_data)

birds_data <- birds_data %>% rename(
  species_common_name = species_common_name_taxon_age_sex_plumage_phase,
  species_scientific_name = species_scientific_name_taxon_age_sex_plumage_phase,
  accompanying_numbers = nacc,
  accompanying_ocurrence = ocacc
  )

ships_data <- clean_names(ships_data)

ships_data <- ships_data %>% rename(
  ship_act = sact,
  cloud_cover =cld,
  precipitation = prec,
  wind_speed = wspeed, 
  sea_state = sste
)

# Select columns----

birds_data <- birds_data %>%
  select(-c(age:sex), -c(nfeed:ocflyp), -c(nfoll:ocnatfed))

ships_data <- ships_data %>%
  select(-ew, -sdir) %>%
  select(-c(csmeth:longecell), -c(wdir:aprs), -c(stmp:depth))


# Missing values -----

ships_data <- ships_data %>%
  drop_na()

birds_data <- birds_data %>%
  drop_na()

pattern <- "[0-9]{4}-[0-9]{2}-[0-9]{2}"
ships_data <- ships_data %>%
  mutate(time = str_replace_all(time, pattern, ""))
  


#Join the 2 tables and safe the data clean----

ship_birds_data <- birds_data %>% 
  inner_join(ships_data, by = "record_id")

ship_birds_data <- ship_birds_data %>% 
  rename( record_birds = record.x,
          record_ship = record.y)

write_csv(ship_birds_data, here("clean_data/ship_birds.csv"))
