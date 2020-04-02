#Libraries ----
library(tidyverse)
library(readr)
library(here)
library(outliers)
locale(encoding = 'Latin1')
#Load data ----
disasters_data <- read_csv(here("raw_data/disasters_with_errors.csv"))

dim(disasters_data)

names(disasters_data)

#Select and clean data ----
disasters_data_1 <- disasters_data %>%
  select(-id) 

disasters_data_1 <- disasters_data_1 %>%
  mutate(duplicate_data =duplicated(disasters_data_1)) %>%
  select(duplicate_data)

disasters_data <- bind_cols(disasters_data, disasters_data_1)

disasters_data <- disasters_data %>%
  filter(duplicate_data != TRUE) %>%
  select(-duplicate_data)

disasters_data <- disasters_data %>%
  mutate(disaster_type = str_replace(disaster_type, "Droght", "Drought")) %>%
  mutate(disaster_type = str_replace(disaster_type, 
                                     "Miscelaneous accident", "Miscelleanous accident"))

disasters_data <- disasters_data %>%
  mutate(iso = str_replace(iso, "CHINA", "CHN"))


disasters_data <- disasters_data %>%
  mutate(country_name = str_replace(country_name, "R\xe9union", "Réunion")) %>%
  mutate(country_name = str_replace(country_name, "C�te d�Ivoire", "Côte d'Ivoire"))  %>%
  mutate(country_name = str_replace(country_name, "Canary Is", "Canary Islands")) %>%
  mutate(country_name = str_replace(country_name, 
                                    "United Kingdom of Great Britain and Northern Irela", 
                                    "United Kingdom of Great Britain and Northern Ireland"))

#Check outliers----
View(disasters_data %>%
  mutate(outliers = scores(occurrence)) %>%
  filter(c(outliers < -3 | outliers >3)) %>%
  select(year, country_name, disaster_type, occurrence, outliers))

View(disasters_data %>%
  filter(occurrence >= 9) %>%
  mutate(outliers = scores(occurrence)) %>%
  filter(c(outliers < -3 | outliers >3)) %>%
  select(year, country_name, disaster_type, occurrence, outliers))

View(disasters_data %>%
       filter(!is.na(total_deaths)) %>%
       mutate(outliers = scores(total_deaths)) %>%
       filter(c(outliers < -3 | outliers >3)))

View(disasters_data %>%
       filter(total_affected != 0) %>%
       mutate(outliers = scores(total_affected)) %>%
       filter(c(outliers < -3 | outliers >3)))


#Filter and choose the columns----

disasters_data <- disasters_data %>%
  mutate(total_deaths = ifelse(is.na(total_deaths), 0, total_deaths)) %>%
  mutate(affected = ifelse(is.na(affected), 0, affected)) %>%
  mutate(injured = ifelse(is.na(injured), 0, injured)) %>%
  mutate(homeless = ifelse(is.na(homeless), 0, homeless)) %>%
  mutate(total_affected = total_deaths + affected + injured + homeless)

disasters_data <- disasters_data %>%
  select(-c(affected:homeless), -total_damage)

disasters_data <- disasters_data %>%
  mutate_at("occurrence", as.integer) %>%
  mutate_at("total_deaths", as.integer) %>%
  mutate_at("total_affected", as.integer) %>%
  filter(total_deaths >= 0)


#Transfer the data ----
write_csv(disasters_data, here("clean_data/disasters_data_clean.csv"))
