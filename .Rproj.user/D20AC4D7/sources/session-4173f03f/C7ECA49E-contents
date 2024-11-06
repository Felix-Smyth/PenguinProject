library(tidyverse)
library(palmerpenguins)
library(janitor)
library(here)
library(dplyr)

# loading of functions file ----
source(here("functions", "cleaning.R"))

# loading of raw data ----
penguins_raw <- read.csv(here("data", "penguins_raw.csv"))

# using functions from the function script ----
cleaning_penguin_columns <- function(raw_data){
  print("Cleaned anmes, removed comments, removed empty rows and cols, removed delta")
  raw_data %>% 
    clean_names() %>% 
    shorten_species() %>% 
    remove_empty(c("rows", "cols")) %>% 
    select(-comments) %>% 
    select(-starts_with("delta"))
}

# using function ----
colnames(penguins_raw)
penguins_clean <- cleaning_penguin_columns(penguins_raw)
colnames(penguins_clean)


# looking at renv
renv:init()
renv::snapshot()
