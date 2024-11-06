# package library
library(tidyverse)
library(palmerpenguins)
library(janitor)
library(here)
library(dplyr)

# loading our function file
source(here("functions", "cleaning.R"))

here::here()

# data was loaded in as part of the library
View(penguins_raw)
head(penguins_raw)
str(penguins_raw)

write.csv(penguins_raw, here("data", "penguins_raw.csv"))

# cleaning of data ----
# use of pipes that prevent overwriting of data sets which run errors when repeatedly run
penguins_clean <- penguins_raw %>% select(-Comments) %>% select(-starts_with("Delta")) %>% clean_names()

#clean names - makes human readable and computer readable


# writing of function to clean data ----
cleaning_penguin_columns <- function(raw_data){
  print("Removed empty columns and rows, cleaned column names, removed comments and delta.") %>% 
  raw_data %>% 
    clean_names() %>%  
    remove_empty(c("rows", "cols")) %>% 
    select(-starts_with("delta")) %>% 
    select(-comments) 
}

# use of the made function to clean code
penguins_clean <- cleaning_penguin_columns(penguins_raw)
colnames(penguins_clean)

# writing the clean data to a csv 
write.csv(penguins_clean, here("data", "penguins_clean.csv"))

# saving a copy of our cleaning code
file.create(here("function"))

# shortening species names 
shorten_species <- function(penguins_data) {
penguins_data %>%
  mutate(species = case_when(
    species == "Adelie Penguin (Pygoscelis adeliae)" ~ "Adelie",
    species == "Chinstrap penguin (Pygoscelis antarctica)" ~ "Chinstrap",
    species == "Gentoo penguin (Pygoscelis papua)" ~ "Gentoo"
  ))
}



# Graphing tutorial ----
penguins_clean <- read_csv(here("data", "penguins_clean.csv"))

# graphing off penguins clean results in errors of data removal

# sorting missing values 
# should only remove the missing values from the column of interest 
# Subset the columns: species and flipper_length_mm
penguins_flippers <- penguins_clean %>% 
  shorten_species() %>% 
  select(species, flipper_length_mm) %>% 
  drop_na()

head(penguins_flippers)

# graph complexities

species_colours <- c("Adelie" = "darkorange", 
                     "Chinstrap" = "purple", 
                     "Gentoo" = "cyan4")

flipper_boxplot <- ggplot(
  data = penguins_flippers, 
  aes(x = species, 
      y = flipper_length_mm)) +
  geom_boxplot(aes(color = species),
               width = 0.3,
        show.legend = FALSE) + 
  geom_jitter(aes(colour = species), 
              alpha = 0.3, 
              show.legend = FALSE,
              position = position_jitter(
                width = 0.2, 
                seed = 0)) + 
  scale_colour_manual(values = species_colours) +
  labs(x = "Penguin species", 
       y = "Flipper length (mm)") + 
  theme_bw()

flipper_boxplot

# jitter gives random x value for the points of each species (within range) to 
# allow for visualisation of the points without extenxive overlap
# However don't particularly want to have the jitter change each time we run the plot
# Use of a random seed to produce same jitter each time

# changing transparency using alpha 

# changing graph code into a function
plot_boxplot <- function(data,
                         x_column, 
                         y_column, 
                         x_label, 
                         y_label,
                         colour_mapping) {
  data <- data %>% 
    drop_na({{ y_column}})
  # now make the plot
  flipper_boxplot <- ggplot(
    data = data, 
    aes(x = {{x_column}}, 
        y = {{y_column}}, 
        color = {{x_column}} )) +
    geom_boxplot(aes(
                 width = 0.3,
                 show.legend = FALSE)) + 
    geom_jitter(aes(size = 1, 
                alpha = 0.3, 
                show.legend = FALSE,
                position = position_jitter(
                  width = 0.2, 
                  seed = 0))) + 
    scale_colour_manual(values = colour_mapping) +
    labs(x = x_label, 
         y = y_label) + 
    theme_bw()
}

species_colours <- c("Adelie" = "darkorange", 
                     "Chinstrap" = "purple", 
                     "Gentoo" = "cyan4")

plot_boxplot(penguins_clean, "species", "flipper_length_mm", "Penguin Species", "Flipper Length (mm)")


