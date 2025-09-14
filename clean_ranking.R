library(tidyverse)
library(stringr)

participants_ranking <- read_csv("input/Grades-DATA201-DATA422-25S2-Written Assignment Ranking-4370474.csv") %>%
  janitor::clean_names() %>%
  #filter(status == "Submitted for grading -  -") %>%
  mutate(identifier = str_split_i(identifier, " ", 2)) %>%
  select(id_number, identifier)

# I used this to get emails and alert people to resubmit.
problem_children <- read_csv("penalty_list.csv") %>%
  mutate(identifier = as.character(identifier)) %>%
  left_join(participants_ranking, by = "identifier")

# Warning - destructively renames files
#file.rename(
  list.files("input/rankings/", full.names = TRUE),
  paste0(
    "input/rankings/",
    str_split_i(list.files("input/rankings/"), "_", 2),
    ".",
    str_split_i(list.files("input/rankings/"), "\\.", -1)
  )
)

# Check for any students who somehow fucked up the file format
# I'm sad that I had to check this
str_split_i(list.files("input/rankings/"), "\\.", -1)

file_checker <- function(form) {
  ncol(form) == 2 &
    nrow(form) == 5 &
    n_distinct(form$ranking) == 5
}

# This was super fiddly, check penalty_list.csv for what people fucked up
# It boggles the mind
########### It currently deletes anything it can't load for testing purposes.
for (file in list.files("input/rankings", full.names = TRUE)) {
  tryCatch(
    {file_checker(read_csv(file, col_types = cols()))},
    error = function(cond) {print(file)},
    warning = function(cond) {print(file)}
    #error = function(cond) {file.remove(file)},
    #warning = function(cond) {file.remove(file)}
  )
}

####################### Actually combining data together now

rankings_raw <- tibble(
  allocated = double(),
  ranking = integer(),
  ranker = character()
)

for (file in list.files("input/rankings", full.names = TRUE)) {
  rankings_raw <- bind_rows(
    rankings_raw,
    read_csv(file) %>%
      mutate(ranker = substr(file, 16, 23))
    )
}

# These people did not rank 1 to 5, includes some typos
people_who_cant_count <- rankings_raw %>%
  summarise(m = mean(ranking), .by = ranker) %>%
  filter(m != 3)

rankings_clean <- rankings_raw %>%
  # Remove people that didn't correctly rank
  filter(!(ranker %in% people_who_cant_count$ranker)) %>%
  select(allocated, ranking, ranker) %>%
  mutate(allocated = as.character(allocated)) %>%
  # Concording from submission IDs to student IDs
  # Note submission ID is not consistent between submission and ranker
  left_join(participants_writing, by = c("allocated" = "identifier")) %>%
  mutate(allocated = as.character(id_number), .keep = "unused")
  #left_join(participants_ranking, by = c("ranker" = "identifier")) %>%
  #mutate(ranker = id_number, .keep = "unused")
  
