library(tidyverse)
library(stringr)

participants <- read_csv("Grades-DATA201-DATA422-25S2-Written Assignment Ranking-4370474.csv") %>%
  janitor::clean_names() %>%
  filter(status == "Submitted for grading -  -") %>%
  mutate(identifier = str_split_i(identifier, " ", 2)) %>%
  select(id_number, identifier)

# Warning - destructively renames files
#file.rename(
  list.files("rankings/", full.names = TRUE),
  paste0(
    "rankings/",
    str_split_i(list.files("rankings/"), "_", 2),
    ".",
    str_split_i(list.files("rankings/"), "\\.", -1)
  )
)

# Check for any students who somehow fucked up the file format
# I'm sad that I had to check this
str_split_i(list.files("rankings/"), "\\.", -1)

file_checker <- function(form) {
  ncol(form) == 2 &
    nrow(form) == 5 &
    n_distinct(form$ranking) == 5
}

# This was super fiddly, check penalty_list.csv for what people fucked up
# It boggles the mind
for (file in list.files("rankings", full.names = TRUE)) {
  tryCatch(
    {file_checker(read_csv(file, col_types = cols()))},
    error = function(cond) {print(file)},
    warning = function(cond) {print(file)}
  )
}

####################### Actually combining data together now

rankings_raw <- tibble(
  allocated = double(),
  ranking = integer(),
  ranker = character()
)

for (file in list.files("rankings", full.names = TRUE)) {
  rankings_raw <- bind_rows(
    rankings_raw,
    read_csv(file) %>%
      mutate(ranker = substr(file, 10, 16))
    )
}

# These people did not rank 1 to 5, includes some typos
people_who_cant_count <- rankings_raw %>%
  summarise(m = mean(ranking), .by = ranker) %>%
  filter(m != 3)

rankings_raw %>%
  filter(!(ranker %in% people_who_cant_count$ranker))