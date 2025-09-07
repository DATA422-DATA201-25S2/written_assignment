
library(tidyverse)
library(stringr)

# Needed to link identifier in filename with student id
participants_writing <- read_csv("input/Grades-DATA201-DATA422-25S2-Written Advice on Māori Data Governance and Sovereignty-4325065.csv") %>%
  janitor::clean_names() %>%
  filter(status == "Submitted for grading -  -") %>%
  mutate(identifier = str_split_i(identifier, " ", 2)) %>%
  select(id_number, identifier)

# Warning - destructively renames files
file.rename(
  list.files("input/submissions/", full.names = TRUE),
  paste0(
    "input/submissions/",
    str_split_i(list.files("input/submissions/"), "_", 2),
    ".",
    str_split_i(list.files("input/submissions/"), "\\.", -1)
    )
)

# Create folder for each ID number
sapply(participants$id_number, function(id) {
  dir.create(file.path("allocation", id), showWarnings = FALSE)
})

# Sample five files from the input pool
for (folder in list.files("allocation", full.names = TRUE)) {
  allocation <- sample(list.files("input/submissions/", full.names = TRUE), 5)
  file.copy(allocation, folder)
}

# Put in a csv for each person to give their rankings
for (folder in list.files("allocation", full.names = TRUE)) {
  print(folder)
  allocated <- str_split_i(list.files(folder), "\\.", 1)[!grepl("RANKING_FORM", str_split_i(list.files(folder), "\\.", 1))]
  write_csv(
    tibble(
      allocated = allocated,
      ranking = rep(0, 5)
    ),
    paste0(folder, "/RANKING_FORM.CSV")
  )
}

# Checking there aren't wildly uneven counts
str_split_i(list.files("allocation", recursive = TRUE), "/", 2) %>%
  as_tibble() %>%
  filter(value != "RANKING_FORM.CSV") %>%
  summarise(cnt = n(), .by = value) %>%
  filter(cnt < 5 | cnt > 5)
