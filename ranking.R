
alpha <- 0.9

# Load and scale scores to 0-1
input_data <- read_csv("synthetic_data.csv") %>%
  select(reviewer, submission, reviewed_rank) %>%
  mutate(review_scaled = reviewed_rank / max(reviewed_rank), .keep = "unused")

# Initialise rankings, e.g., make global list
global_ranks <- input_data %>%
  summarise(
    iteration = 1,
    global_rank = mean(review_scaled),
    .by = submission
  )

# List of agents to iterate over
agents <- unique(input_data$submission)

peer_ranker <- function(input_data, global_ranks, agents) {
  # Adding latest iteration of ranks for trust measures
  iter_data <- input_data %>%
    left_join(
      global_ranks %>%
        filter(iteration == max(iteration)),
      by = c("submission" = "submission")
    ) %>%
    select(-iteration) %>%
    rename(a_bar = global_rank) %>%
    left_join(
      global_ranks %>%
        filter(iteration == max(iteration)),
      by = c("reviewer" = "submission")
    ) %>%
    rename(trust = global_rank)
  
  iter_data <- iter_data %>%
    summarise(
      a_bar = min(a_bar),
      a_bar_weighted = (1-alpha) * a_bar,
      review_weighted = alpha/sum(trust) * sum(review_scaled*trust),
      iteration = min(iteration) + 1,
      .by = c(submission)
    ) %>%
    mutate(peer_score = a_bar_weighted + review_weighted)
  
  iter_data %>%
    select(submission, iteration, global_rank = peer_score)
}

change <- 1

while (change > 0.000000001) {
  global_ranks <- bind_rows(
    global_ranks,
    peer_ranker(input_data, global_ranks, agents)
  )
  
  change <- global_ranks %>%
    filter(iteration >= max(iteration) - 1) %>%
    mutate(change = abs(global_rank - lag(global_rank)),
           .by = submission) %>%
    summarise(change = max(change, na.rm = TRUE)) %>%
    pull()
}

#### Evaluating the algorithm.
global_ranks %>%
  ggplot(aes(x=iteration, y=global_rank, group=submission)) +
  geom_line()

global_ranks %>%
  filter(iteration %in% c(min(iteration), max(iteration))) %>%
  pivot_wider(names_from = iteration, values_from = global_rank)

test_scores <- bind_rows(
  global_ranks %>%
    filter(iteration %in% c(min(iteration), max(iteration))),
  read_csv("synthetic_data.csv") %>%
    mutate(iteration = -99) %>%
    select(submission, iteration, global_rank = submission_rank) %>%
    mutate(global_rank = global_rank / max(global_rank), .keep = "unused") %>%
    distinct()
) 

test_scores %>%
  mutate(iteration = as.factor(iteration)) %>%
  ggplot(aes(x=submission, y=global_rank, group=iteration, color=iteration)) +
  geom_point() +
  coord_flip()

test_scores %>%
  mutate(iteration = as.factor(iteration)) %>%
  ggplot(aes(x=global_rank, group=iteration, color=iteration)) +
  geom_density()

## Compare the rank orders
test_scores %>%
  filter(iteration != 1) %>%
  mutate(
    rank = rank(global_rank, ties.method = "first"),
    .by = iteration,
    .keep = "unused"
    ) %>%
  mutate(
    iteration = ifelse(iteration == -99, "truth", "algo")
  ) %>%
  pivot_wider(
    names_from = iteration, values_from = rank
  ) %>%
  summarise(mean(abs(algo - truth) < 5))
  
