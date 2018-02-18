irv_results <- function(all_votes) {
  require(glue)
  require(dplyr)

  removed_movies <- NULL
  results <- NULL

  for(i in 1:10) {
    results <- c(results, glue("Round {i}"))
    round <- all_votes %>%
      filter(!(movie %in% removed_movies)) %>%
      group_by(person) %>%
      mutate(rank = row_number(rank)) %>%
      group_by(movie) %>%
      summarise(first_votes = sum(rank == 1)/n_distinct(person)) %>%
      arrange(desc(first_votes))

    if(nrow(round) == 0) {
      results <- c(results, "No winner found. No result possible.")
      break
    } else if(max(round$first_votes) > .5) {
      results <- c(results, "Winner found!", round$movie[1])
      break
    } else {
      results <- c(results, "Winner not found, removing:")
      new_removed <- with(round, movie[first_votes == min(first_votes)])
      removed_movies <- c(removed_movies, new_removed)
      results <- c(results, paste0(new_removed, collapse = ", "), "")
    }
  }
  results
}
