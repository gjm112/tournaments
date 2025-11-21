simulate_step_ladder <- function(num_teams, distribution, seeding_structure = NULL,series = 1, theta_hat = NULL) {

  teams <- paste(num_teams:1)

  if (distribution == "Normal") {
    strengths <- sapply(num_teams, function(n) {
      qnorm(1:n/(n+1))
    })
    normal_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      rank_hat = rep(NA, length(teams)),
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams))
    )
    df <- arrange(normal_strengths, true_rank)
  } else if (distribution == "Uniform") {
    strengths <- sapply(num_teams, function(n) {
      qunif(1:n/(n+1), 0, sqrt(12))
    })
    unif_strength <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams)),
      rank_hat = rep(NA, length(teams))
    )
    df <- arrange(unif_strength, true_rank)
  } else if (distribution == "Same"){
    strengths <- sapply(num_teams, function(n) {rep(0,n)})
    normal_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      rank_hat = rep(NA,length(teams)),
      game_wins = rep(0,length(teams)),
      game_losses = rep(0,length(teams))
    )
    df <- arrange(normal_strengths, true_rank)
    } else if (distribution == "Exponential"){
      strengths <- sapply(num_teams, function(n) { qexp(1:n/(n+1)) })
      unif_strength <- data.frame(
        true_rank = as.numeric(teams),
        true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
        game_wins = rep(0,length(teams)),
        game_losses = rep(0,length(teams)),
        rank_hat = rep(NA,length(teams))
      )
      df <- arrange(unif_strength, true_rank)
  } else if (distribution == "Manual") {
    strengths <- theta_hat
    manual_strengths <- data.frame(
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      rank_hat = rep(NA, length(teams)),
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams))
    )
    df <- manual_strengths %>%
      arrange(true_strength) %>%
      mutate(true_rank = as.numeric(teams)) %>%
      arrange(true_strength, true_rank)
  } else {
    stop("Distribution not found: Enter Manual to input your own strengths")
  }

  if(is.null(seeding_structure)){
    seeding_structure <- matrix((num_teams:1), nrow = 1)
  } else{
    seeding_structure <- matrix(seeding_structure, nrow = 1)
  }

  for (i in 1:nrow(seeding_structure)) {
    # Create a new data frame based on the current permutation in 'test'
    permutation <- seeding_structure[i, ]
    permuted_df <- df
    permuted_df$true_rank <- df$true_rank[permutation]  # Update team order
    teams <- permuted_df$true_rank
    permuted_df$true_strength <- df$true_strength[permutation]
    permuted_df$rank_hat <- NA  # Initialize ranks column
    permuted_df$game_wins <- 0  # Initialize wins column
    permuted_df$game_losses <- 0

    strengths <- permuted_df$true_strength
  }
  for (i in 1:num_teams){
    team1 <- teams[i]
    team2 <- teams[i+1]
    #print(teams)
    #print(team1)
    #print(team2)
    #print(num_teams)

    strength1 <- strengths[which(permuted_df$true_rank == team1)]
    strength2 <- strengths[which(permuted_df$true_rank == team2)]

    wins_team1 <- 0
    wins_team2 <- 0
    losses_team1 <- 0
    losses_team2 <- 0
    for (game in 1:series){
      match_winner <- simulate_match(team1, team2, strength1, strength2)
      #cat(sprintf("Matchup:  %s vs %s | Strengths: %.2f vs %.2f | Winner: %s\n", team1, team2, strength1, strength2, match_winner))
      if(!is.na(team1)) {
        if (match_winner == team1) {
          wins_team1 <- wins_team1 + 1
          losses_team2 <- losses_team2 + 1
        } else {
          wins_team2 <- wins_team2 + 1
          losses_team1 <- losses_team1 + 1
        }
      }
      else {wins_team2 <- series / 2 + 1}
        # Determine the overall winner if a majority is reached
      if (wins_team1 > series / 2 || wins_team2 > series / 2) {
        break
      }
    }
    # adds wins and losses of the series to the teams unless they have a bye
    if (!any(is.na(c(team1, team2)))) {
      permuted_df <- permuted_df %>%
        mutate(game_wins = ifelse(true_rank == team1, game_wins + wins_team1, game_wins)) %>%
        mutate(game_wins = ifelse(true_rank == team2, game_wins + wins_team2, game_wins)) %>%
        mutate(game_losses = ifelse(true_rank == team1, game_losses + losses_team1, game_losses)) %>%
        mutate(game_losses = ifelse(true_rank == team2, game_losses + losses_team2, game_losses))
    }

    # Determine the match winner based on series results
    series_winner <- ifelse(wins_team1 > wins_team2, team1, team2)
    #print(wins_team1)
    #print(team1)
    #print(wins_team2)
    #print(team2)
    if (team1 == series_winner){
      teams[i+1] <- team1
      teams[i] <- team2
    } else{
      teams[i] <- team1
      teams[i+1] <- team2
    }
    if (series_winner == team1) {
      permuted_df$rank_hat[permuted_df$true_rank == team2] <- num_teams + 1 - i
      permuted_df$rank_hat[permuted_df$true_rank == team1] <- max(num_teams - i,1)
    } else{
      permuted_df$rank_hat[permuted_df$true_rank == team1] <- num_teams + 1 - i
      permuted_df$rank_hat[permuted_df$true_rank == team2] <- max(num_teams - i,1)
    }


    permuted_df$seed <- rep(paste(seeding_structure[1, ], collapse = ", "), nrow(permuted_df))
    results <- permuted_df
  }
  #final_results <- as.data.frame(do.call(rbind, results))

  final_results <- permuted_df %>%
    mutate(distribution = distribution) %>%
    mutate(true_rank = as.numeric(true_rank)) %>%
    arrange(true_rank)
  return(final_results)
}


simulate_step_ladder(4, "Exponential", series = 3, seeding_structure = c(1,2,3,4))
