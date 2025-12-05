library(tidyverse)
simulate_match <- function(team1, team2, strength1, strength2) {
  if (is.na(team1)) {
    return(team2)
  } else if (is.na(team2)) {
    return(team1)
  } else {
    p <- exp(strength1 - strength2) / (1 + exp(strength1 - strength2))
    return(ifelse(runif(1) < p, team1, team2))  # Randomize outcome
  }
}
generate_elimination_tournament_structure <- function(n) {

  is_power_of_two <- function(x) {
    x > 0 && (x & (x - 1)) == 0
  }

  # If n is not a power of 2, round up to the next power of 2 and add placeholders
  if (!is_power_of_two(n)) {
    next_power_of_two <- 2^ceiling(log2(n))
    extra_teams <- next_power_of_two - n
    #message(sprintf("Rounding up to %d teams. Adding %d NA placeholders.", next_power_of_two, extra_teams))
    n <- next_power_of_two
  } else {
    extra_teams <- 0
  }

  rounds <- log(n, base = 2) - 1
  teams <- c(1, 2)  # Initial list of teams

  for (i in 1:rounds) {
    teams <- nextLayer(teams)
  }

  left_half <- teams[1:(length(teams)/2)]
  right_half <- teams[(length(teams)/2+1):length(teams)]
  right_half <- rev(right_half)
  teams <- c(left_half,right_half)

  # Ensure that the lower number team is on the left side of each matchup
  teams <- unlist(lapply(seq(1, length(teams), by = 2), function(i) {
    if (teams[i] > teams[i + 1]) {
      c(teams[i + 1], teams[i])  # Swap if needed
    } else {
      c(teams[i], teams[i + 1])
    }
  })
  )

  while (extra_teams > 0){
    teams[which.max(teams)] <- NA
    extra_teams <- extra_teams - 1
  }

  return(teams)
}

nextLayer <- function(teams) {
  out <- c()  # Initialize an empty vector
  length <- length(teams) * 2 + 1  # Calculate the length for pairing

  # Generate the next layer by pairing teams
  for (i in teams) {
    out <- c(out, i, length - i)  # Push the current team and its pair
  }

  return(out)
}

winners_bracket <- function(num_teams, distribution, seeding_structure = NULL, series = 1, theta_hat = NULL) {

  if (num_teams <= 3) {
    stop("Number of Teams must be greater than 3.")
  }

  teams <- paste(num_teams:1)

  if (log2(num_teams) %% 1 != 0) {
    power_of_2 <- 2^ceiling(log2(num_teams))
    teams <- c(teams, rep(NA, power_of_2 - num_teams))
  }

  if (distribution == "Normal"){
    strengths <- sapply(num_teams, function(n) { qnorm(1:n/(n+1)) })
    normal_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      rank_hat = rep(NA,length(teams)),
      game_wins = rep(0,length(teams)),
      game_losses = rep(0,length(teams))
    )
    df <- arrange(normal_strengths, true_rank)
  }
  else if (distribution == "Same"){
    strengths <- sapply(num_teams, function(n) {rep(0,n)})
    normal_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      rank_hat = rep(NA,length(teams)),
      game_wins = rep(0,length(teams)),
      game_losses = rep(0,length(teams))
    )
    df <- arrange(normal_strengths, true_rank)
  }
  else if (distribution == "Uniform"){
    strengths <- sapply(num_teams, function(n) { qunif(1:n/(n+1) , 0 , sqrt(12)) })
    unif_strength <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      game_wins = rep(0,length(teams)),
      game_losses = rep(0,length(teams)),
      rank_hat = rep(NA,length(teams))
    )
    df <- arrange(unif_strength, true_rank)
  }
  else if (distribution == "Exponential"){
    strengths <- sapply(num_teams, function(n) { qexp(1:n/(n+1)) })
    unif_strength <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      game_wins = rep(0,length(teams)),
      game_losses = rep(0,length(teams)),
      rank_hat = rep(NA,length(teams))
    )
    df <- arrange(unif_strength, true_rank)
  }
  else if (distribution == "Manual") {
    strengths <- theta_hat
    manual_strengths <- data.frame(
      true_strength = c(strengths, rep(NA, length(teams) - num_teams)),
      rank_hat = rep(NA, length(teams)),
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams))
    )
    df <- manual_strengths %>% arrange(true_strength) %>%
      mutate(true_rank = as.numeric(teams)) %>%
      arrange(true_strength, true_rank)
  }
  else {
    stop("Distribution not found: Enter Manual to input your own strengths")
  }

  # Sets seed to that of common best vs worst structure
  if (is.null(seeding_structure)){
    seeding_structure <- matrix(generate_elimination_tournament_structure(num_teams), nrow = 1)
  } else{
    seeding_structure <- matrix(seeding_structure, nrow = 1)
  }
  if(series %% 2 == 0) {
    stop("Series must be an odd number to avoid potential ties.")
  }

  results <- list()  # Initialize the results list

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
    permuted_df$lost_to <- 0
    permuted_df$round_lost <- 1

    strengths <- permuted_df$true_strength
    round_number <- ceiling(log2(length(teams)))

    while (length(teams) > 1) {
      #cat("\n--- New Round ---\n")

      next_round <- c() # Makes sure only winners are included in next round

      for (j in seq(1, length(teams), by = 2)) { # Gets 2 teams for each match
        team1 <- teams[j]
        team2 <- teams[j+1]

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
        #cat(sprintf("Matchup:  %s vs %s | Winner: %s\n:", team1, team2, series_winner))
        next_round <- c(next_round, series_winner)

        if (round_number > 2){
          # Assign ranks based on the current round
          n <- 2^(round_number)
          #print(series_winner)
          if (!is.na(team1) & series_winner == team1) {
            permuted_df$rank_hat[permuted_df$true_rank == team2] <- n
            permuted_df$lost_to[permuted_df$true_rank == team2] <- team1
            permuted_df$round_lost[permuted_df$true_rank == team2] <- round_number
          } else {
            permuted_df$rank_hat[permuted_df$true_rank == team1] <- n
            permuted_df$lost_to[permuted_df$true_rank == team1] <- team2
            permuted_df$round_lost[permuted_df$true_rank == team1] <- round_number
          }
        } else if (round_number == 2) {  # Semifinals
          n <- 2^(round_number)
          if (match_winner == team1) {
            permuted_df$rank_hat[permuted_df$true_rank == team2] <- n
            permuted_df$lost_to[permuted_df$true_rank == team2] <- team1
            permuted_df$round_lost[permuted_df$true_rank == team2] <- round_number
          } else {
            permuted_df$rank_hat[permuted_df$true_rank == team1] <- n
            permuted_df$lost_to[permuted_df$true_rank == team1] <- team2
            permuted_df$round_lost[permuted_df$true_rank == team1] <- round_number
          }
        } else if (round_number == 1) { # Finals
          permuted_df <- permuted_df %>%
            filter(!is.na(true_rank))
          n <- 2^(round_number)
          if (series_winner == team1) {
            permuted_df$rank_hat[permuted_df$true_rank == team2] <- 2
            permuted_df$rank_hat[permuted_df$true_rank == team1] <- 1
          } else {
            permuted_df$rank_hat[permuted_df$true_rank == team1] <- 2
            permuted_df$rank_hat[permuted_df$true_rank == team2] <- 1
          }
        }
      }

      teams <- next_round  # Winners move to the next round
      round_number <- round_number - 1
    }

    permuted_df$seed <- rep(paste(seeding_structure[i, ], collapse = ", "), nrow(permuted_df))
    results[[i]] <- permuted_df # Store the results of this permutation
  }

  final_results <- do.call(rbind, results)

  final_results <- final_results %>%
    mutate(distribution = distribution) %>%
    mutate(true_rank = as.numeric(true_rank)) %>%
    arrange(true_rank)
  return(final_results)
}

losers_bracket <- function(df, num_teams, series = 1, theta_hat = NULL){
  seeding_structure <- matrix((num_teams:1), nrow = 1)
  for (i in 1:nrow(seeding_structure)) {
    # Create a new data frame based on the current permutation in 'test'
    permutation <- seeding_structure[i, ]
    permuted_df <- df
    permuted_df$true_rank <- df$true_rank[permutation]  # Update team order
    teams <- permuted_df$true_rank
    permuted_df$true_strength <- df$true_strength[permutation]
    permuted_df$rank_hat <- 0  # Initialize ranks column
    permuted_df$game_wins <- 0  # Initialize wins column
    permuted_df$game_losses <- 0

    strengths <- permuted_df$true_strength
  }
  for (i in 1:(num_teams-1)){
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
    if (team1 == series_winner){
      teams[i+1] <- team1
      teams[i] <- team2
      if (i == num_teams - 1) {
        # Final match - winner gets 3rd, loser gets 5th
        permuted_df$rank_hat[permuted_df$true_rank == team1] <- 3
        permuted_df$rank_hat[permuted_df$true_rank == team2] <- 5
      } else{
        permuted_df$rank_hat[permuted_df$true_rank == team2] <- df$rank_hat[df$true_rank == team2]
      }
    } else{
      teams[i] <- team1
      teams[i+1] <- team2
      if (i == num_teams - 1) {
        # Final match - winner gets 3rd, loser gets 5th
        permuted_df$rank_hat[permuted_df$true_rank == team2] <- 3
        permuted_df$rank_hat[permuted_df$true_rank == team1] <- 5
      } else{
        permuted_df$rank_hat[permuted_df$true_rank == team1] <- df$rank_hat[df$true_rank == team2]
      }
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

simulate_repechege_tournament <- function(num_teams, distribution, seeding_structure = NULL, series = 1, ties = F, theta_hat = NULL){
  winner_df <- winners_bracket(num_teams, distribution, seeding_structure = NULL, series = 1, theta_hat = NULL)

  finalist1 <- winner_df$true_rank[winner_df$rank_hat == 1]
  finalist2 <- winner_df$true_rank[winner_df$rank_hat == 2]

  losers_1 <- winner_df %>% filter(lost_to == finalist1) %>% arrange(round_lost)
  first_bracket <- losers_bracket(losers_1, nrow(losers_1)) %>%
    mutate(rank_hat)

  losers_2 <- winner_df %>% filter(lost_to == finalist2) %>% arrange(round_lost)
  second_bracket <- losers_bracket(losers_2, nrow(losers_2))

  rest_of_bracket <- winner_df %>% filter(lost_to != finalist1 & lost_to != finalist2)

  final_df <- rbind(first_bracket, second_bracket, rest_of_bracket) %>%
    select(true_rank, true_strength, rank_hat) %>%
    arrange(true_rank)

  if (ties == F){
    final_df <- final_df %>%
      mutate(rank_hat = rank(rank_hat, ties.method = "random")) %>%
      arrange(true_rank, rank_hat)
  } else{
    final_df <- final_df %>%
      mutate(rank_hat = rank(rank_hat, ties.method = "average")) %>%
      arrange(true_rank)
  }

  return(final_df)
}

test <- simulate_repechege_tournament(16, "Normal")
test
