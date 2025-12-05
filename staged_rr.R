simulate_staged_rr <-  function(num_teams, distribution, group_length = 4, top_k = 2, stages = 2, ties = TRUE, rounds = 1, true_strength_hat = NULL) {

  if (num_teams <= 3) {
    stop("Number of Teams must be greater than 3.")
  }

  if (num_teams %% group_length != 0) {
    stop("Number of teams must be divisible by group length.")
  }

  teams <- paste(num_teams:1)

  # Create team strengths based on distribution
  if (distribution == "Normal"){
    strengths <- sapply(num_teams, function(n) { qnorm(1:n/(n+1)) })
    team_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = strengths,
      rank_hat = rep(NA, length(teams)),
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams))
    )
    df <- arrange(team_strengths, true_rank)
  }
  else if (distribution == "Same"){
    strengths <- sapply(num_teams, function(n) {rep(0, n)})
    team_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = strengths,
      rank_hat = rep(NA, length(teams)),
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams))
    )
    df <- arrange(team_strengths, true_rank)
  }
  else if (distribution == "Uniform"){
    strengths <- sapply(num_teams, function(n) { qunif(1:n/(n+1), 0, sqrt(12)) })
    team_strengths <- data.frame(
      true_rank = as.numeric(teams),
      true_strength = strengths,
      game_wins = rep(0, length(teams)),
      game_losses = rep(0, length(teams)),
      rank_hat = rep(NA, length(teams))
    )
    df <- arrange(team_strengths, true_rank)
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
    strengths <- true_strength_hat
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

  num_groups <- num_teams / group_length

  # Create balanced groups using snake draft seeding
  # Sort teams by strength (best to worst)
  df <- df %>% arrange(desc(true_strength))

  # Assign groups using snake draft method
  df$groups <- NA
  for (i in 1:nrow(df)) {
    group_round <- ceiling(i / num_groups)
    if (group_round %% 2 == 1) {
      # Forward direction for odd rounds
      group_id <- ((i - 1) %% num_groups) + 1
    } else {
      # Reverse direction for even rounds
      group_id <- num_groups - ((i - 1) %% num_groups)
    }
    df$groups[i] <- group_id
  }

  # Reset game stats for group play
  df$game_wins <- 0
  df$game_losses <- 0
  df$rank_hat <- NA

  all_group_results <- list()

  # Simulate each group
  for (g in 1:num_groups) {
    group_teams <- df[df$groups == g, ]
    n_group_teams <- nrow(group_teams)

    # Play round robin within the group
    for (r in 1:rounds) {
      for (i in 1:(n_group_teams - 1)) {
        for (j in (i + 1):n_group_teams) {
          team1 <- group_teams$true_rank[i]
          team2 <- group_teams$true_rank[j]

          strength1 <- group_teams$true_strength[i]
          strength2 <- group_teams$true_strength[j]

          match_winner <- simulate_match(team1, team2, strength1, strength2)

          # Update wins and losses
          if (match_winner == team1) {
            df$game_wins[df$true_rank == team1] <- df$game_wins[df$true_rank == team1] + 1
            df$game_losses[df$true_rank == team2] <- df$game_losses[df$true_rank == team2] + 1
          } else {
            df$game_wins[df$true_rank == team2] <- df$game_wins[df$true_rank == team2] + 1
            df$game_losses[df$true_rank == team1] <- df$game_losses[df$true_rank == team1] + 1
          }
        }
      }
    }

    # Rank teams within each group
    group_df <- df[df$groups == g, ]

    group_df$group_rank <- rank(-group_df$game_wins, ties.method = "random")


    all_group_results[[g]] <- group_df
  }

  # Combine all group results
  final_df <- do.call(rbind, all_group_results)

  # Calculate overall tournament ranking
  # First by group rank, then by total wins, then by true rank as tiebreaker
  final_df <- final_df %>%
    arrange(group_rank, desc(game_wins), true_rank)


  final_df$rank_hat <- rank(interaction(final_df$group_rank, -final_df$game_wins, final_df$true_rank), ties.method = "random")

  bottom_rank <- num_teams/num_groups + 1

  top_group <- final_df %>%
    mutate(distribution = distribution) %>%
    arrange(true_rank) %>%
    mutate(rank_hat = ifelse(group_rank <= top_k, group_rank, bottom_rank)) %>%
    filter(group_rank <= top_k) %>%
    select(true_rank, true_strength, game_wins, game_losses, groups, group_rank, rank_hat, distribution)

  #print(top_group)

  bottom_group <- final_df %>%
    mutate(distribution = distribution) %>%
    arrange(desc(true_rank)) %>%
    mutate(rank_hat = ifelse(group_rank <= top_k, group_rank, bottom_rank)) %>%
    filter(group_rank > top_k) %>%
    select(true_rank, true_strength, game_wins, game_losses, groups, group_rank, rank_hat, distribution)

  #print(bottom_group)

  # Initialize win/loss counters (indexed by original team position)
  game_wins <- rep(0, num_teams)
  game_losses <- rep(0, num_teams)

  top_teams <- top_group$true_rank
  top_strengths <- top_group$true_strength

  # Build the alternating order
  for (round in 1:rounds) {
    for (i in 1:(length(top_teams) - 1)) {
      for (j in (i + 1):length(top_teams)) {
        team1 <- top_teams[i]
        team2 <- top_teams[j]
        strength1 <- top_strengths[i]
        strength2 <- top_strengths[j]

        # Simulate match between team i and team j using their strengths
        match_winner <- simulate_match(team1, team2, strength1, strength2)

        # Check which team won based on the returned team name
        if (match_winner == team1) {
          game_wins[i] <- game_wins[i] + 1
          game_losses[j] <- game_losses[j] + 1
        } else {
          game_wins[j] <- game_wins[j] + 1
          game_losses[i] <- game_losses[i] + 1
        }
      }
    }
  }

  # Create final dataframe with results
  top_results <- data.frame(
    true_rank = top_teams,
    true_strength = top_strengths,
    game_wins = game_wins,
    game_losses = game_losses,
    distribution = distribution
  ) %>% slice(1:(top_k*num_groups))
  if (ties == TRUE) {
    top_results$rank_hat <- rank(-top_results$game_wins, ties.method = "average")
  } else {
    top_results$rank_hat <- rank(-top_results$game_wins, ties.method = "random")
  }
  #print(top_results)


  bottom_teams <- bottom_group$true_rank
  bottom_strengths <- bottom_group$true_strength

  # Build the alternating order
  for (round in 1:rounds) {
    for (i in 1:(nrow(bottom_group) - 1)) {
      for (j in (i + 1):nrow(bottom_group)) {
        team1 <- bottom_teams[i]
        team2 <- bottom_teams[j]
        strength1 <- bottom_strengths[i]
        strength2 <- bottom_strengths[j]

        # Simulate match between team i and team j using their strengths
        match_winner <- simulate_match(team1, team2, strength1, strength2)

        # Check which team won based on the returned team name
        if (match_winner == team1) {
          game_wins[i] <- game_wins[i] + 1
          game_losses[j] <- game_losses[j] + 1
        } else {
          game_wins[j] <- game_wins[j] + 1
          game_losses[i] <- game_losses[i] + 1
        }
      }
    }
  }

  # Create final dataframe with results
  bottom_results <- data.frame(
    true_rank = bottom_teams,
    true_strength = bottom_strengths,
    game_wins = game_wins,
    game_losses = game_losses,
    distribution = distribution
  ) %>% slice(1:(top_k*num_groups))
  if (ties == TRUE) {
    bottom_results$rank_hat <- rank(-bottom_results$game_wins, ties.method = "average") + bottom_rank - 1
  } else {
    bottom_results$rank_hat <- rank(-bottom_results$game_wins, ties.method = "random") + bottom_rank - 1
  }
  #print(bottom_results)

  combined_df <- rbind(top_results, bottom_results)

  if (ties == F){
    final_df <- combined_df %>%
      mutate(rank_hat = rank(rank_hat, ties.method = "random")) %>%
      arrange(true_rank, rank_hat)
  } else{
    final_df <- combined_df %>%
      mutate(rank_hat = rank(rank_hat, ties.method = "average")) %>%
      arrange(true_rank)
  }


  return(final_df)
}

simulate_staged_rr(16, "Normal", rounds=1)

