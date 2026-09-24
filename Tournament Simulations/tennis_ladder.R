simulate_tennis_ladder <- function(num_teams, distribution, seeding_structure = NULL, r = 3, theta_hat = NULL) {

  if (num_teams <= 1) {
    stop("Number of Teams must be greater than 1.")
  }

  if (num_teams %% 2 != 0) {
    stop("Number of teams must be even for tennis ladder.")
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

  # Set up seeding structure
  if(is.null(seeding_structure)){
    # Default: seed by true rank (1 at top, n at bottom)
    seeding_structure <- 1:num_teams
  }

  # Initialize ladder positions based on seeding structure
  # seeding_structure[1] is at ladder position 1, seeding_structure[2] at position 2, etc.
  df$ladder_position <- NA
  for (i in 1:length(seeding_structure)) {
    team_rank <- seeding_structure[i]
    df$ladder_position[df$true_rank == team_rank] <- i
  }

  # Run the specified number of rounds
  for (round in 1:r) {
    #cat("\n=== ROUND", round, "===\n")

    # Sort by current ladder position
    df <- df %>% arrange(ladder_position)

    if (round %% 2 != 0) {
      # Odd rounds: Everyone plays - 1 vs 2, 3 vs 4, etc.
      #cat("Format: Everyone plays (1v2, 3v4, etc.)\n")

      for (i in seq(1, num_teams - 1, by = 2)) {
        team1_idx <- i
        team2_idx <- i + 1

        team1 <- df$true_rank[team1_idx]
        team2 <- df$true_rank[team2_idx]
        strength1 <- df$true_strength[team1_idx]
        strength2 <- df$true_strength[team2_idx]

        pos1 <- df$ladder_position[team1_idx]
        pos2 <- df$ladder_position[team2_idx]

        # Simulate match
        match_winner <- simulate_match(team1, team2, strength1, strength2)

        #cat(sprintf("Pos %d (Team %d) vs Pos %d (Team %d) -> Winner: Team %d\n",
        #            pos1, team1, pos2, team2, match_winner))

        # Update wins and losses
        if (match_winner == team2) {
          df$game_wins[team2_idx] <- df$game_wins[team2_idx] + 1
          df$game_losses[team1_idx] <- df$game_losses[team1_idx] + 1

          # Winner moves up, loser moves down
          new_pos1 <- df$ladder_position[team1_idx]
          new_pos2 <- df$ladder_position[team2_idx]
          df$ladder_position[team1_idx] <- new_pos2
          df$ladder_position[team2_idx] <- new_pos1

          #cat(sprintf("  -> Team %d moves to pos %d, Team %d moves to pos %d\n",
          #            team1, df$ladder_position[team1_idx], team2, df$ladder_position[team2_idx]))
        } else {
          df$game_wins[team1_idx] <- df$game_wins[team1_idx] + 1
          df$game_losses[team2_idx] <- df$game_losses[team2_idx] + 1
          #cat(sprintf("  -> Positions unchanged (higher seed won)\n"))
        }
      }
    } else {
      # Even rounds: Top and bottom get byes - 2 vs 3, 4 vs 5, etc.
      #cat("Format: Top & bottom get byes (2v3, 4v5, etc.)\n")
      #cat(sprintf("Pos 1 (Team %d) gets BYE\n", df$true_rank[1]))
      #cat(sprintf("Pos %d (Team %d) gets BYE\n", num_teams, df$true_rank[num_teams]))

      for (i in seq(2, num_teams - 2, by = 2)) {
        team1_idx <- i
        team2_idx <- i + 1

        team1 <- df$true_rank[team1_idx]
        team2 <- df$true_rank[team2_idx]
        strength1 <- df$true_strength[team1_idx]
        strength2 <- df$true_strength[team2_idx]

        pos1 <- df$ladder_position[team1_idx]
        pos2 <- df$ladder_position[team2_idx]

        # Simulate match
        match_winner <- simulate_match(team1, team2, strength1, strength2)

        #cat(sprintf("Pos %d (Team %d) vs Pos %d (Team %d) -> Winner: Team %d\n",
        #            pos1, team1, pos2, team2, match_winner))

        # Update wins and losses
        if (match_winner == team2) {
          df$game_wins[team2_idx] <- df$game_wins[team2_idx] + 1
          df$game_losses[team1_idx] <- df$game_losses[team1_idx] + 1

          # Winner moves up, loser moves down
          new_pos1 <- df$ladder_position[team1_idx]
          new_pos2 <- df$ladder_position[team2_idx]
          df$ladder_position[team1_idx] <- new_pos2
          df$ladder_position[team2_idx] <- new_pos1

          #cat(sprintf("  -> Team %d moves to pos %d, Team %d moves to pos %d\n",
          #            team1, df$ladder_position[team1_idx], team2, df$ladder_position[team2_idx]))
        } else {
          df$game_wins[team1_idx] <- df$game_wins[team1_idx] + 1
          df$game_losses[team2_idx] <- df$game_losses[team2_idx] + 1
          #cat(sprintf("  -> Positions unchanged (higher seed won)\n"))
        }
      }
    }
  }

  # Final ranking based on ladder position
  df <- df %>% arrange(ladder_position)

  df$rank_hat <- rank(df$ladder_position)

  # Prepare final output
  final_df <- df %>%
    mutate(distribution = distribution) %>%
    select(true_rank, true_strength, game_wins, game_losses, rank_hat, distribution) %>%
    arrange(true_rank)

  return(final_df)
}

# Example usage:
simulate_tennis_ladder(16, "Normal", r = 5)
