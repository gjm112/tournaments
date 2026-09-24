simulate_staged_rr <- function(num_teams, distribution, group_length = 4, top_k = 2,
                               stages = 2, ties = TRUE, rounds = 1, theta_hat = NULL,
                               grouping_mode = "by_rank") {
  # grouping_mode: "by_rank" for 4x4 (rank 1s together, rank 2s together, etc.)
  #                "by_pairs" for 2x8 (1st&2nd together, 3rd&4th together, etc.)

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

  num_groups <- num_teams / group_length

  # Create balanced groups using snake draft seeding
  df <- df %>% arrange(desc(true_strength))

  df$groups <- NA
  for (i in 1:nrow(df)) {
    group_round <- ceiling(i / num_groups)
    if (group_round %% 2 == 1) {
      group_id <- ((i - 1) %% num_groups) + 1
    } else {
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

    for (r in 1:rounds) {
      for (i in 1:(n_group_teams - 1)) {
        for (j in (i + 1):n_group_teams) {
          team1 <- group_teams$true_rank[i]
          team2 <- group_teams$true_rank[j]
          strength1 <- group_teams$true_strength[i]
          strength2 <- group_teams$true_strength[j]

          match_winner <- simulate_match(team1, team2, strength1, strength2)

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

    group_df <- df[df$groups == g, ]
    group_df$group_rank <- rank(-group_df$game_wins, ties.method = "random")
    all_group_results[[g]] <- group_df
  }

  final_df <- do.call(rbind, all_group_results)

  # Organize teams into second-stage groups based on grouping mode
  if (grouping_mode == "by_rank") {
    # Mode 1: Group by same placement (all 1st place together, all 2nd place together, etc.)
    stage2_groups <- list()
    for (rank_position in 1:group_length) {
      stage2_groups[[rank_position]] <- final_df %>%
        filter(group_rank == rank_position) %>%
        arrange(true_rank)
    }
  } else if (grouping_mode == "by_pairs") {
    # Mode 2: Group by pairs of placements (1st&2nd together, 3rd&4th together, etc.)
    stage2_groups <- list()
    teams_per_stage2_group <- num_groups  # Number of teams in each stage 2 group
    num_stage2_groups <- group_length / teams_per_stage2_group  # Number of stage 2 groups

    for (pair_idx in 1:num_stage2_groups) {
      start_rank <- (pair_idx - 1) * teams_per_stage2_group + 1
      end_rank <- pair_idx * teams_per_stage2_group

      stage2_groups[[pair_idx]] <- final_df %>%
        filter(group_rank >= start_rank & group_rank <= end_rank) %>%
        arrange(true_rank)
    }
  } else {
    stop("Invalid grouping_mode. Use 'by_rank' or 'by_pairs'")
  }

  # Simulate second stage for each group
  all_stage2_results <- list()

  for (g in 1:length(stage2_groups)) {
    stage2_team_data <- stage2_groups[[g]]
    n_stage2_teams <- nrow(stage2_team_data)

    # Start with stage 1 wins/losses
    game_wins <- stage2_team_data$game_wins
    game_losses <- stage2_team_data$game_losses

    # Track stage 2 wins separately
    stage2_wins <- rep(0, n_stage2_teams)
    stage2_losses <- rep(0, n_stage2_teams)

    for (r in 1:rounds) {
      for (i in 1:(n_stage2_teams - 1)) {
        for (j in (i + 1):n_stage2_teams) {
          team1 <- stage2_team_data$true_rank[i]
          team2 <- stage2_team_data$true_rank[j]
          strength1 <- stage2_team_data$true_strength[i]
          strength2 <- stage2_team_data$true_strength[j]

          match_winner <- simulate_match(team1, team2, strength1, strength2)

          if (match_winner == team1) {
            game_wins[i] <- game_wins[i] + 1
            game_losses[j] <- game_losses[j] + 1
            stage2_wins[i] <- stage2_wins[i] + 1
            stage2_losses[j] <- stage2_losses[j] + 1
          } else {
            game_wins[j] <- game_wins[j] + 1
            game_losses[i] <- game_losses[i] + 1
            stage2_wins[j] <- stage2_wins[j] + 1
            stage2_losses[i] <- stage2_losses[i] + 1
          }
        }
      }
    }

    # Create results dataframe for this stage 2 group
    stage2_results <- data.frame(
      true_rank = stage2_team_data$true_rank,
      true_strength = stage2_team_data$true_strength,
      game_wins = game_wins,
      game_losses = game_losses,
      stage2_group = g,
      distribution = distribution
    )

    # Rank within this stage 2 group based on stage 2 performance only
    if (ties == TRUE) {
      stage2_results$group_rank <- rank(-stage2_wins, ties.method = "average")
    } else {
      stage2_results$group_rank <- rank(-stage2_wins, ties.method = "random")
    }

    all_stage2_results[[g]] <- stage2_results
  }

  # Combine all stage 2 results
  combined_results <- do.call(rbind, all_stage2_results)

  # Calculate final rankings
  # Rank offset based on which stage 2 group they were in
  combined_results <- combined_results %>%
    group_by(stage2_group) %>%
    mutate(
      rank_offset = (stage2_group - 1) * n(),
      rank_hat = group_rank + rank_offset
    ) %>%
    ungroup() %>%
    arrange(true_rank)

  final_results <- combined_results %>%
    select(true_rank, true_strength, game_wins, game_losses, rank_hat, distribution)

  # Convert to data.frame (not tibble)
  return(as.data.frame(final_results))
}

# Example usage:
# For 4 groups of 4 (winners play together, 2nd place together, etc.):
simulate_staged_rr(16, "Normal", group_length = 4, rounds = 1, grouping_mode = "by_rank")

# For 2 groups of 8 (1st&2nd together, 3rd&4th together, etc.):
simulate_staged_rr(16, "Normal", group_length = 8, rounds = 1, grouping_mode = "by_pairs")
