library(tidyverse)
library(BradleyTerry2)
library(caret)
library(rstan)
library(tie)

compare_goals <- function(lambda_home,lambda_away) {
  home_goals <- rpois(10000, lambda_home)
  away_goals <- rpois(10000, lambda_away)
  diff_goals <- home_goals - away_goals
  pct_home_win <- sum(diff_goals > 0)/10000
  pct_draw <- sum(diff_goals == 0)/10000
  pct_away_win <- sum(diff_goals < 0)/10000
  outcome_list <- c(pct_home_win, pct_draw, pct_away_win)
  return(outcome_list)
}

csv_list <- c("epl_0910.csv", "epl_1011.csv", "epl_1112.csv", "epl_1213.csv", "epl_1314.csv", "epl_1415.csv", "epl_1516.csv", "epl_1617.csv", "epl_1718.csv", "epl_1819.csv")

avg_draws_predicted_poisson_int_cov <- array(1:10)
avg_home_wins_predicted_poisson_int_cov <- array(1:10)
avg_away_wins_predicted_poisson_int_cov <- array(1:10)

mu_array <- array(1:10)
home_advantage_array <- array(1:10)
int_cov_array <- array(1:10)

poisson_int_cov_log_loss_array <- array(1:10)

season_counter <- 0

for (i in csv_list) {
  season_counter <- season_counter + 1
  current_data <- read.csv(file=i, header=TRUE, sep=",")
  current_data <- current_data %>%
    select(Div:FTR) %>%
    mutate(result = ifelse(FTR == "H", 1, ifelse(FTR == "A", 0, 0.5)),
           home_win = ifelse(FTR == "H", 1, 0),
           away_win = ifelse(FTR == "A", 1, 0),
           draw = ifelse(FTR == "D", 1, 0),
           home_field = 1)
  
  home_wins <- current_data %>%
    filter(FTR == "H") %>%
    group_by(HomeTeam) %>%
    summarize(HomeWins = n()) %>%
    arrange(-HomeWins)
  
  away_wins <- current_data %>%
    filter(FTR == "A") %>%
    group_by(AwayTeam) %>%
    summarize(AwayWins = n()) %>%
    arrange(-AwayWins)
  
  home_draws <- current_data %>%
    filter(FTR == "D") %>%
    group_by(HomeTeam) %>%
    summarize(HomeDraws = n()) %>%
    arrange(-HomeDraws)
  
  away_draws <- current_data %>%
    filter(FTR == "D") %>%
    group_by(AwayTeam) %>%
    summarize(AwayDraws = n()) %>%
    arrange(-AwayDraws)
  
  home_gd <- current_data %>%
    group_by(HomeTeam) %>%
    summarize(homeGD = sum(FTHG) - sum(FTAG)) %>%
    arrange(-homeGD)
  
  away_gd <- current_data %>%
    group_by(AwayTeam) %>%
    summarize(awayGD = sum(FTAG) - sum(FTHG)) %>%
    arrange(-awayGD)
  
  club_results_home <- merge(x=home_wins,y=home_draws,by="HomeTeam",all.x=TRUE)
  club_results_home <- merge(x=club_results_home,y=home_gd,by="HomeTeam")
  club_results_away <- merge(x=away_wins,y=away_draws,by="AwayTeam",all.y=TRUE)
  club_results_away <- merge(x=club_results_away,y=away_gd,by="AwayTeam")
  
  club_results <- merge(x=club_results_home,y=club_results_away,by.x="HomeTeam",by.y="AwayTeam",all.x=TRUE)
  club_results[is.na(club_results)] <- 0
  club_results <- club_results %>%
    rename(Team = HomeTeam) %>%
    mutate(wins = HomeWins + AwayWins,
           draws = HomeDraws + AwayDraws) %>%
    mutate(GD = homeGD + awayGD, points = wins*3 + draws) %>%
    arrange(-points)
  
  team_codes <- data.frame("team_code" = 1:20, "Team" = club_results$Team)
  club_results <- merge(x=club_results,y=team_codes,by="Team")
  club_results <- club_results %>%
    arrange(-points)
  
  code_data <- club_results %>%
    select(Team, team_code)
  
  current_data <- merge(x=current_data,y=code_data,by.x="HomeTeam",by.y="Team")
  current_data <- merge(x=current_data,y=code_data,by.x="AwayTeam",by.y="Team")
  current_data <- current_data %>%
    rename(home_team_code = team_code.x, away_team_code = team_code.y)
  
  folds <- createFolds(current_data$result, k = 5, list = TRUE, returnTrain = FALSE)
  splits <- lapply(folds, function(ind, dat) dat[ind,], dat = current_data)
  
  num_draws_predicted_poisson_int_cov <- 0
  num_home_wins_predicted_poisson_int_cov <- 0
  num_away_wins_predicted_poisson_int_cov <- 0
  
  poisson_int_cov_log_loss <- 0
  
  for (j in 1:5) {
    train_splits <- splits[-j]
    train_data <- do.call("rbind", train_splits)
    numGames = nrow(train_data)
    numDraws <- sum(train_data$draw)
    proportion_draws <- numDraws/numGames
    
    test_split <- splits[j]
    test_data <- do.call("rbind", test_split)
    length_test_data = nrow(test_data)
    
    stanfit_int_cov <- stan(file = "bivariate_poisson_int_cov.stan", data = stan_data, chains = 3, iter = 7000, warmup = 2000, control = list(adapt_delta = 0.95))
    
    posterior <- extract(stanfit_int_cov)
    mu <- mean(posterior$mu)
    home_advantage <- mean(posterior$home_field)
    int_cov <- mean(posterior$fixed_cov)
    
    alpha_vals <- array(1:20)
    delta_vals <- array(1:20)
    rho_vals <- array(1:20)
    
    for (s in 1:20) {
      alpha_vals[s] <- mean(posterior$alpha[,s])
      delta_vals[s] <- mean(posterior$delta[,s])
      rho_vals[s] <- mean(posterior$rho[,s])
    }
    
    team_effects <- data.frame("team_code" = 1:20, "alpha" = alpha_vals,
                               "delta" = delta_vals, "rho" = rho_vals)
    
    train_data <- merge(x=train_data,y=team_effects,by.x="home_team_code",by.y="team_code")
    train_data <- merge(x=train_data,y=team_effects,by.x="away_team_code",by.y="team_code")
    train_data <- train_data %>%
      rename(home_alpha = alpha.x, home_delta = delta.x, home_rho = rho.x,
             away_alpha = alpha.y, away_delta = delta.y, away_rho = rho.y) %>%
      mutate(home_xg = exp(mu + home_advantage + home_alpha + away_delta) + exp(int_cov + home_rho + away_rho),
             away_xg = exp(mu + away_alpha + home_delta) + exp(int_cov + home_rho + away_rho))
    
    poisson_simulations <- train_data %>%
      rowwise %>%
      bow(tie(pct_home_win, pct_draw, pct_away_win) := compare_goals(home_xg,away_xg)[c(1,2,3)])
    
    train_data <- cbind(train_data, poisson_simulations)
    poisson_quant <- quantile(train_data$pct_draw, c(1-proportion_draws))
    
    test_data <- merge(x=test_data,y=team_effects,by.x="home_team_code",by.y="team_code")
    test_data <- merge(x=test_data,y=team_effects,by.x="away_team_code",by.y="team_code")
    test_data <- test_data %>%
      rename(home_alpha = alpha.x, home_delta = delta.x, home_rho = rho.x,
             away_alpha = alpha.y, away_delta = delta.y, away_rho = rho.y) %>%
      mutate(home_xg = exp(mu + home_advantage + home_alpha + away_delta) + exp(int_cov + home_rho + away_rho),
             away_xg = exp(mu + away_alpha + home_delta) + exp(int_cov + home_rho + away_rho))
    
    poisson_simulations_test <- test_data %>%
      rowwise %>%
      bow(tie(pct_home_win, pct_draw, pct_away_win) := compare_goals(home_xg,away_xg)[c(1,2,3)])
    
    test_data <- cbind(test_data, poisson_simulations_test)
    
    test_data <- test_data %>%
      mutate(draw_predicted_poisson_int_cov = ifelse(pct_draw > poisson_quant, 1, 0),
             poisson_int_cov_log_loss = log(pct_draw*draw + pct_home_win*home_win + pct_away_win*away_win)) %>%
      mutate(home_win_predicted_poisson_int_cov = ifelse(pct_home_win > pct_away_win & draw_predicted_poisson_int_cov == 0, 1, 0),
             away_win_predicted_poisson_int_cov = ifelse(pct_home_win < pct_away_win & draw_predicted_poisson_int_cov == 0, 1, 0)) %>%
      mutate(correct_draw_prediction_poisson_int_cov = ifelse(draw == draw_predicted_poisson_int_cov & draw == 1, 1, 0),
             correct_home_win_prediction_poisson_int_cov = ifelse(home_win == home_win_predicted_poisson_int_cov & home_win == 1, 1, 0),
             correct_away_win_prediction_poisson_int_cov = ifelse(away_win == away_win_predicted_poisson_int_cov & away_win == 1, 1, 0))
    
    num_draws_predicted_poisson_int_cov <- num_draws_predicted_poisson_int_cov + sum(test_data$correct_draw_prediction_poisson_int_cov)
    num_home_wins_predicted_poisson_int_cov <- num_home_wins_predicted_poisson_int_cov + sum(test_data$correct_home_win_prediction_poisson_int_cov)
    num_away_wins_predicted_poisson_int_cov <- num_away_wins_predicted_poisson_int_cov + sum(test_data$correct_away_win_prediction_poisson_int_cov)
    
    poisson_int_cov_log_loss <- poisson_int_cov_log_loss + sum(test_data$poisson_int_cov_log_loss)/length_test_data
    
    Sys.sleep(20)
  }
  
  num_games_in_season = nrow(current_data)
  
  stan_data <- list(num_clubs = 20,
                    num_games = num_games_in_season,
                    home_team_code = current_data$home_team_code,
                    away_team_code = current_data$away_team_code,
                    h_goals = current_data$FTHG,
                    a_goals = current_data$FTAG)
  
  stanfit_int_cov <- stan(file = "bivariate_poisson_int_cov.stan", data = stan_data, chains = 3, iter = 7000, warmup = 2000, control = list(adapt_delta = 0.95))
  
  posterior <- extract(stanfit_int_cov)
  mu <- mean(posterior$mu)
  home_advantage <- mean(posterior$home_field)
  int_cov <- mean(posterior$fixed_cov)
  
  mu_array[season_counter] <- mu
  home_advantage_array[season_counter] <- home_advantage
  int_cov_array[season_counter] <- int_cov
  
  alpha_vals <- array(1:20)
  delta_vals <- array(1:20)
  rho_vals <- array(1:20)
  
  for (s in 1:20) {
    alpha_vals[s] <- mean(posterior$alpha[,s])
    delta_vals[s] <- mean(posterior$delta[,s])
    rho_vals[s] <- mean(posterior$rho[,s])
  }
  
  team_effects <- data.frame("team_code" = 1:20, "alpha" = alpha_vals,
                             "delta" = delta_vals, "rho" = rho_vals)
  
  club_results <- merge(x=club_results,y=team_effects,by="team_code")
  season_results_name <- paste(substr(i,1,8),"club_results_int_cov.csv", sep="_")
  
  write.csv(club_results, file = season_results_name)
  
  avg_draws_predicted_poisson_int_cov[season_counter] <- num_draws_predicted_poisson_int_cov/5
  avg_home_wins_predicted_poisson_int_cov[season_counter] <- num_home_wins_predicted_poisson_int_cov/5
  avg_away_wins_predicted_poisson_int_cov[season_counter] <- num_away_wins_predicted_poisson_int_cov/5
  
  poisson_int_cov_log_loss_array[season_counter] <- -1*poisson_int_cov_log_loss/5
}

backtest_results <- data.frame("Season" = 1:10,
                               "avg_draws_predicted_poisson_int_cov" = avg_draws_predicted_poisson_int_cov,
                               "avg_home_wins_predicted_poisson_int_cov" = avg_home_wins_predicted_poisson_int_cov,
                               "avg_away_wins_predicted_poisson_int_cov" = avg_away_wins_predicted_poisson_int_cov,
                               "poisson_int_cov_log_loss" = poisson_int_cov_log_loss_array,
                               "poisson_int_cov_expected_goal_parameter" = mu_array,
                               "poisson_int_cov_home_advantage" = home_advantage_array,
                               "int_cov_parameter" = int_cov_array)

write.csv(backtest_results, file = "epl_backtest_results_int_cov.csv")



