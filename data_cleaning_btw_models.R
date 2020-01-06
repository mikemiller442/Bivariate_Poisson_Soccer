library(tidyverse)
library(BradleyTerry2)
library(caret)
library(rstan)
library(tie)

# simulates the probabilities of each action (home win, away win, draw)
# based on the marginal poisson distributions for each side
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

# Log-likelihood function for the Davidson model
fun_lik <- function(param, thd, thw, nhw, nhd, wa, da){
  first_term <- thd*log(param[21])
  second_term <- (thw + .5*thd)*log(param[22])
  third_term <- 0
  for (i in 1:20) {
    third_term <- third_term + (nhw[i] + .5*nhd[i])*log(param[i])
  }
  fourth_term <- 0
  for (i in 1:20) {
    for (j in 1:20) {
      if (i != j) {
        fourth_term <- fourth_term + (wa[j,i] + .5*da[i,j])*log(param[j])
      }
    }
  }
  fifth_term <- 0
  for (i in 1:20) {
    for (j in 1:20) {
      if (i != j) {
        fifth_term <- fifth_term + log(param[22]*param[i] + param[j] + param[21]*sqrt(param[22]*param[i]*param[j]))
      }
    }
  }
  neg_log_lik <- first_term + second_term + third_term + fourth_term - fifth_term
  return(-neg_log_lik)
}

par_initial_values = c(.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5,.5)

# Loops through ten seasons of Premier League Data
csv_list <- c("epl_0910.csv", "epl_1011.csv", "epl_1112.csv", "epl_1213.csv", "epl_1314.csv", "epl_1415.csv", "epl_1516.csv", "epl_1617.csv", "epl_1718.csv", "epl_1819.csv")

avg_draws_predicted_BT <- array(1:10)
avg_home_wins_predicted_BT <- array(1:10)
avg_away_wins_predicted_BT <- array(1:10)

avg_draws_predicted_davidson <- array(1:10)
avg_home_wins_predicted_davidson <- array(1:10)
avg_away_wins_predicted_davidson <- array(1:10)

avg_draws_predicted_poisson <- array(1:10)
avg_home_wins_predicted_poisson <- array(1:10)
avg_away_wins_predicted_poisson <- array(1:10)

avg_draws_predicted_poisson_no_cov <- array(1:10)
avg_home_wins_predicted_poisson_no_cov <- array(1:10)
avg_away_wins_predicted_poisson_no_cov <- array(1:10)

mu_array <- array(1:10)
home_advantage_array <- array(1:10)

davidson_home_edge_array <- array(1:10)
davidson_draw_array <- array(1:10)

davidson_log_loss_array <- array(1:10)
poisson_log_loss_array <- array(1:10)
poisson_no_cov_log_loss_array <- array(1:10)

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
  
  num_draws_predicted_BT <- 0
  num_home_wins_predicted_BT <- 0
  num_away_wins_predicted_BT <- 0
  
  num_draws_predicted_davidson <- 0
  num_home_wins_predicted_davidson <- 0
  num_away_wins_predicted_davidson <- 0
  
  num_draws_predicted_poisson <- 0
  num_home_wins_predicted_poisson <- 0
  num_away_wins_predicted_poisson <- 0
  
  num_draws_predicted_poisson_no_cov <- 0
  num_home_wins_predicted_poisson_no_cov <- 0
  num_away_wins_predicted_poisson_no_cov <- 0
  
  davidson_log_loss <- 0
  poisson_log_loss <- 0
  poisson_no_cov_log_loss <- 0
  
  # Runs 5-fold validation of the models on each of the seasons
  for (j in 1:5) {
    train_splits <- splits[-j]
    train_data <- do.call("rbind", train_splits)
    numGames = nrow(train_data)
    numDraws <- sum(train_data$draw)
    proportion_draws <- numDraws/numGames
    
    total_home_wins <- sum(train_data$home_win)
    total_home_draws <- sum(train_data$draw)
    
    home_wins_array <- train_data %>%
      group_by(home_team_code) %>%
      summarize(num_home_wins = sum(home_win == 1))
    
    home_draws_array <- train_data %>%
      group_by(home_team_code) %>%
      summarize(num_home_draws = sum(draw == 1))
    
    home_wins_array <- merge(x=home_wins_array,y=team_codes,by.x="home_team_code",by.y="team_code")
    home_draws_array <- merge(x=home_draws_array,y=team_codes,by.x="home_team_code",by.y="team_code")
    home_wins_array[is.na(home_wins_array)] <- 0
    home_draws_array[is.na(home_draws_array)] <- 0
    
    wins_matrix <- matrix(0, nrow = 20, ncol = 20)
    draws_matrix <- matrix(0, nrow = 20, ncol = 20)
    
    for (z in 1:numGames) {
      wins_matrix[train_data$home_team_code[z], train_data$away_team_code[z]] = ifelse(train_data$home_win[z] == 1, 1, 0)
      draws_matrix[train_data$home_team_code[z], train_data$away_team_code[z]] = ifelse(train_data$draw[z] == 1, 1, 0)
    }
    
    # estimates team strength, home field advantage, and draw parameters for the Davidson model
    davidson_results <- optim(par = par_initial_values, fn=fun_lik,
                              thd = total_home_draws, thw = total_home_wins,
                              nhw = home_wins_array$num_home_wins, nhd = home_draws_array$num_home_draws,
                              wa = wins_matrix, da = draws_matrix)
    
    davidson_draw <- davidson_results$par[21]
    davidson_home_edge <- davidson_results$par[22]
    davidson_strength <- data.frame(davidson_results$par[1:20])
    davidson_strength <- cbind(davidson_strength,team_codes)
    davidson_strength <- davidson_strength %>%
      rename(davidson_strength = davidson_results.par.1.20.)
    
    train_data <- merge(x=train_data,y=davidson_strength,by.x="HomeTeam",by.y="Team")
    train_data <- merge(x=train_data,y=davidson_strength,by.x="AwayTeam",by.y="Team")
    train_data <- train_data %>%
      rename(home_davidson_strength = davidson_strength.x, away_davidson_strength = davidson_strength.y) %>%
      select(-c(team_code.x,team_code.y))
    train_data <- train_data %>% 
      mutate(prob_draw_davidson = (davidson_draw*sqrt(davidson_home_edge*home_davidson_strength*away_davidson_strength))/(davidson_home_edge*home_davidson_strength + away_davidson_strength + davidson_draw*sqrt(davidson_home_edge*home_davidson_strength*away_davidson_strength)))
    
    davidson_quant <- quantile(train_data$prob_draw_davidson, c(1-proportion_draws))
    
    test_split <- splits[j]
    test_data <- do.call("rbind", test_split)
    length_test_data = nrow(test_data)
    
    test_data <- merge(x=test_data,y=davidson_strength,by.x="HomeTeam",by.y="Team")
    test_data <- merge(x=test_data,y=davidson_strength,by.x="AwayTeam",by.y="Team")
    test_data <- test_data %>%
      rename(home_davidson_strength = davidson_strength.x, away_davidson_strength = davidson_strength.y) %>%
      select(-c(team_code.x,team_code.y))
    test_data <- test_data %>% 
      mutate(prob_draw_davidson = (davidson_draw*sqrt(davidson_home_edge*home_davidson_strength*away_davidson_strength))/(davidson_home_edge*home_davidson_strength + away_davidson_strength + davidson_draw*sqrt(davidson_home_edge*home_davidson_strength*away_davidson_strength)),
             prob_home_win_davidson = (davidson_home_edge*home_davidson_strength)/(davidson_home_edge*home_davidson_strength + away_davidson_strength + davidson_draw*sqrt(davidson_home_edge*home_davidson_strength*away_davidson_strength)),
             prob_away_win_davidson = away_davidson_strength/(davidson_home_edge*home_davidson_strength + away_davidson_strength + davidson_draw*sqrt(davidson_home_edge*home_davidson_strength*away_davidson_strength))) %>%
      mutate(draw_predicted_davidson = ifelse(prob_draw_davidson > davidson_quant, 1, 0),
             davidson_log_loss = log(prob_draw_davidson*draw + prob_home_win_davidson*home_win + prob_away_win_davidson*away_win)) %>%
      mutate(home_win_predicted_davidson = ifelse(prob_home_win_davidson > prob_away_win_davidson & draw_predicted_davidson == 0, 1, 0),
             away_win_predicted_davidson = ifelse(prob_home_win_davidson < prob_away_win_davidson & draw_predicted_davidson == 0, 1, 0)) %>%
      mutate(correct_draw_prediction_davidson = ifelse(draw == draw_predicted_davidson & draw == 1, 1, 0),
             correct_home_win_prediction_davidson = ifelse(home_win == home_win_predicted_davidson & home_win == 1, 1, 0),
             correct_away_win_prediction_davidson = ifelse(away_win == away_win_predicted_davidson & away_win == 1, 1, 0))
    
    # estimates team strength and home field advantage parameters for the Bradley-Terry model
    BT_model <- BTm(result, data.frame(team = HomeTeam, home_field = 1), data.frame(team = AwayTeam, home_field = 0), ~ team + home_field, id = "team", refcat = "Everton", data = train_data)
    abilities <- exp(BTabilities(BT_model))
    abilities <- data.frame(abilities)
    abilities$Team <- rownames(BTabilities(BT_model))
    home <- exp(BT_model$coefficients[20])
    
    train_data <- merge(x=train_data,y=abilities,by.x="HomeTeam",by.y="Team")
    train_data <- merge(x=train_data,y=abilities,by.x="AwayTeam",by.y="Team")
    train_data <- train_data %>%
      rename(home_ability = ability.x, home_s.e. = s.e..x,
             away_ability = ability.y, away_s.e. = s.e..y)
    train_data <- train_data %>% 
      mutate(prob_home = (home*home_ability)/(home*home_ability+away_ability))
    quants <- quantile(train_data$prob_home, c(.5-proportion_draws/2, .5+proportion_draws/2))
    
    test_data <- merge(x=test_data,y=abilities,by.x="HomeTeam",by.y="Team")
    test_data <- merge(x=test_data,y=abilities,by.x="AwayTeam",by.y="Team")
    test_data <- test_data %>%
      rename(home_ability = ability.x, home_s.e. = s.e..x,
             away_ability = ability.y, away_s.e. = s.e..y)
    test_data <- test_data %>% 
      mutate(prob_home_BT = (home*home_ability)/(home*home_ability+away_ability)) %>%
      mutate(draw_predicted_BT = ifelse(prob_home_BT > quants[1] & prob_home_BT < quants[2], 1, 0),
             home_win_predicted_BT = ifelse(prob_home_BT > quants[2], 1, 0),
             away_win_predicted_BT = ifelse(prob_home_BT < quants[1], 1, 0)) %>%
      mutate(correct_draw_prediction_BT = ifelse(draw == draw_predicted_BT & draw == 1, 1, 0),
             correct_home_win_prediction_BT = ifelse(home_win == home_win_predicted_BT & home_win == 1, 1, 0),
             correct_away_win_prediction_BT = ifelse(away_win == away_win_predicted_BT & away_win == 1, 1, 0))
    
    # Uses bivariate poisson model with covariance term with team random effects and no fixed intercept
    stan_data <- list(num_clubs = 20,
                      num_games = numGames,
                      home_team_code = train_data$home_team_code,
                      away_team_code = train_data$away_team_code,
                      h_goals = train_data$FTHG,
                      a_goals = train_data$FTAG)
    
    stanfit <- stan(file = "bivariate_poisson_model.stan", data = stan_data, chains = 3, iter = 7000, warmup = 2000, control = list(adapt_delta = 0.95))
    
    posterior <- extract(stanfit)
    mu <- mean(posterior$mu)
    home_advantage <- mean(posterior$home_field)
    
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
      mutate(home_xg = exp(mu + home_advantage + home_alpha + away_delta) + exp(home_rho + away_rho),
             away_xg = exp(mu + away_alpha + home_delta) + exp(home_rho + away_rho))
    
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
      mutate(home_xg = exp(mu + home_advantage + home_alpha + away_delta) + exp(home_rho + away_rho),
             away_xg = exp(mu + away_alpha + home_delta) + exp(home_rho + away_rho))
    
    poisson_simulations_test <- test_data %>%
      rowwise %>%
      bow(tie(pct_home_win, pct_draw, pct_away_win) := compare_goals(home_xg,away_xg)[c(1,2,3)])
    
    test_data <- cbind(test_data, poisson_simulations_test)
    
    test_data <- test_data %>%
      mutate(draw_predicted_poisson = ifelse(pct_draw > poisson_quant, 1, 0),
             poisson_log_loss = log(pct_draw*draw + pct_home_win*home_win + pct_away_win*away_win)) %>%
      mutate(home_win_predicted_poisson = ifelse(pct_home_win > pct_away_win & draw_predicted_poisson == 0, 1, 0),
             away_win_predicted_poisson = ifelse(pct_home_win < pct_away_win & draw_predicted_poisson == 0, 1, 0)) %>%
      mutate(correct_draw_prediction_poisson = ifelse(draw == draw_predicted_poisson & draw == 1, 1, 0),
             correct_home_win_prediction_poisson = ifelse(home_win == home_win_predicted_poisson & home_win == 1, 1, 0),
             correct_away_win_prediction_poisson = ifelse(away_win == away_win_predicted_poisson & away_win == 1, 1, 0))
    
    # Uses bivariate poisson model with no covariance term
    stanfit <- stan(file = "bivariate_poisson_no_cov.stan", data = stan_data, chains = 3, iter = 7000, warmup = 2000, control = list(adapt_delta = 0.95))
    
    posterior <- extract(stanfit)
    mu <- mean(posterior$mu)
    home_advantage <- mean(posterior$home_field)
    
    alpha_vals <- array(1:20)
    delta_vals <- array(1:20)
    
    for (s in 1:20) {
      alpha_vals[s] <- mean(posterior$alpha[,s])
      delta_vals[s] <- mean(posterior$delta[,s])
    }
    
    team_effects <- data.frame("team_code_no_cov" = 1:20, "alpha_no_cov" = alpha_vals,
                               "delta_no_cov" = delta_vals)
    
    train_data <- merge(x=train_data,y=team_effects,by.x="home_team_code",by.y="team_code_no_cov")
    train_data <- merge(x=train_data,y=team_effects,by.x="away_team_code",by.y="team_code_no_cov")
    train_data <- train_data %>%
      rename(home_alpha_no_cov = alpha_no_cov.x, home_delta_no_cov = delta_no_cov.x,
             away_alpha_no_cov = alpha_no_cov.y, away_delta_no_cov = delta_no_cov.y) %>%
      mutate(home_xg_no_cov = exp(mu + home_advantage + home_alpha_no_cov + away_delta_no_cov),
             away_xg_no_cov = exp(mu + away_alpha_no_cov + home_delta_no_cov))
    
    poisson_simulations <- train_data %>%
      rowwise %>%
      bow(tie(pct_home_win_no_cov, pct_draw_no_cov, pct_away_win_no_cov) := compare_goals(home_xg_no_cov,away_xg_no_cov)[c(1,2,3)])
    
    train_data <- cbind(train_data, poisson_simulations)
    poisson_quant <- quantile(train_data$pct_draw_no_cov, c(1-proportion_draws))
    
    test_data <- merge(x=test_data,y=team_effects,by.x="home_team_code",by.y="team_code_no_cov")
    test_data <- merge(x=test_data,y=team_effects,by.x="away_team_code",by.y="team_code_no_cov")
    test_data <- test_data %>%
      rename(home_alpha_no_cov = alpha_no_cov.x, home_delta_no_cov = delta_no_cov.x,
             away_alpha_no_cov = alpha_no_cov.y, away_delta_no_cov = delta_no_cov.y) %>%
      mutate(home_xg_no_cov = exp(mu + home_advantage + home_alpha_no_cov + away_delta_no_cov),
             away_xg_no_cov = exp(mu + away_alpha_no_cov + home_delta_no_cov))
    
    poisson_simulations_test <- test_data %>%
      rowwise %>%
      bow(tie(pct_home_win_no_cov, pct_draw_no_cov, pct_away_win_no_cov) := compare_goals(home_xg_no_cov,away_xg_no_cov)[c(1,2,3)])
    
    test_data <- cbind(test_data, poisson_simulations_test)
    
    test_data <- test_data %>%
      mutate(draw_predicted_poisson_no_cov = ifelse(pct_draw_no_cov > poisson_quant, 1, 0),
             poisson_no_cov_log_loss = log(pct_draw_no_cov*draw + pct_home_win_no_cov*home_win + pct_away_win_no_cov*away_win)) %>%
      mutate(home_win_predicted_poisson_no_cov = ifelse(pct_home_win_no_cov > pct_away_win_no_cov & draw_predicted_poisson_no_cov == 0, 1, 0),
             away_win_predicted_poisson_no_cov = ifelse(pct_home_win_no_cov < pct_away_win_no_cov & draw_predicted_poisson_no_cov == 0, 1, 0)) %>%
      mutate(correct_draw_prediction_poisson_no_cov = ifelse(draw == draw_predicted_poisson_no_cov & draw == 1, 1, 0),
             correct_home_win_prediction_poisson_no_cov = ifelse(home_win == home_win_predicted_poisson_no_cov & home_win == 1, 1, 0),
             correct_away_win_prediction_poisson_no_cov = ifelse(away_win == away_win_predicted_poisson_no_cov & away_win == 1, 1, 0))
    
    num_draws_predicted_BT <- num_draws_predicted_BT + sum(test_data$correct_draw_prediction_BT)
    num_home_wins_predicted_BT <- num_home_wins_predicted_BT + sum(test_data$correct_home_win_prediction_BT)
    num_away_wins_predicted_BT <- num_away_wins_predicted_BT + sum(test_data$correct_away_win_prediction_BT)
    
    num_draws_predicted_davidson <- num_draws_predicted_davidson + sum(test_data$correct_draw_prediction_davidson)
    num_home_wins_predicted_davidson <- num_home_wins_predicted_davidson + sum(test_data$correct_home_win_prediction_davidson)
    num_away_wins_predicted_davidson <- num_away_wins_predicted_davidson + sum(test_data$correct_away_win_prediction_davidson)
    
    num_draws_predicted_poisson <- num_draws_predicted_poisson + sum(test_data$correct_draw_prediction_poisson)
    num_home_wins_predicted_poisson <- num_home_wins_predicted_poisson + sum(test_data$correct_home_win_prediction_poisson)
    num_away_wins_predicted_poisson <- num_away_wins_predicted_poisson + sum(test_data$correct_away_win_prediction_poisson)
    
    num_draws_predicted_poisson_no_cov <- num_draws_predicted_poisson_no_cov + sum(test_data$correct_draw_prediction_poisson_no_cov)
    num_home_wins_predicted_poisson_no_cov <- num_home_wins_predicted_poisson_no_cov + sum(test_data$correct_home_win_prediction_poisson_no_cov)
    num_away_wins_predicted_poisson_no_cov <- num_away_wins_predicted_poisson_no_cov + sum(test_data$correct_away_win_prediction_poisson_no_cov)
    
    davidson_log_loss <- davidson_log_loss + sum(test_data$davidson_log_loss)/length_test_data
    poisson_log_loss <- poisson_log_loss + sum(test_data$poisson_log_loss)/length_test_data
    poisson_no_cov_log_loss <- poisson_no_cov_log_loss + sum(test_data$poisson_no_cov_log_loss)/length_test_data
    
    Sys.sleep(30)
  }
  
  # Now fits the models on the whole season instead of just a training section. This is to simply observe the model's
  # parameters over time and not used to test their performance.
  num_games_in_season = nrow(current_data)
  
  total_home_wins <- sum(current_data$home_win)
  total_home_draws <- sum(current_data$draw)
  
  home_wins_array <- current_data %>%
    group_by(home_team_code) %>%
    summarize(num_home_wins = sum(home_win == 1))
  
  home_draws_array <- current_data %>%
    group_by(home_team_code) %>%
    summarize(num_home_draws = sum(draw == 1))
  
  home_wins_array <- merge(x=home_wins_array,y=team_codes,by.x="home_team_code",by.y="team_code")
  home_draws_array <- merge(x=home_draws_array,y=team_codes,by.x="home_team_code",by.y="team_code")
  home_wins_array[is.na(home_wins_array)] <- 0
  home_draws_array[is.na(home_draws_array)] <- 0
  
  wins_matrix <- matrix(0, nrow = 20, ncol = 20)
  draws_matrix <- matrix(0, nrow = 20, ncol = 20)
  
  for (z in 1:num_games_in_season) {
    wins_matrix[current_data$home_team_code[z], current_data$away_team_code[z]] = ifelse(current_data$home_win[z] == 1, 1, 0)
    draws_matrix[current_data$home_team_code[z], current_data$away_team_code[z]] = ifelse(current_data$draw[z] == 1, 1, 0)
  }
  
  davidson_results <- optim(par = par_initial_values, fn=fun_lik,
                            thd = total_home_draws, thw = total_home_wins,
                            nhw = home_wins_array$num_home_wins, nhd = home_draws_array$num_home_draws,
                            wa = wins_matrix, da = draws_matrix)
  
  davidson_draw <- davidson_results$par[21]
  davidson_home_edge <- davidson_results$par[22]
  davidson_strength <- data.frame(davidson_results$par[1:20])
  davidson_strength <- cbind(davidson_strength,team_codes)
  davidson_strength <- davidson_strength %>%
    rename(davidson_strength = davidson_results.par.1.20.)
  
  davidson_home_edge_array[season_counter] <- davidson_home_edge
  davidson_draw_array[season_counter] <- davidson_draw
  
  stan_data <- list(num_clubs = 20,
                    num_games = num_games_in_season,
                    home_team_code = current_data$home_team_code,
                    away_team_code = current_data$away_team_code,
                    h_goals = current_data$FTHG,
                    a_goals = current_data$FTAG)
  
  stanfit <- stan(file = "bivariate_poisson_model.stan", data = stan_data, chains = 3, iter = 7000, warmup = 2000, control = list(adapt_delta = 0.95))
  
  posterior <- extract(stanfit)
  mu <- mean(posterior$mu)
  home_advantage <- mean(posterior$home_field)
  
  mu_array[season_counter] <- mu
  home_advantage_array[season_counter] <- home_advantage
  
  alpha_vals <- array(1:20)
  delta_vals <- array(1:20)
  rho_vals <- array(1:20)
  
  for (s in 1:20) {
    alpha_vals[s] <- mean(posterior$alpha[,s])
    delta_vals[s] <- mean(posterior$delta[,s])
    rho_vals[s] <- mean(posterior$rho[,s])
  }
  
  team_effects <- data.frame("team_code" = 1:20, "alpha" = alpha_vals,
                             "delta" = delta_vals, "rho" = rho_vals,
                             "davidson_estimates" = davidson_strength$davidson_strength)
  
  club_results <- merge(x=club_results,y=team_effects,by="team_code")
  season_results_name <- paste(substr(i,1,8),"club_results.csv", sep="_")
  
  write.csv(club_results, file = season_results_name)
  
  avg_draws_predicted_BT[season_counter] <- num_draws_predicted_BT/5
  avg_home_wins_predicted_BT[season_counter] <- num_home_wins_predicted_BT/5
  avg_away_wins_predicted_BT[season_counter] <- num_away_wins_predicted_BT/5
  
  avg_draws_predicted_davidson[season_counter] <- num_draws_predicted_davidson/5
  avg_home_wins_predicted_davidson[season_counter] <- num_home_wins_predicted_davidson/5
  avg_away_wins_predicted_davidson[season_counter] <- num_away_wins_predicted_davidson/5
  
  avg_draws_predicted_poisson[season_counter] <- num_draws_predicted_poisson/5
  avg_home_wins_predicted_poisson[season_counter] <- num_home_wins_predicted_poisson/5
  avg_away_wins_predicted_poisson[season_counter] <- num_away_wins_predicted_poisson/5
  
  avg_draws_predicted_poisson_no_cov[season_counter] <- num_draws_predicted_poisson_no_cov/5
  avg_home_wins_predicted_poisson_no_cov[season_counter] <- num_home_wins_predicted_poisson_no_cov/5
  avg_away_wins_predicted_poisson_no_cov[season_counter] <- num_away_wins_predicted_poisson_no_cov/5
  
  davidson_log_loss_array[season_counter] <- -1*davidson_log_loss/5
  poisson_log_loss_array[season_counter] <- -1*poisson_log_loss/5
  poisson_no_cov_log_loss_array[season_counter] <- -1*poisson_no_cov_log_loss/5
}

backtest_results <- data.frame("Season" = 1:10,
                               "avg_draws_predicted_BT" = avg_draws_predicted_BT,
                               "avg_home_wins_predicted_BT" = avg_home_wins_predicted_BT,
                               "avg_away_wins_predicted_BT" = avg_away_wins_predicted_BT,
                               "avg_draws_predicted_davidson" = avg_draws_predicted_davidson,
                               "avg_home_wins_predicted_davidson" = avg_home_wins_predicted_davidson,
                               "avg_away_wins_predicted_davidson" = avg_away_wins_predicted_davidson,
                               "avg_draws_predicted_poisson" = avg_draws_predicted_poisson,
                               "avg_home_wins_predicted_poisson" = avg_home_wins_predicted_poisson,
                               "avg_away_wins_predicted_poisson" = avg_away_wins_predicted_poisson,
                               "avg_draws_predicted_poisson_no_cov" = avg_draws_predicted_poisson_no_cov,
                               "avg_home_wins_predicted_poisson_no_cov" = avg_home_wins_predicted_poisson_no_cov,
                               "avg_away_wins_predicted_poisson_no_cov" = avg_away_wins_predicted_poisson_no_cov,
                               "davidson_log_loss" = davidson_log_loss_array,
                               "poisson_log_loss" = poisson_log_loss_array,
                               "poisson_no_cov_log_loss" = poisson_no_cov_log_loss_array,
                               "poisson_expected_goal_parameter" = mu_array,
                               "poisson_home_advantage" = home_advantage_array,
                               "davidson_home_advantage" = davidson_home_edge_array,
                               "davidson_draw_parameter" = davidson_draw_array)

write.csv(backtest_results, file = "epl_backtest_results.csv")

# collect the variance components for the random effects
# record the number of home wins predicted by each method
# record the number of draws in each season
# maybe save some plots??


