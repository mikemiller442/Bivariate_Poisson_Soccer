# Bivariate_Poisson_Soccer
Assessing Bivariate Poisson Models and Pairwise Comparison Models in Predicting the Outcomes of Soccer Matches. The final paper is posted in the file MS251_Final_Report.Rmd.

5-fold validation on ten seasons of English Premier League data is run in the script data_cleaning_btw_models.R, analyzing which models perform the best with respect to binary loss and log loss. The script uses various different specifications for the bivariate poisson models, primarily altering the covariance term to see which model performs better. The first model is found in bivariate_poisson_no_cov.stan, which does not fit a covariance term in the model. The second model is in bivariate_poisson_model.stan, which has a covariance term with team-specific parameters but with no fixed intercept. The third model in bivariate_poisson_int_cov.stan allows for team-specific parameters and an intercept in the covariance term.

The programming language Stan was used with the R package RStan to fit these models. Attacking strengths, defensive strengths, and team-specific effects on the covariance were modelled as random effects normally distributed with mean zero and unknown variance. Prior distributions for the variance parameters were diffuse inverse-gamma distributions.

The pairwise comparison models include the Bradley-Terry model, which cannot model draws, and the Davidson model, which does give probability for draws. These models were fit using Maximum Likelhood Estimation. There is a great package for BT that will estimate the models, but to estimate the Davidson model with a home field advantage I fit the model myself using the log-likelihood equation and the optim library in R.

This project was done for my Sports Analytics class at Skidmore.
