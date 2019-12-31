# Bivariate_Poisson_Soccer
Assessing Bivariate Poisson Models and Pairwise Comparison Models in Predicting the Outcomes of Soccer Matches.

K-fold validation on ten seasons of English Premier League data is run in the script data_cleaning_btw_models.R, analyzing which models perform the best with respect to binary loss and log loss. The script uses various different specifications for the bivariate poisson models, primarily altering the covariance term to see which model performs better. In addition, the pairwise comparison models include the Bradley-Terry model, which cannot model draws, and the Davidson model, which does give probability for draws. These models were fit using Maximum Likelhood Estimation.
