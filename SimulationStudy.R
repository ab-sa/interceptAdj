library(predtools)
library(pROC)

source("functions.R")

#### Simulation study
mu_vals <- c(0.1, 0.25, 0.50)
q_vals <- c(-10, -25, -50)
# q_vals <- c(10, 25, 50)

simRes <- data.frame(Run = rep(c(1 : (length(q_vals) * length(mu_vals))), each = 50),
                     mu = rep(mu_vals, each = 50 * length(q_vals)),
                     AUC = NA, Var_sim = NA, CV_sim = NA,
                     Exact_OR = NA, Naive_OR = NA, TaylorApprox_OR = NA)

# i <- 1
for (i in 1 : length(mu_vals)) {
  
  # j <- 1
  for (j in 1 : length(q_vals)) {
    
    simRes[((i - 1) * length(q_vals) * 50 + (j - 1) * 50 + 1) : ((i - 1) * length(q_vals) * 50 + (j) * 50) ,
           c("Var_sim", "CV_sim", "AUC", "Exact_OR", "Naive_OR", "TaylorApprox_OR")] <-
      EstOR_compare(q_changeRate = q_vals[j], mu = mu_vals[i])
    
  }
  
}

head(simRes)
