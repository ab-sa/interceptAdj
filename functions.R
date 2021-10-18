EstOR_compare <- function(q_changeRate = 10, mu = 0.5, pi_val = NULL, mu_val = NULL,
                          SetSeed = NULL, n_sim = 10000) {
  
  if (! is.null(SetSeed)) set.seed(SetSeed)
  
  ## Var range
  v_vals <- seq(0, (mu) * (1 - mu), length.out = 50)

  res <- data.frame(Var = v_vals,
                    CV = sqrt(v_vals) / mu,
                    AUC = NA,
                    Exact_OR = NA,
                    Janssen_OR = NA,
                    TaylorApprox_OR = NA)
  
  
  for (i in 1 : length(v_vals)) {
    
    v <- v_vals[i]
    
    if (v == 0 | v >= (mu * (1 - mu) - 0.01)) res[i , c(3 : 7)] <- NA
    else {
      
      pi_sim <- rbeta(n = n_sim,
                      shape1 = (mu * (1 - mu) / v - 1) * mu,
                      shape2 = (mu * (1 - mu) / v - 1) * (1 - mu))
      
      
      pi_sim[pi_sim == 1] <- 0.99
      pi_sim[pi_sim == 0] <- 0.01
      n_sim_adj <- length(pi_sim)
      
      l_OR_sim <- log(pi_sim / (1 - pi_sim))
      pi_mean <- mean(pi_sim)
      pi_var <- var(pi_sim)
      q <- mean(pi_sim) + mean(pi_sim) * q_changeRate / 100

      y_temp <- c(rep(1, round(n_sim_adj * q)), rep(0, n_sim_adj - round(n_sim_adj * q)))
      glm_temp <- glm(y_temp ~ offset(l_OR_sim), family = binomial(link = "logit"))
      res$Exact_OR[i] <- exp(coef(glm_temp))

      pi_sim_adj <- ((pi_sim / (1 - pi_sim)) * res$Exact_OR[i]) / (1 + ((pi_sim / (1 - pi_sim)) * res$Exact_OR[i]))
      y_sim <- rbinom(n = n_sim, size = 1, prob = pi_sim_adj)
      res$AUC[i] <- ifelse(mean(y_sim) > 0, try(auc(y_sim, pi_sim_adj), silent = T), NA)
      
      ## prevalance estimate (Janssen's suggestion)
      res$Janssen_OR[i] <- (q / (1 - q)) / (pi_mean / (1 - pi_mean))

      ## our estimate (Taylor approximation)
      res$TaylorApprox_OR[i] <- odds_adjust(p0 = pi_mean, p1 = q, v = pi_var)
    }
    
  }
  
  return(res)
}

