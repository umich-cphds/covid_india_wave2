model_stochastic_simulateR <- function(init_obs_current, init_obs_daily, period_start, pi_fn,timestrain,timestest, pars, fix_pars, T_predict, ...){
  stochastic_sampleR <- function(stage_pars, fix_pars, old_values) {
     ## stage pars
    b = stage_pars[1]
    r = stage_pars[2]
    ## fixed pars
    alpha= fix_pars[1]
    beta_1 = fix_pars[2] 
    delta_1 = fix_pars[3]
    lambda = fix_pars[4]
    mu = fix_pars[5]
    mu_c = fix_pars[6]
    De = fix_pars[7]
    Dr = fix_pars[8]
    N = fix_pars[9]
    ## old values
    S = old_values[1]
    E = old_values[2]
    U = old_values[3]
    P = old_values[4]
    RU = old_values[5]
    RR = old_values[6]
    DU = old_values[7]
    DR = old_values[8] 
    ## new values
    
    pS_vec <- c( 0.5* b * (P + alpha*U) / N , mu, 1 -  0.5 *b * (P + alpha*U) / N - mu)

    sample_S <- rmultinom(1, size = S, prob = pS_vec)
    ##
    pE_vec <- c((1-r) / De, r/ De, mu,1 - 1 / De - mu)
    sample_E <- rmultinom(1, size = E, prob = pE_vec)
    ##
    pU_vec <- c(1/(beta_1*Dr),delta_1*mu_c,mu,1-1/(beta_1*Dr)-delta_1*mu_c-mu)
    
    sample_U <- rmultinom(1, size = U, prob = pU_vec)
    ##
    pP_vec <- c(1 / Dr, mu_c,mu, 1 - 1 / Dr - mu-mu_c)
    sample_P <- rmultinom(1, size = P, prob = pP_vec)
    ##
    pRU_vec <- c( mu,1-mu)
    sample_RU <- rmultinom(1, size = RU, prob = pRU_vec)
    ##
    pRR_vec <- c(mu,1-mu)
    sample_RR <- rmultinom(1, size = RR, prob = pRR_vec)
    ## new values
    S_new <- sample_S[3] + lambda*N
    E_new <- sample_E[4] + sample_S[1]
    U_new <- sample_U[4] + sample_E[1]
    P_new <- sample_P[4] + sample_E[2]
    RU_new <- sample_RU[2] + sample_U[1]
    RR_new <- sample_RR[2] + sample_P[1]
    DU_new <- DU + sample_U[2]
    DR_new <- DR + sample_P[2]
    est_P_new<- sample_E[2]
    est_RR_new<- sample_P[1]
    est_DR_new <- sample_P[2]
    est_U_new<-sample_E[1]
    return(c(S_new, E_new, U_new, P_new, RU_new, RR_new, DU_new, DR_new, 
             est_P_new,est_RR_new,est_DR_new,est_U_new))
  }
  n_period = length(period_start)
  
  which.period <- function(i, phase = period_start){ # function to determine which period i falls in
    sum(i >= phase)
  }

   for(i in 1:length(timestrain)){
    stage_pars <- c(b = pars[which.period(i)], r = pars[n_period + which.period(i)])
    if(i == 1) {
      old_values <- init_obs_current
      results = matrix(c(init_obs_current,init_obs_daily,0,0), nrow = 1)
    } else {
      now_values <- stochastic_sampleR(stage_pars = stage_pars, fix_pars = fix_pars, old_values = old_values)
      results = rbind(results, now_values)
      old_values <- now_values[1:8]
    }
  }
  
  for(j in 1:length(timestest)){
    stage_pars <- c(b=pars[n_period]*pi_fn[j], r = pars[2*n_period])
    now_values <- stochastic_sampleR(stage_pars = stage_pars, fix_pars = fix_pars, old_values = old_values)
    results = rbind(results, now_values)
    old_values <- now_values[1:8]
    }

  return(results)
}