model_deterministic_simulateR <- function(init_obs, period_start, times, pars, fix_pars){
  ode_solveR <- function(stage_pars, fix_pars, old_values) {
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
    
    S_new = S - 0.5* b * S * (P + alpha* U) / N + lambda * N - mu * S
    E_new = E + 0.5 * b * S * (P + alpha * U ) / N - E / De - mu * E
    U_new = U + (1 - r) * E / De - U / (beta_1 * Dr) - delta_1 * mu_c * U - mu * U
    P_new = P + r * E / De - P / Dr - mu_c * P - mu * P
    RU_new <- RU + U / (beta_1 * Dr) - mu * RU
    RR_new <- RR + P / Dr - mu * RR
    DU_new <- DU + delta_1 * mu_c * U
    DR_new <- DR + mu_c * P 
    est_P_new_n <- E
    est_P_new_prob <- r/ De 
    est_RD_new_n <- P
    est_RD_new_prob_R <- 1 / Dr 
    est_RD_new_prob_D <- mu_c 
    
    return(c(S_new, E_new, U_new, P_new,RU_new, RR_new, DU_new, DR_new, 
             est_P_new_n, est_P_new_prob,
             est_RD_new_n, est_RD_new_prob_R, est_RD_new_prob_D))
  }
  n_period = length(period_start)
  ymat = matrix(0, length(times), length(init_obs) + 6)
  ymat[, 1] = times
  
  colnames(ymat) <- c("time", names(init_obs), "est_p_n", "est_p_prob", "est_RD_n", "est_RD_prob_R", "est_RD_prob_D")
  
  which.period <- function(i, phase = period_start){ # function to determine which period i falls in
    sum(i >= phase)
  }
  
  for(i in 1:length(times)){
    stage_pars <- c(b = pars[which.period(i)], r = pars[n_period + which.period(i)])
    if(i == 1) {
      old_values <- init_obs
    } else {
      old_values <- ymat[i - 1, 2:9]
    }
    ymat[i, 2:14] <- ode_solveR(stage_pars = stage_pars, fix_pars = fix_pars, old_values = old_values)
  }
  return(ymat)
}