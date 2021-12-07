# LIBRARIES -----------
library(tidyverse)
library(glue)

summary <- function(df, dfA, urf = 15){
  inc = df[,,2] + df[,,3] + dfA/(1 + urf)  
  return(cbind(apply(inc, 2, quantile, probs = 0.025),
        apply(inc, 2, mean),
        apply(inc, 2, quantile, probs = 1- 0.025)))
  
}


# FOR eSIR OUTPUT ----------
cleanr_esir <- function(f_out = NULL, name = NULL, adj = T, adj_len = 2, out_obs = FALSE, obs_dat = NULL, N = 1.34e9, urf = 15) {
  
  if (is.null(N)) {
    return("You need to specify the 'N' argument (population size)...")
  } else {
    things <- list(
      T_prime          = f_out$plot_infection$plot_env$T_prime,
      T_fin            = f_out$plot_infection$plot_env$T_fin,
      chron_ls         = f_out$plot_infection$plot_env$chron_ls,
      data_comp        = f_out$plot_infection$plot_env$data_comp,
      data_comp_R      = f_out$plot_infection$plot_env$data_comp_R,
      theta_post       = f_out$plot_infection$plot_env$theta_pp, 
      theta_pre       = f_out$plot_infection$plot_env$theta_p,
      thetaA_post       = f_out$plot_infection$plot_env$thetaA_pp, 
      thetaA_pre       = f_out$plot_infection$plot_env$thetaA_p
    )
    
    inc_pre <- round(N*summary(things$theta_pre, things$thetaA_pre, urf= urf))
    inc_post <- round(N*summary(things$theta_post, things$thetaA_post, urf= urf))
    
    if (adj == TRUE) {
      adj_v <- mean(
        as.vector(NI_complete[(things$T_prime - adj_len):things$T_prime]) / 
          N / 
          (things$data_comp[(things$T_prime - adj_len):things$T_prime, "mean"] +
             things$data_comp_R[(things$T_prime - adj_len):things$T_prime, "mean"])
      )
      
      confirm     <- round(inc_post * adj_v)
      confirm2     <- round(rbind(inc_pre, inc_post) * adj_v)
    }
    
    tib <- tibble(
      date        = as.Date(things$chron_ls, "%m/%d/%y") %>% tail(-things$T_prime) %>% head(-1),
      value       = na.trim(confirm),
      upper_ci    = na.trim(confirm_up),
      lower_ci    = na.trim(confirm_low)
    ) %>%
      mutate(
        incidence = value - dplyr::lag(value)
      )
    
    tib2 <- tibble(
      date = as.Date(things$chron_ls, "%m/%d/%Y") %>% head(-1),
      value = confirm2,
      upper_ci = confirm_up2,
      lower_ci = confirm_low2
    ) %>%
      mutate(
        incidence = value - dplyr::lag(value)
      )
    
    if (!is.null(name)) {
      tib <- tib %>% add_column(scenario = name)
      tib2 <- tib2 %>% add_column(scenario = name)
    }
    
    if (out_obs == TRUE) {
      if (is.null(obs_dat)) {
        return("If you want to output observed data, you need to specify 'obs_dat' with columns 'Date' and 'Cases'...")
      } else {
        
        obs_dat <- obs_dat %>% dplyr::select(date = Date, value = Cases)
        
        if (!is.null(name)) {
          obs_dat <- obs_dat %>% add_column(scenario = "Observed")
        }  
        tib <- obs_dat %>% add_column(forecast = 0) %>% bind_rows(tib %>% add_column(forecast = 1))
      }
    }
    
    data <- tib %>% dplyr::select(date, value, lower_ci, upper_ci, incidence, everything())
    data2 <- tib2 %>% dplyr::select(date, value, lower_ci, upper_ci, incidence, everything())
    
    posterior <- tibble(
      stat  = c("r0", "beta", "gamma"),
      mean  = c(f_out$out_table$R0_p_mean, f_out$out_table$beta_p_mean, f_out$out_table$gamma_p_mean),
      lower = c(f_out$out_table$R0_p_ci_low, f_out$out_table$beta_p_ci_low, f_out$out_table$gamma_p_ci_low),
      upper = c(f_out$out_table$R0_p_ci_up, f_out$out_table$beta_p_ci_up, f_out$out_table$gamma_p_ci_up)
    )
    
    out_tib <- f_out$out_table %>% t() %>% as_tibble(rownames = "stat") %>% dplyr::rename(value = V1)
    
    list(
      data      = data,      # prediction only
      data2     = data2,     # training fit and prediction
      posterior = posterior,
      out_tib   = out_tib
    )
    
  }
  
}

# PREPARING DATA FOR PLOTTING -----------
# case data
clean_scenario <- function(dat, p, stop_obs, scen, end_date = "2021-07-31") {
  dat %>%  # dat is the observed data
    filter(date <= stop_obs) %>% 
    select(date, daily_cases) %>% 
    rename(incidence = daily_cases) %>% 
    add_row(p %>%  
              filter(scenario == scen) %>% 
              select(date, incidence) %>% 
              drop_na()) %>% 
    add_column(scenario = scen) %>%
    filter(date <= end_date)
}

# death -----------
table_of_cfrs <- read_csv("https://raw.githubusercontent.com/maxsal/science_revision/main/data/cfr_schedule_14day_lag.txt",
                          show_col_types = FALSE)

clean_scenario_cfr <- function(dat, p, stop_obs, scen, use_cfr) {
  
  if (!exists("d")) {
    d <- table_of_cfrs
  }
  
  if (any(grepl("cfr",names(d))) == FALSE) {
    stop("Object `d` should be table of CFR vectors. Max apologizes for this poor coding practice. See: https://github.com/maxsal/science_revision/blob/main/data/cfr_schedule_14day_lag.txt")
  } else {
  
  dat %>% # dat is the observed data
    filter(date <= stop_obs) %>% 
    select(date, daily_deaths) %>% 
    rename(incidence = daily_deaths) %>% 
    add_row(p %>%  
              filter(scenario == scen) %>% 
              select(date, incidence) %>% 
              drop_na() %>% 
              left_join(d %>% select(date, cfr = {{ use_cfr }}), by = "date") %>%
              mutate(incidence = incidence*cfr) %>% 
              select(-cfr)) %>% 
    add_column(scenario = scen) 
    
  }
  
}



# PLOT HELPERS -----------
# formatting dates
format_date <- function(x) {
  
  glue("{trimws(format(x, '%B'))} {trimws(format(x, '%e'))}, {trimws(format(x, '%Y'))}")
  
}

# colors
colores4 <- c(
  "Observed"          = "#0b0c0c",
  "Tier 2"            = "#e6811c",
  "Tier 3"            = "#e0101a",
  "Tier 4"            = "#9900d1",
  "Tier 4 - March 30" = "#9900d1",
  "Tier 4 - April 15" = "#9900d1"
)