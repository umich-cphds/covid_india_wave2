library(data.table)
library(janitor)
library(tidyverse)

get_cfr <- function(end_date = "2021-05-15") {
  
  out <- fread("cfr_schedule_14day_lag.txt")[, date := as.Date(date)]
  
  return(out[date <= end_date])
  
}

generate_cfr <- function(scen_start, end_date) {
  
  cfr <- get_cfr(end_date = end_date) %>%
    mutate(
      cfr_mod_smooth  = case_when(date < as.Date(scen_start) ~ cfr_mod_daily, T ~ cfr_mod_smooth),
      cfr_high_smooth = case_when(date < as.Date(scen_start) ~ cfr_mod_daily, T ~ cfr_high_smooth),
      cfr_low_smooth  = case_when(date < as.Date(scen_start) ~ cfr_mod_daily, T ~ cfr_low_smooth)
    ) %>%
    mutate(
      cfr_mod_smooth  = replace(cfr_mod_smooth, date > end_date, 0),
      cfr_high_smooth = replace(cfr_high_smooth, date > end_date, 0),
      cfr_low_smooth  = replace(cfr_low_smooth, date > end_date, 0)
    ) %>%
    filter(date >= scen_start & date <= end_date) %>%
    .[[glue("cfr_{cfr_sched}_smooth")]]
  
}

period_summary <- function(
  scen       = "t3",
  text       = "", 
  scen_start = as.Date("2021-03-13"),
  end_date   = "2021-05-15",
  cfr_sched  = "mod",
  use_theta  = TRUE,
  use_adj_v  = FALSE,
  mh         = FALSE,
  adj_len    = 2,
  base_path  = NULL,
  N          = 1.34e9,
  waning     = FALSE
) {

  # if (is.null(base_path)) {
  #   base_path <- "early_lockdown"
  # }
  
  scen_name <- glue("{scen_start}_{scen}")
  
  dir <- glue("{scen_start}_{scen}")
  
  load(glue("{dir}_forecast_MCMC.RData"))

  if (use_theta == FALSE) {
    i_compartment_draws <- data.frame(Y_pp)
    r_compartment_draws <- data.frame(R_pp)
  }
  
  if (use_theta == TRUE) {
    i_compartment_draws <- data.frame(theta_pp[, , 2])
    r_compartment_draws <- data.frame(theta_pp[, , 3])
    a_compartment_draws <- data.frame(thetaA_pp)
  }
  
  cumulative_draws <- i_compartment_draws + r_compartment_draws # + a_compartment_draws/16
  
  load(glue("{dir}_plot_data.RData"))
  
  
  other_plot        <- plot_data_ls[[2]]
  T_prime           <- other_plot[[1]]
  infection_plot_ls <- plot_data_ls[[4]]
  data_comp         <- infection_plot_ls[[3]]
  removed_plot_ls   <- plot_data_ls[[5]]
  data_comp_R       <- removed_plot_ls[[3]]
  
  tmp_d <- fread("https://raw.githubusercontent.com/maxsal/science_revision/main/data/covid19india_national_counts_20211031.csv",
                 showProgress = FALSE) %>%
    rename(
      date_ymd = date,
      total_confirmed = total_cases,
      total_deceased = total_deaths
      )
  
  period_cases  <- tmp_d %>% filter(date_ymd == end_date) %>% pull(total_confirmed) - tmp_d %>% filter(date_ymd == scen_start) %>% pull(total_confirmed)
  period_deaths <- tmp_d %>% filter(date_ymd == end_date) %>% pull(total_deceased) - tmp_d %>% filter(date_ymd == scen_start) %>% pull(total_deceased)
  
  if (use_adj_v == TRUE) {
    # Computing scale-free adjustment factor
    
    ni_complete <-  tmp_d %>%
      filter(date_ymd <= (as.Date(scen_start) - 1) & date_ymd >= (as.Date(scen_start) - 100)) %>%
      pull(total_confirmed)
    adj_v <-mean(
      as.vector(ni_complete[(T_prime - adj_len):T_prime]) /
             N /
             (data_comp[(T_prime - adj_len):T_prime, "mean"] + data_comp_R[(T_prime - adj_len):T_prime, "mean"]))
    
    cumulative_draws <- data.frame(cumulative_draws * N * adj_v)
  
  } else {
    
    cumulative_draws <- data.frame(cumulative_draws * N)
    
  }
  
  ######
  
  # Daily new cases
  
  daily_new_draws <- data.frame(cbind(
    cumulative_draws[, 1] - (
      infection_plot_ls$data_comp$mean[T_prime] + removed_plot_ls$data_comp_R$mean[T_prime]
    ) * N,
    t(diff(t(
      cumulative_draws
    )))
  ))
  
  # Daily_New_Draws = Diff_I_Draws + Diff_R_Draws
  daily_new_summary <- data.frame(
    lower = apply(
      daily_new_draws,
      2,
      quantile,
      probs = 0.025,
      na.rm = T
    ),
    median = apply(
      daily_new_draws,
      2,
      quantile,
      probs = 0.5,
      na.rm = T
    ),
    upper = apply(
      daily_new_draws,
      2,
      quantile,
      probs = 0.975,
      na.rm = T
    ),
    mean = apply(
      daily_new_draws,
      2,
      mean,
      na.rm = T
      )
  )

  # Total cases during a period
  
  t_pred <- length(seq.Date(from = as.Date(scen_start), to = as.Date(end_date), by = "day"))
  
  # T_Pred = 30 # Change to period length of interest accordingly
  
  draws_total_period <- rowSums(daily_new_draws[, 1:t_pred, drop = F])
  
  ###This thing below is what you will use for the tables.###
  pred_total_period <- c(
    quantile(draws_total_period, probs = c(0.025, 0.5, 0.975)),
    mean(draws_total_period)
  )
  pred_total_period[pred_total_period < 0] <- 0
  
  # Daily new deaths
  
  cfr <- generate_cfr(scen_start = scen_start, end_date = end_date)

  daily_deaths_draws <- data.frame(t(t(daily_new_draws[, 1:t_pred]) * cfr))
  
  daily_deaths_summary <- data.frame(
    "lower" = apply(
      daily_deaths_draws,
      2,
      quantile,
      probs = 0.025,
      na.rm = T
    ),
    "median" = apply(
      daily_deaths_draws,
      2,
      quantile,
      probs = 0.5,
      na.rm = T
    ),
    "upper" = apply(
      daily_deaths_draws,
      2,
      quantile,
      probs = 0.975,
      na.rm = T
    ),
    mean = apply(
      daily_deaths_draws,
      2,
      mean,
      na.rm = T
    )
  )
  
  # Total deaths during a period
  draws_deaths_period <- rowSums(daily_deaths_draws[, 1:t_pred, drop = F])
  
  ###This thing below is what you will use for the tables.###
  deaths_total_period <- c(
    quantile(draws_deaths_period, probs = c(0.025, 0.5, 0.975)),
    mean(draws_deaths_period)
  )
  deaths_total_period[deaths_total_period < 0] <- 0

  return(
    list(
      "specs" = tibble(
        "start_date"   = scen_start,
        "end_date"     = end_date,
        "cfr_schedule" = cfr_sched
      ),
      "observed_period_cases"   = round(period_cases / 1e6, 1),
      "predicted_period_cases"  = round((pred_total_period / 1e6)[c(4, 1, 3)], 1),
      "pred_cases_averted"      = round((-1 * (pred_total_period - period_cases) / 1e6)[c(4, 3, 1)], 1),
      "pct_cases_averted"       = round((period_cases - pred_total_period) * 100 / period_cases, 1)[c(4, 3, 1)],
      "observed_period_deaths"  = round(period_deaths / 1e3, 1),
      "predicted_period_deaths" = round((deaths_total_period / 1e3)[c(4, 1, 3)], 1),
      "pred_deaths_averted"     = round((-1 * (deaths_total_period - period_deaths) / 1e3)[c(4, 3, 1)], 1),
      "pct_deaths_averted"      = round((period_deaths - deaths_total_period) * 100 / period_deaths, 1)[c(4, 3, 1)]
    )
  )
}


