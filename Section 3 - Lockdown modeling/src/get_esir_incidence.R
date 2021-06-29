# pacman::p_load(tidyverse, here, glue, janitor)
# 
# source(here("lockdown", "extract_cfr.R"))
# 
# base_path <- "/Users/maxsalvatore/local/science_covind/main"
# scenarios <- c("2021-03-01", "2021-03-15", "2021-03-30", "2021-04-15", "2021-04-30")
# i         <- 1
# end_date  <- "2021-05-15"
# cfrs      <- extract_cfr() %>%
#   select(
#     date, 
#     cfr_india = cfr_t7,
#     cfr_mh    = cfr_mh_t7,
#     cfr_kl    = cfr_kl_t7
#   ) %>%
#   mutate(
#     cfr_india = replace(cfr_india, date > end_date, 0),
#     cfr_mh    = replace(cfr_mh, date > end_date, 0),
#     cfr_kl    = replace(cfr_kl, date > end_date, 0)
#   )
# get_alpha_dates <- function(scen, end_date = "2021-05-15") {
#   
#   tmp_d <- seq.Date(from = as.Date(scen), by = "day", length.out = 200)
#   tmp   <- length(tmp_d[tmp_d <= end_date])
#   seq.Date(from = as.Date(scen), by = "day", length.out = tmp)
#   
# }

# cfrs %>%
#   pivot_longer(
#     names_to = "cfr",
#     values_to = "value",
#     -date
#   ) %>%
#   ggplot(aes(x = date, y = value, color = cfr)) +
#   geom_line()

# incidence ------------
get_esir_incidence <- function(
  scen,
  base_path = "/Users/maxsalvatore/local/science_covind/main",
  mh        = FALSE,
  N         = 1.34e9,
  end_date  = "2021-05-15"
  ) {
  
  if (mh == FALSE) {
    tmp_filename <- glue("{scen}_smooth1_mcmc.RData")
  } else {
    tmp_filename <- glue("{scen}_mh_smooth1_mcmc.RData")
  }
  
  message("***beep boop***")
  message(glue("loading: {base_path}/{tmp_filename}..."))
  
  load(glue("{base_path}/{tmp_filename}"))
  
  message("calculating...")
  thetaI_band <-
    data.frame(t(apply(
      theta_pp[, , 2],
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  combined_I_matrix <- data.frame(theta_pp[, , 2])
  
  diff_I_matrix <-
    cbind(combined_I_matrix[, 1], t(diff(t(combined_I_matrix))))
  diff_I_band <-
    data.frame(t(apply(
      diff_I_matrix,
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  
  thetaR_band <-
    data.frame(t(apply(
      theta_pp[, , 3],
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  combined_R_matrix <- data.frame(theta_pp[, , 3])
  
  diff_R_matrix <-
    cbind(combined_R_matrix[, 1], t(diff(t(combined_R_matrix))))
  diff_R_band <-
    data.frame(t(apply(
      diff_R_matrix,
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  
  colnames(diff_I_band) = colnames(diff_R_band) = c("lower", "mid", "upper")
  
  incidence_band <- diff_I_band + diff_R_band
  incidence_band[incidence_band < 0] <- 0
  
  message("***beep boop***")
  return(as_tibble(incidence_band) %>%
           mutate(
             lower    = lower * N,
             mid      = mid * N,
             upper    = upper * N,
             scenario = scen,
             date     = seq.Date(from = as.Date(scen), by = "day", length.out = dim(incidence_band)[1])
             ) %>%
           filter(date <= end_date))

}

# mar1  <- get_esir_incidence(scen = scenarios[1])
# mar15 <- get_esir_incidence(scen = scenarios[2])
# mar30 <- get_esir_incidence(scen = scenarios[3])
# apr15 <- get_esir_incidence(scen = scenarios[4])
# apr30 <- get_esir_incidence(scen = scenarios[5])
# 
# tmp <- bind_rows(mar1, mar15, mar30, apr15, apr30) %>%
#   filter(date <= "2021-05-15")
# 
# tmp %>%
#   group_by(scenario) %>%
#   slice(2:n()) %>%
#   ungroup() %>%
#   ggplot(aes(x = date, y = lower_n)) +
#   geom_line(aes(color = scenario)) +
#   scale_y_continuous(labels = scales::comma) +
#   theme_classic()
# 
# tmp <- mar1 %>%
#   mutate(
#     date = as.Date(as.Date("2021-03-01"):(as.Date("2021-03-01")+n()-1), origin = "1970-01-01")
#   )
# 
# mar1 %>%
#   mutate(n = 1:n()) %>%
#   filter(n > 1) %>%
#   ggplot(aes(x = n)) +
#   geom_ribbon(aes(ymin = lower_n, ymax = upper_n), alpha = 0.5) +
#   scale_y_continuous(labels = scales::comma) +
#   theme_classic()
# 
# mar1_d <- read_tsv(here("lockdown", "data", "final", "2021-03-01_smooth1_data.txt"))
# 
# mar1_d %>% left_join(tmp %>% select(-scenario), by = "date")

# cumulative death ------------
get_esir_death <- function(
  scen,
  base_path = "/Users/maxsalvatore/local/science_covind/main",
  alpha     = "india",
  mh        = FALSE,
  N         = 1.34e9,
  end_date  = "2021-05-15"
) {
  
  if (mh == FALSE) {
    tmp_filename <- glue("{scen}_smooth1_mcmc.RData")
  } else {
    tmp_filename <- glue("{scen}_mh_smooth1_mcmc.RData")
  }
  
  message("***beep boop***")
  
  cfrs <- extract_cfr() %>%
    select(
      date,
      cfr_daily = cfr,
      cfr_india = cfr_t7,
      cfr_mh    = cfr_mh_t7,
      cfr_kl    = cfr_kl_t7
    ) %>%
    mutate(
      cfr_india = case_when(date < as.Date(scenarios[i]) ~ cfr_daily, T ~ cfr_india),
      cfr_mh    = case_when(date < as.Date(scenarios[i]) ~ cfr_daily, T ~ cfr_mh),
      cfr_kl    = case_when(date < as.Date(scenarios[i]) ~ cfr_daily, T ~ cfr_kl)
    ) %>%
    mutate(
      cfr_india = replace(cfr_india, date > end_date, 0),
      cfr_mh    = replace(cfr_mh, date > end_date, 0),
      cfr_kl    = replace(cfr_kl, date > end_date, 0)
    ) %>%
    select(-cfr_daily) %>%
    slice(2:n())
  
  tmp_alpha <- cfrs[[glue("cfr_{alpha}")]]
  
  message(glue("loading: {base_path}/{tmp_filename}..."))
  
  load(glue("{base_path}/{tmp_filename}"))

  # alpha = tmp_alpha
  
  thetaI_band <-
    data.frame(t(apply(
      theta_pp[, , 2] %*% tmp_alpha,
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  combined_I_matrix <- data.frame(theta_pp[, , 2] %*% tmp_alpha)
  
  diff_I_band <-
    data.frame(t(apply(
      combined_I_matrix,
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  
  thetaR_band <-
    data.frame(t(apply(
      theta_pp[, , 3] %*% tmp_alpha,
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  combined_R_matrix <- data.frame(theta_pp[, , 3] %*% tmp_alpha)
  
  diff_R_band <-
    data.frame(t(apply(
      combined_R_matrix,
      2,
      quantile,
      probs = c(0.025, 0.5, 0.975),
      na.rm = T
    )))
  
  colnames(diff_I_band) = colnames(diff_R_band) = c("lower", "mid", "upper")
  
  death_band <- diff_I_band + diff_R_band
  death_band[death_band < 0] <- 0
  
  return(as_tibble(death_band) %>%
           mutate(
             lower    = lower * N,
             mid      = mid * N,
             upper    = upper * N,
             scenario = scen,
             date     = end_date
             ))

}

# mar1_d <- get_eSIR_death(scen = scenarios[1], alpha = "india")
# 
# f <- (158.73 * 1e3) / mar1_d$mid
# 
# mar1_d %>% mutate(across(where(is.numeric), \(x) x * f))
# 
# ds <- seq.Date(from = as.Date(scenario), by = "day", length.out = 200)
# 
# get_alpha_dates(scen = scenarios[5], end_date = "2021-05-15")
