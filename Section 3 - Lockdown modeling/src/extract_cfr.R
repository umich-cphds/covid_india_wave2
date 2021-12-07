extract_cfr <- function() {
  
  message("pulling time series from covid19india...")
  d <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                col_types = cols()) %>%
    clean_names() %>%
    select(-date) %>%
    select(date = date_ymd, daily_cases = daily_confirmed, daily_deaths = daily_deceased,
           total_cases = total_confirmed, total_deaths = total_deceased, 
           everything())

  message("extracting Kerala CFR...")
    taco_kl <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                     col_types = cols()) %>%
      clean_names() %>%
      select(date = date_ymd, status, value = kl) %>%
      pivot_wider(
        names_from  = "status",
        values_from = "value",
        id_cols     = "date"
      ) %>%
      select(date, daily_cases = Confirmed, daily_deaths = Deceased) %>%
      mutate(cfr_kl = daily_deaths / daily_cases) %>%
      mutate(cfr_kl_t7 = zoo::rollmean(cfr_kl, k = 7, fill = NA, align = "right"))
    d <- d %>% left_join(taco_kl %>% select(date, cfr_kl, cfr_kl_t7), by = "date")
    
    message("extracting Maharashtra CFR...")
    taco <- read_csv("https://api.covid19india.org/csv/latest/state_wise_daily.csv",
                     col_types = cols()) %>%
      clean_names() %>%
      select(date = date_ymd, status, value = mh) %>%
      pivot_wider(
        names_from  = "status",
        values_from = "value",
        id_cols     = "date"
      ) %>%
      select(date, daily_cases = Confirmed, daily_deaths = Deceased) %>%
      mutate(cfr_mh = daily_deaths / daily_cases) %>%
      mutate(cfr_mh_t7 = zoo::rollmean(cfr_mh, k = 7, fill = NA, align = "right"))
    d <- d %>% left_join(taco %>% select(date, cfr_mh, cfr_mh_t7), by = "date")
    
    message("extracting India CFR...")
    d <- d  %>%
      mutate(cfr = daily_deaths / daily_cases) %>%
      mutate(cfr_t7 = zoo::rollmean(x = cfr, k = 7, fill = NA, align = "right")) %>%
      filter(date >= "2021-02-15")
  
  return(d)
  
}

# extract plot defaults -----------
get_plt_def <- function(mh, kl) {
  
  if (mh == TRUE & kl == TRUE) {
    # cfr <- cfr %>% filter(place == "Maharashtra")
    tmp_title    <- "Predicted number of daily COVID-19 deaths using Maharashtra lockdown schedule"
    tmp_subtitle <- "Using Kerala CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(250, 4000, 4000, 4000)
    tmp_nudge    <- 500
    tmp_filename <- "deaths_kl_mh.pdf"
  } else if (mh == TRUE & kl == FALSE) {
    tmp_title    <- "Predicted number of daily COVID-19 deaths using Maharashtra lockdown schedule"
    tmp_subtitle <- "Using Maharashtra CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(400, 9000, 9000, 9000)
    tmp_nudge    <- 1000
    tmp_filename <- "deaths_mh.pdf"
  } else if (kl == TRUE) {
    tmp_title    <- "Predicted number of daily COVID-19 deaths using India lockdown schedule"
    tmp_subtitle <- "Using Kerala CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(250, 4000, 4000, 4000)
    tmp_nudge    <- 500
    tmp_filename <- "deaths_kl.pdf"
  } else {
    # cfr <- cfr %>% filter(place == "India")
    tmp_title    <- "Predicted number of daily COVID-19 deaths using India lockdown schedule"
    tmp_subtitle <- "Using India CFR schedule; February 15, 2021 to May 15, 2021"
    tmp_repel_y  <- c(250, 4000, 4000, 4000)
    tmp_nudge    <- 500
    tmp_filename <- "deaths.pdf"
  }
  
  list(
    "tmp_title"    = tmp_title,
    "tmp_subtitle" = tmp_subtitle,
    "tmp_repel_y"  = tmp_repel_y,
    "tmp_nudge"    = tmp_nudge,
    "tmp_filename" = tmp_filename
  )
  
}

# clean scenario data -----------
clean_scenario <- function(dat, p, stop_obs, scen) {
  dat %>% 
    filter(date <= stop_obs) %>% 
    select(date, daily_cases) %>% 
    rename(incidence = daily_cases) %>% 
    add_row(p %>%  
              filter(scenario == scen) %>% 
              select(date, incidence) %>% 
              drop_na()) %>% 
    add_column(scenario = scen) %>%
    filter(date <= "2021-05-15")
}

# clean scenario data for cfr -----------
clean_scenario_cfr <- function(dat, p, stop_obs, scen, use_cfr) {
  
  dat %>% 
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