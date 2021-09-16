# libraries ------------
librarian::shelf(
  tidyverse, here, janitor, glue
)
source(here("Section 3 - Lockdown modeling", "src", "extract_cfr.R"))
source(here("Section 3 - Lockdown modeling", "src", "get_esir_incidence.R"))

# specifications -----------
base_path    <- "PATH_TO/DIRECTORY_WITH/*_mcmc.RData" # output from eSIR models
scenarios    <- c("2021-03-01", "2021-03-15", "2021-03-30", "2021-04-15", "2021-04-30")
cfr_scenario <- "india" # india, mh, or kl
end_date     <- "2021-05-15"
mh           <- TRUE

cfr_prefix <- ifelse(cfr_scenario == "india", "mod",
                     ifelse(cfr_scenario == "mh", "high",
                            ifelse(cfr_scenario == "kl", "low", NA)))
le_prefix <- ifelse(mh == FALSE, "strong", ifelse(mh == TRUE, "mod", NA))

file_name <- glue("{le_prefix}le_{cfr_prefix}cfr_main")

dates <- as.Date(c(
  "2021-03-01",
  "2021-03-15",
  "2021-03-30",
  "2021-04-15",
  "2021-04-30",
  "2021-05-15"
))

quick_death_sum <- function(base_path, scenarios, cfr_scenario, end_date, i) {
  # step 1: CFR schedule ------------
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
    filter(date <= "2021-05-15")
  
  # step 2: cumulative death projection ------------
  if (mh == TRUE) {
    tmp_filename <- glue("{scenarios[i]}_mh_smooth1_data.txt")
  } else {
    tmp_filename <- glue("{scenarios[i]}_smooth1_data.txt")
  }
  obs_death    <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                           col_types = cols()) %>%
    filter(Date_YMD == scenarios[i]) %>%
    pull(`Total Deceased`)
  p            <- read_tsv(here("lockdown", "data", "final", tmp_filename),
                        col_types = cols()) %>%
    mutate(scenario = scenarios[i]) %>%
    filter(date <= "2021-05-15") %>%
    left_join(cfrs, by = "date") %>%
    mutate(
      death_incidence = incidence * .[[glue("cfr_{cfr_scenario}")]]
    ) %>%
    mutate(
      death_incidence = case_when(death_incidence < 0 ~ 0, T ~ death_incidence)
    ) %>%
    drop_na(incidence) %>%
    mutate(
      death_cumulative = obs_death + cumsum(death_incidence)
    )

  # step 3: cumlative death interval -----------
  for (j in seq_along(dates)) {
    tmp_date <- as.Date(dates[j])
    if (!(tmp_date %in% p$date)) next
    tmp_p_end <- p %>% filter(date == tmp_date) %>% pull(death_cumulative) %>% round()
    tmp_death_int <- get_esir_death(scen = scenarios[i], alpha = "india", end_date = tmp_date, mh = mh)
    tmp_death_factor <- tmp_p_end / tmp_death_int$mid
    tmp_death_int <- tmp_death_int  %>% mutate(across(where(is.numeric), \(x) round(x * tmp_death_factor)))
    if (!exists("death_int")) {
      death_int <- tmp_death_int
    } else {
        death_int <- bind_rows(death_int, tmp_death_int)
      }
  }
  
  # step 4: death incidence interval -----------
  inc <- get_esir_incidence(scen = scenarios[i], end_date = end_date, mh = mh)
  
  inc_dat <- p %>%
    left_join(inc %>% select(-scenario), by = "date") %>%
    slice(2:n()) %>%
    mutate(
      death_f = death_incidence / mid
    ) %>%
    mutate(
      death_f = ifelse(is.infinite(death_f), NA, death_f)
    ) %>%
    mutate(
      lower = round(lower * mean(death_f, na.rm = T)),
      mid   = round(mid *   mean(death_f, na.rm = T)),
      upper = round(upper * mean(death_f, na.rm = T))
    )
  
  out <- list(
    "cfr" = cfrs,
    "obs_death" = obs_death,
    "death_dat" = inc_dat %>% select(date, lower, est = death_incidence, upper, scenario),
    "death_int" = death_int,
    "peak_death_int" = inc_dat %>%
      filter(death_incidence == max(death_incidence, na.rm = T)) %>%
      mutate(across(where(is.numeric), \(x) round(x))) %>%
      select(date, lower, est = death_incidence, upper)
    
  )
  
  return(out)

}

lst <- list()
for (i in seq_along(scenarios)) {
  
 lst[[scenarios[i]]] <- quick_death_sum(
   base_path    = base_path,
   scenarios    = scenarios,
   cfr_scenario = cfr_scenario,
   end_date     = end_date,
   i            = i
 )
  
}

for (i in seq_along(scenarios)) {
  if (i == 1) {
    death_int_out <- lst[[scenarios[i]]][["death_int"]]
  } else {
    death_int_out <- bind_rows(death_int_out, lst[[scenarios[i]]][["death_int"]])
  }
}

d <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
              col_types = cols()) %>%
  clean_names() %>%
  select(-date) %>%
  select(date = date_ymd, daily_cases = daily_confirmed, daily_deaths = daily_deceased,
         total_cases = total_confirmed, total_deaths = total_deceased, 
         everything())

obs <- d %>%
  select(date, value = total_deaths) %>%
  filter(date %in% dates) %>%
  mutate(
    scenario = "Observed"
  )

# save output ----------
death_int_out %>%
  mutate(across(where(is.numeric), \(x) x / 1000)) %>%
  left_join(obs %>% mutate(value = value / 1000) %>% select(-scenario), by = "date") %>%
  mutate(
    avg_avt = (mid - value),
    lo_avt  = (upper - value),
    hi_avt  = (lower - value)
  ) %>%
  mutate(
    avg_pct = ((mid - value) * -100) / value,
    lo_pct  = ((upper - value) * -100) / value,
    hi_pct  = ((lower - value) * -100) / value
  ) %>%
  as.data.frame() %>%
  write_tsv(here("Section 3 - Lockdown modeling", "out", glue("{file_name}.txt")))
