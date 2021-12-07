# libraries ----------
pacman::p_load(tidyverse, chron, rjags, gtools, VGAM, here, devtools, eSIR, glue)

f <- list.files("~/projects/science_revision/src/")
for (i in seq_along(f)) {source(paste0("~/projects/science_revision/src/", f[i]))}

g <- list.files("~/projects/science_revision/src_waning/")
for (i in seq_along(g)) {source(paste0("~/projects/science_revision/src_waning/", g[i]))}

today   <- Sys.Date() - 1
arrayid <- Sys.getenv("SLURM_ARRAY_TASK_ID")
set.seed(20192020) # default: 20192020

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") { 
  data_repo <- "~/projects/science_revision/data/early_intervention/"
  Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
  data_repo <- "~/projects/science_revision/test/early_intervention/"
  Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

# specifications ----------
R_0                <- 2     # basic reproduction number
span               <- 1     # span for loess smoother on pi schedule
save_files         <- FALSE
save_mcmc          <- TRUE # output MCMC files (default = TRUE; needed for incidence CI calculations)
save_plot_data     <- TRUE  # should be true
use_sched          <- "Maharashtra early"
rayleigh           <- TRUE
sigma0             <- 310

dat <- read_csv("~/projects/science_revision/data_for_lockdown_extended.csv", col_types = cols()) %>%
  filter(date <= "2021-07-31")

pi_sched  <- read_tsv("~/projects/science_revision/pi_schedule_extended.txt", col_types = cols())
use_these_pis <- pi_sched %>%
  select(-c(r_est)) %>%
  dplyr::filter(place == use_sched) %>%
  arrange(date) %>%
  pull(smooth_pis) %>%
  c(1, .)%>%
  head(., -1)


# directory ----------
setwd(data_repo)

# models ---------
if (arrayid == 1) {
  
  message("pre-lockdown start date: January 1, 2021")
  last_obs   <- as.Date("2021-01-01")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_waning")
  
  jan01_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- jan01_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "MH Pre-lockdown")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}

if (arrayid == 2) {
  
  message("pre-lockdown start date (early MH + 20%): January 1, 2021")
  last_obs   <- as.Date("2021-01-01")
  start_obs  <- last_obs - 99
  start_proj <- last_obs + 1
  last_proj  <- last_obs + 200
  proj_days  <- as.numeric(last_proj - start_proj) - 1
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat %>%
    filter(place == "India" & date >= start_obs & date <= last_obs)
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_pis <- use_these_pis * 1.2
  use_these_pis[use_these_pis > 1] <- 1
  
  use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
  
  casename   <- glue("{last_obs + 1}_20pct_waning")
  
  jan01_20pct_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis,
    change_time    = use_these_dates,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- jan01_20pct_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "MH Pre-lockdown + 20%")   
  write_tsv(clean_out$data, file = paste0("./", casename, "_data.txt"))
  write_tsv(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"))
  
}
