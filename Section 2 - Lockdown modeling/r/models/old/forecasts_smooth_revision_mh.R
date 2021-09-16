# libraries ----------
pacman::p_load(tidyverse, chron, rjags, gtools, here, devtools, eSIR, glue, data.table)

f <- list.files("~/projects/covind_wave2/functions/")
for (i in seq_along(f)) {source(paste0("~/projects/covind_wave2/functions/", f[i]))}

# today        <- Sys.Date() - 1
arrayid      <- Sys.getenv("SLURM_ARRAY_TASK_ID")
data_through <- "2021-07-31"
proj_date    <- "2021-09-15"
set.seed(20192020) # default: 20192020

# Set variables based on testing or production
if (Sys.getenv("production") == "TRUE") { 
  data_repo <- "~/projects/covind_wave2/data/"
  Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)
} else {
  data_repo <- "~/projects/covind_wave2/test/"
  Ms        <- 5e3    # 5e5 recommended (5e3 for testing - but not stable)
  nburnins  <- 2e3    # 2e5 recommended (2e3 for testing - but not stable)
}

# specifications ----------
R_0                <- 2     # basic reproduction number
span               <- 1     # span for loess smoother on pi schedule
save_files         <- FALSE
save_mcmc          <- TRUE # output MCMC files (default = TRUE; needed for incidence CI calculations)
save_plot_data     <- TRUE  # should be true
use_sched          <- "Maharashtra"

dat       <- fread("~/projects/covind_wave2/data_for_lockdown_revision.txt")[date <= data_through]
pi_sched  <- fread("~/projects/covind_wave2/pi_schedule_revision.txt")

use_these_pis <- c(1, pi_sched[, !c("nseq", "r_est")][place == use_sched][order(date)][, smooth_pis])


# directory ----------
setwd(data_repo)

# models ---------
if (arrayid == 1) {
  
  message("lockdown start date: March 1, 2021")
  last_obs   <- as.Date("2021-02-28")
  start_obs  <- as.Date(last_obs - 99)
  start_proj <- as.Date(last_obs + 1)
  proj_days  <- as.numeric(as.Date(proj_date) - as.Date(start_proj))
  last_proj  <- start_proj + proj_days
  esir_days  <- as.numeric(last_proj - start_obs)
  
  use_these_dates <- head(format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y"), -1)
  
  d <- dat[place == "India" & date >= start_obs & date <= last_obs]
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  casename   <- glue("{last_obs + 1}_smooth{span}")

  march1_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis[1:(proj_days+1)],
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
  
  clean_out <- march1_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "Informed")   
  fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
  fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")
  
}

if (arrayid == 2) {
  
  message("lockdown start date: March 15, 2021")
  last_obs   <- as.Date("2021-03-14")
  start_obs  <- as.Date(last_obs - 99)
  start_proj <- as.Date(last_obs + 1)
  proj_days  <- as.numeric(as.Date(proj_date) - as.Date(start_proj))
  last_proj  <- start_proj + proj_days
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat[place == "India" & date >= start_obs & date <= last_obs]
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_dates <- head(format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y"), -1)
  
  casename   <- glue("{last_obs + 1}_smooth{span}")

  march15_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis[1:(proj_days+1)],
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
  
  clean_out <- march15_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "Informed")   
  fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
  fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")
  
}

if (arrayid == 3) {
  
  message("lockdown start date: March 30, 2021")
  last_obs   <- as.Date("2021-03-29")
  start_obs  <- as.Date(last_obs - 99)
  start_proj <- as.Date(last_obs + 1)
  proj_days  <- as.numeric(as.Date(proj_date) - as.Date(start_proj))
  last_proj  <- start_proj + proj_days
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat[place == "India" & date >= start_obs & date <= last_obs]
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_dates <- head(format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y"), -1)
  
  casename   <- glue("{last_obs + 1}_smooth{span}")

  march30_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis[1:(proj_days+1)],
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
  
  clean_out <- march30_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "Informed")   
  fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
  fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")
  
}

if (arrayid == 4) {
  
  message("lockdown start date: April 15, 2021")
  last_obs   <- as.Date("2021-04-14")
  start_obs  <- as.Date(last_obs - 99)
  start_proj <- as.Date(last_obs + 1)
  proj_days  <- as.numeric(as.Date(proj_date) - as.Date(start_proj))
  last_proj  <- start_proj + proj_days
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat[place == "India" & date >= start_obs & date <= last_obs]
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_dates <- head(format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y"), -1)
  
  casename   <- glue("{last_obs + 1}_smooth{span}")

  april15_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis[1:(proj_days+1)],
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
  
  clean_out <- april15_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "Informed")   
  fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
  fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")
  
}

if (arrayid == 5) {
  
  message("lockdown start date: April 30, 2021")
  last_obs   <- as.Date("2021-04-29")
  start_obs  <- as.Date(last_obs - 99)
  start_proj <- as.Date(last_obs + 1)
  proj_days  <- as.numeric(as.Date(proj_date) - as.Date(start_proj))
  last_proj  <- start_proj + proj_days
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat[place == "India" & date >= start_obs & date <= last_obs]
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  use_these_dates <- head(format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y"), -1)
  
  casename   <- glue("{last_obs + 1}_smooth{span}")

  april30_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    pi0            = use_these_pis[1:(proj_days+1)],
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
  
  clean_out <- april30_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "Informed")   
  fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
  fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")
  
}

if (arrayid == 6) {
  
  message("no intervention")
  last_obs   <- as.Date("2021-07-31")
  start_obs  <- as.Date(last_obs - 99)
  start_proj <- as.Date(last_obs + 1)
  proj_days  <- as.numeric(as.Date(proj_date) - as.Date(start_proj))
  last_proj  <- start_proj + proj_days
  esir_days  <- as.numeric(last_proj - start_obs)
  
  d <- dat[place == "India" & date >= start_obs & date <= last_obs]
  
  NI_complete <- d$cases
  RI_complete <- d$recovered + d$deaths
  N           <- 1.34e9                          # population of India
  R           <- unlist(RI_complete/N)           # proportion of recovered per day
  Y           <- unlist(NI_complete/N-R)
  
  casename   <- glue("no_intervention_smooth{span}")
  
  no_int_mod <- tvt.eSIR(
    Y,
    R,
    begin_str      = format(start_obs, "%m/%d/%Y"),
    death_in_R     = 0.2,
    T_fin          = esir_days,
    R0             = R_0,
    dic            = TRUE,
    casename       = casename,
    save_files     = save_files,
    save_mcmc      = save_mcmc,
    save_plot_data = save_plot_data,
    M              = Ms,
    nburnin        = nburnins
  )
  
  clean_out <- no_int_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = "Informed")   
  fwrite(clean_out$data, file = paste0("./", casename, "_data.txt"), sep = "\t")
  fwrite(clean_out$out_tib, file = paste0("./", casename, "_out_table.txt"), sep = "\t")
  
}