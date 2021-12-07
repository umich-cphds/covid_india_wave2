### library functions needed
librarian::shelf(tidyverse, chron, rjags, 
                 gtools, here, devtools, lubridate,
                 lilywang1988/eSIR, 
                 glue, zoo, data.table)

### helper scripts (included in same folder)
source(paste0("esir_ally.R"))
source(paste0("tvt.eSAIR_mod.R"))
source(paste0("utils_functions.R"))

today   <- Sys.Date() - 1
set.seed(20192020) # default: 20192020

Ms        <- 5e5    # 5e5 recommended (5e3 for testing - but not stable)
nburnins  <- 2e5    # 2e5 recommended (2e3 for testing - but not stable)

# specifications ----------
R_0            <- 2 # basic reproduction number
save_files     <- TRUE
save_mcmc      <- TRUE
save_plot_data <- TRUE

### observed data
dat <- read_csv(paste0("data_for_lockdown_extended.csv"), col_types = cols()) %>% filter(date <= "2021-07-31")
### pi(t) modifier
pi_sched  <- read_tsv(paste0("pi_schedule_extended.txt"), col_types = cols())
### beta(t) modifier
beta <- read_csv(paste0("beta_t.csv"))
### alpha(t) modifier
sero <- read_csv(paste0("sero_t.csv"))


### toy run for 100 day training period until March 19, 2021 with forcast of 100 days
last_obs   <- as.Date("2021-03-18")
start_obs  <- last_obs - 99
start_proj <- last_obs + 1
last_proj  <- last_obs + 100
proj_days  <- as.numeric(last_proj - start_proj) - 1
esir_days  <- as.numeric(last_proj - start_obs)

d <- dat %>%  filter(place == "India" & date >= start_obs & date <= last_obs)
f = sero %>% filter(date == start_obs) %>% pull(vals)

NI_complete <- d$cases
RI_complete <- d$recovered + d$deaths
N           <- 1.34e9                           # population of India
R           <- unlist(RI_complete/N)           # proportion of recovered per day
Y           <- unlist(NI_complete/N-R)

### maharashtra intervention-based pi schedule used
use_these_pis <- pi_sched %>% select(-c(r_est)) %>%
  dplyr::filter(place == "Maharashtra") %>%  arrange(date) %>%
  pull(smooth_pis) %>% c(1, .)%>%  head(., -1)
use_these_pis <- ifelse(use_these_pis != 1, 1, 1)

use_these_dates <- format(as.Date(start_proj:last_proj, origin = "1970-01-01"), "%m/%d/%Y")[1:(length(use_these_pis) - 1)]
multi <- beta %>% filter(date <= last_proj & date >= start_obs) %>% pull(beta_t) %>% head(., -1)
use_these_a <- sero %>% filter(date <= last_proj & date >= start_obs) %>% pull(vals) %>% head(., -1) 
use_these_a[2:length(use_these_a)] <- 0


casename   <- glue("{last_obs + 1}_esair_r{R_0}")

dir.path <- paste0(data_repo, "/", casename)
dir.create(dir.path)
setwd(dir.path)
feb19_20pct_mod <- tvt.eSAIR(Y, R,
                             begin_str      = format(start_obs, "%m/%d/%Y"),
                             death_in_R     = d_i_R,
                             T_fin          = esir_days,
                             pi0            = use_these_pis,
                             change_time    = use_these_dates,
                             multiplier     = multi, 
                             a0             = use_these_a, 
                             R0             = R_0,
                             dic            = TRUE,
                             casename       = casename,
                             save_files     = save_files,
                             save_mcmc      = save_mcmc,
                             rayleigh       = FALSE, 
                             save_plot_data = save_plot_data,
                             M              = Ms,
                             nburnin        = nburnins
)
### cleaning output
clean_out <- feb19_20pct_mod %>% cleanr_esir(N = N, adj = T, adj_len = 2, name = casename)   
### saving output
write_tsv(clean_out$data, file = paste0(data_repo, "/", casename, "_data.txt"))
write_tsv(clean_out$data2, file = paste0(data_repo, "/", casename, "_data_full.txt"))
write_tsv(clean_out$out_tib, file = paste0(data_repo, "/", casename, "_out_table.txt"))