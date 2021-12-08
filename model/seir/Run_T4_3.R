library(dplyr)
library(arm)
library(data.table)
source("par_initializeR.r")
source("model_estimateR.r")
source("model_initializeR.r")
source("mcmc_performR.r")
source("model_deterministic_simulateR.r")
source("R0_calculateR.r")
source("model_predictR.r")
source("model_stochastic_simulateR.r")
source("model_plotR.r")


end_beta=1

#phases = c(as.Date("2020-12-09"),as.Date("2021-01-01"), as.Date("2021-01-16"),
#as.Date("2021-02-01"),as.Date("2021-02-16"), as.Date("2021-03-01"),as.Date("2021-03-12"))

get_phase <- function(start_date = min_date, end_date = max_date, phase_length = 15) {
  
  tmp_dates <- seq.Date(from = as.Date(start_date),
                        to   = as.Date(end_date),
                        by    = "day")
  
  tmp_phases <- c(1,
                  tmp_dates %>% which(x = . %in% rev(rev(tmp_dates)[seq(1, length(tmp_dates), phase_length)]))) %>%
    unique()
  
  tmp_phases <- head(tmp_phases[!tmp_phases %in% 2:phase_length], -1)
  
  return(tmp_phases)
  
}
start_date <- as.Date("2021-04-15")
date_end   <- start_date - 1
date_start  <- date_end - 99
data = read.csv(url("https://api.covid19india.org/csv/latest/case_time_series.csv"))
data = data %>%
  mutate(Current.Confirmed = Total.Confirmed - Total.Recovered - Total.Deceased)
date_initial= which(as.Date(data$Date_YMD) == date_start)
date_final = which(as.Date(data$Date_YMD) == date_end)
data_initial = data[date_initial, ]
data_initial = unname(as.numeric(data_initial[1, c(4,6,8,3,5,7)]))
data_train = data[date_initial : date_final, ]
daily_confirmed <- data_train[,"Daily.Confirmed"] ## Daily Positive
daily_recovered <- data_train[,"Daily.Recovered"] ## Daily Recovered
daily_deaths <- data_train[,"Daily.Deceased"] ##  Daily Deaths

data_country_confirmed <- data_train[ ,"Total.Confirmed"]
data_country_recovered <- data_train[ ,"Total.Recovered"]
data_country_deaths <- data_train[ ,"Total.Deceased"]

N = 1341e6 #population of India
country_name = "India"

data_multinomial = data.frame("Confirmed" = daily_confirmed, "Recovered" = daily_recovered, "Deceased" = daily_deaths)
rownames(data_multinomial) = 1:nrow(data_multinomial)

mcfr = tail(data_country_deaths,1) / (tail(data_country_deaths,1) + tail(data_country_recovered,1))

dbirth_rate_current_country = 1/(365 * 69.4)  #average lifespan - 72.6 years

#dates = seq(as.Date(date_start), as.Date(date_end), by = 1)
#phases = sapply(phases[-length(phases)], function(x)  which(dates == as.Date(x)))
tpred=150
sched <- fread("https://raw.githubusercontent.com/maxsal/science_revision/main/pi_schedule_extended.txt?token=AJI5MRDHU2GXFFB56Z76PBDBQKRLA",
               showProgress = FALSE)
tier4_1 <- sched[place== "Maharashtra"]
use_these_pis <- tier4_1[, .(pis = smooth_pis)] # vector of pis
pi_f=as.numeric(unlist((use_these_pis)))
pi_f=c(pi_f,rep(pi_f[length(pi_f)],times=(tpred-length(pi_f))))
(phases <- get_phase(start_date = date_start, end_date = date_end, phase_length = 15))

Result = model_predictR(data = data_multinomial,init_pars=NULL,data_init = data_initial, T_predict = tpred, pi=pi_f,
                        niter = 3e5, BurnIn = 3e5 ,model = "Multinomial", N = N, lambda = 1/(69.416 * 365),
                        mu = 1/(69.416 * 365), period_start = phases, opt_num = 300, auto.initialize=T,alpha=1,end_beta=end_beta,
                        plot = T, save_plots = FALSE)
saveRDS(Result,"Result_Tier4_3.rds")
