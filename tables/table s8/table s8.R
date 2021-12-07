librarian::shelf(tidyverse, glue, here, janitor, data.table)

source("period_summary_new.R")

r_0       <- 2
cfr_sched <- "mod"
mh        <- FALSE
wane      <- FALSE

le_type   <- ifelse(mh == TRUE, "mod", ifelse(mh == FALSE, "strong", NA))
wane_type <- ifelse(wane == TRUE, "wane", "main")

feb_19_mar_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-02-19"), end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_apr_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-02-19"), end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_apr_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-02-19"), end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_may_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-02-19"), end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_may_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-02-19"), end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
feb_19_jun_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-02-19"), end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

mar_13_mar_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-13"), end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_apr_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-13"), end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_apr_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-13"), end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_may_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-13"), end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_may_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-13"), end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_13_jun_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-13"), end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

mar_19_mar_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-19"), end_date = "2021-03-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_apr_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-19"), end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_apr_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-19"), end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_may_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-19"), end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_may_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-19"), end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_19_jun_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-19"), end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

mar_30_apr_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-30"), end_date = "2021-04-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_apr_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-30"), end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_may_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-30"), end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_may_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-30"), end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
mar_30_jun_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-03-30"), end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)

apr_15_apr_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-04-15"), end_date = "2021-04-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
apr_15_may_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-04-15"), end_date = "2021-05-15", waning = wane, cfr_sched = cfr_sched, mh = mh)
apr_15_may_30 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-04-15"), end_date = "2021-05-30", waning = wane, cfr_sched = cfr_sched, mh = mh)
apr_15_jun_15 <- period_summary(scen = glue("t5_r{r_0}"), scen_start = as.Date("2021-04-15"), end_date = "2021-06-15", waning = wane, cfr_sched = cfr_sched, mh = mh)


print_cases <- function(x) {
  
  a <- x$observed_period_cases
  b <- x$predicted_period_cases
  d <- x$pred_cases_averted
  f <- x$pct_cases_averted
  
  glue("
       {format(a, nsmall = 1)}
       {format(b[1], nsmall = 1)} [{format(b[2], nsmall = 1)}, {format(b[3], nsmall = 1)}]
       {format(d[1], nsmall = 1)} [{format(d[2], nsmall = 1)}, {format(d[3], nsmall = 1)}]
       {format(f[1], nsmall = 1)}% [{format(f[2], nsmall = 1)}%, {format(f[3], nsmall = 1)}%]
       ")
  
}

cases_out <- tibble(
  "Tier II" = c(print_cases(feb_19_mar_30), print_cases(feb_19_apr_15), print_cases(feb_19_apr_30), print_cases(feb_19_may_15), print_cases(feb_19_may_30), print_cases(feb_19_jun_15)),
  "Tier III" = c(print_cases(mar_13_mar_30), print_cases(mar_13_apr_15), print_cases(mar_13_apr_30), print_cases(mar_13_may_15), print_cases(mar_13_may_30), print_cases(mar_13_jun_15)),
  "Tier IV" = c(print_cases(mar_19_mar_30), print_cases(mar_19_apr_15), print_cases(mar_19_apr_30), print_cases(mar_19_may_15), print_cases(mar_19_may_30), print_cases(mar_19_jun_15)),
  "Tier IV - March 30" = c( "-", print_cases(mar_30_apr_15), print_cases(mar_30_apr_30), print_cases(mar_30_may_15), print_cases(mar_30_may_30), print_cases(mar_30_jun_15)),
  "Tier IV - April 15" = c( "-", "-", print_cases(apr_15_apr_30), print_cases(apr_15_may_15), print_cases(apr_15_may_30), print_cases(apr_15_jun_15))
)
write_csv(cases_out, glue("table s8.csv"))