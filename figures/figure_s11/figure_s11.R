# libraries ----------
#install.packages("librarian")
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel, janitor, glue,
            here, ggtext, patchwork, data.table)

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2021-06-30")

colors <- c(Observed = "#0b0c0c", `Tier 2` = "#e6811c", `Tier 3` = "#e0101a", 
            `Tier 4` = "#9900d1", `Tier 4 - March 30` = "#9900d1", `Tier 4 - April 15` = "#9900d1")

# helper functions --------------
clean_scenario <- function(dat, p, stop_obs, scen, end_date = "2021-05-15") {
  dat %>% 
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

format_date <- function(x) {
  
  glue("{trimws(format(x, '%B'))} {trimws(format(x, '%e'))}, {trimws(format(x, '%Y'))}")
  
}

# data -------------
obs <- fread("model/covid19india_national_counts_20211031.csv")[, date := as.Date(date)][date >= start_date][]

t2   <- fread("model/seir_results/plot_data/Tier2.csv")[, .(N = V1, daily_cases = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 2 - February 19"]
t3   <- fread("model/seir_results/plot_data/Tier3.csv")[, .(N = V1, daily_cases = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 3 - March 13"]
t4_1 <- fread("model/seir_results/plot_data/Tier4_1.csv")[, .(N = V1, daily_cases = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 4 - March 19"]
t4_2 <- fread("model/seir_results/plot_data/Tier4_2.csv")[, .(N = V1, daily_cases = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 4 - March 30"]
t4_3 <- fread("model/seir_results/plot_data/Tier4_3.csv")[, .(N = V1, daily_cases = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 4 - April 15"]

p <- rbindlist(list(
  t2, t3, t4_1, t4_2, t4_3
))[, .(date, scenario, incidence = daily_cases)]

# prepare data -----------
clean_prep <- function(x) {
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = end_date + 14, end_date = end_date + 14, scen = "No intervention")
  feb_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-02-19", end_date = end_date, scen = "Tier 2 - February 19")
  mar_13 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-13", end_date = end_date, scen = "Tier 3 - March 13")
  mar_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-19", end_date = end_date, scen = "Tier 4 - March 19")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", end_date = end_date, scen = "Tier 4 - March 30")
  apr_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", end_date = end_date, scen = "Tier 4 - April 15")
  
  total <- none %>%
    add_row(feb_19) %>%
    add_row(mar_13) %>% 
    add_row(mar_19) %>%
    add_row(mar_30) %>%
    add_row(apr_15)
  
  total.smoothed <- total %>% 
    filter(date <= end_date) %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.25),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total.smoothed.plot <- total.smoothed %>% 
    filter(scenario == "No intervention") %>%
    filter(date <= end_date) %>% 
    mutate(scenario = "Observed") %>% 
    add_row(total.smoothed %>%
              filter(scenario == "Tier 2 - February 19",
                     date >= "2021-02-10")) %>%
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 3 - March 13", 
                     date >= "2021-03-04")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - March 19", 
                     date >= "2021-03-10")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - March 30", 
                     date >= "2021-03-21")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - April 15", 
                     date >= "2021-04-08")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 2 - February 19", "Tier 3 - March 13", "Tier 4 - March 19", "Tier 4 - March 30", "Tier 4 - April 15"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)

tps <- as.data.table(total_smoothed_plot)
tps[scenario == "Tier 2 - February 19", scenario := "Tier 2"]
tps[scenario == "Tier 3 - March 13", scenario := "Tier 3"]
tps[scenario == "Tier 4 - March 19", scenario := "Tier 4"]
tps[, scenario := factor(scenario, levels = c("Observed", "Tier 2", "Tier 3", "Tier 4",
                                              "Tier 4 - March 30", "Tier 4 - April 15"))]

(cases_p <- tps[data.table::between(date, start_date, end_date)][, lt := "solid"][scenario == "Tier 4 - March 30", lt := "longdash"][scenario == "Tier 4 - April 15", lt := "dotted"][] %>%
  # filter(date >= "2021-02-01" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario, linetype = lt), size = 1) +
  scale_linetype_identity() +
  scale_color_manual(values = colors) +
  # vertical lines
  geom_vline(data = tps[, .SD[date == min(date)], by = scenario][, .(scenario, date)][!(scenario %in% c("Observed", "No intervention"))],
             aes(xintercept = date, color = scenario),
             linetype = "dashed") +
  
  # peak case count labels
  geom_label_repel(data = rbindlist(list(
    tps[, .SD[fitted == max(fitted)], by = scenario][, .(scenario, date, fitted)][!(scenario %in% c("Observed", "No intervention"))][, fitted_val := fitted][],
    data.table(scenario = "Observed", date = as.Date("2021-05-03"), fitted = 414280, fitted_val = tps[scenario == "Observed" & date == "2021-05-03", fitted])), fill = TRUE),
    aes(x = date, y = fitted_val, label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"), color = scenario, family = "Helvetica"),
    nudge_y = 50000,
    nudge_x = -10,
    size = 3.5,
    show.legend = FALSE,
    segment.size = 1) +
  
  # date labels
  geom_text(data = tps[, .SD[date == min(date)], by = scenario][, .(scenario, date, fitted)][, `:=` (
    text = c("Observed data", "February 19\nModerate PHI\n(non-lockdown)\nR(t)>1", "March 13\nStrengthened PHI\n(non-lockdown)\nR(t)>1.2", "March 19\nModerate\nlockdown\nR(t)>1.4", "March 30\nModerate\nlockdown", "April 15\nModerate\nlockdown"), 
    x    = as.Date(c("2021-05-19", "2021-02-09", "2021-03-03", "2021-03-11", "2021-03-23", "2021-04-09")), 
    y    = c(250000, rep(350000, 5)) 
  )][],
  aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Helvetica"),
  size = 3.5, hjust = c(1, 1, 1, 0, 0, 0), show.legend = FALSE) +
  
    # guides(color = guide_legend(nrow = 1)) + 
    labs(title    = "SEIR predicted number of daily COVID-19 cases under various interventions",
         subtitle = glue("{format_date(start_date)} to {format_date(end_date)}"),
         caption  = glue("**Notes:** Observations and prediction period until {format_date(end_date)}. ",
                         "Figures in boxes show peak number of cases for each intervention."),
         y        = "Daily cases",
         x        = "",
         color    = "Date of intervention") +
    
    # other stuff
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B", date_breaks = "1 month") +
  theme_classic() +
  theme(
    text            = element_text(family = "Helvetica"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  ))


# save output ----------
ggsave(filename = "figures/figure_s11/figure_s11.pdf",
       plot     = cases_p,
       height   = 5,
       width    = 15,
       units = "in", device = cairo_pdf)


