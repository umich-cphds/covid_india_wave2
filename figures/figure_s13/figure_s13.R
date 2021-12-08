# libraries ----------
#install.packages("librarian")
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel, janitor, glue,
            here, ggtext, patchwork, data.table)

start_date <- as.Date("2021-01-01")
end_date <- as.Date("2021-06-30")

colors <- c(Observed = "#0b0c0c", `Tier 2` = "#e6811c", `Tier 3` = "#e0101a", 
            `Tier 4` = "#9900d1", `Tier 4 - March 30` = "#9900d1", `Tier 4 - April 15` = "#9900d1")

d <- fread("model/cfr_schedule_14day_lag.txt")[, date := as.Date(date)][date >= as.Date(start_date)]

# helper functions --------------
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
              mutate(incidence = incidence * cfr) %>% 
              select(-cfr)) %>% 
    add_column(scenario = scen) 
  
}

format_date <- function(x) {
  
  glue("{trimws(format(x, '%B'))} {trimws(format(x, '%e'))}, {trimws(format(x, '%Y'))}")
  
}

# data -------------
obs <- fread("model/covid19india_national_counts_20211031.csv")[, date := as.Date(date)][date >= start_date][]

t2   <- fread("model/seir_results/plot_data/Tier2.csv")[, .(N = V1, daily_deaths = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 2 - February 19"]
t3   <- fread("model/seir_results/plot_data/Tier3.csv")[, .(N = V1, daily_deaths = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 3 - March 13"]
t4_1 <- fread("model/seir_results/plot_data/Tier4_1.csv")[, .(N = V1, daily_deaths = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 4 - March 19"]
t4_2 <- fread("model/seir_results/plot_data/Tier4_2.csv")[, .(N = V1, daily_deaths = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 4 - March 30"]
t4_3 <- fread("model/seir_results/plot_data/Tier4_3.csv")[, .(N = V1, daily_deaths = value, date = dates)][, date := as.Date(date)][N > 100][, scenario := "Tier 4 - April 15"]

p <- rbindlist(list(
  t2, t3, t4_1, t4_2, t4_3
))[, .(date, scenario, incidence = daily_deaths)]

# prepare data -----------
clean_prep <- function(x, var) {
  
  none   <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = end_date + 14, scen = "No intervention")
  feb_19 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-02-19", scen = "Tier 2 - February 19")
  mar_13 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-13", scen = "Tier 3 - March 13")
  mar_19 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-19", scen = "Tier 4 - March 19")
  mar_30 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-30", scen = "Tier 4 - March 30")
  apr_15 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-04-15", scen = "Tier 4 - April 15")
  
  
  total <- obs %>% 
    filter(date <= end_date) %>% 
    select(date, daily_deaths) %>% 
    rename(incidence = daily_deaths) %>% 
    add_column(scenario = "Observed") %>% 
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
  
  total_smoothed_plot <- total.smoothed %>% 
    filter(scenario == "Observed") %>% 
    filter(date <= end_date)  %>% 
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
                     date >= "2021-03-22")) %>%
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - April 15", 
                     date >= "2021-04-08")) %>%
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 2 - February 19", "Tier 3 - March 13", "Tier 4 - March 19", "Tier 4 - March 30", "Tier 4 - April 15")))%>%
    filter(date <= end_date) 
  
  return(total_smoothed_plot)
  
}

tsp_india <- clean_prep(x = p, var = cfr_mod_smooth)
tsp_mh    <- clean_prep(x = p, var = cfr_high_smooth)
tsp_kl    <- clean_prep(x = p, var = cfr_low_smooth)

make_tps <- function(x) {
  tmp <- as.data.table(x)
  tmp[scenario == "Tier 2 - February 19", scenario := "Tier 2"]
  tmp[scenario == "Tier 3 - March 13", scenario := "Tier 3"]
  tmp[scenario == "Tier 4 - March 19", scenario := "Tier 4"]
  tmp[, scenario := factor(scenario, levels = c("Observed", "Tier 2", "Tier 3", "Tier 4", "Tier 4 - March 30", "Tier 4 - April 15"))]
  
  return(tmp[])
}

tps_india <- make_tps(tsp_india)
tps_mh    <- make_tps(tsp_mh)
tps_kl    <- make_tps(tsp_kl)

death_plt <- function(dat, title,
                      tmp_nudge = 500, tmp_repel_y = c(2000, rep(4000, 5))) {
  
  # plt_def <- get_plt_def(mh = FALSE, kl = FALSE)
  
  deaths_p <- dat[data.table::between(date, start_date, end_date)][, lt := "solid"][scenario == "Tier 4 - March 30", lt := "longdash"][scenario == "Tier 4 - April 15", lt := "dotted"][] %>% 
    ggplot() + 
    geom_line(aes(x = date, y = fitted, color = scenario, linetype = lt), size = 1) +
    scale_linetype_identity() +
    scale_color_manual(values = colors) +
    labs(y        = "Daily cases",
         x        = "",
         color    = "Date of intervention") +
    
    # vertical lines
    geom_vline(data = dat[, .SD[date == min(date)], by = scenario][, .(scenario, date)][!(scenario %in% c("Observed", "No intervention"))],
               aes(xintercept = date, color = scenario),
               linetype = "dashed") +  
    
    # peak case count labels
    geom_label_repel(data = rbindlist(list(
      unique(dat[, .SD[fitted == max(fitted)], by = scenario][, .(scenario, date, fitted)][!(scenario %in% c("Observed", "No intervention"))][, fitted_val := fitted][]),
      data.table(scenario = "Observed", date = as.Date("2021-05-18"), fitted = 4529, fitted_val = dat[scenario == "Observed" & date == "2021-05-18", fitted])), fill = TRUE),
      aes(x = date, y = fitted_val, label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " deaths"), color = scenario, family = "Helvetica Neue"),
      nudge_y = tmp_nudge,
      nudge_x = -10,
      size = 4,
      show.legend = FALSE,
      segment.size = 1) + 
    
    # date labels
    geom_text(data = dat[, .SD[date == min(date)], by = scenario][, .(scenario, date, fitted)][, `:=` (
      text = c("Observed data", "February 19\nModerate PHI\n(non-lockdown)\nR(t)>1", "March 13\nStrengthened PHI\n(non-lockdown)\nR(t)>1.2", "March 19\nModerate\nlockdown\nR(t)>1.4", "March 30\nModerate\nlockdown", "April 15\nModerate\nlockdown"), 
      x    = as.Date(c("2021-04-22", "2021-02-09", "2021-03-03", "2021-03-11", "2021-03-23", "2021-04-09")), 
      y    = tmp_repel_y 
    )][],
    aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Helvetica Neue"),
    size = 4, hjust = c(0, 1, 1, 0, 0, 0), show.legend = FALSE) +
    
    # # date labels
    # geom_text(data = dat[, .SD[date == min(date)], by = scenario][, .(scenario, date, fitted)][, `:=` (
    #   text = c("Observed data", "February 19\nTier II", "March 13\nTier III", "March 19\nTier IV", "March 30\nTier IV", "April 15\nTier IV"), 
    #   x    = as.Date(c("2021-04-22", "2021-02-09", "2021-03-03", "2021-03-11", "2021-03-23", "2021-04-09")), 
    #   y    = tmp_repel_y)][],
    # aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Helvetica Neue"),
    # size = 4, hjust = c(0, 1, 1, 0, 0, 0), show.legend = FALSE) +
    
    # other stuff
    guides(color = guide_legend(nrow = 1)) + 
    labs(title    = title,
         y        = "Daily deaths",
         x        = "",
         color    = "Date of intervention") +
    scale_y_continuous(labels = scales::comma) +
    scale_x_date(date_labels = "%B", date_breaks = "1 month") +
    theme_classic() +
    theme(
      text            = element_text(family = "Helvetica Neue"),
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
    )
}

deaths_p    <- death_plt(dat = tps_india, title = "Moderate CFR")
deaths_p_mh <- death_plt(dat = tps_mh, title = "High CFR", tmp_repel_y = c(2000, rep(10000, 5)))
deaths_p_kl <- death_plt(dat = tps_kl, title = "Low CFR")

patched <- deaths_p_mh / deaths_p / deaths_p_kl

full_plt <- patched +
  plot_annotation(
    title    = "Predicted number of daily COVID-19 deaths under various interventions",
    subtitle = glue("{format_date(start_date)} to {format_date(end_date)}"),
    caption  = glue("**Notes:** Observations and prediction period until {format_date(end_date)}. ",
                    "Figures in boxes show peak number of deaths for each intervention.<br>SEIR predicted cases reported are multipled by the given CFR schedule.<br>",
                    "**Abbrev:** CFR, case-fatality rate"),
    tag_levels = c("A")
  ) &
  theme(
    text              = element_text(family = "Helvetica Neue"),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 12, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Helvetica Neue", face = "bold")
  )

full_plt


# save output ----------
ggsave(filename = "figures/figure_s13/figure_s13.pdf",
       plot     = full_plt,
       height   = 12,
       width    = 15,
       units = "in", device = cairo_pdf)
