# libraries ----------
rm(list = ls())
librarian::shelf(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
            ggtext, patchwork, data.table)

source("esir_ally.R")

# specs -------------
start_date <- as.Date("2021-01-01")
end_date   <- as.Date("2021-07-31")

tmp_outname  <- "esir_unlock_case_plot.pdf"
tmp_title    <- "Effect of different interventions at different times"

path_to_lockdown_output_files <- ""

# load data ----------
obs <- fread("covid19india_national_counts_20211031.csv")[date >= start_date]

scen_dates <- c("2021-03-19", "2021-03-30", "2021-04-15")

for (i in seq_along(scen_dates)) {
  tmp_filename <- glue("{scen_dates[i]}_t4_r2_data.txt")
  if (i == 1) {
    p <- fread(glue("{path_to_lockdown_output_files}{tmp_filename}"))[, `:=` (start_date = scen_dates[i], tier = "Tier 4")]
  } else {
    if (!file.exists(paste0(path_to_lockdown_output_files, tmp_filename))) {
      next
    }
    p <- rbindlist(list(
      p,
      fread(glue("{path_to_lockdown_output_files}{tmp_filename}"))[, `:=` (start_date = scen_dates[i], tier = "Tier 4")]
    ))
  }
}

p <- p[, scenario := paste0(tier, " - ", scenario)][]

# prepare data ----------
clean_prep <- function(x) {
  
  #none   <- obs %>% clean_scenario(p = x, stop_obs = end_date + 14, end_date = end_date + 14, scen = "No intervention")
  none <- obs %>% select(c(date, daily_cases)) %>% rename(incidence = daily_cases) %>% 
    add_column(scenario = "No intervention") %>% filter(date <= end_date)
  mar_19 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-19", end_date = end_date, scen = "Tier 4 - 2021-03-19_t4_r2") %>% 
    mutate(scenario = "Tier 4 - March 19")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", end_date = end_date, scen = "Tier 4 - 2021-03-30_t4_r2") %>% 
    mutate(scenario = 'Tier 4 - March 30')
  apr_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", end_date = end_date, scen = "Tier 4 - 2021-04-15_t4_r2") %>% 
  mutate(scenario = 'Tier 4 - April 15')
  
  total <- none %>%
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
              filter(scenario == "Tier 4 - March 19", 
                     date >= "2021-03-10")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - March 30", 
                     date >= "2021-03-21")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Tier 4 - April 15", 
                     date >= "2021-04-06")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "Tier 4 - March 19", "Tier 4 - March 30", "Tier 4 - April 15"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

tsp <- as.data.table(clean_prep(x = p))[scenario == "Tier 4 - March 19", scenario := "Tier 4"][
  , `:=` (
    scenario = factor(scenario, levels = c("Observed", "Tier 4", "Tier 4 - March 30", "Tier 4 - April 15")),
    date     = as.Date(date)
  )][]

# plot ------------
colors <- c(
  "Observed"          = "#0b0c0c",
  "Tier 4"            = pal_lancet()(3)[1],
  "Tier 4 - March 30" = pal_lancet()(3)[2],
  "Tier 4 - April 15" = pal_lancet()(3)[3]
)

unlock_plot <- tsp[data.table::between(date, start_date, end_date)] %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_color_manual(values = colors) +
  
  # vertical lines
  geom_vline(data = tsp[, .SD[date == min(date)], by = scenario][, .(scenario, date)][!(scenario %in% c("Observed", "No intervention"))],
             aes(xintercept = date, color = scenario),
             linetype = "solid") +
  
  geom_vline(
    data = data.table(
      scenario = c("Tier 4", "Tier 4 - March 30", "Tier 4 - April 15"),
      date     = as.Date(c("2021-03-19", "2021-03-30", "2021-04-15")) + 54
    ),
    aes(xintercept = date, color = scenario), linetype = "dashed"
  ) +
  
  # peak case count labels
  geom_label_repel(data = rbindlist(list(
    tsp[, .SD[fitted == max(fitted)], by = scenario][, .(scenario, date, fitted)][!(scenario %in% c("Observed", "No intervention"))][, fitted_val := fitted][],
    data.table(scenario = "Observed", date = as.Date("2021-05-03"), fitted = 414280, fitted_val = tsp[scenario == "Observed" & date == "2021-05-03", fitted])), fill = TRUE),
    aes(x = date, y = fitted_val, label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"), color = scenario, family = "Helvetica Neue"),
    nudge_y = 100000,
    nudge_x = -10,
    size = 4,
    show.legend = FALSE,
    segment.size = 1) +
  
  # date labels
  geom_text(data = tsp[, .SD[date == min(date)], by = scenario][, .(scenario, date, fitted)][, `:=` (
    text = c("Observed data", "March 19\nModerate\nlockdown", "March 30\nModerate\nlockdown", "April 15\nModerate\nlockdown"), 
    x    = as.Date(c("2021-04-10", "2021-03-09", "2021-03-22", "2021-04-07")), 
    y    = c(125000, rep(350000, 3)) 
  )][],
  aes(x = x, y = y, label = text, color = scenario, vjust = 1, family = "Helvetica Neue"),
  size = 4, hjust = c(0, 1, 0, 0), show.legend = FALSE) +
  guides(color = guide_legend(nrow = 1)) + 
  labs(
    title    = "Effect of unlocking the lockdown",
    subtitle = glue("{format(start_date, '%B %e, %Y')} to {format(end_date, '%B %e, %Y')}"),
    caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}. ",
                    "Solid vertical lines indicate lockdown start date and dashed vertical lines indicate lockdown end date.<br>",
                    "Figures in boxes show peak number of cases for each intervention."),
    y        = "Daily cases",
    x        = "",
    color    = "Date of intervention"
  ) +
  
  # other stuff
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

# save output ----------
ggsave(filename = glue("figure s8.pdf"),
       plot     = unlock_plot,
       height   = 5,
       width    = 15,
       units = "in", device = cairo_pdf)
