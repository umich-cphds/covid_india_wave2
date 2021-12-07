# libraries ----------
pacman::p_load(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
               ggtext, patchwork)
source(here("src", "extract_cfr.R"))

end_date <- as.Date("2021-06-30")

# load data ----------
obs <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
              col_types = cols()) %>%
  clean_names() %>%
  rename(
    daily_cases  = daily_confirmed,
    daily_deaths = daily_deceased,
    total_cases  = total_confirmed,
    total_deaths = total_deceased
  ) %>%
  select(-date) %>%
  rename(date = date_ymd) %>%
  filter(date >= "2021-02-15")

scenarios <- c("4wk", "6wk", "8wk")
scenarios_ind <- c("6wk", "9wk", "12wk")

for (i in seq_along(scenarios)) {
  tmp_filename     <- glue("2021-04-15_smooth1_{scenarios[i]}_data.txt")
  tmp_filename_ind <- glue("2021-04-15_smooth1_{scenarios_ind[i]}_ind_data.txt")
  if (i == 1) {
    p <- read_tsv(here("data", "lol",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
    p_ind <- read_tsv(here("data", "lol",
                       tmp_filename_ind),
                  col_types = cols()) %>%
      mutate(scenario = scenarios_ind[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("data", "lol",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
    p_ind <- bind_rows(p_ind,
                       read_tsv(here("data", "lol",
                                              tmp_filename_ind),
                                         col_types = cols()) %>%
                         mutate(scenario = scenarios_ind[i]))
  }
  
}

p <- p %>%
  mutate(
    scenario = case_when(
      scenario == "4wk" ~ "4 weeks",
      scenario == "6wk" ~ "6 weeks",
      scenario == "8wk" ~ "8 weeks"
    )
  )

p_ind <- p_ind %>%
  mutate(
    scenario = case_when(
      scenario == "6wk" ~ "6 weeks",
      scenario == "9wk" ~ "9 weeks",
      scenario == "12wk" ~ "12 weeks"
    )
  )

tmp_outname  <- "fig_lol_case_plot.pdf"
# tmp_title    <- "Strong lockdown effect"
# tmp_mh_title <- "Moderate lockdown effect"

# p <- p %>%
#   mutate(
#     scenario = paste(
#       trimws(format(as.Date(scenario), '%B')),
#       trimws(format(as.Date(scenario), '%e'))
#     )
#   )
# 
# p_mh <- p_mh %>%
#   mutate(
#     scenario = paste(
#       trimws(format(as.Date(scenario), '%B')),
#       trimws(format(as.Date(scenario), '%e'))
#     )
#   )

# prepare data ----------
clean_prep <- function(x, ind = FALSE) {
  
  if (ind == FALSE) {
    scens <- c("4 weeks", "6 weeks", "8 weeks")
  } else if (ind == TRUE) {
    scens <- c("6 weeks", "9 weeks", "12 weeks")
  } else {
    stop()
  }
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = end_date, end_date = end_date, scen = "No intervention")
  wk_4 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-14", end_date = end_date, scen = scens[1])
  wk_6 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-14", end_date = end_date, scen = scens[2])
  wk_8 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-14", end_date = end_date, scen = scens[3])
  
  total <- none %>%
    add_row(wk_4) %>% 
    add_row(wk_6) %>%
    add_row(wk_8)
  
  total.smoothed <- total %>% 
    filter(date <= end_date) %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total.smoothed.plot <- total.smoothed %>% 
    filter(scenario == "No intervention") %>%
    filter(date <= end_date) %>% 
    mutate(scenario = "Observed") %>% 
    add_row(total.smoothed %>% 
              filter(scenario == scens[1], 
                     date >= "2021-04-03")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == scens[2], 
                     date >= "2021-04-03")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == scens[3], 
                     date >= "2021-04-03")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", scens[1], scens[2], scens[3]))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)
total_smoothed_plot_ind <- clean_prep(x = p_ind, ind = TRUE)
# total_mh_smoothed_plot <- clean_prep(x = p_mh)

# plot -----------
cases_p <- total_smoothed_plot %>% 
  filter(date >= "2021-02-15" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  xlab("Date") + 
  ylab("Daily cases") + 
  geom_vline(data = total_smoothed_plot %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(    title    = "Moderate lockdown",
       y        = "Daily cases",
       x        = "",
       color    = "Date of intervention") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.title    = element_blank(),
    # legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 10),
    legend.position = "top",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

cases_p_ind <- total_smoothed_plot_ind %>% 
  filter(date >= "2021-02-15" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  xlab("Date") + 
  ylab("Daily cases") + 
  geom_vline(data = total_smoothed_plot_ind %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(    title    = "Strong lockdown",
           y        = "Daily cases",
           x        = "",
           color    = "Date of intervention") +
  scale_y_continuous(labels = scales::comma) +
  scale_x_date(date_labels = "%B") +
  theme_classic() +
  theme(
    text            = element_text(family = "Lato"),
    axis.text.x     = element_text(size = 11, vjust = 0.5),
    axis.text.y     = element_text(size = 11),
    axis.title.x    = element_text(size = 11, face = "bold"),
    axis.title.y    = element_text(size = 11, face = "bold"),
    legend.title    = element_blank(),
    # legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 10),
    legend.position = "top",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

patched <- cases_p + cases_p_ind

full_plot <- patched +
  plot_annotation(
    title    = "Predicted number of daily COVID-19 cases under lockdown",
    subtitle = glue("February 15, 2021 to {format(end_date, '%B %e, %Y')}"),
    caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}.<br>",
                    "**\uA9 COV-IND-19 Study Group**"),
    tag_levels = c("A")
  ) &
  theme(
    text              = element_text(family = "Lato"),
    plot.title        = element_text(size = 18, face = "bold"),
    plot.subtitle     = element_text(size = 14, hjust = 0, color = "gray40"),
    plot.caption      = element_markdown(size = 12, hjust = 0),
    plot.tag.position = c(0, 1),
    plot.tag          = element_text(size = 18, hjust = 0, vjust = 1, family = "Lato", face = "bold")
  )

# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = full_plot,
       height   = 5,
       width    = 12,
       units = "in", device = cairo_pdf)
