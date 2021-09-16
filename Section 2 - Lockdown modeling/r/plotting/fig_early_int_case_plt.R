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
  filter(date >= "2020-11-01")

scenarios <- c("2021-03-15", "2021-03-30", "no_intervention")

p    <- read_tsv(here("data", "early_intervention", "2021-01-02_smooth1_data.txt")) %>%
  mutate(scenario = "MH Pre-lock")
p_mh <- read_tsv(here("data", "early_intervention", "2021-01-02_20pct_smooth1_data.txt")) %>%
  mutate(scenario = "MH Pre-lock (+20%)")


# for (i in seq_along(scenarios)) {
#   tmp_filename    <- glue("{scenarios[i]}_smooth1_data.txt")
#   tmp_mh_filename <- glue("{scenarios[i]}_smooth1_mh_data.txt")
#   if (i == 1) {
#     p <- read_tsv(here("data", "early_lockdown",
#                        tmp_filename),
#                   col_types = cols()) %>%
#       mutate(scenario = scenarios[i])
#     
#     p_mh <- read_tsv(here("data", "early_lockdown",
#                        tmp_mh_filename),
#                   col_types = cols()) %>%
#       mutate(scenario = scenarios[i])
#     
#   } else {
#     p <- bind_rows(p,
#                    read_tsv(here("data", "early_lockdown",
#                                  tmp_filename),
#                             col_types = cols()) %>%
#                      mutate(scenario = scenarios[i]))
#     
#     p_mh <- bind_rows(p_mh,
#                    read_tsv(here("data", "early_lockdown",
#                                  tmp_mh_filename),
#                             col_types = cols()) %>%
#                      mutate(scenario = scenarios[i]))
#   }
#   
# }

tmp_outname  <- "fig_early_intervention_case_plot.pdf"
tmp_title    <- "MH Pre-lock"
tmp_mh_title <- "MH Pre-lock (+20%)"

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

total_smoothed_plot <- obs %>%
  dplyr::filter(date <= end_date) %>%
  mutate(scenario = "Observed") %>%
  select(date, incidence = daily_cases, scenario) %>%
  bind_rows(obs %>%
              filter(date <= "2021-01-01") %>% 
              select(date, daily_cases) %>% 
              rename(incidence = daily_cases) %>% 
              add_row(p %>%  
                        filter(scenario == "MH Pre-lock") %>% 
                        select(date, incidence) %>% 
                        drop_na()) %>% 
              add_column(scenario = "MH Pre-lock") %>%
              filter(date <= end_date)) %>%
  bind_rows(obs %>%
              filter(date <= "2021-01-01") %>% 
              select(date, daily_cases) %>% 
              rename(incidence = daily_cases) %>% 
              add_row(p_mh %>%  
                        filter(scenario == "MH Pre-lock (+20%)") %>% 
                        select(date, incidence) %>% 
                        drop_na()) %>% 
              add_column(scenario = "MH Pre-lock (+20%)") %>%
              filter(date <= end_date)) %>%
  nest(data = c(date, incidence)) %>%
  mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.25),
         fitted = purrr::map(m, `[[`, "fitted")) %>%
  select(-m) %>%
  unnest(cols = c(data, fitted)) %>%
  mutate(scenario = factor(scenario, levels = c("Observed", "MH Pre-lock", "MH Pre-lock (+20%)"))) %>%
  filter(date >= "2020-11-01") %>%
  filter(!(scenario %in% c("MH Pre-lock", "MH Pre-lock (+20%)") & date <= "2021-01-01"))

# plot -----------
cols <- pal_lancet()(3)
names(cols) <- c("Observed", "MH Pre-lock", "MH Pre-lock (+20%)")

full_plt <- total_smoothed_plot %>%
  filter(scenario != "MH Pre-lock (+20%)") %>%
  filter(date >= "2020-11-01" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) +
  geom_line(aes(group = scenario, color = scenario), size = 1) +
  scale_color_manual(values = cols) +
  
  # geom_label_repel(data = total_smoothed_plot %>% 
  #                    group_by(scenario) %>% 
  #                    filter(fitted == max(fitted)) %>% 
  #                    dplyr::ungroup() %>% 
  #                    select(scenario, date, fitted) %>% 
  #                    filter(!(scenario %in% c("Observed", "No intervention"))), 
  #                  aes(x = date, 
  #                      y = fitted, 
  #                      label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
  #                      color = scenario,
  #                      family = "Lato"), 
  #                  nudge_y = 100000, 
  #                  nudge_x = -10, 
  #                  size = 4, 
  #                  show.legend  = FALSE, 
  #                  segment.size = 1) + 
  
  # scale_color_lancet() +
  labs(    title    = "Predicted number of daily COVID-19 cases under early intervention",
           subtitle = glue("November 1, 2020 to {format(end_date, '%B %e, %Y')}"),
           caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}. ",
                           "Figures in boxes show peak number of cases for each intervention.<br>",
                           "**\uA9 COV-IND-19 Study Group**"),
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
    legend.text     = element_text(size = 11),
    legend.position = "top",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )


# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = full_plt,
       height   = 5,
       width    = 7,
       units = "in", device = cairo_pdf)
