# libraries ----------
pacman::p_load(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
               ggtext, patchwork)
f <- list.files(here("src"))
for (i in seq_along(f)) {source(here("src", f[i]))}

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

scenarios <- c("2021-03-15", "2021-03-30")

for (i in seq_along(scenarios)) {
  tmp_filename    <- glue("{scenarios[i]}_waning_data.txt")
  tmp_mh_filename <- glue("{scenarios[i]}_waning_mh_data.txt")
  if (i == 1) {
    p <- read_tsv(here("data", "early_lockdown",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
    
    p_mh <- read_tsv(here("data", "early_lockdown",
                       tmp_mh_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
    
  } else {
    p <- bind_rows(p,
                   read_tsv(here("data", "early_lockdown",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
    
    p_mh <- bind_rows(p_mh,
                   read_tsv(here("data", "early_lockdown",
                                 tmp_mh_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

p_e <- read_tsv(here("data", "early_intervention", "2021-01-02_20pct_waning_data.txt")) %>%
  mutate(scenario = "MH Pre-lock")

tmp_outname  <- "fig01_wane_case_plot.pdf"
tmp_title    <- "Strong lockdown effect"
tmp_mh_title <- "Moderate lockdown effect"

p <- p %>%
  mutate(
    scenario = paste(
      trimws(format(as.Date(scenario), '%B')),
      trimws(format(as.Date(scenario), '%e'))
    )
  )

p_mh <- p_mh %>%
  mutate(
    scenario = paste(
      trimws(format(as.Date(scenario), '%B')),
      trimws(format(as.Date(scenario), '%e'))
    )
  )

# prepare data ----------
clean_prep <- function(x) {
  
  none   <- obs %>% mutate(scenario = "No intervention") %>% select(scenario, date, incidence = daily_cases)
  mar_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-15", end_date = end_date, scen = "March 15")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", end_date = end_date, scen = "March 30")
  
  total <- none %>%
    add_row(mar_15) %>% 
    add_row(mar_30)
  
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
              filter(scenario == "March 15", 
                     date >= "2021-03-06")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "March 30", 
                     date >= "2021-03-21")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "March 15", "March 30"))) %>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)
total_mh_smoothed_plot <- clean_prep(x = p_mh)

obs <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
                col_types = cols(), show_col_types = FALSE) %>%
  clean_names() %>%
  rename(
    daily_cases  = daily_confirmed,
    daily_deaths = daily_deceased,
    total_cases  = total_confirmed,
    total_deaths = total_deceased
  ) %>%
  select(-date) %>%
  rename(date = date_ymd) %>%
  filter(date >= "2020-12-01")

total_smoothed_plot_e <- obs %>%
  dplyr::filter(date <= end_date) %>%
  mutate(scenario = "Observed") %>%
  select(date, incidence = daily_cases, scenario) %>%
  bind_rows(obs %>%
              filter(date <= "2021-01-01") %>% 
              select(date, daily_cases) %>% 
              rename(incidence = daily_cases) %>% 
              add_row(p_e %>%  
                        filter(scenario == "MH Pre-lock") %>% 
                        select(date, incidence) %>% 
                        drop_na()) %>% 
              add_column(scenario = "MH Pre-lock") %>%
              filter(date <= end_date)) %>%
  # bind_rows(obs %>%
  #             filter(date <= "2021-01-01") %>% 
  #             select(date, daily_cases) %>% 
  #             rename(incidence = daily_cases) %>% 
  #             add_row(p_mh %>%  
  #                       filter(scenario == "MH Pre-lock (+20%)") %>% 
  #                       select(date, incidence) %>% 
  #                       drop_na()) %>% 
  #             add_column(scenario = "MH Pre-lock (+20%)") %>%
  #             filter(date <= end_date)) %>%
  nest(data = c(date, incidence)) %>%
  mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.25),
         fitted = purrr::map(m, `[[`, "fitted")) %>%
  select(-m) %>%
  unnest(cols = c(data, fitted)) %>%
  mutate(scenario = factor(scenario, levels = c("Observed", "MH Pre-lock"))) %>%
  filter(date >= "2020-12-01") %>%
  filter(!(scenario %in% c("MH Pre-lock") & date <= "2021-01-01"))

total_smoothed_plot <- bind_rows(
  bind_rows(
    total_smoothed_plot_e %>% filter(scenario == "Observed") %>% filter(date <= "2021-03-27"),
    total_smoothed_plot %>% filter(scenario == "Observed") %>% filter(date >= "2021-03-28")),
  total_smoothed_plot %>% filter(scenario != "Observed"),
  total_smoothed_plot_e %>% filter(scenario != "Observed")
)


# plot -----------
strong_cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Strong lockdown"]])


## strong lockdown plot -----------
cases_p <- total_smoothed_plot %>%
  filter(date >= "2020-12-01" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_color_manual(values = strong_cols) +
  # scale_color_lancet() + 
  geom_vline(data = total_smoothed_plot_e %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_vline(data = total_smoothed_plot %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_label_repel(data = total_smoothed_plot %>% 
                     group_by(scenario) %>% 
                     filter(fitted == max(fitted)) %>% 
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     filter(!(scenario %in% c("Observed", "No intervention"))) %>%
                     add_row(scenario = "Observed", date = as.Date("2021-05-03"), fitted = 414280) %>%
                     mutate(fitted_val = case_when(scenario == "Observed" ~ 377636, T ~ fitted)), 
                    aes(x = date, 
                        y = fitted_val, 
                        label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
                        color = scenario,
                        family = "Lato"), 
                   nudge_y = 100000, 
                   nudge_x = -10, 
                   size = 4, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = total_smoothed_plot %>% 
                    filter(scenario != "MH Pre-lock") %>%
                     group_by(scenario) %>% 
                     filter(date == min(date)) %>% 
                    distinct() %>%
                    slice(1) %>%
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     mutate(text = c("Observed data", "March 15\nlockdown", "March 30\nlockdown"), 
                            x = as.Date(c("2021-05-05", "2021-02-26", "2021-04-01")), 
                            y = c(125000, rep(500000, 2))), 
                   aes(x = x, 
                       y = y, 
                       label = text,
                       color = scenario,
                       family = "Lato"), 
                   nudge_x = -1,
                   size = 4,
                  hjust = 0,
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = total_smoothed_plot_e %>%
                    filter(scenario == "MH Pre-lock") %>%
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    distinct() %>%
                    slice(1) %>%
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Early non-lockdown\nintervention"), 
                           x = as.Date(c("2021-01-01")), 
                           y = c(500000)), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario,
                      family = "Lato"), 
                  nudge_x = -1,
                  size = 4,
                  hjust = 1,
                  show.legend  = FALSE, 
                  segment.size = 1) + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = tmp_title,
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
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

## moderate lockdown plot ----------
total_mh_smoothed_plot <- bind_rows(
  bind_rows(
    total_smoothed_plot_e %>% filter(scenario == "Observed") %>% filter(date <= "2021-03-27"),
    total_mh_smoothed_plot %>% filter(scenario == "Observed") %>% filter(date >= "2021-03-28")),
  total_mh_smoothed_plot %>% filter(scenario != "Observed"),
  total_smoothed_plot_e %>% filter(scenario != "Observed")
)


mod_cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Moderate lockdown"]])

cases_p_mh <- total_mh_smoothed_plot %>%
  filter(date >= "2020-12-01" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_color_manual(values = mod_cols) +
  # scale_color_lancet() + 
  geom_vline(data = total_smoothed_plot_e %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_vline(data = total_mh_smoothed_plot %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_label_repel(data = total_mh_smoothed_plot %>% 
                     group_by(scenario) %>% 
                     filter(fitted == max(fitted)) %>% 
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     filter(!(scenario %in% c("Observed", "No intervention"))) %>%
                     add_row(scenario = "Observed", date = as.Date("2021-05-03"), fitted = 414280) %>%
                     mutate(fitted_val = case_when(scenario == "Observed" ~ 377636, T ~ fitted)), 
                   aes(x = date, 
                       y = fitted_val, 
                       label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
                       color = scenario,
                       family = "Lato"), 
                   nudge_y = 100000, 
                   nudge_x = -10, 
                   size = 4, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = total_mh_smoothed_plot %>% 
                    filter(scenario != "MH Pre-lock") %>%
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    distinct() %>%
                    slice(1) %>%
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Observed data", "March 15\nlockdown", "March 30\nlockdown"), 
                           x = as.Date(c("2021-05-05", "2021-02-26", "2021-04-01")), 
                           y = c(125000, rep(500000, 2))), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario,
                      family = "Lato"), 
                  nudge_x = -1,
                  size = 4,
                  hjust = 0,
                  show.legend  = FALSE, 
                  segment.size = 1) + 
  geom_text_repel(data = total_smoothed_plot_e %>%
                    filter(scenario == "MH Pre-lock") %>%
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    distinct() %>%
                    slice(1) %>%
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Early non-lockdown\nintervention"), 
                           x = as.Date(c("2021-01-01")), 
                           y = c(500000)), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario,
                      family = "Lato"), 
                  nudge_x = -1,
                  size = 4,
                  hjust = 1,
                  show.legend  = FALSE, 
                  segment.size = 1) + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = tmp_mh_title,
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
    legend.title    = element_text(size = 11, face = "bold"),
    legend.text     = element_text(size = 11, face = "bold"),
    legend.position = "none",
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

# combine plots -----------
patched <- cases_p / cases_p_mh 

full_plt <- patched +
  plot_annotation(
    title    = "Predicted number of daily COVID-19 cases under various interventions with waning immunity",
    subtitle = glue("December 1, 2020 to {format(end_date, '%B %e, %Y')}"),
    caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}. ",
                    "Figures in boxes show peak number of cases for each intervention.<br>",
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
       plot     = full_plt,
       height   = 7,
       width    = 12,
       units = "in", device = cairo_pdf)
