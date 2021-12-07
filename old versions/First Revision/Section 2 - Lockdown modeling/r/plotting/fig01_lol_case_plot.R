# libraries ----------
pacman::p_load(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here,
               ggtext, patchwork)
source(here("src", "extract_cfr.R"))

prod <- TRUE
end_date <- "2021-06-30"

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

scenarios  <- c("4wk", "6wk", "8wk")
scenario_names <- c("4 weeks", "6 weeks", "8 weeks")
tmp_folder <- ifelse(prod == TRUE, "data", "test")

for (i in seq_along(scenarios)) {
  
  tmp_filename    <- glue("2021-04-15_smooth1_{scenarios[i]}_data.txt")
  
  if (i == 1) {
    p <- read_tsv(here(tmp_folder, "lol", tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenario_names[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here(tmp_folder, "lol", tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenario_names[i]))
  }
  
}

tmp_outname  <- "fig01_lol_case_plot.pdf"
tmp_title    <- "Effect of length of lockdown"

# prepare data ----------
clean_prep <- function(x) {
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = end_date, scen = "No intervention", end_date = end_date)
  wk4    <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", scen = "4 weeks", end_date = end_date)
  wk6    <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", scen = "6 weeks", end_date = end_date)
  wk8    <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", scen = "8 weeks", end_date = end_date)
  
  total <- none %>%
    add_row(wk4) %>% 
    add_row(wk6) %>% 
    add_row(wk8)
  
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
              filter(scenario == "4 weeks", 
                     date >= "2021-04-09")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "6 weeks", 
                     date >= "2021-04-09")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "8 weeks", 
                     date >= "2021-04-09")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "4 weeks", "6 weeks", 
                                                  "8 weeks")))%>%
    filter(date <= end_date) 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)

# plot -----------
cases_p <- total_smoothed_plot %>% 
  filter(date >= "2021-02-15" & date <= end_date) %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_color_lancet() + 
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
                     filter(!(scenario %in% c("Observed", "No intervention"))), 
                    aes(x = date, 
                        y = fitted, 
                        label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " cases"),
                        color = scenario,
                        family = "Lato"), 
                   nudge_y = 100000, 
                   nudge_x = -10, 
                   size = 4, 
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = tmp_title,
       y        = "Daily cases",
       x        = "",
       subtitle = glue::glue("February 15, 2021 to {format(as.Date(end_date), '%B %e, %Y')}"),
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
    legend.text     = element_text(size = 10),
    legend.position = "top",
    legend.title = element_blank(),
    plot.title      = element_text(size = 14, face = "bold"),
    plot.subtitle   = element_text(size = 11, hjust = 0, color = "gray40"),
    plot.caption    = element_markdown(size = 10, hjust = 0)
  )

# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = cases_p,
       height   = 5,
       width    = 7,
       units = "in", device = cairo_pdf)
