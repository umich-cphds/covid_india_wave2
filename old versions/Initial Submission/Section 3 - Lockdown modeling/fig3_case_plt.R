# libraries ----------
librarian::shelf(
  tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here, ggtext, patchwork
  )
source(here("Section 3 - Lockdown modeling", "src", "extract_cfr.R"))

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

scenarios <- c("2021-03-01", "2021-03-15", "2021-03-30",
               "2021-04-15", "2021-04-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  tmp_filename    <- glue("{scenarios[i]}_smooth1_data.txt")
  tmp_mh_filename <- glue("{scenarios[i]}_mh_smooth1_data.txt")
  
  if (i == 1) {
    p <- read_tsv(here("Section 3 - Lockdown modeling", "data",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
    
    p_mh <- read_tsv(here("Section 3 - Lockdown modeling", "data",
                       tmp_mh_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
    
  } else {
    p <- bind_rows(p,
                   read_tsv(here("Section 3 - Lockdown modeling", "data",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
    
    p_mh <- bind_rows(p_mh,
                   read_tsv(here("Section 3 - Lockdown modeling", "data",
                                 tmp_mh_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

tmp_outname  <- "fig3_case_plot.pdf"
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
  
  none   <- obs %>% clean_scenario(p = x, stop_obs = "2021-05-15", scen = "No intervention")
  # mar_01 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-01", scen = "March 1")
  mar_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-15", scen = "March 15")
  mar_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-03-30", scen = "March 30")
  apr_15 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-15", scen = "April 15")
  # apr_30 <- obs %>% clean_scenario(p = x, stop_obs = "2021-04-30", scen = "April 30")
  
  total <- none %>%
    add_row(mar_15) %>% 
    add_row(mar_30) %>% 
    add_row(apr_15)
    # add_row(apr_30) 
  
  total.smoothed <- total %>% 
    filter(!(scenario %in% c("March 1", "April 30"))) %>% 
    filter(date <= "2021-05-31") %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total.smoothed.plot <- total.smoothed %>% 
    filter(scenario == "No intervention") %>%
    filter(date <= "2021-05-15") %>% 
    mutate(scenario = "Observed") %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "March 15", 
                     date >= "2021-03-08")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "March 30", 
                     date >= "2021-03-23")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "April 15", 
                     date >= "2021-04-06")) %>% 
    # add_row(total.smoothed %>% 
    #           filter(scenario == "April 30", 
    #                  date >= "2021-04-21")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "March 15", "March 30", 
                                                  "April 15")))%>%
    filter(date <= "2021-05-15") 
  
  return(total.smoothed.plot)
  
}

total_smoothed_plot    <- clean_prep(x = p)
total_mh_smoothed_plot <- clean_prep(x = p_mh)

# plot -----------
cases_p <- total_smoothed_plot %>% 
  filter(date >= "2021-02-15" & date <= "2021-05-15") %>%
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
  geom_text_repel(data = total_smoothed_plot %>% 
                     group_by(scenario) %>% 
                     filter(date == min(date)) %>% 
                    distinct() %>%
                    slice(1) %>%
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     mutate(text = c("Observed data", "March 15\nlockdown", "March 30\nlockdown", 
                                     "April 15\nlockdown"), 
                            x = as.Date(c("2021-03-01", "2021-03-08", "2021-03-23", "2021-04-06")), 
                            y = c(35000, rep(500000, 3))), 
                   aes(x = x, 
                       y = y, 
                       label = text,
                       color = scenario,
                       family = "Lato"), 
                   nudge_x = -1,
                   size = 4, 
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

cases_p_mh <- total_mh_smoothed_plot %>% 
  filter(date >= "2021-02-15" & date <= "2021-05-15") %>%
  ggplot(aes(x = date, y = fitted)) + 
  geom_line(aes(color = scenario), size = 1) +
  scale_colour_lancet() + 
  xlab("Date") + 
  ylab("Daily cases") + 
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
  geom_text_repel(data = total_mh_smoothed_plot %>% 
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Observed data", "March 15\nlockdown", "March 30\nlockdown", 
                                    "April 15\nlockdown"), 
                           x = as.Date(c("2021-03-01", "2021-03-08", "2021-03-23", "2021-04-06")), 
                           y = c(35000, rep(500000, 3))), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario,
                      family = "Lato"), 
                  nudge_x = -1,
                  size = 4, 
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

patched <- cases_p / cases_p_mh 

full_plt <- patched +
  plot_annotation(
    title    = "Predicted number of daily COVID-19 cases under lockdown",
    subtitle = "February 15, 2021 to May 15, 2021",
    caption  = glue("**Notes:** Observations and prediction period until May 15, 2021. ",
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
ggsave(filename = here("Section 3 - Lockdown modeling", "fig", tmp_outname),
       plot     = full_plt,
       height   = 8,
       width    = 10,
       units = "in", device = cairo_pdf)
