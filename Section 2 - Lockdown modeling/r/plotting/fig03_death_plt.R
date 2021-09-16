# libraries ----------
pacman::p_load(tidyverse, lubridate, ggsci, ggrepel, janitor, glue, here, ggtext, patchwork)
f <- list.files(here("src"))
for (i in seq_along(f)) {source(here("src", f[i]))}

end_date <- as.Date("2021-06-30")

# # use maharashtra pi schedule? use kerala cfr schedule? ----------
mh <- FALSE
if (mh == TRUE) {
  tmp_outname <- "fig02_death_plot.pdf"
  plt_title   <- "Predicted number of daily COVID-19 deaths under moderate lockdown effect"
  cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Moderate lockdown"]])
  names(cols)[names(cols) == "MH Pre-lock"] <- "Early intervention"
} else {
  tmp_outname <- "fig03_death_plot.pdf"
  plt_title   <- "Predicted number of daily COVID-19 deaths under strong lockdown effect"
  cols <- c(colores[["Observed"]], colores[["MH Pre-lock"]], colores[["Strong lockdown"]])
  names(cols)[names(cols) == "MH Pre-lock"] <- "Early intervention"
}


# load data -----------
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
  filter(date >= "2021-02-15") %>% 
  mutate(cfr = daily_deaths/daily_cases)

scenarios <- c("2021-03-15", "2021-03-30", "no_intervention")

for (i in seq_along(scenarios)) {
  
  if (mh == TRUE) {
    tmp_filename <- glue("{scenarios[i]}_smooth1_mh_data.txt")
  } else {
    tmp_filename <- glue("{scenarios[i]}_smooth1_data.txt")
  }
  
  if (i == 1) {
    p <- read_tsv(here("data", "early_lockdown",
                       tmp_filename),
                  col_types = cols()) %>%
      mutate(scenario = scenarios[i])
  } else {
    p <- bind_rows(p,
                   read_tsv(here("data", "early_lockdown",
                                 tmp_filename),
                            col_types = cols()) %>%
                     mutate(scenario = scenarios[i]))
  }
  
}

p <- p %>%
  mutate(
    scenario = paste(
      trimws(format(as.Date(scenario), '%B')),
      trimws(format(as.Date(scenario), '%e'))
    )
  ) %>%
  mutate(
    scenario = case_when(
      scenario == "NA NA" ~ "No intervention",
      T ~ scenario
    )
  ) %>%
  drop_na(incidence)

ei <- read_tsv(here("data", "early_intervention", "2021-01-02_20pct_smooth1_data.txt"),
               show_col_types = FALSE) %>%
  mutate(scenario = "Early intervention") %>%
  drop_na(incidence)

ei <- read_csv("https://api.covid19india.org/csv/latest/case_time_series.csv",
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
  filter(date >= "2020-12-01" & date <= "2021-01-01") %>% 
  mutate(cfr = daily_deaths/daily_cases) %>% 
  select(date, daily_cases) %>%
  rename(incidence = daily_cases) %>%
  add_row(ei %>%
            select(date, incidence) %>%
            drop_na()) %>%
  add_column(scenario = "Early intervention") %>%
  filter(date <= end_date)

p <- bind_rows(p, ei)

# extract CFR schedule and get plot defaults -----------
d       <- extract_cfr()

# prepare data -----------
clean_prep <- function(x, var) {
  
  none   <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = end_date, scen = "No intervention")
  mar_15 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-15", scen = "March 15")
  mar_30 <- obs %>% clean_scenario_cfr(p = x, use_cfr = {{ var }}, stop_obs = "2021-03-30", scen = "March 30")
  ei <- x %>% filter(scenario == "Early intervention") %>%
    left_join(extract_cfr(start_date = "2021-01-01") %>%
                select(date, cfr = {{ var }}),
              by = "date") %>%
    mutate(incidence = incidence * cfr) %>%
    select(-cfr) %>%
    filter(date >= "2021-02-15")
  
  total <- obs %>% 
    filter(date <= end_date) %>% 
    select(date, daily_deaths) %>% 
    rename(incidence = daily_deaths) %>% 
    add_column(scenario = "Observed") %>% 
    add_row(mar_15) %>% 
    add_row(mar_30) %>%
    bind_rows(ei)
  
  total.smoothed <- total %>% 
    filter(date <= end_date) %>% 
    nest(data = c(date, incidence)) %>% 
    mutate(m = purrr::map(data, loess, formula = incidence ~ as.numeric(date), span = 0.5),
           fitted = purrr::map(m, `[[`, "fitted")) %>% 
    select(-m) %>% 
    unnest(cols = c(data, fitted))
  
  total_smoothed_plot <- total.smoothed %>% 
    filter(scenario == "Observed") %>% 
    filter(date <= end_date)  %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "Early intervention", 
                     date >= "2021-02-15")) %>%
    add_row(total.smoothed %>% 
              filter(scenario == "March 15", 
                     date >= "2021-03-09")) %>% 
    add_row(total.smoothed %>% 
              filter(scenario == "March 30", 
                     date >= "2021-03-24")) %>% 
    mutate(scenario = factor(scenario, levels = c("Observed", "Early intervention", "March 15", "March 30")))%>%
    filter(date <= end_date) 
  
  return(total_smoothed_plot)
  
}

# tsp_india <- total_smoothed_plot
tsp_india <- clean_prep(x = p, var = cfr_t7)
tsp_mh    <- clean_prep(x = p, var = cfr_mh_t7)
tsp_kl    <- clean_prep(x = p, var = cfr_kl_t7)


# plot ----------
death_plt <- function(dat, title,
                      tmp_nudge = 500, tmp_repel_y = c(2000, 1750, rep(4000, 2))) {
    
  # plt_def <- get_plt_def(mh = FALSE, kl = FALSE)
    
  deaths_p <- dat %>% 
  ggplot() + 
  geom_line(aes(x = date, y = fitted, color = scenario), size = 1) +
    scale_color_manual(values = cols) +
  # scale_colour_lancet() + 
  xlab("Date") + 
  ylab("Daily deaths") + 
  geom_vline(data = dat %>% 
               group_by(scenario) %>% 
               filter(date == min(date)) %>% 
               dplyr::ungroup() %>% 
               select(scenario, date) %>% 
               filter(!(scenario %in% c("Observed", "No intervention", "Early intervention"))), 
             aes(xintercept = date, color = scenario), 
             linetype = 'dashed') + 
  geom_label_repel(data = dat %>% 
                     group_by(scenario) %>% 
                     filter(fitted == max(fitted)) %>% 
                     dplyr::ungroup() %>% 
                     select(scenario, date, fitted) %>% 
                     filter(!(scenario %in% c("Observed", "No intervention"))) %>%
                     add_row(scenario = "Observed", date = as.Date("2021-05-18"), fitted = 4529), 
                   aes(x = date, 
                       y = fitted, 
                       label = paste0(formatC(round(fitted), format="f", big.mark=",", digits=0), " deaths"),
                       color = scenario,
                       family = "Lato"), 
                   nudge_y = tmp_nudge, 
                   nudge_x = -10, 
                   size = 4,
                   show.legend  = FALSE, 
                   segment.size = 1) + 
  geom_text_repel(data = dat %>% 
                    group_by(scenario) %>% 
                    filter(date == min(date)) %>% 
                    dplyr::ungroup() %>% 
                    select(scenario, date, fitted) %>% 
                    mutate(text = c("Observed data", "Early intervention\nJanuary 1", "March 15\nlockdown", "March 30\nlockdown"), 
                           x = as.Date(c("2021-05-09", "2021-02-20", "2021-03-09", "2021-03-24")), 
                           y = tmp_repel_y), 
                  aes(x = x, 
                      y = y, 
                      label = text,
                      color = scenario,
                      family = "Lato"), 
                  nudge_x      = -5,
                  size         = 4, 
                  show.legend  = FALSE, 
                  segment.size = 0) + 
  guides(color = guide_legend(nrow = 1)) + 
  labs(title    = title,
       y        = "Daily deaths",
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
  }

deaths_p    <- death_plt(dat = tsp_india, title = "Moderate CFR")
deaths_p_mh <- death_plt(dat = tsp_mh, title = "High CFR")
deaths_p_kl <- death_plt(dat = tsp_kl, title = "Low CFR")

patched <- deaths_p_mh / deaths_p / deaths_p_kl

full_plt <- patched +
  plot_annotation(
    title    = plt_title,
    subtitle = glue("February 15, 2021 to {format(end_date, '%B %e, %Y')}"),
    caption  = glue("**Notes:** Observations and prediction period until {format(end_date, '%B %e, %Y')}. ",
                    "Figures in boxes show peak number of deaths for each intervention.<br>",
                    "**Abbrev:** CFR, case-fatality rate<br>",
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

full_plt


# save output ----------
ggsave(filename = here("fig", tmp_outname),
       plot     = full_plt,
       height   = 12,
       width    = 10,
       units = "in", device = cairo_pdf)
