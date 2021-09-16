ally::libri(tidyverse, here, glue, ggtext)

dat <- read_csv("data_for_lockdown_extended.csv", col_types = cols()) %>%
  filter(date <= "2021-08-15")

pi_sched  <- read_tsv("pi_schedule.txt", col_types = cols())

# india ------------
location   <- "India"
start_proj <- as.Date("2020-03-27")
length_out <- 199
end_proj   <- start_proj + length_out
dates      <- seq.Date(from = start_proj, to = end_proj, by = "day")

start_r <-  (dat %>% filter(place == location & between(date, start_proj - 7, start_proj - 1)) %>% pull(r_est) %>% mean(na.rm = T)) 

pi_sched_ext <- dat %>%
  filter(place == location & between(date, start_proj, end_proj)) %>%
  select(place, date, cases, daily_cases, r_est) %>%
  arrange(date) %>%
  mutate(
    pis = r_est / start_r,
    nom = 1:nrow(.)
  ) %>%
  mutate(
    smooth_pis = predict(loess(pis ~ nom, span = 0.5))
  )

if (nrow(pi_sched_ext) < length(dates)) {
  pi_sched_ext <- bind_rows(
    pi_sched_ext,
    tibble(date = seq.Date(from = max(pi_sched_ext$date) + 1, length.out = length(dates) - nrow(pi_sched_ext), by = "day"), smooth_pis = NA),
    ) %>%
    mutate(smooth_pis = data.table::nafill(smooth_pis, type = "locf"))
}

pi_sched_ext_ind <- pi_sched_ext %>%
  mutate(
    smooth_pis = case_when(smooth_pis > 1 ~ 1, T ~ smooth_pis),
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    scenario = "India lockdown - 200 days"
  )

  v_date <- "2020-06-01"

comp_plt <- bind_rows(
  pi_sched %>% filter(place == location) %>% select(place, date, smooth_pis) %>% mutate(schedule = "old"),
  pi_sched_ext_ind %>% select(place, date, smooth_pis) %>% mutate(schedule = "new")
) %>%
  ggplot(aes(x = date, y = smooth_pis, group = schedule, color = schedule)) +
  geom_vline(xintercept = as.Date(v_date), color = "gray40", size = 1, linetype = 2) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_labels = "%b %Y") +
  labs(
    title = glue("Comparison of {location} lockdown pi schedules"),
    x     = "Date",
    y     = "Pi",
    caption = "Both schedules start on March 27, 2020.<br>**Old:** LOESS smoother (span = 1) using data from 3/27 through 7/25 (120-days).<br>**New:** LOESS smoother (span = 1), using data from 3/27 through 10/12 (200 days)."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )

comp_plt

ggsave(plot = comp_plt,
       filename = here("fig", "diagnostic", glue("{location}_pi_comp.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# India length of lockdown ----------
ind_unlock   <- pi_sched_ext_ind %>% filter(between(date, as.Date("2020-06-01"), as.Date("2020-10-31")))
ind_6_weeks  <- pi_sched_ext_ind %>% slice(1:(6*7)) %>% filter(date <= "2020-06-01")
ind_9_weeks  <- pi_sched_ext_ind %>% slice(1:(9*7)) %>% filter(date <= "2020-06-01")
ind_12_weeks <- pi_sched_ext_ind %>% slice(1:(12*7)) %>% filter(date <= "2020-06-01") %>%
  bind_rows(
    tibble(date = NA, smooth_pis = NA, .rows = (12*7) - nrow(.))
  ) %>%
  mutate(
    date = seq.Date(from = start_proj, length.out = 12*7, by = "day"),
    smooth_pis = data.table::nafill(smooth_pis, type  = "locf")
  )

ind_6_weeks <- bind_rows(
  ind_6_weeks,
  ind_unlock %>%
    mutate(smooth_pis = smooth_pis * ((last(ind_6_weeks$smooth_pis)) / (first(ind_unlock$smooth_pis))))) %>%
  mutate(nom = 1:nrow(.)) %>%
  mutate(smooth_pis = predict(loess(smooth_pis ~ nom, span = 0.5))) %>%
  mutate(scenario = "India 6 weeks", place = "India 6 weeks")

ind_9_weeks <- bind_rows(
  ind_9_weeks,
  ind_unlock %>%
    mutate(smooth_pis = smooth_pis * ((last(ind_9_weeks$smooth_pis)) / (first(ind_unlock$smooth_pis))))) %>%
  mutate(nom = 1:nrow(.)) %>%
  mutate(smooth_pis = predict(loess(smooth_pis ~ nom, span = 0.5))) %>%
  mutate(scenario = "India 9 weeks", place = "India 9 weeks")

ind_12_weeks <- bind_rows(
  ind_12_weeks,
  ind_unlock %>%
    mutate(smooth_pis = smooth_pis * ((last(ind_12_weeks$smooth_pis)) / (first(ind_unlock$smooth_pis))))) %>%
  mutate(nom = 1:nrow(.)) %>%
  mutate(smooth_pis = predict(loess(smooth_pis ~ nom, span = 0.5))) %>%
  mutate(scenario = "India 12 weeks", place = "India 12 weeks")

pi_sched_ext_lol_ind <- bind_rows(
  ind_6_weeks,
  ind_9_weeks,
  ind_12_weeks
)

lol_comp_plt <- pi_sched_ext_lol_ind %>%
  ggplot(aes(x = nom, y = smooth_pis, color = scenario, group = scenario)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Comparison of India lenth of lockdown schedules",
    x     = "Day",
    y     = "Pi",
    caption = "All schedules are based on full 200-day India lockdown.<br>Schedules are truncated for lockdown (with LOCF for 12 weeks) and then append unlock portion of schedule.<br>These stictched schedules are LOESS smoothed (span = 0.5)."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )
ggsave(plot = lol_comp_plt,
       filename = here("fig", "diagnostic", glue("{location}_lol_pi_comp.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# Maharashtra lockdown 200-day-------------
location   <- "Maharashtra"
start_proj <- as.Date("2021-04-14")
length_out <- 199
end_proj   <- start_proj + length_out
dates      <- seq.Date(from = start_proj, to = end_proj, by = "day")

start_r <-  (dat %>% filter(place == location & between(date, start_proj - 7, start_proj - 1)) %>% pull(r_est) %>% mean(na.rm = T)) 

pi_sched_ext <- dat %>%
  filter(place == location & between(date, start_proj, end_proj)) %>%
  select(place, date, cases, daily_cases, r_est) %>%
  arrange(date) %>%
  mutate(
    pis = r_est / start_r,
    nom = 1:nrow(.)
  ) %>%
  mutate(
    smooth_pis = predict(loess(pis ~ nom, span = 1))
  )

pi_sched_ext_mh <- pi_sched_ext %>%
  mutate(
    smooth_pis = case_when(smooth_pis > 1 ~ 1, T ~ smooth_pis),
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    scenario = "Maharashtra lockdown - 200 days"
  )

v_date <- "2021-06-07"

comp_plt <- bind_rows(
  pi_sched %>% filter(place == location) %>% select(place, date, smooth_pis) %>% mutate(schedule = "old"),
  pi_sched_ext_mh %>% select(place, date, smooth_pis) %>% mutate(schedule = "new")
) %>%
  ggplot(aes(x = date, y = smooth_pis, group = schedule, color = schedule)) + geom_vline(xintercept = as.Date(start_proj) + (4*7), color = "blue", size = 1, linetype = 2) +
  geom_vline(xintercept = as.Date(start_proj) + (6*7), color = "blue", size = 1, linetype = 2) +
  geom_vline(xintercept = as.Date(start_proj) + (8*7), color = "blue", size = 1, linetype = 2) +
  
  geom_vline(xintercept = as.Date(v_date), color = "gray40", size = 1, linetype = 2) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  scale_x_date(date_labels = "%b %Y") +
  labs(
    title = glue("Comparison of {location} lockdown pi schedules"),
    x     = "Date",
    y     = "Pi",
    caption = "Both schedules start on April 14, 2021.<br>**Old:** LOESS smoother (span = 1) using data from 4/14 through 8/11 (120-days, with smoothing).<br>**New:** LOESS smoother (span = 1), using data from 4/14 through 8/15 (200 days)."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )

ggsave(plot = comp_plt,
       filename = here("fig", "diagnostic", glue("{location}_pi_comp.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# Maharashtra length of lockdown -----------

mh_unlock  <- pi_sched_ext_mh %>% filter(between(date, as.Date("2021-06-07"), as.Date("2021-07-31")))
mh_4_weeks <- pi_sched_ext_mh %>% slice(1:(4*7)) %>% filter(date <= "2021-06-07")
mh_6_weeks <- pi_sched_ext_mh %>% slice(1:(6*7)) %>% filter(date <= "2021-06-07")
mh_8_weeks <- pi_sched_ext_mh %>% slice(1:(8*7)) %>% filter(date <= "2021-06-07") %>%
  add_row() %>%
  mutate(
    date = seq.Date(from = as.Date("2021-04-14"), length.out = 8*7, by = "day"),
    smooth_pis = data.table::nafill(smooth_pis, type = "locf")
    )

mh_4_weeks <- bind_rows(
  mh_4_weeks,
  mh_unlock %>%
    mutate(smooth_pis = smooth_pis * ((last(mh_4_weeks$smooth_pis)) / (first(mh_unlock$smooth_pis))))) %>%
  mutate(nom = 1:nrow(.)) %>%
  mutate(smooth_pis = predict(loess(smooth_pis ~ nom, span = 0.5))) %>%
  mutate(scenario = "Maharashtra 4 weeks", place = "Maharashtra 4 weeks")

mh_6_weeks <- bind_rows(
  mh_6_weeks,
  mh_unlock %>%
    mutate(smooth_pis = smooth_pis * ((last(mh_6_weeks$smooth_pis)) / (first(mh_unlock$smooth_pis))))) %>%
  mutate(nom = 1:nrow(.)) %>%
  mutate(smooth_pis = predict(loess(smooth_pis ~ nom, span = 0.5))) %>%
  mutate(scenario = "Maharashtra 6 weeks", place = "Maharashtra 6 weeks")

mh_8_weeks <- bind_rows(
  mh_8_weeks,
  mh_unlock %>%
    mutate(smooth_pis = smooth_pis * ((last(mh_8_weeks$smooth_pis)) / (first(mh_unlock$smooth_pis))))) %>%
  mutate(nom = 1:nrow(.)) %>%
  mutate(smooth_pis = predict(loess(smooth_pis ~ nom, span = 0.5))) %>%
  mutate(scenario = "Maharashtra 8 weeks", place = "Maharashtra 8 weeks")

pi_sched_ext_lol <- bind_rows(
  mh_4_weeks,
  mh_6_weeks,
  mh_8_weeks
)

lol_comp_plt <- pi_sched_ext_lol %>%
  ggplot(aes(x = nom, y = smooth_pis, color = scenario, group = scenario)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    title = "Comparison of lenth of lockdown schedules",
    x     = "Day",
    y     = "Pi",
    caption = "All schedules are based on full 200-day Maharashtra lockdown.<br>Schedules are truncated for lockdown (with LOCF for 8 weeks) and then append unlock portion of schedule.<br>These stictched schedules are LOESS smoothed (span = 0.5)."
  ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )
ggsave(plot = lol_comp_plt,
       filename = here("fig", "diagnostic", glue("{location}_lol_pi_comp.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# Maharashtra early intervention -----------
location   <- "Maharashtra"
start_proj <- as.Date("2021-03-28")
end_proj   <- as.Date("2021-04-14")
dates      <- seq.Date(from = start_proj, to = end_proj, by = "day")
length_out <- length(dates)
end_proj   <- start_proj + length_out


start_r <-  (dat %>% filter(place == location & between(date, start_proj - 7, start_proj - 1)) %>% pull(r_est) %>% mean(na.rm = T))

pi_sched_ext <- dat %>%
  filter(place == location & between(date, start_proj, end_proj)) %>%
  select(place, date, cases, daily_cases, r_est) %>%
  arrange(date) %>%
  mutate(
    pis = r_est / start_r,
    nom = 1:nrow(.)
  ) %>%
  mutate(
    smooth_pis = predict(loess(pis ~ nom, span = 1))
  )

# pi_sched_early_mh <- pi_sched_ext %>%
#   mutate(
#     smooth_pis = case_when(smooth_pis > 1 ~ 1, T ~ smooth_pis),
#     scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
#     scenario = "Maharashtra pre-lockdown"
#   )

pi_sched_early_mh <- tibble(
  date = seq.Date(from = start_proj, length.out = 100, by = "day"),
  pis = data.table::nafill(pi_sched_ext$smooth_pis[1:100], type = "locf")) %>%
  mutate(
    smooth_pis     = predict(loess(pis ~ as.numeric(date), span = 1)),
    place          = "Maharashtra early",
    scenario       = "Maharashtra early",
    scenario_dates = glue("{format(start_proj, '%b %d')} - {format(end_proj, '%b %d')}, {format(end_proj, '%Y')}"),
    nom            = 1:100
    )
  
pre_lock_plt <- pi_sched_early_mh %>%
  mutate(smooth_pis_20pct = ifelse(smooth_pis * 1.2 > 1, 1, smooth_pis * 1.2)) %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = smooth_pis), size = 1, color = "blue") +
  geom_line(aes(y =  smooth_pis_20pct), size = 1, color = "red") +
  labs(
    title = glue("Comparison of {location} pre-lockdown pi schedules"),
    x     = "Date",
    y     = "Pi",
    caption = "Both schedules start on March 28, 2021 and go to April 14, 2021.<br>Blue line is smooth observed schedule (March 28 - April 14, locf up to 100 days then smoothed).<br>Red line is blue line increased by 20%"
    ) +
  theme_minimal() +
  theme(
    text = element_text(family = "Lato"),
    legend.title = element_blank(),
    legend.position = "top",
    plot.caption = element_markdown(hjust = 0),
    plot.title = element_text(face = "bold")
  )

ggsave(plot = pre_lock_plt,
       filename = here("fig", "diagnostic", glue("{location}_pi_prelock.pdf")),
       width = 7, height = 5,
       device = cairo_pdf)

# combine ------------
pi_ext_comb <- bind_rows(
  pi_sched_ext_ind,
  pi_sched_ext_lol_ind,
  pi_sched_ext_mh,
  pi_sched_ext_lol,
  pi_sched_early_mh
)

write_tsv(x = pi_ext_comb, file = "pi_schedule_extended.txt")
