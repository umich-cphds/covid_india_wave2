pacman::p_load(
  tidyverse, EpiEstim, gt, glue, lubridate, janitor, scales, ggtext, here, httr,
  ggbump
)
# source(here("r", "functions.R"))

set_seed <- 46342
set.seed(set_seed)

dat <- covid19india::get_all_data()
# dat    <- do_it_all()

calc_peaks <- function(d) {
  
  d |>
    group_by(place)|>
    filter(daily_cases == max(daily_cases, na.rm = T)) |>
    ungroup() |>
    select(date, place, peak_daily_cases = daily_cases) |>
    left_join(
      d |>
        select(date, place, daily_cases) |>
        group_by(place) |>
        summarize(total_cases = sum(daily_cases, na.rm = T)),
      by = "place"
    ) |>
    filter(peak_daily_cases > 0) |>
    arrange(date)
  
}

get_peaks <- function(d,
                      wave_1_start = "2020-06-13",
                      wave_2_start = "2021-02-14") {
  
  w1 <- d |>
    filter(date >= wave_1_start & date < wave_2_start)
  w2 <- d |>
    filter(date >= wave_2_start & date <= "2021-07-31")
  
  w1_peaks <- calc_peaks(w1) |>
    distinct() |>
    rename(
      w1_peak_date        = date,
      w1_peak_daily_cases = peak_daily_cases,
      w1_total_cases      = total_cases
    ) |>
    mutate(
      w1_order = 1:n()
    )
  
  w2_peaks <- calc_peaks(w2) |>
    distinct() |>
    rename(
      w2_peak_date        = date,
      w2_peak_daily_cases = peak_daily_cases,
      w2_total_cases      = total_cases
    ) |>
    mutate(
      w2_order = 1:n()
    )
  
  peaks <- left_join(
    w1_peaks, w2_peaks, by = "place"
  ) %>%
    mutate(
      pct_cases_w2 = w2_total_cases / (w2_total_cases + w1_total_cases)
    )
  
  return(peaks)
  
}

peaks <- dat |>
  get_peaks() |>
  select(place, w1_order, w1_peak_date, w1_peak_daily_cases, w1_total_cases,
         w2_order, w2_peak_date, w2_peak_daily_cases, w2_total_cases,
         pct_cases_w2) |>
  left_join(read_csv(here("data", "wave_tiers.csv"), col_types = cols()) %>% select(place, case_metric, case_tier),
            by = "place") |>
  invisible()

write_csv(x = peaks, file = here("data", "case_peaks_by_state_table.csv"))


# deaths -----------
calc_d_peaks <- function(d) {
  
  d |>
    group_by(place)|>
    filter(daily_deaths == max(daily_deaths, na.rm = T)) |>
    ungroup() |>
    select(date, place, peak_daily_deaths = daily_deaths) |>
    left_join(
      d |>
        select(date, place, daily_deaths) |>
        group_by(place) |>
        summarize(total_deaths = sum(daily_deaths, na.rm = T)),
      by = "place"
    ) |>
    filter(peak_daily_deaths > 0) |>
    arrange(date) |>
    group_by(place) |>
    filter(date == min(date, na.rm = T)) |>
    ungroup()
  
}


get_d_peaks <- function(d,
                      wave_1_start = "2020-06-13",
                      wave_2_start = "2021-02-14") {
  
  w1 <- d |>
    filter(date >= wave_1_start & date < wave_2_start)
  w2 <- d |>
    filter(date >= wave_2_start & date <= "2021-05-31")
  
  w1_peaks <- calc_d_peaks(w1) |>
    distinct() |>
    rename(
      w1_peak_date         = date,
      w1_peak_daily_deaths = peak_daily_deaths,
      w1_total_deaths      = total_deaths
    ) |>
    mutate(
      w1_order = 1:n()
    )
  
  w2_peaks <- calc_d_peaks(w2) |>
    distinct() |>
    rename(
      w2_peak_date         = date,
      w2_peak_daily_deaths = peak_daily_deaths,
      w2_total_deaths      = total_deaths
    ) |>
    mutate(
      w2_order = 1:n()
    )
  
  peaks <- left_join(
    w1_peaks, w2_peaks, by = "place"
  ) %>%
    mutate(
      pct_deaths_w2 = w2_total_deaths / (w2_total_deaths + w1_total_deaths)
    )
  
  return(peaks)
  
}

dat <-dat |> filter(date != "2020-06-16")

peaks_d <- dat |> get_d_peaks() |>
  select(place, w1_order, w1_peak_date, w1_peak_daily_deaths, w1_total_deaths,
         w2_order, w2_peak_date, w2_peak_daily_deaths, w2_total_deaths,
         pct_deaths_w2) |>
  left_join(read_csv(here("data", "wave_tiers.csv"), col_types = cols()) %>% select(place, death_metric, death_tier),
            by = "place") |>
  janitor::clean_names()
  # # dplyr::filter(w1_peak_daily_deaths > 10) |>
  # dplyr::group_by(place) |>
  # dplyr::filter(w1_peak_date == min(w1_peak_date))

write_csv(x = peaks_d, file = here("data", "death_peaks_by_state_table.csv"))





##############
### OLD ######
##############
# peaks_plt_dat <- peaks %>% select(place, w1_order, w2_order) %>%
#   pivot_longer(
#     names_to = "wave",
#     values_to = "rank",
#     -place
#   ) %>%
#   mutate(wave = case_when(wave == "w1_order" ~ 1, T ~ 2)) 
# 
# peaks_plt_dat |>
#   ggplot(aes(x = wave, y = rank, color = place)) +
#   geom_bump(size = 2, smooth = 8) +
#   geom_point(size = 7) +
#   geom_text(data = peaks_plt_dat %>% filter(wave == 1),
#             aes(x = wave - .05, label = place), size = 5, hjust = 1) +
#   geom_text(data = peaks_plt_dat %>% filter(wave == 2),
#             aes(x = wave + .05, label = place), size = 5, hjust = 0) +
#   labs(
#     title = "Order of COVID-19 peaks in Wave 1 and Wave 2 in India",
#     x     = "Wave",
#     y     = "Order"
#   ) +
#   coord_cartesian(xlim = c(0.65, 2.35), ylim = rev(c(1, 37)), clip = "off") +
#   scale_y_reverse(breaks = 1:37) +
#   scale_x_continuous(breaks = c(1,2)) +
#   theme_minimal() +
#   theme(legend.position = "none",
#         panel.grid = element_blank(),
#         plot.title = element_text(face = "bold"),
#         text = element_text(family = "Lato"))
# 
# ggsave(here("data", "output", "peaks_by_state.pdf"),
#        height = 10, width = 15, device = cairo_pdf)
