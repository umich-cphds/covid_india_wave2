bake_pi <- function(pi_start = NULL, today, start_proj, last_proj, span = 0.75, plot = TRUE) {
  
  message("**beep boop** baking pi...")
  
  if (is.null(pi_start)) {
    message("using default pi_start = '2020-03-25'")
    pi_start <- as.Date("2020-03-25")
  } else {
    pi_start <- as.Date(pi_start)
  }
  
  dates  <- format(as.Date(start_proj:(last_proj - 1), origin = "1970-01-01"), "%m/%d/%Y")
  pi_end <- pi_start + length(dates) - 1
  
  d <- read_csv(glue::glue("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/{today}/everything.csv"),
                col_types = cols()) %>%
    filter(place == "India")
  
  start_r <- d %>% filter(date <= pi_start) %>% slice_tail(n = 7) %>% pull(r_est) %>% mean(., na.rm = TRUE)
  
  taco <- d %>%
    filter(date >= pi_start & date <= pi_end) %>%
    mutate(
      pis = r_est / start_r
    ) %>%
    select(place, date, r_est, pis)
  
  smoothr <- taco %>%
    arrange(date) %>%
    mutate(nom = 1:nrow(taco)) %>%
    loess(pis ~ nom, data = ., span = span) %>%
    predict()
  
  taco$smooth_pis <- smoothr
  
  out <- list(
    "data"        = taco,
    "dates"       = dates,
    "pis"         = c(1, taco$pis),
    "start_r"     = start_r,
    "smooth_pis"  = c(1, taco$smooth_pis),
    "smooth_span" = span
  )
  
  if (plot == TRUE) {
    
    out[["plot"]] <- taco %>%
      ggplot(aes(x = date, y = pis)) +
      geom_line(size = 1) +
      geom_line(aes(y = smooth_pis), size = 1, linetype = 2, color = "gray40") +
      labs(
        title = "Pi schedule",
        x     = "Date",
        y     = "Pi"
      ) +
      theme_classic() +
      theme(
        plot.title = element_text(face = "bold", size = 18)
      )
    
  }
  
  message("**ding** pi is done!")
  
  return(out)
  
}
