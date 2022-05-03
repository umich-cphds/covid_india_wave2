require(lspline)
require(lubridate)
require(tidyverse)

sero_val <- c(0.73, 7.1, 24.1, 67)/100
sero_date <- ymd(c("2020-06-01", "2020-09-01", "2021-01-31", "2021-07-01"))
sero_date_num <- as.numeric(sero_date)
m <- min(sero_date_num)
sero_date_num <- sero_date_num - m
sero_pred <- lm(sero_val ~ lspline(sero_date_num, c(92, 244)))
time <- -100:578
vals <- predict(sero_pred, data.frame(sero_date_num = time))
vals <- ifelse(ifelse(vals <= 0, 0, vals) >= 1, 1, ifelse(vals <= 0, 0, vals))
sero <- as_tibble(ymd(as.Date(time + m, origin = "1970-01-01"))) %>%
  rename(date = value) %>%
  add_column(vals) %>%
  mutate(sero = vals - lag(vals)) %>% 
  drop_na() %>% 
  write_csv("figures/figure s2/sero_t.csv") ### used in model runs

lims <- lubridate::ymd(c("2020-05-01",
                         "2021-09-30"))

mypal = ggsci::pal_aaas("default")(10)

full_plot <- as_tibble(ymd(as.Date(time + m, origin = "1970-01-01"))) %>%
  rename(date = value) %>%
  add_column(s = 100*vals) %>% 
  ggplot(aes(x = date, y = s)) + 
  geom_line() + 
  theme_bw() +
  scale_x_date(date_breaks = "months" , 
               date_labels = "%b-%y", 
               limits = lims,
               expand = c(0, 0)) + 
  geom_vline(xintercept = sero_date[1], 
             linetype = "dashed", 
             size = 1, 
             color = mypal[1]) + 
  annotate("text", 
           x = sero_date[1] + 1, 
           y = 15, 
           label = paste0("Reported seroprevalence\nfrom first serosurvey: ", 100*sero_val[1], ".\nDate: June, 2020."),  
           hjust = 0, 
           color = mypal[1], 
           fontface = "bold",
           size = 6) + 
  geom_vline(xintercept = sero_date[2], 
             size = 1, 
             linetype = "dashed", 
             color = mypal[3]) + 
  annotate("text", 
           x = sero_date[2] + 1,
           y = 25, 
           label = paste0("Reported seroprevalence\nfrom second serosurvey: ", 100*sero_val[2], ".\nDate: September, 2020."),  
           hjust = 0, 
           color = mypal[3], 
           fontface = "bold",
           size = 6) + 
  geom_vline(xintercept = sero_date[3], 
             linetype = "dashed", 
             size = 1, 
             color = mypal[4]) + 
  annotate("text", 
           x = sero_date[3]+1,
           y = 15, 
           label = paste0("Reported seroprevalence\nfrom third serosurvey: ", 100*sero_val[3], ".\nDate: February, 2021."),  
           hjust = 0, 
           color = mypal[4], 
           fontface = "bold",
           size = 6) +
  geom_vline(xintercept = sero_date[4], 
             linetype = "dashed", 
             color = mypal[8], 
             size = 1) + 
  annotate("text", 
           x = sero_date[4] - 95,
           y = 70, 
           label = paste0("Reported seroprevalence\nfrom fourth serosurvey: ", 100*sero_val[4], ".\nDate: July, 2021."),  
           hjust = 0, 
           fontface = "bold",
           color = mypal[8], 
           size = 6) +
  geom_point(data = as_tibble(x = sero_date) %>% add_column(s = sero_val*100), 
             aes(x = value, y = s), 
             size = 4) +
  ylab("Seroprevalence (%)") + 
  xlab("Date") + 
  theme(axis.title.y = element_text(size = 18*1.2, face = "bold"), 
        axis.title.x = element_text(size = 18*1.2, face = "bold"), 
        axis.text.x = element_text(size = 14*1.2), 
        axis.text.y = element_text(size = 14*1.2), 
        plot.title = element_text(size = 18*1.2, face = "bold"), 
        legend.title = element_text(size = 14*1.2), 
        legend.text = element_text(size = 14*1.2))  + 
  labs(title = "Construction of time-varying seroprevalence based on nation-wide SARS-CoV-2 serosurveys conducted in in India.")


ggsave(filename = glue("figures/figure s2/figure s2.pdf"),
       plot     = full_plot, 
       height   = 7.5,
       width    = 18,
       device   = cairo_pdf)


