require(ggsci)
require(tidyverse)
require(lubridate)
require(glue)

beta_low <- 0.008904271
beta_up <- 0.1504359

beta_ancestral <- 0.05703967
beta_alpha <- 1.5*beta_ancestral
beta_delta <- 2.5*beta_ancestral

strain <- read_csv("figures/figure s3/strains.csv") %>% 
  mutate(Date = my(Date))

strain <- strain %>% 
  rowwise() %>% 
  mutate(beta = delta*beta_delta + alpha*beta_alpha + ancestral*beta_ancestral) 

strain <- strain %>% 
  mutate(low = (beta_low/beta_ancestral)*beta, 
         high = (beta_up/beta_ancestral)*beta)

ylim.prim <- c(0, 1)   # in this example, precipitation
ylim.sec <-  c(min(strain$low), max(strain$high))    # in this example, temperature

b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here

break_helper <- function(lim){
  round(min(lim) + (1:5)*((max(lim) - min(lim))/6), 3)
}

strain %>% write_csv("figures/figure s3/figure s3.csv")

fullplot <- strain %>% 
  pivot_longer(cols = -c(Date, beta, low, high)) %>% 
  mutate(name = factor(stringr::str_to_sentence(name), levels = c("Ancestral", "Alpha", "Delta"))) %>% 
  filter(Date <= ymd("2021-07-31"), 
         Date >= ymd("2020-08-01")) %>% 
  ggplot(aes(fill=name, y=value, x=Date)) + 
  geom_bar(position="fill", stat="identity", alpha = 0.7) +
  geom_ribbon(aes(x = Date, ymin = a + b*low, ymax = a + b*high), alpha = 0.3, color = "black", fill = "grey") + 
  geom_line(mapping = aes(x = Date, y = a + beta*b), size = 2, color = "black") +
  geom_point(mapping = aes(x = Date, y = a + beta*b),  color = "black") +
  scale_y_continuous("Proportion of strain", 
                     sec.axis = sec_axis(~ (. - a)/b, 
                                         name = latex2exp::TeX("$\\beta_t$"), 
                                         breaks = break_helper)) + 
  scale_x_date(date_breaks = "months" , 
               date_labels = "%b-%y", 
               expand = c(0.005, 0.005)) + 
  scale_fill_aaas() + 
  xlab("") + 
  theme_minimal() + 
  theme(legend.position = "bottom") + 
  labs(fill = "Strain") + 
  theme(axis.title.y = element_text(size = 18*1.2, face = "bold"), 
        axis.title.x = element_text(size = 18*1.2, face = "bold"), 
        axis.text.x = element_text(size = 14*1.2), 
        axis.text.y = element_text(size = 14*1.2), 
        plot.title = element_text(size = 18*1.2, face = "bold"), 
        legend.title = element_text(size = 14*1.2), 
        legend.text = element_text(size = 14*1.2)) + 
  guides(fill = guide_legend(override.aes = list(shape = NA))) + 
  labs(title = "Construction of time-varying transmission rate based on time-varying prevalences of top three dominant strains of \nSARS-CoV-2 in India.")


ggsave(filename = glue("figures/figure s3/figure s3.pdf"),
       plot     = fullplot, 
       height   = 7.5,
       width    = 18,
       device   = cairo_pdf)
