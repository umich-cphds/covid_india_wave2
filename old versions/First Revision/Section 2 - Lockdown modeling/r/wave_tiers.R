library(tidyverse)
library(data.table)

#### map data state level ####
data.cum <- read_csv("https://api.covid19india.org/csv/latest/states.csv") %>% 
  mutate(State = ifelse(State == "Andaman and Nicobar Islands", "Andaman & Nicobar Island", State)) %>% 
  mutate(State = ifelse(State == "Arunachal Pradesh", "Arunanchal Pradesh", State)) %>% 
  mutate(State = ifelse(State == "Delhi", "NCT of Delhi", State)) %>% 
  mutate(State = ifelse(State == "Jammu and Kashmir", "Jammu & Kashmir", State)) 

threshold <- data.cum %>% 
  filter(State == "India") %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  mutate(act = Confirmed - lag(Confirmed)) %>% 
  drop_na() %>% 
  filter(Date == "2021-02-14") %>% 
  pull(act)

fig.init <- data.cum %>% 
  filter(State == "India") %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  mutate(act = Confirmed - lag(Confirmed)) %>% 
  drop_na() %>% 
  filter(act >= threshold)%>% 
  ggplot(aes(x = Date, y = act/(10^3))) + 
  geom_line() + 
  geom_smooth(span = 0.1, se = F) + 
  ggsci::scale_color_aaas() + 
  ylab(latex2exp::TeX("Daily new cases $(10^3)$")) +
  xlab("") + 
  geom_vline(xintercept = c(as.Date("2021-02-14"), as.Date("2020-06-13"))) + 
  geom_hline(yintercept = 11706/1000) + 
  scale_x_date(date_breaks = "2 week", date_labels = "%d - %b - %Y") + 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  labs(title = "Plot of new daily cases for waves 1 and 2", 
       subtitle = "Wave 1: June 13, 2020 - February 13, 2021.\nWave 2: February 14, 2021 - May 15, 2021.", 
       caption = "Line in black shows daily new cases and line in blue is a loess smoother.\nFor internal use only. NOT FOR PUBLICATION") 

##### cases #####

### metric for cases, wave 1
obj1 <- data.cum %>% 
  filter(Date %in% as.Date(c("2020-06-12"))) %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  # filter(State %in% states$st_nm) %>%
  invisible()

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-02-13"))) %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  # filter(State %in% states$st_nm) %>%
  invisible()

wave1 <- right_join(obj1, obj2, by = "State") %>% 
  #drop_na() %>% 
  mutate(day.diff = as.numeric(Date.y - Date.x), 
         case.diff = Confirmed.y - Confirmed.x) %>% 
  mutate(metric = case.diff/day.diff) %>% 
  dplyr::select(c(State, metric))


### metric for cases, wave 2
obj1 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-02-13"))) %>% 
  dplyr::select(-c(Recovered,Deceased,Other, Tested)) %>% 
  # filter(State %in% states$st_nm) %>%
  invisible()

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-07-31"))) %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  # filter(State %in% states$st_nm) %>%
  invisible()

wave2 <- right_join(obj1, obj2, by = "State") %>% 
  #drop_na() %>% 
  mutate(day.diff = as.numeric(Date.y - Date.x), 
         case.diff = Confirmed.y - Confirmed.x) %>% 
  mutate(metric = case.diff/day.diff) %>% 
  dplyr::select(c(State, metric))

cases <- left_join(wave1, wave2, by = "State") %>% 
  #drop_na() %>% 
  mutate(metric = metric.y/metric.x) %>% 
  mutate(tier = (paste0(floor(metric), " - ", ceiling(metric)))) %>% 
  mutate(tier = ifelse(tier == "0 - 1", "< 1", 
                       ifelse(tier == "5 - 6", "> 5", tier))) %>% 
  mutate(tier = factor(tier, 
                       levels = c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "> 5")))



##### deaths #####



### metric for cases, wave 1
obj1 <- data.cum %>% 
  filter(Date %in% as.Date(c("2020-06-12"))) %>% 
  dplyr::select(-c(Recovered, Confirmed, Other, Tested)) %>% 
  # filter(State %in% states$st_nm) %>%
  invisible()

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-02-13"))) %>% 
  dplyr::select(-c(Recovered, Confirmed, Other, Tested)) %>% 
  # filter(State %in% states$st_nm)  %>%
  invisible()

wave1 <- right_join(obj1, obj2, by = "State") %>% 
  drop_na() %>% 
  mutate(day.diff = as.numeric(Date.y - Date.x), 
         case.diff = Deceased.y - Deceased.x) %>% 
  mutate(metric = case.diff/day.diff) %>% 
  dplyr::select(c(State, metric))


### metric for cases, wave 2
obj1 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-02-13"))) %>% 
  dplyr::select(-c(Recovered, Confirmed,Other, Tested)) %>% 
  # filter(State %in% states$st_nm) %>%
  invisible()

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-07-31"))) %>% 
  dplyr::select(-c(Recovered,  Confirmed, Other, Tested)) %>% 
  # filter(State %in% states$st_nm)  %>%
  invisible()

wave2 <- right_join(obj1, obj2, by = "State") %>% 
  drop_na() %>% 
  mutate(day.diff = as.numeric(Date.y - Date.x), 
         case.diff = Deceased.y - Deceased.x) %>% 
  mutate(metric = case.diff/day.diff) %>% 
  dplyr::select(c(State, metric))

deaths <- left_join(wave1, wave2, by = "State") %>% 
  drop_na() %>% 
  mutate(metric = metric.y/metric.x) %>% 
  mutate(tier = paste0(floor(metric), " - ", ceiling(metric)))  %>% 
  mutate(tier = ifelse(tier == "0 - 1", "< 1", 
                       ifelse(tier >= "5 - 6", "> 5", tier))) %>% 
  mutate(tier = factor(tier, 
                       levels = c("< 1", "1 - 2", "2 - 3", "3 - 4", "4 - 5", "> 5")))

setDT(cases)
setDT(deaths)

out <- data.table::merge.data.table(cases[, .(place = State, case_metric = metric, case_tier = tier)],
                             deaths[, .(place = State, death_metric = metric, death_tier = tier)],
                             all.x = TRUE,
                             by = "place")

fwrite(x = out, file = here("data", "wave_tiers.csv"))
