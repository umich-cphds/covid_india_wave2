library(tidyverse)
library(sf)
# read shape and convert state names to lower case 
states <- st_read("~/Downloads/India Map/States/States.shp") %>%
  mutate(Name = str_to_lower(st_nm))


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
  filter(State %in% states$st_nm) 

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-02-13"))) %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  filter(State %in% states$st_nm)  

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
  filter(State %in% states$st_nm) 

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-05-31"))) %>% 
  dplyr::select(-c(Recovered, Deceased, Other, Tested)) %>% 
  filter(State %in% states$st_nm)  

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
  filter(State %in% states$st_nm) 

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-02-13"))) %>% 
  dplyr::select(-c(Recovered, Confirmed, Other, Tested)) %>% 
  filter(State %in% states$st_nm)  

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
  filter(State %in% states$st_nm) 

obj2 <- data.cum %>% 
  filter(Date %in% as.Date(c("2021-05-31"))) %>% 
  dplyr::select(-c(Recovered,  Confirmed, Other, Tested)) %>% 
  filter(State %in% states$st_nm)  

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

colors <- c(
  "< 1" = "#BDD7e7",
  "1 - 2" = "#FEE5D9",
  "2 - 3" = "#FCAE91",
  "3 - 4" = "#FB6A4A",
  "4 - 5" = "#DE2D26",
  "> 5" = "#A50F15"
)

textcol <- 
  c(
    "< 1" = "black",
    "1 - 2" = "black",
    "2 - 3" = "black",
    "3 - 4" = "black",
    "4 - 5" = "white",
    "> 5" = "white"
  )


state <- states_population %>% 
  drop_na() %>% 
  mutate(label = paste0(st_nm, ": ", round(metric, 2))) %>% 
  mutate(nx = ifelse(st_nm %in% c("NCT of Delhi", "Chandigarh",
                                  "Puducherry", "Uttarakhand"), 1, 
                     ifelse(st_nm %in% c("Goa", "Punjab"), -1, 0 ))) %>% pull(st_nm)
nx <-  states_population %>% 
  drop_na() %>% 
  mutate(label = paste0(st_nm, ": ", round(metric, 2))) %>% 
  mutate(nx = ifelse(st_nm %in% c("NCT of Delhi", "Chandigarh",
                                  "Puducherry", "Uttarakhand"), 1, 
                     ifelse(st_nm %in% c("Goa", "Punjab"), -1, 0 ))) %>% pull(nx)
nx <- as_tibble(state) %>% 
  add_column(nx) %>% 
  arrange(value) %>% 
  pull(nx)


##### plotting #####

title <- "(A) Standardised case rate ratio"

""
subtitle <- glue("Based on data from June 13, 2020 - May 31, 2021.\nWave 1: June 13, 2020 - February 13, 2021.\nWave 2: February 14, 2021 - May 15, 2021.")
# keep - unless the data is not through May 18 for whatever reason
#y_lab    <- "Estimated underreporting factor"                                                      # this will need to change
caption  <- glue("**\uA9 COV-IND-19 Study Group**<br>",              # keep
                 "**Source:** covid19india.org<br>",                 # keep
                 "**Note:**<br>",                                    # these lines may need to change
                 #  " - Owing to lack of sufficient data, estimates from states with less than 500 cumulative deaths (as of May 15, 2021) have been left out.<br>", 
                 " - Standardised case intensity for a specific wave is defined as the cumulative case count reported, standardised by the duration of the wave.<br>", 
                 " - Standardised case intensity for a specific wave is defined as the cumulative case count reported, standardised by the duration of the wave.<br>")


# merge spatial data with population data, also convert state names to lower case in the latter
states_population <- states %>%
  left_join(cases %>% mutate(Name = str_to_lower(State)), "Name")
# grey states are the result of unmatched states outlined above
p.cases <- ggplot(states_population %>% 
                    drop_na() %>% 
                    mutate(label = paste0(st_nm)) %>% 
                    filter(!(st_nm %in% c("Puducherry", "Chandigarh"))), 
                  aes(fill = tier)) +
  geom_sf(color = "black") +
  geom_sf_label(aes(label = label, fill = tier, color = tier), 
                size = 3.5, family = "sans", show.legend = FALSE) + 
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = textcol) + 
  ggthemes::theme_map() + 
  labs(fill = "Case rate ratio") + 
  theme(legend.text = element_text(size=12), 
        legend.title = element_text(size=14))
ggsave("cases_2021_06_11.png", p.cases, dpi = 300, width = 35, height = 35, units = "cm")
system("say Code finished!")



states_population <- states %>%
  left_join(deaths %>% mutate(Name = str_to_lower(State)), "Name")
# grey states are the result of unmatched states outlined above
p.deaths <- ggplot(states_population %>% 
                    drop_na() %>% 
                    mutate(label = paste0(st_nm)) %>% 
                    filter(!(st_nm %in% c("Puducherry", "Chandigarh"))), 
                  aes(fill = tier)) +
  geom_sf(color = "black") +
  geom_sf_label(aes(label = label, fill = tier, color = tier), 
                size = 3.5, family = "sans", show.legend = FALSE) + 
  scale_fill_manual(values = colors) + 
  scale_color_manual(values = textcol) + 
  ggthemes::theme_map() + 
  labs(fill = "Death rate ratio") + 
  theme(legend.text = element_text(size=12), 
        legend.title = element_text(size=14))
ggsave("deaths_2021_06_11.png", p.deaths, dpi = 300, width = 35, height = 35, units = "cm")
system("say Code finished!")
