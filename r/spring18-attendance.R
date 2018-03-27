library(tidyverse)
library(lubridate)

signin_dat <- read_csv("data/UF R Meetup Sign-In.csv")
sup_dat <- read_csv("data/Supp Data - UF R Meetup Sign-In - Sheet1.csv") %>% 
  select(Timestamp = Date, Name, Position, Department) %>% 
  mutate(Timestamp = as_date(Timestamp))

dat <- signin_dat %>% 
  mutate(Timestamp = as_date(Timestamp)) %>% 
  rbind(sup_dat)

write_csv(dat, "data/Spring2018-Attendance.csv")

dat %>% 
  group_by(Timestamp) %>% 
  tally

dat %>% 
  select(Name, Department) %>% 
  distinct() %>% 
  group_by(Department) %>% 
  tally() 