library(tidyverse)
library(lubridate)
library(ggthemes)

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

workshop_dat <- read_csv("data/carpentry-attendance.csv") %>% 
  select(Timestamp) 

workshop_att <- workshop_dat %>% 
  mutate(Timestamp = lubridate::mdy_hm(Timestamp)) %>% 
  mutate(Timestamp = as_date(Timestamp)) %>% 
  filter(Timestamp >= as_date("2018-1-1")) %>% 
  group_by(Timestamp) %>% 
  tally()


current_att <- dat %>% 
  group_by(Timestamp) %>% 
  tally() %>% 
  mutate(Timestamp = Timestamp + days(1)) %>% 
  rbind(workshop_att) %>% 
  group_by(Timestamp) %>% 
  summarize(count = sum(n))
  

dat %>% 
  select(Name, Department) %>% 
  distinct() %>% 
  group_by(Department) %>% 
  tally() 

# Adding in previous attendance numbers ---

spring_17_attend <- data.frame(Attendance = c(48, 31, 18, 23, 14, 26, 
                                              27, 16, 13, 13, 19, 16, 15),
                               Week = 1:13,
                               Semester = "Spring 2017")
fall_17_attend   <- data.frame(Attendance = c(52, 37, 24, 20, 22, 13, 
                                              18, 20, 22, 23, 6),
                               Week = 1:11,
                               Semester = "Fall 2017")
spring_18_attend <- data.frame(Attendance = current_att$count,
                               Week = 1:length(current_att$count),
                               Semester = "Spring 2018")

attendance <- rbind(spring_17_attend, fall_17_attend, spring_18_attend)

attend_plot <- ggplot(attendance, aes(x = Week, y = Attendance, color = Semester)) +
  geom_line(size = 2) +
  geom_point(size = 3, shape = 21, color = "black", aes(fill = Semester)) +
  theme_classic() +
  theme(legend.position = "top")

ggsave("img/attendance.png", attend_plot, device = "png")
