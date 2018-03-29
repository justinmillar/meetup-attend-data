library(googlesheets)
library(lubridate)
library(tidyverse)

# Running record of attendance ----
record <- gs_title("UF R Meetup Record") %>% 
  gs_read(ws = 1) %>% 
  janitor::clean_names() %>% 
  mutate(date = as_date(date))

# Total attendance
record %>%  
  mutate(num = pmax(attendence, head_count, na.rm = T)) %>%
  select(num) %>% 
  na.omit() %>%
  summarise_all(sum)

# Attendance by week for each semester
df <- record %>%  
  mutate(num = pmax(attendence, head_count, na.rm = T)) %>% 
  select(date, sm, wk, num) %>% 
  na.omit() %>% 
  group_by(sm, wk) %>% 
  summarize(num = sum(num))

plotAttend <- ggplot(df, aes(x = wk, y = num, color = sm)) +
  geom_line(size = 2) +
  geom_point(size = 3, shape = 21, color = "black", aes(fill = sm)) +
  scale_color_manual(values = c("#009688", "#FF6F00", "#9E9E9E")) +
  scale_fill_manual(values = c("#009688", "#FF6F00", "#9E9E9E")) +
  xlab("Week") + ylab("Attendance") +
  theme_classic() +
  theme(legend.position = "top")

ggsave("img/attendance.png", plotAttend)

# Unique Presenters
unique(na.omit(record$presenter))

# Spring 2018 Sign-in data ----
first_week

# Departmental attendence
s18_record <- gs_title("UF R Meetup Sign-In (Responses)") %>% 
  gs_read(ws = 1) %>% 
  janitor::clean_names() %>% 
  mutate(timestamp = lubridate::mdy_hms(timestamp)) %>% 
  mutate(date = as_date(timestamp),
         department = abbreviate(department)) %>% 
  select(date, position, department)

plotDep <- ggplot(s18_record, aes(x = department, fill = department)) +
  geom_bar() +
  #geom_text()
  coord_polar() +
  theme_minimal() +
  #margin(t = 0, r = 0, b = 0, l = 0, unit = "pt") +
  theme(legend.position = "none", 
        axis.line=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"),
        #axis.text.x=element_blank(),
        #axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
plotDep
ggsave("img/s18-department.png", plotDep)
