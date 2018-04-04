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

plotAttend
ggsave("img/attendance.png", plotAttend)

# Unique Presenters
unique(na.omit(record$presenter))

# Spring 2018 Sign-in data ----
first_week <- gs_title("Supp Data - UF R Meetup Sign-In") %>% 
  gs_read(ws = 1) %>% 
  janitor::clean_names() %>% 
  mutate(date = as_date(date)) %>% 
  select(date, position, department)

s18_record <- gs_title("UF R Meetup Sign-In (Responses)") %>% 
  gs_read(ws = 1) %>% 
  janitor::clean_names() %>% 
  mutate(timestamp = lubridate::mdy_hms(timestamp)) %>% 
  mutate(date = as_date(timestamp)) %>% 
  select(date, position, department) %>% 
  filter(department != "zCount") %>% 
  rbind(first_week)

# Departmental attendance
unique(na.omit(s18_record$department))

s18_dep <- s18_record %>% 
  mutate(department = abbreviate(department, 8))

plotDep <- ggplot(s18_dep, aes(x = department, fill = department)) +
  geom_bar() +
  coord_polar() +
  theme_minimal() +
  ggtitle("Spring 2018 Attendance by Department") +
  theme(legend.position = "none", 
        axis.line=element_blank(),
        plot.margin = margin(0,0,0,0, "cm"),
        axis.text.x=element_text(size = 6, color = "black"),
        #axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank())
plotDep
ggsave("img/s18-department.png", plotDep)

# Positional attendance
s18_pos <- s18_record %>% 
  select(date, position) %>% 
  mutate(position = abbreviate(position, 12)) %>% 
  na.omit() %>% 
  group_by(position) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n/sum(n))

plotPos <- ggplot(s18_pos, aes(x = position, y = freq, fill = position)) +
  geom_col() + 
  scale_y_continuous() +
  xlab("") + ylab("% of Attendance") + 
  ggtitle("Spring 2018 Attendance by Position") +
  theme_classic() +
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 30, hjust = 1))
plotPos
ggsave("img/s18-position.png", plotPos)
