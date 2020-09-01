library(tidyverse)
library(lubridate)
library(ggplot2)
library(forecast)
library(zoo)
library(Hmisc) # For summary stats (describe command)
library(plotly)


cgmData_raw <- read.csv("~/Bloodsugar project/CGM-project/Tyryn Carnegie 01.09.2020.csv")


# A bit of cleaning ####

cgmData <- cgmData_raw %>%
  select(Date, Time, 32) %>%
  rename(sensor_glucose=3) %>%
  na_if("") %>%
  drop_na(sensor_glucose) %>%
  unite("date_time", Date:Time, remove = TRUE, sep = " ") %>%
  mutate(date_time = str_replace_all(date_time, "/", "-")) %>%
  mutate(date_time = ymd_hms(date_time, tz=Sys.timezone())) %>%
  mutate(sensor_glucose = as.numeric(sensor_glucose)) %>%
  drop_na(date_time)


# Get the standard deviation by day

cgmData <- cgmData %>%
  mutate(date = as.Date(date_time, "%Y-%m-%d", tz=Sys.timezone())) %>%
  group_by(date) %>%
  mutate(glucose_mean = mean(sensor_glucose)) %>%
  mutate(glucose_sd = sd(sensor_glucose))

# Check that standard deviation is a good measure by comparing ts graphs days with high and low standard deviation
# cgmData_1 <- cgmData %>%
#   filter(date==as.Date("2020-02-11"))
# line_val_1 <- cgmData_1$glucose_mean[[1]]
# 
# plot_1 <- ggplot(cgmData_1, aes(x=date_time, y=sensor_glucose)) +
#   geom_line()  + ylim(c(0, 15))  + geom_hline(yintercept = line_val_1) +
#   ggtitle(cgmData_1$glucose_sd[[1]])
# plot_1
# 
# cgmData_2 <- cgmData %>%
#   filter(date==as.Date("2020-01-23"))
# line_val_2 <- cgmData_2$glucose_mean[[1]]
# 
# plot_2 <- ggplot(cgmData_2, aes(x=date_time, y=sensor_glucose)) +
#   geom_line() + ylim(c(0, 15))  + geom_hline(yintercept = line_val_2) +
#   ggtitle(cgmData_2$glucose_sd[[1]])
# plot_2
# 
# cgmData_3 <- cgmData %>%
#   filter(date==as.Date("2020-04-06"))
# line_val_3 <- cgmData_3$glucose_mean[[1]]
# 
# plot_3 <- ggplot(cgmData_3, aes(x=date_time, y=sensor_glucose)) +
#   geom_line() + ylim(c(0, 15)) + geom_hline(yintercept = line_val_3) +
#   ggtitle(cgmData_3$glucose_sd[[1]])
# plot_3

# Percent of observations within range each day ####
# Set range to 4.5 - 8
cgmData <- cgmData %>%
  mutate(in_range = ifelse(sensor_glucose>=4.5 & sensor_glucose<=8, 1, 0)) %>%
  group_by(date) %>%
  add_tally() %>%
  mutate(percent_in_range = (sum(in_range)/n)*100) %>%
  filter(n>200) %>% # Quite a sharp limit on a days minimum sample size - possibly convert rows to NA
  distinct(date, .keep_all = TRUE) 
  
describe(cgmData$n)

# Make a lollipop plot of standard deviations for each day
p <- ggplot(cgmData, aes(x=date_time, y=glucose_sd, color=percent_in_range, 
                         text=paste0("Date: ", date, "<br>",
                                    "Percent in range: ", paste0(round(percent_in_range),"%"), "<br>",
                                    "Std. deviation: ", round(glucose_sd, 2)))) +
  geom_segment(aes(x=date_time, xend=date_time, y=0, yend=glucose_sd)) +
  geom_point( size=5,  alpha=0.7)+
  theme_minimal() + 
  scale_color_viridis_c()

figure <- ggplotly(p, tooltip = c("text"))
figure
