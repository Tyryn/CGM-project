

# Setting 90 day intervals from today's date until 1-1-2020 (example)

date_1 <- Sys.Date()
date_2 <- as.Date("2020-1-1")

num_days <- date_1 - date_2
num_periods <- as.numeric(num_days/90)

num_periods <- floor(num_periods)

date_list <- c(date_1)

x <- 1
while(x<=num_periods){
  date_diff <- tail(date_list, n=1) - 90
  date_list <- append(date_list, date_diff)
  print(date_list)
  x <- x +1
}

# Append one day previous to the final date_list element, then append date_2
last_date <- tail(date_list, n=1) + 1
date_list <- append(date_list, date_2)

