

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
date_list

# Add a day to each date starting from the second and excluding the second last
date_list_num <- c(3:length(date_list)-1)
date_list_add <- lapply(date_list_num, function(x){
  date_list_add <- date_list[x] + 1
})

date_list <- append(date_list, date_list_add)

class(date_list)

# Order
date_list <- date_list[rev(order(date_list))]

# Convert dates to Month Year, Day
date_example <- format(as.Date(date_list[[4]]), "%B %Y")
day_example <- format(as.Date(date_list[[4]]), "%d")



