library(RSelenium)
library(tidyverse)
library(zoo)
library(lubridate)


driver <- RSelenium::rsDriver(browser=c("chrome"), chromever = "85.0.4183.83",
                              port = 4837L)

remote_driver <- driver[["client"]]
remote_driver$open()

# Landing page ####
remote_driver$navigate("https://carelink.minimed.eu/patient/sso/login?country=za&lang=en")

username_textfield <- remote_driver$findElement(using = "id", value = "username")
username_textfield$sendKeysToElement(list("Tyryn"))

password_textfield <- remote_driver$findElement(using = "id", value = "password")
password_textfield$sendKeysToElement(list("golfball"))

submit_button <- remote_driver$findElement(using = "name", value = "actionButton")
submit_button$clickElement()

# Home page ####
Sys.sleep(12)
reports_button <- remote_driver$findElement(using = "id", value = "h-reports")
reports_button$clickElement()




### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Dates for scraping script ####
### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###

# Setting 90 day intervals from today's date until 1-1-2020 (example)
date_1 <- Sys.Date() - 30
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

# Order
date_list <- date_list[rev(order(date_list))]

# # Convert dates to Month Year, Day
# date_example <- format(as.Date(date_list[[1]]), "%B %Y")
# day_example <- format(as.Date(date_list[[1]]), "%d")





### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###
# Set time period ####
### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###

rollapply(date_list, 2, function(x) {
    
  # Refresh page (so that the loop works)
    remote_driver$refresh()
    Sys.sleep(10)
    date_pair <- paste0(x, collapse = "|")
    start_date <- as.Date(sub("\\|.*","",date_pair))
    end_date <- as.Date(substring(date_pair, 12)) # Remove first 11 characters
    if(start_date<end_date){    # To cut down the loop items
      return()
    }
    if(start_date-end_date<2){
      return()
    }
    # print(start_date)
    # print(end_date)
    #print(date_pair)

  # Convert dates to Month Year, Day
  start_month <- as.character(format(as.Date(start_date), "%B %Y"))
  start_day <-  as.character(format(as.Date(start_date), "%d"))

  
  end_month <- as.character(format(as.Date(end_date), "%B %Y"))
  end_day <-  as.character(format(as.Date(end_date), "%d"))
  


  Sys.sleep(4)
  calendar_button <- remote_driver$findElement(using = "class", 
                                               value = "cl-reports-custom-range-button")
  calendar_button$clickElement()
  

  
  # If month year does not equal June 2020 in the either of the two calendars, click back.
  Sys.sleep(4)
  month<- remote_driver$findElement(using = "class", value = "cl-calendar-header-month")
  month_value <- month$getElementText()
  month_value <- unlist(month_value)
  
  # Try the clicking the previous month button
  month_button <- remote_driver$findElement(using = "css", "[class='cl-calendar-header-month-offset _prev ng-star-inserted']")
  month_button$clickElement()
  
  # Start date ####
  # Create function that finds the start month from string value June 2020
 # start_month <- "May 2020"
  print(start_month)
  print(month_value)
  
  while(start_month!=month_value) {
    calendar_month <- remote_driver$findElement(using = "class", value = "cl-calendar-header-month")
    month_value <- unlist(calendar_month$getElementText())
    month_button <- remote_driver$findElement(using = "css", "[class='cl-calendar-header-month-offset _prev ng-star-inserted']")
    month_button$clickElement()
    month_value <- floor_date(parse_date_time(month_value, "my") - months(1))
    month_value <- format(as.Date(month_value), "%B %Y")
  }
  # month_value_2 <- floor_date(parse_date_time(month_value, "my") - months(1))
  # month_value_2 <- format(as.Date(month_value_2), "%B %Y")
  # month_value_2 <- format(as.Date(parse_date_time(month_value, "my")), "%B %Y")
  
  # Start day
 # start_day <- "8"   # This value will need to change
  day_value <- "1"
  print(start_day)
  print(day_value)
  for(x in c(5:35)){
    day <- remote_driver$findElement(using = "xpath", paste0("/html/body/app-root/app-dashboard-wrapper/div/mat-sidenav-container/mat-sidenav-content/div[2]/app-reports/div/div[2]/div/div[2]/div/div[3]/div[3]/div[2]/app-daterange/div[2]/div[3]/div[", x, "]/div"))
    day_value <- unlist(day$getElementText())
    if(start_day==day_value){
      break
    }
  }
  day$clickElement()

  # End date ####
#  end_month <- "March 2020"
  
  while(end_month!=month_value) {
    calendar_month <- remote_driver$findElement(using = "class", value = "cl-calendar-header-month")
    month_value <- unlist(calendar_month$getElementText())
    month_button <- remote_driver$findElement(using = "css", "[class='cl-calendar-header-month-offset _prev ng-star-inserted']")
    month_button$clickElement()
    month_value <- floor_date(parse_date_time(month_value, "my") - months(1))
    month_value <- format(as.Date(month_value), "%B %Y")
  }
  
  # End day
#  end_day <- "15"   # This value will need to change
  day_value <- "1"
  for(x in c(5:35)){
    day <- remote_driver$findElement(using = "xpath", paste0("/html/body/app-root/app-dashboard-wrapper/div/mat-sidenav-container/mat-sidenav-content/div[2]/app-reports/div/div[2]/div/div[2]/div/div[3]/div[3]/div[2]/app-daterange/div[2]/div[3]/div[", x, "]/div"))
    day_value <- unlist(day$getElementText())
    if(end_day==day_value){
      break
    }
  }
  day$clickElement()
  
  Sys.sleep(4)
  # Click Continue
  continue <- remote_driver$findElement(using = "xpath", "//*[@id='p-reports-range-modal-continue']")
  continue$clickElement()
  
 
  # Download CSV ####
  csv_button <- remote_driver$findElement(using = "id", value =  "p-button-export-reports")
  csv_button$clickElement()
  Sys.sleep(70)   # Give time to download csv before refresh
}) 
