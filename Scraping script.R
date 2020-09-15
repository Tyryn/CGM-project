library(RSelenium)
library(tidyverse)


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
reports_button <- remote_driver$findElement(using = "id", value = "h-reports")
reports_button$clickElement()

# Set time period ####

# Download CSV ####
csv_button <- remote_driver$findElement(using = "id", value =  "p-button-export-reports")
csv_button$clickElement()
