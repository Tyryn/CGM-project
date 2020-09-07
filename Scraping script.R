library(rvest)
library(tidyverse)

login <- 'https://carelink.minimed.eu/patient/sso/login?country=za&lang=en'

carelink_login <- html_session(login)

form <- html_form(carelink_login)[[1]]

filled_form <- set_values(form, username = "Tyryn", password = "golfball") %>%
  session_history() 

main_page <- submit_form(carelink_login, filled_form) %>%
  session_history()

x <- main_page %>%
  jump_to("https://carelink.minimed.eu/app/reports") %>%
  read_html() %>%
  html_nodes("#h-reports") 





url<-"https://carelink.minimed.eu/patient/sso/login?country=za&lang=en"
page<-html_session(url)
page<-rvest:::request_POST(page,url="https://carelink.minimed.eu/patient/sso/login?country=za&lang=en",
                           body=list("username"="Tyryn",
                                     "password"="golfball",
                                     "redirect_url"="https://carelink.minimed.eu/app/home"),
                           encode='json'
)
