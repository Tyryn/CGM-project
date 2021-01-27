# App where csvs can be uploaded and the data analyzed (maybe something so that users can compare against each other?)

# Load packages ####
library(plotly)
library(tidyverse)
library(shiny)
library(zoo)
library(lubridate)
library(scales)
library(shinydashboard)
library(zoo)
library(shinyjs)
library(shinybusy)

# User created functions ####
# Function to make first row column names
header.true <- function(df) {
    names(df) <- as.character(unlist(df[1, ]))
    df[-1, ]
}

# Function to create frames for the animated plotly line graph
accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
        cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
}

# Function to get the mode (for getting the hour with most lows and highs)
getmode <- function(v) {
    uniqv <- unique(v)
    uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Uniform distribution generator with lower and upper bounds
myrunif <- function(n, min=0, max=1) {
    min + (sample(.Machine$integer.max, n) - 1) / (.Machine$integer.max - 1) *
        (max - min)
}


# Increase the file upload limit to 30mb
options(shiny.maxRequestSize = 30 * 1024 ^ 2)


############################ UI #######################################################################

# Below done to make sidebar reactive with horizontal rules disappearing when appropriate

ui <- dashboardPage(
    dashboardHeader(title = "CGM data explorer"),
    dashboardSidebar(
        # Spinner for when the app is busy with any operation
        add_busy_spinner(
            spin = "fading-circle",
            position =  "top-right",
            margins = c(60, 40),
            timeout = 200
        ),
        useShinyjs(),
        # Tag so that the calendars don't get hidden by the title bar
        tags$div(tags$style(
            HTML(".dropdown-menu{z-index:10000 !important;}")
        )),
        
        # Sidebar panel where users upload CSVs
        fluidRow(hr(id = "horizontal_rule1")),
        fileInput(
            "infile",
            "Upload Carelink csv files (multiple possible)",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
        ),
        fluidRow(hr(id = "horizontal_rule2")),
        radioButtons(
            "bar_density",
            choices = c("Bar graph",
                        "Density plot"),
            label = "Select data visualisation:"
        ),
        
        fluidRow(hr(id = "horizontal_rule3")),
        uiOutput("target_range"),
        checkboxInput(
            "interquartile_range",
            "Display middle 50% of readings per time period"
        ),
        fluidRow(hr(id = "horizontal_rule4")),
        checkboxInput("date_comparison", label = "Compare time periods", value = FALSE),
        uiOutput("dateInput_1"),
        uiOutput("dateInput_2"),
        fluidRow(hr(id = "horizontal_rule5")),
        downloadButton('downloadData', 'Download')
        
        # Main panel to display table
    ),
    dashboardBody(
        tabsetPanel(
            id = "tabs",
            type = "tabs",
            tabPanel(
                "Overview",
                value = "A",
                fluidRow(
                    box(
                        title = "# and % days with data",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("daysData_box")
                    ),
                    box(
                        title = "% days >70% in range",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("days70_box")
                    ),
                    box(
                        title = "Best day of the week",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("bestDay_box")
                    ),
                    box(
                        title = "Worst day of the week",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("worstDay_box")
                    )
                ),
                fluidRow(
                    box(
                        title = "Daily average low events",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("meanLow_box")
                    ),
                    box(
                        title = "Daily average high events",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("meanHigh_box")
                    ),
                    box(
                        title = "Hour with most low events",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("hourLow_box")
                    ),
                    box(
                        title = "Hour with most high events",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("hourHigh_box")
                    )
                ),
                fluidRow(
                    box(
                        title = "% in range 12am-7am",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("period00_07_box")
                    ),
                    box(
                        title = "% in range 7am-11am",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("period07_11_box")
                    ),
                    box(
                        title = "% in range 11am-6pm",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("period11_18_box")
                    ),
                    box(
                        title = "% in range 6pm-12am",
                        width = 3,
                        solidHeader = TRUE,
                        status = "primary",
                        uiOutput("period18_00_box")
                    )
                ),
                fluidRow(
                    box(
                        title = "Daily readings",
                        solidHeader = TRUE,
                        status = "primary",
                        width = 12,
                        plotlyOutput("graph"),
                        plotlyOutput("dailyGraph")
                    )
                )
            ),
            tabPanel(
                "Average glucose readings",
                value = "E",
                box(
                    title = "Average glucose reading per time period",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("glucoseAverage_plot")
                )
            ),
            tabPanel(
                "Low and high glucose events",
                value = "C",
                box(
                    title = "When low events are most frequent",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("low_density")
                ),
                box(
                    title = "When high events are most frequent",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("high_density")
                )
            ),
            tabPanel(
                "Performance over time",
                value = "B",
                box(
                    title = "% readings in range - 50 day rolling average",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("cum_inrange")
                )
            ),
            tabPanel(
                "Days of the week",
                value = "D",
                box(
                    title = "Percent in range by day of week",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("day_of_week_bar")
                )
            ),
            tabPanel(
                "Impact of exercise",
                value = "F",
                box(
                    title = "Average glucose 24 hours after exercise - at time of day",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("average_exercise")
                ),
                box(
                    title = "Average glucose 24 hours after exercise - since exercise",
                    solidHeader = TRUE,
                    status = "primary",
                    width = 12,
                    plotlyOutput("average_exercise_24")
                )
            )
        ),
        fluidRow(textOutput("dailyTable"))
    )
)


############################ Server #######################################################################
server <- function(session, input, output) {
    # Hide the sidebar panel widgets dynamic if certain tabs selected ####
    observe({
        shinyjs::toggle(id = "date1", condition = {
            "A" %in% input$tabs ||
                "C" %in% input$tabs ||
                "D" %in% input$tabs || "E" %in% input$tabs || "F" %in% input$tabs
        })
        shinyjs::toggle(id = "date2", condition = {
            "A" %in% input$tabs ||
                "C" %in% input$tabs || "D" %in% input$tabs || "E" %in% input$tabs
        })
        shinyjs::toggle(id = "target_range", condition = {
            "A" %in% input$tabs ||
                "B" %in% input$tabs || "C" %in% input$tabs || "D" %in% input$tabs
        })
        shinyjs::toggle(id = "interquartile_range", condition = {
            "E" %in% input$tabs || "F" %in% input$tabs
        })
        shinyjs::toggle(id = "bar_density", condition = {
            "C" %in% input$tabs
        })
        shinyjs::toggle(id = "horizontal_rule2", condition = {
            "C" %in% input$tabs
        })
        shinyjs::toggle(id = "horizontal_rule4", condition = {
          "A" %in% input$tabs || "C" %in% input$tabs || "D" %in% input$tabs ||
             "E" %in% input$tabs
        })
    })
    
    # Second calendar appears if comparison radio button selected
    observe({
        shinyjs::toggle(id = "date2", condition = {
            TRUE %in% input$date_comparison &&
                (
                    "A" %in% input$tabs ||
                        "C" %in% input$tabs ||
                        "D" %in% input$tabs || "E" %in% input$tabs
                )
        })
        shinyjs::toggle(id = "date_comparison", condition = {
            "A" %in% input$tabs ||
                "C" %in% input$tabs || "D" %in% input$tabs || "E" %in% input$tabs
        })
    })
    
  
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Make blocks UI dynamic ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Need the second block to appear only when the user chooses to compare time periods
    output$daysData_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("daysData1"), align = "center"),
                box(textOutput("daysData2"), align = "center")
            ))
        } else {
            return(box(
                textOutput("daysData1"),
                width = 12,
                align = "center"
            ))
        }
    })
    
    output$days70_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("days_70_1"), align = "center"),
                box(textOutput("days_70_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("days_70_1"),
                width = 12,
                align = "center"
            )))
        }
    })
    
    output$bestDay_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("best_day_1"), align = "center"),
                box(textOutput("best_day_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("best_day_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$worstDay_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("worst_day_1"), align = "center"),
                box(textOutput("worst_day_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("worst_day_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$meanLow_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("average_low_1"), align = "center"),
                box(textOutput("average_low_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("average_low_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$meanHigh_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("average_high_1"), align = "center"),
                box(textOutput("average_high_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("average_high_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$hourLow_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("hour_low_1"), align = "center"),
                box(textOutput("hour_low_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("hour_low_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$hourHigh_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("hour_high_1"), align = "center"),
                box(textOutput("hour_high_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("hour_high_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$period00_07_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("period_00_07_1"), align = "center"),
                box(textOutput("period_00_07_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("period_00_07_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$period00_07_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("period_00_07_1"), align = "center"),
                box(textOutput("period_00_07_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("period_00_07_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$period07_11_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("period_07_11_1"), align = "center"),
                box(textOutput("period_07_11_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("period_07_11_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$period11_18_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("period_11_18_1"), align = "center"),
                box(textOutput("period_11_18_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("period_11_18_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    output$period18_00_box <- renderUI({
        if (input$date_comparison == TRUE) {
            return(fluidRow(
                box(textOutput("period_18_00_1"), align = "center"),
                box(textOutput("period_18_00_2"), align = "center")
            ))
        } else {
            return(fluidRow(box(
                textOutput("period_18_00_1"),
                align = "center",
                width = 12
            )))
        }
    })
    
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # # Clean and append the CSVs together ----
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    getData <- reactive({
        inFile <- input$infile
        if (is.null(inFile)) {
            return(NULL)
        } else {
            # browser()
            numfiles = nrow(inFile)
            csv1 = list()
            
            
            files <- lapply(inFile$datapath, function(y) {
                csv <- read.csv(y, header = FALSE)
                
                # Get a data frame with the exercise data
                csv_exercise <- csv
                names(csv_exercise) <-
                    csv_exercise %>% slice(5) %>% unlist()
                
                csv_exercise <- csv_exercise %>%
                    select(Date, Time, `Event Marker`) %>%
                    filter(`Event Marker` == "Exercise") %>%
                    # Convert to time date values
                    unite("date_time",
                          Date:Time,
                          remove = FALSE,
                          sep = " ") %>%
                    mutate(date_time = str_replace_all(date_time, "/", "-")) %>%
                    mutate(date_time = ymd_hms(date_time, tz = Sys.timezone())) %>%
                    drop_na(date_time)
                
                # Data frame with the glucose level data
                csv <- csv %>%
                  # Keep only the rows with the sensor blood glucose data
                  slice(str_which(V2, "Date")[1]:nrow(.)) %>%
                  slice(2:nrow(.)) %>%
                  slice(str_which(V2, "Date")[1]:nrow(.)) %>%
                  # Keep only datetime and sensor blood glucose columns
                  select(V2, V3, V32) %>%
                  header.true(.) %>%
                  # Convert to time date values
                  unite("date_time",
                        Date:Time,
                        remove = FALSE,
                        sep = " ") %>%
                  mutate(date_time = str_replace_all(date_time, "/", "-")) %>%
                  mutate(date_time = ymd_hms(date_time, tz = Sys.timezone()))
                
                # To account for data inputted with mmol/L or mg/dL
                if ("Sensor Glucose (mmol/L)" %in% colnames(csv)){
                  csv <- csv %>% 
                    mutate(`Sensor Glucose` = as.numeric(`Sensor Glucose (mmol/L)`))  %>%
                    drop_na(date_time) %>% 
                    select(date_time, Date, Time, `Sensor Glucose`)
                } else {
                  csv <- csv %>% 
                    mutate(`Sensor Glucose` = as.numeric(`Sensor Glucose (mg/dL)`))  %>%
                    drop_na(date_time) %>% 
                    select(date_time, Date, Time, `Sensor Glucose`)
                }
                
                # Add row with missing glucose reading if time between reading is greater than 20 minutes
                csv_with_time_added <- csv %>%
                    mutate(time_elapsed = date_time - lag(date_time)) %>%
                    dplyr::filter(time_elapsed < (-1500)) %>%
                    # The +600 is random and just to create another time period.
                    mutate(
                        date_time = date_time + 600,
                        Date = "",
                        Time = "",
                        `Sensor Glucose` = NA
                    ) %>%
                    select(-time_elapsed)
                
                csv <- csv %>%
                    dplyr::bind_rows(csv_exercise) %>%
                    dplyr::bind_rows(csv_with_time_added) %>%
                    arrange(date_time) %>%
                    # Fill in missing dates
                    mutate(Date = as.character(
                        as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())
                    ))
                csv
            })
            # Append the files together
            combined <- do.call(bind_rows, files)
            combined <- combined %>%
                distinct(date_time, .keep_all = TRUE)
        }
    })
    
    # Data per day
    cgmData <- reactive({
        cgmData <- getData() %>%
            mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
            distinct(date_time, .keep_all = TRUE) %>%
            group_by(date) %>%
            drop_na(`Sensor Glucose`) %>%
            mutate(glucose_mean = mean(`Sensor Glucose`)) %>%
            mutate(glucose_sd = sd(`Sensor Glucose`)) %>%
            mutate(
                in_range = ifelse(
                    `Sensor Glucose` >= input$target_range[1] &
                        `Sensor Glucose` <= input$target_range[2],
                    1,
                    0
                )
            ) %>%
            group_by(date) %>%
            add_tally() %>%
            mutate(percent_in_range = (sum(in_range, na.rm = TRUE) / n)) %>%
            filter(n > 200) %>% # Quite a sharp limit on a days minimum sample size - possibly convert rows to NA
            mutate(percent_in_range = (sum(in_range, na.rm = TRUE) / n)) %>%
            # Below is for the daily average low and high events
            mutate(low_event = ifelse(`Sensor Glucose` < input$target_range[1], 1, 0)) %>%
            mutate(high_event = ifelse(`Sensor Glucose` > input$target_range[2], 1, 0)) %>%
            mutate(low_event_first = ifelse(low_event != lag(low_event) &
                                                low_event == 1, 1, 0)) %>%
            mutate(high_event_first = ifelse(high_event != lag(high_event) &
                                                 high_event == 1, 1, 0)) %>%
            group_by(date) %>%
            mutate(count = n()) %>%
            mutate(low_events_day = sum(low_event_first)) %>%
            mutate(high_events_day = sum(high_event_first)) %>%
            mutate(hour = lubridate::hour(date_time)) %>%
            # Redoing the *_event_first so there is another count if sequential events span across hours
            mutate(low_event_first = ifelse((
                low_event != lag(low_event) | hour != lag(hour)
            ) &
                low_event == 1, 1, 0)) %>%
            mutate(high_event_first = ifelse((
                high_event != lag(high_event) |
                    hour != lag(hour)
            ) & high_event == 1, 1, 0)) %>%
            
            # For low blood sugars hours
            # Identify the rows which need to have their hour kept (need to change up the low_event_first identifier later down the line)
            mutate(low_event_hour = if_else(low_event_first == 1, hour, NULL)) %>%
            # Reshape, so that every hour within a group gets its own variable
            group_by(date) %>%
            mutate(seq_value = dense_rank(low_event_hour)) %>%
            spread(seq_value, low_event_hour, sep = "_low_") %>%
            fill(contains("seq_value_low"), .direction = "downup") %>%
            ungroup() %>%
            # For high blood sugars hours
            mutate(high_event_hour = if_else(high_event_first == 1, hour, NULL)) %>%
            # Reshape, so that every hour within a group gets its own variable
            group_by(date) %>%
            mutate(seq_value = dense_rank(high_event_hour)) %>%
            spread(seq_value, high_event_hour, sep = "_high_") %>%
            fill(contains("seq_value_high"), .direction = "downup") %>%
            ungroup() %>%
            select(-c(contains("_NA"))) %>%
            distinct(date, .keep_all = TRUE) %>%
            ungroup() %>%
            # Below is for best and worst day of the week
            mutate(day_of_week = weekdays(date))
        cgmData
    })
    
    # Create mmol/L or mg/dL reactive character string ####
    # Used for labeling graphs etc.
    mmol_or_dL <- reactive({
      if (max(getData()$`Sensor Glucose`, na.rm = TRUE)>30){
        mmol_or_dL <- "(mg/dL)"
      } else {
        mmol_or_dL <-"(mmol/L)"
      }
      mmol_or_dL
    })
    
    # Make target range slider reactive to mmol/L or mg/dL ####
    output$target_range <- renderUI({
      if(mmol_or_dL()=="(mmol/L)") {
        sliderInput(
          "target_range",
          "Target blood glucose range",
          min = 0,
          max = 20,
          value = c(4, 10),
          step = 0.5
        )
      } else {
        sliderInput(
          "target_range",
          "Target blood glucose range",
          min = 25,
          max = 360,
          value = c(72, 180),
          step = 10
        )
      }
    })
    
    # Make date ranges dynamic ####
    start_date <- reactive({
        min(cgmData()$date)
    })
    end_date <- reactive({
        max(cgmData()$date)
    })
    
    output$dateInput_1 <- renderUI({
        if (input$date_comparison == FALSE & !is.null(input$infile)) {
            return(
                dateRangeInput(
                    "date1",
                    "Pick a date range",
                    start = start_date(),
                    end = end_date(),
                    format = "yyyy-mm-dd"
                )
            )
        } else if (input$date_comparison == TRUE &
                   !is.null(input$infile)) {
            return(
                dateRangeInput(
                    "date1",
                    "Pick a date range",
                    start = end_date() - 30,
                    end = end_date(),
                    format = "yyyy-mm-dd"
                )
            )
            
        } else if (is.null(input$infile)) {
            return(
                dateRangeInput(
                    "date1",
                    "Pick a date range",
                    start = Sys.Date() - 30,
                    end = Sys.Date(),
                    format = "yyyy-mm-dd"
                )
            )
        }
    })
    
    output$dateInput_2 <- renderUI({
        if (!is.null(input$infile) & input$date_comparison == TRUE) {
            return(
                dateRangeInput(
                    "date2",
                    "Pick a date range",
                    start = end_date() - 61,
                    end = end_date() - 31,
                    format = "yyyy-mm-dd"
                )
            )
        } else if (input$date_comparison == TRUE) {
            return(
                dateRangeInput(
                    "date2",
                    "Pick a date range",
                    start = Sys.Date() - 61,
                    end = Sys.Date() - 31,
                    format = "yyyy-mm-dd"
                )
            )
            
        } else {
            return(NULL)
        }
        
    })
    
    
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Blocks ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Date range 1 data ####
    date1Data <- reactive({
        date1_data <- cgmData() %>%
            # mutate(Date = as.Date(Date, "%Y-%m-%d", tz=Sys.timezone())) %>%
            dplyr::filter(date >= as.Date(input$date1[1]) &
                              date <= as.Date(input$date1[2])) %>%
            ungroup() %>%
            mutate(n = n()) %>%
            # Count the number of days with over 70% in range
            mutate(count_70 = sum(percent_in_range >= 0.70)) %>%
            # Average daily low and high events
            mutate(low_events_day_average = round(mean(low_events_day, na.rm = TRUE), 1)) %>%
            mutate(high_events_day_average = round(mean(high_events_day, na.rm = TRUE), 1))
        
        
        date1_data
    })
    
    # Date range 2 data ####
    date2Data <- reactive({
        date2_data <- cgmData() %>%
            dplyr::filter(date >= as.Date(input$date2[1]) &
                              date <= as.Date(input$date2[2])) %>%
            ungroup() %>%
            mutate(n = n()) %>%
            # Count the number of days with over 70% in range
            mutate(count_70 = sum(percent_in_range >= 0.70)) %>%
            # Average daily low and high events
            mutate(low_events_day_average = round(mean(low_events_day, na.rm = TRUE), 1)) %>%
            mutate(high_events_day_average = round(mean(high_events_day, na.rm = TRUE), 1))
        
        date2_data
    })
    
    # % of days with CGM data ####
    
    # First date range
    output$daysData1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        
        days_data_1 <- date1Data() %>%
            arrange(date) %>%
            filter(!is.null(date))
        
        diff_days <- days_data_1$n[1]
        
        number_days_picked <-
            difftime(input$date1[2], input$date1[1], units = "days") + 1 # Adding the plus one because dates could fall on the same day
        
        days_data_prop <- paste0(diff_days, ":\n", "(",
                                 round((
                                     as.numeric(diff_days) / as.numeric(number_days_picked)
                                 ) * 100), "%)")
        
        
        days_data_prop
    })
    
    # First date range
    output$daysData2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        
        days_data_2 <- date2Data() %>%
            arrange(date) %>%
            filter(!is.null(date))
        
        diff_days <- days_data_2$n[1]
        
        number_days_picked <-
            difftime(input$date2[2], input$date2[1], units = "days") + 1 # Adding the plus one because dates could fall on the same day
        
        days_data_prop <- paste0(diff_days, ":\n", "(",
                                 round((
                                     as.numeric(diff_days) / as.numeric(number_days_picked)
                                 ) * 100), "%)")
        
        
        days_data_prop
    })
    
    
    # % of days >70% in range ####
    
    # First date range
    output$days_70_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        
        days_over_70_percent <-
            paste0(round((
                date1Data()$count_70[1] / date1Data()$n[1]
            ) * 100), "%")
        days_over_70_percent
        
    })
    
    # Second date range
    output$days_70_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date2Data()), "---"))
        
        days_over_70_percent <-
            paste0(round((
                date2Data()$count_70[1] / date2Data()$n[1]
            ) * 100), "%")
        days_over_70_percent
        
    })
    
    # Best and worst days of the week ####
    # For each day in the data, get the daily % readings in range, then get the average by day of the week
    
    ## First date range
    dayData1 <- reactive({
        day_data <- date1Data() %>%
            ungroup() %>%
            group_by(day_of_week) %>%
            mutate(day_of_week_mean = mean(percent_in_range, na.rm = TRUE)) %>%
            distinct(day_of_week, .keep_all = TRUE) %>%
            select(day_of_week, day_of_week_mean) %>%
            mutate(day_of_week = factor(
                day_of_week,
                levels = c(
                    "Monday",
                    "Tuesday",
                    "Wednesday",
                    "Thursday",
                    "Friday",
                    "Saturday",
                    "Sunday"
                )
            ))
        day_data
    })
    
    # Best and worst days
    output$best_day_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        best_day_percent <- dayData1() %>%
            arrange(desc(day_of_week_mean))
        
        best_day_percent <-
            paste0(best_day_percent$day_of_week[1],
                   ":\n ",
                   paste0(
                       round(best_day_percent$day_of_week_mean[1] * 100),
                       "% IR"
                   ))
        best_day_percent
    })
    
    output$worst_day_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        worst_day_percent <- dayData1() %>%
            arrange(day_of_week_mean)
        
        worst_day_percent <-
            paste0(worst_day_percent$day_of_week[1],
                   ":\n ",
                   paste0(
                       round(worst_day_percent$day_of_week_mean[1] * 100),
                       "% IR"
                   ))
        worst_day_percent
    })
    
    ## Second date range
    dayData2 <- reactive({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        day_data <- date2Data() %>%
            ungroup() %>%
            group_by(day_of_week) %>%
            mutate(day_of_week_mean = mean(percent_in_range, na.rm = TRUE)) %>%
            distinct(day_of_week, .keep_all = TRUE) %>%
            select(day_of_week, day_of_week_mean) %>%
            mutate(day_of_week = factor(
                day_of_week,
                levels = c(
                    "Monday",
                    "Tuesday",
                    "Wednesday",
                    "Thursday",
                    "Friday",
                    "Saturday",
                    "Sunday"
                )
            ))
        day_data
        day_data
    })
    
    # Best and worst days
    output$best_day_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date2Data()), "---"))
        best_day_percent <- dayData2() %>%
            arrange(desc(day_of_week_mean))
        
        best_day_percent <-
            paste0(best_day_percent$day_of_week[1],
                   ":\n ",
                   paste0(
                       round(best_day_percent$day_of_week_mean[1] * 100),
                       "% IR"
                   ))
        best_day_percent
    })
    
    output$worst_day_2 <- renderText({
        worst_day_percent <- dayData2() %>%
            arrange(day_of_week_mean)
        
        worst_day_percent <-
            paste0(worst_day_percent$day_of_week[1],
                   ":\n ",
                   paste0(
                       round(worst_day_percent$day_of_week_mean[1] * 100),
                       "% IR"
                   ))
        worst_day_percent
    })
    
    ### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    # Average low and high events per day ####
    ### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    # Low
    output$average_low_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        date1Data()$low_events_day_average[1]
    })
    output$average_low_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date2Data()), "---"))
        date2Data()$low_events_day_average[1]
    })
    # High
    output$average_high_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        date1Data()$high_events_day_average[1]
    })
    output$average_high_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date2Data()), "---"))
        date2Data()$high_events_day_average[1]
    })
    
    ### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    # Hour with most low and high events ####
    ### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    output$hour_low_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        df <- date1Data() %>%
            select(starts_with("seq_value")) %>%
            gather(low_event_number,
                   hour_low,
                   starts_with("seq_value_low")) %>%
            select(hour_low) %>%
            # gather(high_event_number, hour_high, starts_with("seq_value_high")) %>%
            drop_na()
        
        most_lows_hour <- as.numeric(getmode(df$hour_low))
        most_lows_hour_next <- most_lows_hour + 1
        
        return(paste0(
            sprintf("%02d", most_lows_hour),
            ":00",
            "-",
            sprintf("%02d", most_lows_hour_next),
            ":00"
        ))
        
    })
    
    output$hour_low_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date2Data()), "---"))
        df <- date2Data() %>%
            select(starts_with("seq_value")) %>%
            gather(low_event_number,
                   hour_low,
                   starts_with("seq_value_low")) %>%
            select(hour_low) %>%
            drop_na()
        
        most_lows_hour <- as.numeric(getmode(df$hour_low))
        most_lows_hour_next <- most_lows_hour + 1
        
        return(paste0(
            sprintf("%02d", most_lows_hour),
            ":00",
            "-",
            sprintf("%02d", most_lows_hour_next),
            ":00"
        ))
    })
    
    output$hour_high_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date1Data()), "---"))
        df <- date1Data() %>%
            select(starts_with("seq_value")) %>%
            gather(high_event_number,
                   hour_high,
                   starts_with("seq_value_high")) %>%
            select(hour_high) %>%
            drop_na()
        
        most_highs_hour <- as.numeric(getmode(df$hour_high))
        most_highs_hour_next <- most_highs_hour + 1
        
        return(paste0(
            sprintf("%02d", most_highs_hour),
            ":00",
            "-",
            sprintf("%02d", most_highs_hour_next),
            ":00"
        ))
        # print(most_highs_hour)
        
    })
    
    output$hour_high_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(date2Data()), "---"))
        df <- date2Data() %>%
            select(starts_with("seq_value")) %>%
            gather(high_event_number,
                   hour_high,
                   starts_with("seq_value_high")) %>%
            select(hour_high) %>%
            drop_na()
        
        most_highs_hour <- as.numeric(getmode(df$hour_high))
        most_highs_hour_next <- most_highs_hour + 1
        
        return(paste0(
            sprintf("%02d", most_highs_hour),
            ":00",
            "-",
            sprintf("%02d", most_highs_hour_next),
            ":00"
        ))
    })
    
    ### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    # Time periods in range ####
    ### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    timePeriod <- reactive({
        time_period_df <- getData() %>%
            mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
            distinct(date_time, .keep_all = TRUE) %>%
            mutate(time = format(date_time, "%H:%M:%S")) %>%
            mutate(time_category = ifelse(
                time >= "00:00:00" & time <= "06:59:59",
                "12am-7am",
                ifelse(
                    time >= "07:00:00" & time <= "10:59:59",
                    "7am-11am",
                    ifelse(
                        time >= "11:00:00" & time <= "17:59:59",
                        "11am-6pm",
                        ifelse(time >= "18:00:00" &
                                   time <= "23:59:59", "6pm-12am", "")
                    )
                )
            ))
        time_period_df
        
    })
    
    timePeriod_1 <- reactive({
        time_period_1 <- timePeriod() %>%
            dplyr::filter(date >= input$date1[1] &
                              date <= input$date1[2]) %>%
            mutate(
                in_range = ifelse(
                    `Sensor Glucose` >= input$target_range[1] &
                        `Sensor Glucose` <= input$target_range[2],
                    1,
                    0
                )
            ) %>%
            group_by(time_category) %>%
            mutate(n = n()) %>%
            mutate(count_in_range = sum(in_range, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(time_percent_in_range = count_in_range / n) %>%
            distinct(time_category, .keep_all = TRUE)
        time_period_1
    })
    
    timePeriod_2 <- reactive({
        time_period_2 <- timePeriod() %>%
            dplyr::filter(date >= input$date2[1] &
                              date <= input$date2[2]) %>%
            mutate(
                in_range = ifelse(
                    `Sensor Glucose` >= input$target_range[1] &
                        `Sensor Glucose` <= input$target_range[2],
                    1,
                    0
                )
            ) %>%
            group_by(time_category) %>%
            mutate(n = n()) %>%
            mutate(count_in_range = sum(in_range, na.rm = TRUE)) %>%
            ungroup() %>%
            mutate(time_percent_in_range = count_in_range / n) %>%
            distinct(time_category, .keep_all = TRUE)
        time_period_2
    })
    
    # 12am to 7am
    
    output$period_00_07_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_1()), "---"))
        
        period_00_07_1_df <- timePeriod_1() %>%
            filter(time_category == "12am-7am") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_00_07_1_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    output$period_00_07_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_2()), "---"))
        
        period_00_07_2_df <- timePeriod_2() %>%
            filter(time_category == "12am-7am") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_00_07_2_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    # 7am to 11am
    output$period_07_11_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_1()), "---"))
        
        
        period_07_11_1_df <- timePeriod_1() %>%
            filter(time_category == "7am-11am") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_07_11_1_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    output$period_07_11_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_2()), "---"))
        
        period_07_11_2_df <- timePeriod_2() %>%
            filter(time_category == "7am-11am") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_07_11_2_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    # 11am to 6pm
    output$period_11_18_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_1()), "---"))
        
        period_11_18_1_df <- timePeriod_1() %>%
            filter(time_category == "11am-6pm") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_11_18_1_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    output$period_11_18_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_2()), "---"))
        
        period_11_18_2_df <- timePeriod_2() %>%
            filter(time_category == "11am-6pm") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_11_18_2_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    # 6pm to 12am
    output$period_18_00_1 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_1()), "---"))
        
        
        period_18_00_1_df <- timePeriod_1() %>%
            filter(time_category == "6pm-12am") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_18_00_1_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    output$period_18_00_2 <- renderText({
        validate(need(!is.null(getData()), "---"))
        validate(need(!is.null(timePeriod_2()), "---"))
        
        
        period_18_00_2_df <- timePeriod_2() %>%
            filter(time_category == "6pm-12am") %>%
            select(time_percent_in_range)
        
        percent_in_range <-
            paste0(round(period_18_00_2_df$time_percent_in_range[1] * 100),
                   "%")
        percent_in_range
    })
    
    
    
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Blood sugar within range graphs ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    # Create graph showing the days with best average blood glucose and lowest standard deviation
    
    
    
    output$graph <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        # For the colored rectangles
        if (input$date_comparison == TRUE) {
            switch = TRUE
        } else {
            switch = FALSE
        }
        
        p <-
            ggplot(cgmData(),
                   aes(
                       x = date,
                       y = percent_in_range,
                       color = glucose_sd,
                       text = paste0(
                           "Date: ",
                           date,
                           "<br>",
                           "Percent in range: ",
                           paste0(round(percent_in_range * 100), "%"),
                           "<br>",
                           "Std. deviation: ",
                           round(glucose_sd, 2)
                       )
                   )) +
            # Get colored bars showing the selected date range
            annotate(
                "rect",
                xmin = input$date1[2],
                xmax = input$date1[1],
                ymin = 0,
                ymax = 1,
                fill = "blue",
                alpha = .2,
                color = NA
            ) +
            {
                if (switch)
                    annotate(
                        "rect",
                        xmin = input$date2[2],
                        xmax = input$date2[1],
                        ymin = 0,
                        ymax = 1,
                        fill = "red",
                        alpha = .2,
                        color = NA
                    )
            } +
            # Create lollipops
            geom_segment(aes(
                x = date,
                xend = date,
                y = 0,
                yend = percent_in_range
            )) +
            geom_point(size = 5,  alpha = 0.7) + scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                                                    limits = c(0, 1)) +
            theme_minimal() + xlab("") + ylab("% readings in range") 
          
          if(mmol_or_dL()=="(mg/dL)"){
          p <- p +  scale_color_gradient(
              high = "#FF0000",
              low = "#66ff00",
              name = "Daily blood \nglucose variability",
              breaks = c(9, 99),
              labels = c("Low", "High"),
              limits = c(1.8, 108)
            )
          } else {
            p <- p +  scale_color_gradient(
              high = "#FF0000",
              low = "#66ff00",
              name = "Daily blood \nglucose variability",
              breaks = c(0.5, 5.5),
              labels = c("Low", "High"),
              limits = c(0.1, 6)
            )
          }
        
        
        p <- ggplotly(p, tooltip = c("text"), source = "date") %>%
            event_register("plotly_click")
        p
    })
    
    dateClicked <- reactive({
        eventDate <- event_data(event = "plotly_click", source = "date")
        rowNumber <- eventDate[, 2] + 1
        
        data <- cgmData() %>%
            ungroup() %>%
            slice(rowNumber)
        
        date_clicked <- data[[1, 2]]
        date_clicked
        
    })
    
    dailyData <- reactive({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        validate(
            need(
                !is.null(event_data(
                    event = "plotly_click", source = "date"
                )),
                "Click a point in the graph above to show the readings for that day"
            )
        )
        validate(
            need(
                is.character(dateClicked()),
                "Click a point in the graph above to show the readings for that day"
            )
        )
        
        
        daily_data <- getData() %>%
            filter(Date == dateClicked()) %>%
            rename(`Date-time` = date_time)
        daily_data
    })
    
    output$dailyGraph <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        validate(
            need(
                !is.null(event_data(
                    event = "plotly_click", source = "date"
                )),
                "Click a point in the graph above to show the readings for that day"
            )
        )
        validate(
            need(
                is.character(dateClicked()),
                "Click a point in the graph above to show the readings for that day"
            )
        )
        
        
        # To create start and end times
        start_time <-
            as.POSIXct(paste(dailyData()$Date[[1]], "00:00:01"), format = "%Y-%m-%d %H:%M:%S")
        end_time <-
            as.POSIXct(paste(dailyData()$Date[[1]], "23:59:59"), format = "%Y-%m-%d %H:%M:%S")
        
        # Graph
        plot <-
            ggplot(dailyData(),
                   aes(x = `Date-time`, y = `Sensor Glucose`)) +
            annotate(
                "rect",
                xmin = start_time,
                xmax = end_time,
                ymin = input$target_range[1],
                ymax = input$target_range[2],
                fill = "green",
                alpha = .2,
                color = NA
            ) +
            geom_line() + xlab("") + theme_minimal() 
        
        if(mmol_or_dL()=="(mmol/L)"){
          plot <- plot + scale_y_continuous(limits = c(0, 25), name = "Sensor glucose (mmol/L)") 
        } else {
          plot <- plot + scale_y_continuous(limits = c(0*18, 25*18), name = "Sensor glucose (mg/dL)") 
        }
        
        plot <- ggplotly(plot)
        
        plot
    })
    
    output$dailyTable <- renderText({
       NULL #print(class(exercise_df_24()$time_elapsed_exercise))
    })
    
    
    # #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
    # Cumulative average days in range ####
    # #### #### #### #### #### #### #### #### #### #### #### #### #### #### #### ###
    
    cumAverage_df <- reactive({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        validate(need(nrow(cgmData()>=200), "Need at least 50 days of data"))
  
            df <- cgmData() %>%
                ungroup() %>%
                mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
                arrange(date)  %>%
                select(date, percent_in_range) %>%
                drop_na() %>%
                mutate(
                    in_range_cum_mean =  zoo::rollmean(
                        percent_in_range,
                        50,
                        fill = NA,
                        align = "right"
                    )
                ) 
        df
    })
    
    
    
    output$cum_inrange <- renderPlotly({
      validate(need(!is.null(getData()), "No data uploaded yet"))
      
      ymin <- max(0, (min(cumAverage_df()$in_range_cum_mean, na.rm = TRUE)-0.15), na.rm = TRUE)
      ymax <- min(1, (max(cumAverage_df()$in_range_cum_mean, na.rm = TRUE)+0.15), na.rm = TRUE)
      
      plot <-
        ggplot(cumAverage_df(),
               aes(
                 x = date,
                 y = in_range_cum_mean, group=1,
                 text = paste0("Date: ",
                               date,
                               "<br>",
                               "Percent in range: ",
                               paste0(round(in_range_cum_mean*100,1),"%"))
               )) +
        geom_line() + theme_minimal() +  scale_x_date(name = "") +
        scale_y_continuous(labels = scales::percent_format(accuracy = 1),name = "% readings in range (rolling average)",
                           limits = c(ymin, ymax))
      
      plot <- ggplotly(plot, tooltip = c("text")) 
      
      plot
        
    })
    
    
    # #### #### #### #### #### #### #### #### #### #### #### #### ###
    # Bar graph and density plot of low and high events ####
    # #### #### #### #### #### #### #### #### #### #### #### #### ###
    
    
    
    date_densityData <- reactive({
        if (input$date_comparison == TRUE) {
            df <-  getData() %>%
                mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
                mutate(
                    date_rangeIndicator = ifelse(
                        date >= as.Date(input$date1[1]) & date <= as.Date(input$date1[2]),
                        "Date range 1",
                        ifelse(
                            date >= as.Date(input$date2[1]) & date <= as.Date(input$date2[2]),
                            "Date range 2",
                            "Not in range"
                        )
                    )
                ) %>%
                dplyr::filter(date_rangeIndicator != "Not in range") %>%
                mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) %>%
                mutate(low_event = ifelse(
                    `Sensor Glucose` < input$target_range[1],
                    1,
                    0
                )) %>%
                mutate(high_event = ifelse(
                    `Sensor Glucose` > input$target_range[2],
                    1,
                    0
                ))
        } else if (input$date_comparison == FALSE)  {
            df <-  getData() %>%
                mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
                dplyr::filter(date >= as.Date(input$date1[1]) &
                                  date <= as.Date(input$date1[2])) %>%
                mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) %>%
                mutate(low_event = ifelse(
                    `Sensor Glucose` < input$target_range[1],
                    1,
                    0
                )) %>%
                mutate(high_event = ifelse(
                    `Sensor Glucose` > input$target_range[2],
                    1,
                    0
                )) %>%
                mutate(date_rangeIndicator = "Date range 1")
        }
        
        df
    })
    
    lowBar_df <- reactive({
        df <- date_densityData() %>%
            # Add time periods of 30 minute periods for the bars
            mutate(Time_round = lubridate::round_date(Time, "30 minutes")) %>%
            group_by(Date, Time_round, date_rangeIndicator) %>%
            filter(low_event == 1) %>%
            distinct(Time_round, .keep_all = TRUE) %>%
            ungroup() %>%
            group_by(Time_round, date_rangeIndicator) %>%
            add_count(low_event, name = "low_event_count") %>%
            distinct(Time_round, .keep_all = TRUE) %>%
            mutate(Time_round = as.POSIXct(Time_round, format = "%H:%M:%S", tz = Sys.timezone())) %>%
            ungroup()
        
    })
    
    highBar_df <- reactive({
        df <- date_densityData() %>%
            # Add time periods of 30 minute periods for the bars
            mutate(Time_round = lubridate::round_date(Time, "30 minutes")) %>%
            group_by(Date, Time_round, date_rangeIndicator) %>%
            filter(high_event == 1) %>%
            distinct(Time_round, .keep_all = TRUE) %>%
            ungroup() %>%
            group_by(Time_round, date_rangeIndicator) %>%
            add_count(low_event, name = "high_event_count") %>%
            distinct(Time_round, .keep_all = TRUE) %>%
            mutate(Time_round = as.POSIXct(Time_round, format = "%H:%M:%S", tz = Sys.timezone())) %>%
            ungroup()
        
    })
    
    
    output$low_density <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        df <-  date_densityData() %>%
            filter(low_event == 1)
        
        
        if (input$date_comparison == TRUE &
            input$bar_density == "Density plot") {
            plot <-
                ggplot(df, aes(x = Time)) + geom_density(aes(fill = date_rangeIndicator,
                                                             group = date_rangeIndicator),
                                                         alpha = 0.3) +
                scale_fill_manual(values = c("blue", "red")) +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                theme(
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_blank(),
                    legend.title = element_blank()
                )
        } else if (input$date_comparison == FALSE &
                   input$bar_density == "Density plot") {
            plot <-
                ggplot(df, aes(x = Time)) + geom_density(fill = "blue", alpha = 0.3) +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                theme(
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_blank()
                )
            
        } else if (input$date_comparison == FALSE &
                   input$bar_density == "Bar graph") {
            plot <- ggplot(lowBar_df(),
                           aes(x = Time_round, y = low_event_count, text = paste0("Time: ",
                                                                                  strftime(Time_round, format = "%H:%M"),
                                                                               "<br>",
                                                                               "Low events: ",
                                                                               low_event_count))) +
                geom_bar(stat = "identity",
                         fill = "blue",
                         alpha = 0.5)  +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                ylab("Number of low events") + scale_y_continuous(breaks = pretty_breaks()) +
                theme(axis.title.x = element_blank())
        } else {
            plot <- ggplot(
                lowBar_df(),
                aes(
                    x = Time_round,
                    y = low_event_count,
                    fill = date_rangeIndicator,
                    text = paste0("Time: ",
                                  strftime(Time_round, format = "%H:%M"),
                                  "<br>",
                                  "Low events: ",
                                  low_event_count)
                )
            ) +
                geom_bar(stat = "identity",
                         position = "dodge",
                         alpha = 0.5)  +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                scale_fill_manual(values = c("blue", "red")) +
                ylab("Number of low events") + scale_y_continuous(breaks = pretty_breaks()) +
                theme(axis.title.x = element_blank(),
                      legend.title = element_blank())
        }
        
        plot <- ggplotly(plot, tooltip = c("text"))
        plot
    })
    
    output$high_density <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        df <- date_densityData() %>%
            filter(high_event == 1)
        
        if (input$date_comparison == TRUE &
            input$bar_density == "Density plot") {
            plot <-
                ggplot(df, aes(x = Time)) + geom_density(aes(fill = date_rangeIndicator,
                                                             group = date_rangeIndicator),
                                                         alpha = 0.3) +
                scale_fill_manual(values = c("blue", "red")) +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                theme(
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_blank(),
                    legend.title = element_blank()
                )
        } else if (input$date_comparison == FALSE &
                   input$bar_density == "Density plot") {
            plot <-
                ggplot(df, aes(x = Time)) + geom_density(fill = "blue", alpha = 0.3) +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                theme(
                    axis.title.y = element_blank(),
                    axis.title.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.grid.minor.y = element_blank(),
                    panel.grid.major.y = element_blank()
                )
            
        } else if (input$date_comparison == FALSE &
                   input$bar_density == "Bar graph") {
            plot <-
                ggplot(highBar_df(),
                       aes(x = Time_round, y = high_event_count,
                           text = paste0("Time: ",
                                                                              strftime(Time_round, format = "%H:%M"),
                                                                              "<br>",
                                                                              "High events: ",
                                                                              high_event_count))) +
                geom_bar(stat = "identity",
                         fill = "blue",
                         alpha = 0.5)  +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                ylab("Number of high events") + scale_y_continuous(breaks = pretty_breaks()) +
                theme(axis.title.x = element_blank())
        } else {
            plot <- ggplot(
                highBar_df(),
                aes(
                    x = Time_round,
                    y = high_event_count,
                    fill = date_rangeIndicator,
                    text = paste0("Time: ",
                                  strftime(Time_round, format = "%H:%M"),
                                  "<br>",
                                  "High events: ",
                                  high_event_count)
                )
            ) +
                geom_bar(stat = "identity",
                         position = "dodge",
                         alpha = 0.5)  +
                scale_x_datetime(labels = date_format("%H:%M", tz = Sys.timezone()),
                                 breaks = "2 hours") + theme_minimal() +
                scale_fill_manual(values = c("blue", "red")) +
                ylab("Number of high events") + scale_y_continuous(breaks = pretty_breaks()) +
                theme(axis.title.x = element_blank(),
                      legend.title = element_blank())
        }
        
        plot <- ggplotly(plot, tooltip = "text")
        plot
    })
    
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Percent in range, day of week ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    output$day_of_week_bar <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        if (input$date_comparison == FALSE) {
            p <- ggplot(data = dayData1(),
                        aes(
                            x = day_of_week,
                            y = day_of_week_mean,
                            label = paste0(round(day_of_week_mean * 100), "%")
                        )) +
                geom_bar(stat = "identity",
                         fill = "blue",
                         alpha = 0.5) +
                scale_y_continuous(
                    labels = scales::percent_format(accuracy = 1),
                    limits = c(0, 1)
                ) +
                xlab("") + ylab("Percent in range") + theme_minimal() +
                geom_text(size = 3)
        } else {
            dayData1 <- dayData1() %>%
                select(day_of_week, day_of_week_mean) %>%
                mutate(date_range = "Date range 1")
            dayData2 <- dayData2() %>%
                select(day_of_week, day_of_week_mean) %>%
                mutate(date_range = "Date range 2")
            
            dayData_merged <- bind_rows(dayData1, dayData2)
            
            p <- ggplot(
                data = dayData_merged,
                aes(
                    x = day_of_week,
                    y = day_of_week_mean,
                    fill = date_range,
                    label = paste0(round(day_of_week_mean * 100), "%")
                )
            ) +
                geom_bar(position = "dodge" ,
                         stat = "identity",
                         alpha = 0.5) +
                scale_fill_manual(values = c("blue", "red")) +
                scale_y_continuous(
                    labels = scales::percent_format(accuracy = 1),
                    limits = c(0, 1)
                ) +
                xlab("") + ylab("Percent in range") + theme_minimal() +
                geom_text(size = 3, position = position_dodge(0.9)) +
                theme(legend.title = element_blank())
            
        }
        
        p <- ggplotly(p, tooltip = "none")
        p
    })
    
    #### #### #### #### #### #### #### #### #### #### #### #### ### #
    # Daily average glucose readings ####
    ### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    
    dailyAverage_df <- reactive({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        if (input$date_comparison == FALSE) {
            df <- getData() %>%
                mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
                filter(date >= as.Date(input$date1[1]) &
                           date <= as.Date(input$date1[2])) %>%
                mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) %>%
                mutate(Time_round = lubridate::round_date(Time, "15 minutes")) %>%
                group_by(Time_round) %>%
                mutate(glucose_mean = mean(`Sensor Glucose`, na.rm = TRUE)) %>%
                mutate(pLow = quantile(
                    `Sensor Glucose`,
                    probs = 0.25,
                    na.rm = TRUE
                )) %>%
                mutate(pHigh = quantile(
                    `Sensor Glucose`,
                    probs = 0.75,
                    na.rm = TRUE
                )) %>%
                distinct(Time_round, .keep_all = TRUE) %>%
                ungroup() %>%
                drop_na(Time_round) %>%
                arrange(Time_round)
        } else {
            df <- getData() %>%
                mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) %>%
                mutate(Time_round = lubridate::round_date(Time, "15 minutes")) %>%
                mutate(
                    date_rangeIndicator = ifelse(
                        Date >= as.Date(input$date1[1]) & Date <= as.Date(input$date1[2]),
                        "Date range 1",
                        ifelse(
                            Date >= as.Date(input$date2[1]) & Date <= as.Date(input$date2[2]),
                            "Date range 2",
                            "Not in range"
                        )
                    )
                ) %>%
                dplyr::filter(date_rangeIndicator != "Not in range") %>%
                group_by(Time_round, date_rangeIndicator) %>%
                mutate(glucose_mean = mean(`Sensor Glucose`, na.rm = TRUE)) %>%
                mutate(pLow = quantile(
                    `Sensor Glucose`,
                    probs = 0.25,
                    na.rm = TRUE
                )) %>%
                mutate(pHigh = quantile(
                    `Sensor Glucose`,
                    probs = 0.75,
                    na.rm = TRUE
                )) %>%
                distinct(Time_round, date_rangeIndicator, .keep_all = TRUE) %>%
                drop_na(Time_round) %>%
                ungroup() %>%
                arrange(Time_round)
        }
    })
    
    
    
    output$glucoseAverage_plot <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        if (input$interquartile_range == TRUE) {
            if (input$date_comparison == FALSE) {
                dailyAverage_df() %>%
                    plot_ly(
                        x =  ~ Time_round,
                        y =  ~ glucose_mean,
                        type = 'scatter',
                        mode = 'lines',
                        name = "Average sensor glucose",
                        hoverinfo = 'text',
                        text = ~ paste0(
                            "Time: ",
                            strftime(Time_round, format = "%H:%M"),
                            "<br>",
                            "Sensor glucose: ",
                            round(glucose_mean, 1)
                        )
                    ) %>%
                    add_ribbons(
                        data = dailyAverage_df(),
                        ymin = ~ pLow,
                        ymax = ~ pHigh,
                        line = list(color = 'rgba(7, 164, 181, 0.05)'),
                        fillcolor = 'rgba(0, 0, 225, 0.3)',
                        name = 'Middle 50% of readings per time period'
                    ) %>%
                    layout(
                        yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                     ticklen = 5),
                        xaxis = list(
                            type = "date",
                            tickformat = "%H:%M",
                            title = ''
                        )
                    )
                
            } else {
                df_1 <-
                    dailyAverage_df() %>% filter(date_rangeIndicator == "Date range 1")
                df_2 <-
                    dailyAverage_df() %>% filter(date_rangeIndicator == "Date range 2")
                
                df_1 %>%
                    plot_ly(
                        x =  ~ Time_round,
                        y =  ~ glucose_mean,
                        type = 'scatter',
                        mode = 'lines',
                        legendgroup = 'group1',
                        name = "Date range 1 \nAverage sensor glucose",
                        hoverinfo = 'text',
                        text = ~ paste0(
                            "Time: ",
                            strftime(Time_round, format = "%H:%M"),
                            "<br>",
                            "Sensor glucose: ",
                            round(glucose_mean, 1)
                        )
                    ) %>%
                    add_ribbons(
                        data = df_1,
                        ymin = ~ pLow,
                        ymax = ~ pHigh,
                        line = list(color = 'rgba(7, 164, 181, 0.05)'),
                        fillcolor = 'rgba(7, 164, 181, 0.2)',
                        legendgroup = 'group1',
                        name = 'Date range 1 \nMiddle 50% of readings per time period'
                    ) %>%
                    add_lines(
                        data = df_2,
                        x =  ~ Time_round,
                        y =  ~ glucose_mean,
                        legendgroup = 'group2',
                        name = "Date range 2 \nAverage sensor glucose",
                        line = list(color = 'rgba(255, 0, 0, 0.8)')
                    ) %>%
                    add_ribbons(
                        data = df_2,
                        ymin = ~ pLow,
                        ymax = ~ pHigh,
                        line = list(color = 'rgba(255, 0, 0, 0.05)'),
                        fillcolor = 'rgba(255, 0, 0, 0.2)',
                        legendgroup = 'group2',
                        name = 'Date range 2 \nMiddle 50% of readings per time period'
                    ) %>%
                    layout(
                        yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                     ticklen = 5),
                        xaxis = list(
                            type = "date",
                            tickformat = "%H:%M",
                            title = ''
                        )
                    )
                
            }
        } else {
            if (input$date_comparison == FALSE) {
                dailyAverage_df() %>%
                    plot_ly(
                        x =  ~ Time_round,
                        y =  ~ glucose_mean,
                        type = 'scatter',
                        mode = 'lines',
                        name = "Average sensor glucose",
                        hoverinfo = 'text',
                        text = ~ paste0(
                            "Time: ",
                            strftime(Time_round, format = "%H:%M"),
                            "<br>",
                            "Sensor glucose: ",
                            round(glucose_mean, 1)
                        )
                    ) %>%
                    layout(
                        yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                     ticklen = 5),
                        xaxis = list(
                            type = "date",
                            tickformat = "%H:%M",
                            title = ''
                        )
                    )
                
            } else {
                df_1 <-
                    dailyAverage_df() %>% filter(date_rangeIndicator == "Date range 1")
                df_2 <-
                    dailyAverage_df() %>% filter(date_rangeIndicator == "Date range 2")
                
                df_1 %>%
                    plot_ly(
                        x =  ~ Time_round,
                        y =  ~ glucose_mean,
                        type = 'scatter',
                        mode = 'lines',
                        legendgroup = 'group1',
                        name = "Date range 1 \nAverage sensor glucose",
                        hoverinfo = 'text',
                        text = ~ paste0(
                            "Time: ",
                            strftime(Time_round, format = "%H:%M"),
                            "<br>",
                            "Sensor glucose: ",
                            round(glucose_mean, 1)
                        )
                    ) %>%
                    add_lines(
                        data = df_2,
                        x =  ~ Time_round,
                        y =  ~ glucose_mean,
                        legendgroup = 'group2',
                        name = "Date range 2 \nAverage sensor glucose",
                        line = list(color = 'rgba(255, 0, 0, 0.8)')
                    ) %>%
                    layout(
                        yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                     ticklen = 5),
                        xaxis = list(
                            type = "date",
                            tickformat = "%H:%M",
                            title = ''
                        )
                    )
                
            }
        }
        
    })
    
    #### #### #### #### #### #### #### #### #### #### #### #### ### #
    # Impact of exercise ####
    ### #### #### #### #### #### #### #### #### #### #### #### #### #
    
    exercise_df <- reactive({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        df_exercise <- getData() %>%
            # Create data frame that consists of the blood glucose readings 12 hours after exercise
            # mutate(`Event Marker` = replace_na(`Event Marker`, "Nothing")) %>%
            mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
            filter(date >= as.Date(input$date1[1]) &
                       date <= as.Date(input$date1[2])) %>%
            mutate(exercise_group = ifelse(`Event Marker` == "Exercise", seq(1, 10000000, by =
                                                                                 1), NA)) %>%
            fill(exercise_group, .direction = "down") %>%
            # By group, check if the time is within twelve hours of the first observations
            group_by(exercise_group) %>%
            mutate(exercise_time = first(date_time)) %>%
            mutate(exercise_within_time_12 = if_else(
                difftime(date_time, exercise_time, units = "hours") <= 12,
                1,
                0
            )) %>%
            mutate(exercise_within_time_24 = if_else(
                difftime(date_time, exercise_time, units = "hours") <= 24,
                1,
                0
            )) 
    })
    

    
    exercise_df_24 <- reactive({
        exercise_df() %>%
            filter(exercise_within_time_24 == 1 &
                       !is.na(exercise_group) & is.na(`Event Marker`)) %>%
            select(date_time, Date, Time, `Sensor Glucose`, `Event Marker`) %>%
            # Need to rename date_time and Sensor.glucose. to match names in other dataframe
            mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) %>%
            mutate(Time_round = lubridate::round_date(Time, "15 minutes")) %>%
            group_by(Time_round) %>%
            mutate(glucose_mean = mean(`Sensor Glucose`, na.rm = TRUE)) %>%
            mutate(pLow = quantile(
                `Sensor Glucose`,
                probs = 0.25,
                na.rm = TRUE
            )) %>%
            mutate(pHigh = quantile(
                `Sensor Glucose`,
                probs = 0.75,
                na.rm = TRUE
            )) %>%
            distinct(Time_round, .keep_all = TRUE) %>%
            drop_na(Time_round) %>%
            ungroup() %>%
            arrange(Time_round)
        
    })
    
    no_exerciseAverage_df <-  reactive({
        exercise_df() %>%
            filter(exercise_within_time_24 != 1) %>%
            select(date_time, Date, Time, `Sensor Glucose`) %>%
            # Need to rename date_time and Sensor.glucose. to match names in other dataframe
            mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) %>%
            mutate(Time_round = lubridate::round_date(Time, "15 minutes")) %>%
            group_by(Time_round) %>%
            mutate(glucose_mean = mean(`Sensor Glucose`, na.rm = TRUE)) %>%
            mutate(pLow = quantile(
                `Sensor Glucose`,
                probs = 0.25,
                na.rm = TRUE
            )) %>%
            mutate(pHigh = quantile(
                `Sensor Glucose`,
                probs = 0.75,
                na.rm = TRUE
            )) %>%
            distinct(Time_round, .keep_all = TRUE) %>%
            drop_na(Time_round) %>%
            ungroup() %>%
            arrange(Time_round)
    })
    
    # Generate df that is only the exercise start times
    exercise_df_start <- reactive({
        df <- exercise_df() %>% 
            filter(`Event Marker` == "Exercise") %>% 
            mutate(Time = as.POSIXct(Time, format = "%H:%M:%S", tz = Sys.timezone())) 
            
        if(mmol_or_dL()=="mmol/L"){
          df$y <- myrunif(nrow(df), min=0.5, max=1)
        } else {
          df$y <- myrunif(nrow(df), min=10, max=20)
        }
        df
    })
    
    exercise_df_24_elapse <- reactive({
        df <- exercise_df() %>%
            filter(exercise_within_time_24 == 1 &
                       !is.na(exercise_group) & is.na(`Event Marker`)) %>%
            arrange(date_time) %>% 
            group_by(exercise_group) %>% 
            mutate(group_first_time = first(date_time)) %>% 
            mutate(time_elapsed_round = (round_date(date_time, "15 minutes"))) %>%
            mutate(group_first_time_round = (round_date(group_first_time, "15 minutes"))) %>%
            mutate(time_elapsed_exercise = difftime(time_elapsed_round, group_first_time_round, units = "secs",
                                                    tz = Sys.timezone())) %>% 
            mutate(time_elapsed_exercise = seconds_to_period(time_elapsed_exercise)) %>% 
            # Get average by 15 minutes, by exercise group
            group_by(time_elapsed_exercise) %>% 
            mutate(glucose_mean = mean(`Sensor Glucose`, na.rm = TRUE)) %>% 
            mutate(pLow = quantile(
                `Sensor Glucose`,
                probs = 0.25,
                na.rm = TRUE
            )) %>%
            mutate(pHigh = quantile(
                `Sensor Glucose`,
                probs = 0.75,
                na.rm = TRUE
            )) %>%
            ungroup() %>% 
            distinct(time_elapsed_exercise, .keep_all = TRUE) %>% 
            # Need to change time elapsed to factor
            arrange(time_elapsed_exercise) %>% 
            mutate(time_elapsed_exercise = as.character(time_elapsed_exercise)) %>% 
            mutate(time_elapsed_exercise = sub("^(\\S*\\s+\\S+).*", "\\1", time_elapsed_exercise))
        
        levels_df <- df %>% 
            pull(time_elapsed_exercise)
        
        df$time_elapsed_exercise <- factor(df$time_elapsed_exercise, levels = levels_df)
        
        df
        
    })
    
    
    
    
    output$average_exercise <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
        if (input$interquartile_range == TRUE) {
            plot <-
                plot_ly() %>%
                add_lines(
                    data = no_exerciseAverage_df(),
                    x =  ~ Time_round,
                    y =  ~ glucose_mean,
                    type = 'scatter',
                    mode = 'lines',
                    name = "Average sensor glucose - no exercise",
                    line = list(
                        color = "black",
                        widthh = 0.5,
                        dash = "dot"
                    ),
                    hoverinfo = 'text',
                    showlegend = TRUE,
                    legendgroup = 'group1',
                    text = ~ paste0(
                        "Time: ",
                        strftime(Time_round, format = "%H:%M"),
                        "<br>",
                        "Sensor glucose: ",
                        round(glucose_mean, 1)
                    )
                ) %>%
                add_ribbons(
                    data = no_exerciseAverage_df(),
                    x = ~ Time_round,
                    ymin = ~ pLow,
                    ymax = ~ pHigh,
                    hoverinfo = 'text',
                    text = ~ paste0(
                        "Time: ",
                        strftime(Time_round, format = "%H:%M"),
                        "<br>",
                        "Sensor glucose: ",
                        round(glucose_mean, 1)
                    ),
                    line = list(color = 'rgba(128, 128, 128, 0.05)'),
                    fillcolor = 'rgba(128, 128, 128, 0.3)',
                    legendgroup = 'group1',
                    name = 'Middle 50% of readings per time period - \nno exercise'
                ) %>%
                add_lines(
                    data = exercise_df_24(),
                    x =  ~ Time_round,
                    y =  ~ glucose_mean,
                    connectgaps = FALSE,
                    showlegend = TRUE,
                    line = list(color = 'rgba(0, 0, 225, 0.8)'),
                    hoverinfo = 'text',
                    legendgroup = 'group2',
                    name = "Average sensor glucose - exercise",
                    text = ~ paste0(
                        "Time: ",
                        strftime(Time_round, format = "%H:%M"),
                        "<br>",
                        "Sensor glucose: ",
                        round(glucose_mean, 1)
                    )
                ) %>%
                add_ribbons(
                    data = exercise_df_24(),
                    x = ~ Time_round,
                    ymin = ~ pLow,
                    ymax = ~ pHigh,
                    hoverinfo = 'text',
                    text = ~ paste0(
                        "Time: ",
                        strftime(Time_round, format = "%H:%M"),
                        "<br>",
                        "Sensor glucose: ",
                        round(glucose_mean, 1)
                    ),
                    line = list(color = 'rgba(0, 0, 225, 0.05)'),
                    fillcolor = 'rgba(0, 0, 225, 0.3)',
                    legendgroup = 'group2',
                    name = 'Middle 50% of readings per time period - \nexercise'
                ) %>%
                add_markers(data = exercise_df_start(),
                            x = ~Time,
                            y = ~y,
                            hoverinfo = 'text',
                            text = ~paste0(
                                "Date: ", as.character(Date),
                                "<br>",
                                "Time: ",
                                strftime(Time, format = "%H:%M")
                            ),
                            name = "Exercise start time",
                            color = 'rgba(34,139,34)'
                ) %>% 
                highlight(
                    data = exercise_df_24(),
                    on = "plotly_hover",
                    off = "plotly_doubleclick",
                    opacityDim = 0.5
                ) %>%
                layout(
                    yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                 ticklen = 5),
                    showlegend = TRUE,
                    xaxis = list(
                        type = "date",
                        tickformat = "%H:%M",
                        title = 'Time of day'
                    )
                )
        } else {
            plot <-
                plot_ly() %>%
                add_lines(
                    data = no_exerciseAverage_df(),
                    x =  ~ Time_round,
                    y =  ~ glucose_mean,
                    type = 'scatter',
                    mode = 'lines',
                    name = "Average sensor glucose - no exercise",
                    line = list(
                        color = "black",
                        widthh = 0.5,
                        dash = "dot"
                    ),
                    hoverinfo = 'text',
                    showlegend = TRUE,
                    text = ~ paste0(
                        "Time: ",
                        strftime(Time_round, format = "%H:%M"),
                        "<br>",
                        "Sensor glucose: ",
                        round(glucose_mean, 1)
                    )
                ) %>%
                add_lines(
                    data = exercise_df_24(),
                    x =  ~ Time_round,
                    y =  ~ glucose_mean,
                    connectgaps = FALSE,
                    showlegend = TRUE,
                    line = list(color = 'rgba(0, 0, 225, 0.8)'),
                    hoverinfo = 'text',
                    name = "Average sensor glucose - exercise",
                    text = ~ paste0(
                        "Time: ",
                        strftime(Time_round, format = "%H:%M"),
                        "<br>",
                        "Sensor glucose: ",
                        round(glucose_mean, 1)
                    )
                ) %>%
                add_markers(data = exercise_df_start(),
                            x = ~Time,
                            y = ~y,
                            name = "Exercise start time",
                            color = 'rgba(34,139,34)',
                            hoverinfo = 'text',
                            text = ~paste0(
                                "Date: ", as.character(Date),
                                "<br>",
                                "Time: ",
                                strftime(Time, format = "%H:%M")
                            ),
                            name = "Exercise start time",
                            color = 'rgba(34,139,34)'
                ) %>% 
                highlight(
                    data = exercise_df_24(),
                    on = "plotly_hover",
                    off = "plotly_doubleclick",
                    opacityDim = 0.5
                ) %>%
                layout(
                    yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                 ticklen = 5),
                    showlegend = TRUE,
                    xaxis = list(
                        type = "date",
                        tickformat = "%H:%M",
                        title = 'Time of day'
                    )
                )
            
        }
        
        
        plot
    })
    
    
    output$average_exercise_24 <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        if (input$interquartile_range == FALSE) {
        plot_ly() %>% 
            add_lines(data = exercise_df_24_elapse(),
                      x = ~time_elapsed_exercise,
                      y= ~glucose_mean,
                      connectgaps=FALSE,
                      showlegend = TRUE,
                      line = list(color = 'rgba(0, 0, 225, 0.8)'),
                      name = "Average sensor glucose",
                      hoverinfo = 'text',
                      text = ~paste0(
                          "Time: ",
                          time_elapsed_exercise,
                          "<br>",
                          "Sensor glucose: ",
                          round(glucose_mean, 1)
                      )) %>% 
            layout(
                yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                             ticklen = 5),
                showlegend = TRUE,
                xaxis = list(
                    title = 'Time since exercise',
                    autotick = F, dtick = 4
                )
            )
        } else {
            plot_ly() %>% 
                add_lines(data = exercise_df_24_elapse(),
                          x = ~time_elapsed_exercise,
                          y= ~glucose_mean,
                          connectgaps=FALSE,
                          showlegend = TRUE,
                          line = list(color = 'rgba(0, 0, 225, 0.8)'),
                          name = "Average sensor glucose",
                          hoverinfo = 'text',
                          text = ~paste0(
                              "Time: ",
                              time_elapsed_exercise,
                              "<br>",
                              "Sensor glucose: ",
                              round(glucose_mean, 1)
                          )) %>% 
                add_ribbons(
                    data =  exercise_df_24_elapse(),
                    x = ~ time_elapsed_exercise,
                    ymin = ~ pLow,
                    ymax = ~ pHigh,
                    hoverinfo = 'text',
                    text = ~ paste0(
                        "Time: ",
                        time_elapsed_exercise
                    ),
                    line = list(color = 'rgba(0, 0, 225, 0.05)'),
                    fillcolor = 'rgba(0, 0, 225, 0.3)',
                    name = 'Middle 50% of readings per time after exercise'
                ) %>%
                layout(
                    yaxis = list(title = paste0("Sensor glucose ", mmol_or_dL()),
                                 ticklen = 5),
                    showlegend = TRUE,
                    xaxis = list(
                        title = 'Time since exercise',
                        autotick = F, dtick = 4
                    )
                )
        }
    })
    
    ### #
    
    # Download the data ####
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(cumAverage_df(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
