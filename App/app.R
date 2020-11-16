# App where csvs can be uploaded and the data analyzed (maybe something so that users can compare against each other?)

library(plotly)
library(tidyverse)
library(shiny)
library(zoo)
library(lubridate)
library(scales)
library(shinydashboard)

# Function to make first row column names
header.true <- function(df) {
    names(df) <- as.character(unlist(df[1, ]))
    df[-1, ]
}

# Increase the file upload limit to 30mb
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

############################ UI #######################################################################

ui <- dashboardPage(
    dashboardHeader(title = "CGM explorer"),
    dashboardSidebar(
        # Tag so that the calendars don't get hidden by the title bar
        tags$div(tags$style(
            HTML(".dropdown-menu{z-index:10000 !important;}")
        )),
        
        # Sidebar panel where users upload CSVs
        fileInput(
            "infile",
            "Upload Carelink csv files (multiple possible)",
            multiple = TRUE,
            accept = c("text/csv",
                       "text/comma-separated-values,text/plain",
                       ".csv")
        ),
        fluidRow(hr()),
        dateRangeInput(
            "date1",
            "Pick a date range",
            start = Sys.Date() - 30,
            end = Sys.Date(),
            format = "yyyy-mm-dd"
        ),
        dateRangeInput(
            "date2",
            "Pick a date range to compare",
            start = Sys.Date() - 61,
            end = Sys.Date() - 31,
            format = "yyyy-mm-dd"
        ),
        fluidRow(hr()),
        downloadButton('downloadData', 'Download')
        # Main panel to display table
    ),
    dashboardBody(
        fluidRow(
            box(
                title = "# and % days with data",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("daysData1")), box(textOutput("daysData2")))
            ),
            box(
                title = "% days >70% in range",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("days_70_1")), box(textOutput("days_70_2")))
            ),
            box(
                title = "Best day of the week",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("best_day_1")), box(textOutput("best_day_2")))
            ),
            box(
                title = "Worst day of the week",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("worst_day_1")), box(textOutput("worst_day_2")))
            )
        ),
        fluidRow(
            box(
                title = "% in range 12am-7am",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("period_00_07_1")), box(textOutput("period_00_07_2")))
            ),
            box(
                title = "% in range 7am-11am",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("period_07_11_1")), box(textOutput("period_07_11_2")))
            ),
            box(
                title = "% in range 11am-6pm",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("period_11_18_1")), box(textOutput("period_11_18_2")))
            ),
            box(
                title = "% in range 6pm-12am",
                width = 3,
                solidHeader = TRUE,
                status = "primary",
                fluidRow(box(textOutput("period_18_00_1")), box(textOutput("period_18_00_2")))
            )
        ),
        fluidRow(plotlyOutput("graph")),
        fluidRow(plotlyOutput("dailyGraph")) ,
        fluidRow(tableOutput("dailyTable"))
    )
)


############################ Server #######################################################################

# Define server logic required to draw a histogram
server <- function(session, input, output) {
    # Clean and append the CSVs together ----
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
                csv <- csv %>%
                    # Keep only the rows with the sensor blood sugar data
                    slice(str_which(V2, "Date")[1]:nrow(.)) %>%
                    slice(2:nrow(.)) %>%
                    slice(str_which(V2, "Date")[1]:nrow(.)) %>%
                    # Keep only datetime and sensor blood sugar columns
                    select(V2, V3, V32) %>%
                    header.true(.) %>%
                    # Convert to time date values
                    unite("date_time",
                          Date:Time,
                          remove = FALSE,
                          sep = " ") %>%
                    mutate(date_time = str_replace_all(date_time, "/", "-")) %>%
                    mutate(date_time = ymd_hms(date_time, tz = Sys.timezone())) %>%
                    mutate(`Sensor Glucose (mmol/L)` = as.numeric(`Sensor Glucose (mmol/L)`))  %>%
                    drop_na(date_time)
                
                # Add row with missing glucose reading if time between reading is greater than 20 minutes
                csv_with_time_added <- csv %>%
                    mutate(time_elapsed = date_time - lag(date_time)) %>%
                    dplyr::filter(time_elapsed < (-1500)) %>%
                    # The +600 is random and just to create another time period.
                    mutate(
                        date_time = date_time + 600,
                        Date = "",
                        Time = "",
                        `Sensor Glucose (mmol/L)` = NA
                    ) %>%
                    select(-time_elapsed)
                
                csv <- csv %>%
                    rbind(csv_with_time_added) %>%
                    arrange(date_time) %>%
                    # Fill in missing dates
                    mutate(Date = as.character(
                        as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())
                    ))
                csv
            })
            # Append the files together
            do.call(bind_rows, files)
        }
    })
    
    # Data per day
    cgmData <- reactive({
        cgmData <- getData() %>%
            mutate(date = as.Date(date_time, "%Y-%m-%d", tz = Sys.timezone())) %>%
            distinct(date_time, .keep_all = TRUE) %>%
            group_by(date) %>%
            mutate(glucose_mean = mean(`Sensor Glucose (mmol/L)`, na.rm = TRUE)) %>%
            mutate(glucose_sd = sd(`Sensor Glucose (mmol/L)`, na.rm = TRUE)) %>%
            mutate(in_range = ifelse(
                `Sensor Glucose (mmol/L)` >= 4.5 &
                    `Sensor Glucose (mmol/L)` <= 8,
                1,
                0
            )) %>%
            group_by(date) %>%
            add_tally() %>%
            mutate(percent_in_range = (sum(in_range, na.rm = TRUE) / n)) %>%
            filter(n > 200) %>% # Quite a sharp limit on a days minimum sample size - possibly convert rows to NA
            distinct(date, .keep_all = TRUE) %>%
            # Below is for best and worst day of the week
            mutate(day_of_week = weekdays(date))
        cgmData
    })
    
    # Update the date ranges to the first and last dates in the cgm data
    observeEvent(input$infile, {
        end_date <- max(cgmData()$date)
        start_date <- end_date - 30

        updateDateRangeInput(session, inputId = "date1", start = start_date,
                             end = end_date)
    })
    
    observeEvent(input$infile, {
        end_date <- max(cgmData()$date) - 31
        start_date <- min(cgmData()$date)
        
        # if(difftime(end_date, start_date, units = "days")>)
        updateDateRangeInput(session, inputId = "date2", start = start_date,
                             end = end_date)
    })

    
    
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Blocks ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    
    # Date range 1 data ####
    date1Data <- reactive({
        date1_data <- cgmData() %>%
            # mutate(Date = as.Date(Date, "%Y-%m-%d", tz=Sys.timezone())) %>%
            dplyr::filter(date >= input$date1[1] &
                              date <= input$date1[2]) %>%
            ungroup() %>%
            mutate(n = n()) %>%
            # Count the number of days with over 70% in range
            mutate(count_70 = sum(percent_in_range >= 0.70))
        
        date1_data
    })
    
    # Date range 2 data ####
    date2Data <- reactive({
        date2_data <- cgmData() %>%
            dplyr::filter(date >= input$date2[1] &
                              date <= input$date2[2]) %>%
            ungroup() %>%
            mutate(n = n()) %>%
            # Count the number of days with over 70% in range
            mutate(count_70 = sum(percent_in_range >= 0.70))
        
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
        
        number_days_picked <- difftime(input$date1[2], input$date1[1], units = "days") + 1 # Adding the plus one because dates could fall on the same day
        
        days_data_prop <- paste0(diff_days, ":\n", "(",
                                 round((as.numeric(diff_days)/as.numeric(number_days_picked))*100), "%)")
        
       
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
        
        number_days_picked <- difftime(input$date2[2], input$date2[1], units = "days") + 1 # Adding the plus one because dates could fall on the same day
        
        days_data_prop <- paste0(diff_days, ":\n", "(",
                                 round((as.numeric(diff_days)/as.numeric(number_days_picked))*100), "%)")
        
        
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
            select(day_of_week, day_of_week_mean)
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
            distinct(day_of_week, .keep_all = TRUE)
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
            mutate(in_range = ifelse(
                `Sensor Glucose (mmol/L)` >= 4.5 &
                    `Sensor Glucose (mmol/L)` <= 8,
                1,
                0
            )) %>%
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
            mutate(in_range = ifelse(
                `Sensor Glucose (mmol/L)` >= 4.5 &
                    `Sensor Glucose (mmol/L)` <= 8,
                1,
                0
            )) %>%
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
    # Graphs ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    # Create graph showing the days with best average blood sugar and lowest standard deviation
    
    
    
    output$graph <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        
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
                alpha = .1,
                color = NA
            ) +
            annotate(
                "rect",
                xmin = input$date2[2],
                xmax = input$date2[1],
                ymin = 0,
                ymax = 1,
                fill = "pink",
                alpha = .2,
                color = NA
            ) +
            # Create lollipops
            geom_segment(aes(
                x = date,
                xend = date,
                y = 0,
                yend = percent_in_range
            )) +
            geom_point(size = 5,  alpha = 0.7) + scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                                                                    limits = c(0, 1)) +
            theme_minimal() + xlab("") + ylab("% readings in range") +
            scale_color_gradient(
                high = "#FF0000",
                low = "#66ff00",
                name = "Daily blood \nsugar variability",
                breaks = c(0.5, 5.5),
                labels = c("Low", "High"),
                limits = c(0, 6)
            )
        
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
        daily_data <- getData() %>%
            filter(Date == dateClicked()) %>%
            rename(`Date-time` = date_time)
        daily_data
    })
    
    output$dailyGraph <- renderPlotly({
        validate(need(!is.null(getData()), "No data uploaded yet"))
        validate(need(
            !is.null(dailyData()),
            "Select a data point on the plot above"
        ))
        
        # To create start and end times
        start_time <-
            as.POSIXct(paste(dailyData()$Date[[1]], "00:00:01"), format = "%Y-%m-%d %H:%M:%S")
        end_time <-
            as.POSIXct(paste(dailyData()$Date[[1]], "23:59:59"), format = "%Y-%m-%d %H:%M:%S")
        
        # Graph
        plot <-
            ggplot(dailyData(),
                   aes(x = `Date-time`, y = `Sensor Glucose (mmol/L)`)) +
            annotate(
                "rect",
                xmin = start_time,
                xmax = end_time,
                ymin = 4.5,
                ymax = 8,
                fill = "green",
                alpha = .2,
                color = NA
            ) +
            geom_line() + xlab("") + theme_minimal() + scale_y_continuous(limits = c(0, 25))
        
        plot <- ggplotly(plot)
        
        plot
    })
    
    # output$dailyTable <- renderTable({
    #     date1Data() %>% 
    #         mutate(date = as.character(date))
    # })
    
    
    # Download the data ####
    output$downloadData <- downloadHandler(
        filename = function() {
            paste("data-", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            write.csv(getData(), file, row.names = FALSE)
        }
    )
    
    
}

# Run the application
shinyApp(ui = ui, server = server)
