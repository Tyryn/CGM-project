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
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
}

# Increase the file upload limit to 30mb
options(shiny.maxRequestSize = 30*1024^2)


ui <- dashboardPage(
    dashboardHeader(title = "CGM explorer"),
    dashboardSidebar(
   # Sidebar panel where users upload CSVs
            fileInput("infile", "Upload Carelink csv files (multiple possible)",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            dateRangeInput("date1", "Pick a first date range", start = Sys.Date() - 30, end = Sys.Date(),
                           format = "yyyy-mm-dd"),
            dateRangeInput("date2", "Pick a second date range", start = Sys.Date() - 61, end = Sys.Date()-31,
                           format = "yyyy-mm-dd"),
            downloadButton('downloadData', 'Download')
   # Main panel to display table
        ),
   dashboardBody(
       fluidRow(
         box(title = "% days with data", width = 3, solidHeader = TRUE, status = "primary",
             fluidRow(box(),box())),
         box(title = "% days >75% in range", width = 3, solidHeader = TRUE, status = "primary",
             fluidRow(box(),box())),
         box(title = "Best day of the week", width = 3, solidHeader = TRUE, status = "primary",
             fluidRow(box(),box())),
         box(title = "Worst day of the week", width = 3, solidHeader = TRUE, status = "primary",
             fluidRow(box(),box()))),
       fluidRow(
       box(title = "% in range 12am-7am", width = 3, solidHeader = TRUE, status = "primary",
           fluidRow(box(),box())),
       box(title = "% in range 7am-11am", width = 3, solidHeader = TRUE, status = "primary",
           fluidRow(box(),box())),
       box(title = "% in range 11am-6pm", width = 3, solidHeader = TRUE, status = "primary",
           fluidRow(box(),box())),
       box(title = "% in range 6pm-12am", width = 3, solidHeader = TRUE, status = "primary",
           fluidRow(box(),box()))  
       ),
       fluidRow(
           plotlyOutput("graph")
       ),
       fluidRow(
           plotlyOutput("dailyGraph")
       ),
       fluidRow(
           tableOutput("days_75_1")
       )
   )
 )


############################ Server #######################################################################

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Clean and append the CSVs together ----
    getData <- reactive({
        inFile <- input$infile
        if (is.null(inFile)){
            return(NULL)
        }else {
            # browser()
            numfiles = nrow(inFile) 
            csv1 = list()
            
            
            files <- lapply(inFile$datapath, function(y){
                
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
                    unite("date_time", Date:Time, remove = FALSE, sep = " ") %>%
                    mutate(date_time = str_replace_all(date_time, "/", "-")) %>%
                    mutate(date_time = ymd_hms(date_time, tz=Sys.timezone())) %>%
                    mutate(`Sensor Glucose (mmol/L)` = as.numeric(`Sensor Glucose (mmol/L)`))  %>%
                    drop_na(date_time)
                
                # Add row with missing glucose reading if time between reading is greater than 20 minutes
                csv_with_time_added <- csv %>%
                    mutate(time_elapsed = date_time - lag(date_time)) %>%
                    dplyr::filter(time_elapsed<(-1500)) %>%
                    # The +600 is random and just to create another time period.
                    mutate(date_time = date_time+600, Date="", Time="", `Sensor Glucose (mmol/L)`=NA) %>%
                    select(-time_elapsed)

                csv <- csv %>%
                    rbind(csv_with_time_added) %>%
                    arrange(date_time) %>%
                # Fill in missing dates
                    mutate(Date = as.character(as.Date(date_time, "%Y-%m-%d", tz=Sys.timezone())))
                csv
            })
            # Append the files together
              do.call(bind_rows, files)
        }
        })

    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Blocks #### 
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    # Date range 1 data ####
    date1Data <- reactive({
        date1_data <- getData() %>%
            mutate(Date = as.Date(Date, "%Y-%m-%d", tz=Sys.timezone())) %>%
            dplyr::filter(Date>=input$date1[1] & Date<=input$date1[2])
        date1_data
    })
    
    # Date range 2 data ####
    date2Data <- reactive({
        date2_data <- getData() %>%
            mutate(Date = as.Date(Date, "%Y-%m-%d", tz=Sys.timezone())) %>%
            dplyr::filter(Date>=input$date2[1] & Date<=input$date2[2])
        date2_data
    })
    
    # % of days >75% in range ####
    output$days_75_1 <- renderTable({
        days_75_1_data <- date1Data() %>%
            distinct(date_time, .keep_all = TRUE) %>%
            group_by(Date) %>%
            mutate(glucose_mean = mean(`Sensor Glucose (mmol/L)`, na.rm = TRUE)) %>%
            mutate(glucose_sd = sd(`Sensor Glucose (mmol/L)`, na.rm = TRUE)) %>%
            mutate(in_range = ifelse(`Sensor Glucose (mmol/L)`>=4.5 & `Sensor Glucose (mmol/L)`<=8, 1, 0)) %>%
            group_by(Date) %>%
            add_tally() %>%
            mutate(percent_in_range = (sum(in_range, na.rm = TRUE)/n)) %>%
            filter(n>100) %>% # Quite a sharp limit on a days minimum sample size - possibly convert rows to NA
            distinct(Date, .keep_all = TRUE) %>%
            ungroup() %>%
            mutate(n = n()) # %>%
            # Over here, count the number of days with over 75% in range
        
        days_75_1_data
    })
    
    
    
    
    
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    # Graphs ####
    #### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### ##### #
    
    # Create graph showing the days with best average blood sugar and lowest standard deviation
    
    # Get the standard deviation by day
    cgmData <- reactive({
        cgmData <- getData() %>%
            mutate(date = as.Date(date_time, "%Y-%m-%d", tz=Sys.timezone())) %>%
            distinct(date_time, .keep_all = TRUE) %>%
            group_by(date) %>%
            mutate(glucose_mean = mean(`Sensor Glucose (mmol/L)`, na.rm = TRUE)) %>%
            mutate(glucose_sd = sd(`Sensor Glucose (mmol/L)`, na.rm = TRUE)) %>%
            mutate(in_range = ifelse(`Sensor Glucose (mmol/L)`>=4.5 & `Sensor Glucose (mmol/L)`<=8, 1, 0)) %>%
            group_by(date) %>%
            add_tally() %>%
            mutate(percent_in_range = (sum(in_range, na.rm = TRUE)/n)) %>%
            filter(n>100) %>% # Quite a sharp limit on a days minimum sample size - possibly convert rows to NA
            distinct(date, .keep_all = TRUE) 
        cgmData
    })

    output$graph <- renderPlotly({
        
        validate(
            need(!is.null(getData()), "No data uploaded yet")
        )
        
        p <- ggplot(cgmData(), aes(x=date_time, y=percent_in_range, color=glucose_sd, 
                                 text=paste0("Date: ", date, "<br>",
                                             "Percent in range: ", paste0(round(percent_in_range*100),"%"), "<br>",
                                             "Std. deviation: ", round(glucose_sd, 2)))) +
            geom_segment(aes(x=date_time, xend=date_time, y=0, yend=percent_in_range)) +
            geom_point( size=5,  alpha=0.7)+ scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,1)) +
            theme_minimal() + xlab("") + ylab("% readings in range") +
            scale_color_gradient(high = "#FF0000", low = "#66ff00", name = "Daily blood \nsugar variability",
                                 breaks = c(0.5, 5.5), labels = c("Low", "High"), limits = c(0,6))

        p <- ggplotly(p, tooltip = c("text"), source = "date") %>%
            event_register("plotly_click")
        p 
    })
    
    dateClicked <- reactive({
        eventDate <- event_data(event = "plotly_click", source = "date") 
        rowNumber <- eventDate[,2] + 1
        
        data <- cgmData() %>%
            ungroup() %>%
            slice(rowNumber)
        
        date_clicked <- data[[1,2]]
        date_clicked

    })
    
    dailyData <- reactive({
        daily_data <- getData() %>%
            filter(Date==dateClicked()) %>%
            rename(`Date-time`=date_time)
        daily_data
    })
    
    output$dailyGraph <- renderPlotly({
        validate(
            need(!is.null(getData()), "No data uploaded yet")
        )
        validate(
            need(!is.null(dailyData()), "Select a data point on the plot above")
        )
        
        # To create start and end times
        start_time <- as.POSIXct(paste(dailyData()$Date[[1]], "00:00:01"), format="%Y-%m-%d %H:%M:%S")
        end_time <- as.POSIXct(paste(dailyData()$Date[[1]], "23:59:59"), format="%Y-%m-%d %H:%M:%S")
        
        # Graph
        plot <- ggplot(dailyData(), aes(x=`Date-time`, y=`Sensor Glucose (mmol/L)`)) +
            annotate("rect", xmin = start_time, xmax = end_time, ymin = 4.5, 
                     ymax=8, fill="green", alpha=.2,color=NA) +
            geom_line() + xlab("") + theme_minimal() + scale_y_continuous(limits = c(0,25)) 
        
        plot <- ggplotly(plot)
        
        plot
    })
    
    # output$dailyTable <- renderTable({
    #     cgmData()
    # })

    
    # Download the data ####
    output$downloadData <- downloadHandler(
        filename = function() { 
            paste("data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) { 
            write.csv(getData(), file, row.names=FALSE)   
        })
    

}

# Run the application 
shinyApp(ui = ui, server = server)
