# App where csvs can be uploaded and the data analyzed (maybe something so that users can compare against each other?)


library(tidyverse)
library(shiny)
library(zoo)
library(lubridate)
library(scales)

# Function to make first row column names
header.true <- function(df) {
    names(df) <- as.character(unlist(df[1,]))
    df[-1,]
}

# Increase the file upload limit to 30mb
options(shiny.maxRequestSize = 30*1024^2)


ui <- fluidPage(
    titlePanel("CGM data"),
    sidebarLayout(
   # Sidebar panel where users upload CSVs
        sidebarPanel(
            fileInput("infile", "Upload Carelink csv file",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            downloadButton('downloadData', 'Download')
   # Main panel to display table
        ),
   mainPanel(
       fluidRow(
           plotlyOutput("graph")
         #  tableOutput("contents")  
       )
   )
 )
)

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
            
            
            files3 <- lapply(inFile$datapath, function(y){
                
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
                    mutate(`Sensor Glucose (mmol/L)` = as.numeric(`Sensor Glucose (mmol/L)`)) %>%
                    drop_na(date_time)
                csv
            })
            # Append the files together
            do.call(rbind, files3)
        }
        })

    # Create graph showing the days with best average blood sugar and lowest standard deviation
    
    # Get the standard deviation by day
    cgmData <- reactive({
        cgmData <- getData() %>%
            mutate(date = as.Date(date_time, "%Y-%m-%d", tz=Sys.timezone())) %>%
            distinct(date_time, .keep_all = TRUE) %>%
            group_by(date) %>%
            mutate(glucose_mean = mean(`Sensor Glucose (mmol/L)`)) %>%
            mutate(glucose_sd = sd(`Sensor Glucose (mmol/L)`)) %>%
            mutate(in_range = ifelse(`Sensor Glucose (mmol/L)`>=4.5 & `Sensor Glucose (mmol/L)`<=8, 1, 0)) %>%
            group_by(date) %>%
            add_tally() %>%
            mutate(percent_in_range = (sum(in_range)/n)) %>%
            filter(n>100) %>% # Quite a sharp limit on a days minimum sample size - possibly convert rows to NA
            distinct(date, .keep_all = TRUE) 
        cgmData
    })

    output$graph <- renderPlotly({
        
        p <- ggplot(cgmData(), aes(x=date_time, y=percent_in_range, color=glucose_sd, 
                                 text=paste0("Date: ", date, "<br>",
                                             "Percent in range: ", paste0(round(percent_in_range*100),"%"), "<br>",
                                             "Std. deviation: ", round(glucose_sd, 2)))) +
            geom_segment(aes(x=date_time, xend=date_time, y=0, yend=percent_in_range)) +
            geom_point( size=5,  alpha=0.7)+ scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
            theme_minimal() + xlab("Date") + ylab("% readings in range") +
            scale_color_gradient(high = "#FF0000", low = "#66ff00")

        figure <- ggplotly(p, tooltip = c("text"))
        figure
    })
    
    
    
    # output$contents <- renderTable({
    #     cgmData()
    # })
    # 
    
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
