library(shiny)
library(lubridate)
library(dplyr)
library(highcharter)
library(shinythemes)
library(leaflet)
library(lubridate)
library(shinycssloaders)
library(shinydashboard)
library(shinyalert)

source("helpers.R")

### 1- Data Cleaning

# Reading (Separator)

ui <- fluidPage(
  tags$head(tags$style(HTML_css())),
  useShinyalert(),
  br(),
  sidebarLayout(
    sidebarPanel(
      
      fileInput('upload_file', 'Upload dataset (csv format)'),
      br(),
      sliderInput("slider", "Minimum Magnitude",min=6,max=9,value =7,
                             step = 1)),
  mainPanel(
    align = 'center',
    h2("Overview"),
    wellPanel(
      leafletOutput("leaflet") %>% 
        withSpinner(color="#3C8DBC",type=4, size = 0.7)
    ),
    h2("Top 10 Earthquakes"),
    wellPanel(
      highchartOutput("highmaps") %>% 
        withSpinner(color="#3C8DBC",type=4, size = 0.7)
    ),br(),
    
    
  ))
)

server <- function(input, output) {
  
  observeEvent(c(input$upload_file,input$slider),ignoreInit = T,
               {
                 shinyalert(text = HTML("<div class='cssload-loader'>Loading (5 sec)</div>"),
                            timer = 5000,
                            animation = "pop",
                            showConfirmButton = FALSE,
                            html = TRUE)
               })

  Data_reactive_lefleat <- eventReactive(
    c(input$slider,input$upload_file), ignoreInit = T,
    {
      
      Data <- read.csv2(input$upload_file$datapath, sep=',')
      # Get rid of useless columns
      Data <-
        Data[
          Data$type == "earthquake"
          ,
          c("time","latitude","longitude","mag","place","Lives.Lost","Financial.impact")
        ]
      Data$time <-
        as_date(Data$time)
      Data$longitude <-
        as.numeric(Data$longitude)
      Data$mag <-
        as.numeric(Data$mag)
      Data$latitude <-
        as.numeric(Data$latitude)
      Data$Financial.impact <-
        substring(Data$Financial.impact,3) %>%
        gsub(",","",.) %>%
        as.numeric()
      # Replace NA by 0
      Data[
        is.na(Data$Financial.impact),
        "Financial.impact"
      ] <- 0
      Data[
        is.na(Data$Lives.Lost),
        "Lives.Lost"
      ] <- 0
      # filter select input
      Data[Data$mag >= input$slider,]
    }
  )
  
  output$leaflet <- renderLeaflet({
    pal <- colorNumeric(
      palette = "Reds",
      Data_reactive_lefleat()$mag
    )
    leaflet(Data_reactive_lefleat(), options = leafletOptions(
      attributionControl=FALSE)) %>%
      addProviderTiles(providers$ CartoDB.Positron) %>%
      addCircles(lng = ~longitude, lat = ~latitude, fillOpacity = 0.73, color = "Red",
                 fillColor = ~pal(mag),
                 stroke = TRUE , weight = 1, radius = ~6*mag^4,
                 popup = ~paste0(" Magnitude : <b>", mag,"</b>", "<br> <em>",place,"</em>",
                                 "<br> <b> Date </b> = <em>",format(time,"%Y-%m-%d"), "</em>")) %>%
      clearBounds()
  })
  
  output$highmaps <- renderHighchart({
    Data <- Data_reactive_lefleat()
    Data_top10 <- Data[order(Data$mag, decreasing = T),] %>% head(.,10)
    #map 2
    cities <- data.frame(
      name = Data_top10$place,
      lat = Data_top10$latitude,
      lon = Data_top10$longitude,
      z = Data_top10$mag,
      lives = Data_top10$Lives.Lost,
      financial = Data_top10$Financial.impact %>% format(big.mark = ",",scientific = F),
      date = Data_top10$time
    )
    hcmap(showInLegend = F) %>%
      hc_add_series(
        data = cities,
        type = "mapbubble",
        name = "Earthquake",
        dataLabels = list(enabled = FALSE),
        tooltip = list(
          pointFormat = 
            "<em>- {point.name} </em> <br>
      Date : <b> {point.date} </b> <br>
      Magnitude : <b> {point.z} </b> <br>
      Lives lost : <b> {point.lives} </b> <br>
      Financial impact : $<b> {point.financial} </b>"
        ),
        minSize = "5%",
        maxSize = "15%",
        color = "red"
      ) %>%
      hc_mapNavigation(enabled = TRUE)
  })
  
}


shinyApp(ui, server)