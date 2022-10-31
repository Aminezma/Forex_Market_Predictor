## app.R ##
library(shiny)
library(shinydashboard)
library(dplyr)
library(readxl)
library(plotly)
library(rworldmap)
library(dygraphs)
library(xts)          # To make the convertion data-frame / xts format
library(tidyverse)
library(lubridate)

# import data
requests <- read_xlsx("Historical_data.xlsx", sheet = "Requests")
survey <- read_xlsx("Historical_data.xlsx", sheet = "Survey")

# user interface
ui <- dashboardPage(

  skin = "blue",
  
  # header
  dashboardHeader(
    title = "Forex Predictor",
    tags$li(class = "dropdown",
            style = "margin-top: 7px; margin-right: 5px;",
            actionButton(icon = icon("question"), "help", "", title = "Start a tour of the dashboard"))
  ),
    
  
  # sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Currency Location", tabName = "map", icon = icon("map")),
      menuItem("Filter", icon = icon("filter"), startExpanded = FALSE,
               radioButtons("center", "Filter by Currency", list("Combined", "Currency Center A", "Currency Center B", "Currency Center C", "Currency Center D"))
      ),
      menuItem("Latest Market Trends", icon = icon("download",lib='glyphicon'), href = "https://www.tradingview.com/")
      
    )
  ),
  
  # body
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard",
              
        # link custom javascript
        tags$head(tags$script(src="myscript.js")),
        
        # link custom css, intro.css and c3.css
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/intro.js/2.9.3/introjs.css"),
          tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.1/c3.min.css")
        ),
        
        # link intro.js, d3.js and c3.js
        tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/intro.js/2.9.3/intro.js")),
        tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/d3/5.9.2/d3.min.js")),
        tags$head(tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/c3/0.7.1/c3.min.js")),
        
        # row one
        frow1 <- fixedRow(
          valueBoxOutput("value1", width = 3),
          valueBoxOutput("value2", width = 3),
          valueBoxOutput("value3", width = 3),
          valueBoxOutput("value4", width = 3)
        ),
        
        # # row 2
        # frow2 <- fluidRow( 
        #   box(
        #     title = "Major Issues"
        #     ,id = "box1"
        #     ,status = "primary"
        #     ,solidHeader = FALSE 
        #     ,collapsible = TRUE 
        #     ,plotlyOutput("bar", height = "300px")
        #   )
        #   ,box(
        #     title = "Channel Types"
        #     ,id = "box2"
        #     ,status = "primary"
        #     ,solidHeader = FALSE 
        #     ,collapsible = TRUE 
        #     ,plotlyOutput("pie", height = "300px")
        #   )
        # ),
        # 
        #row 3
        frow3 <- fluidRow(
          box(
            title = "2015 - 2016 Market Charts",
            id = "box3",
            status = "primary",
            solidHeader = FALSE,
            collapsible = TRUE,
            width = 12,
            plotlyOutput("line", height = "300px")
          )
        )
      ),
      
      # map tab
      tabItem(tabName = "map",
        frow4 <- fluidRow(
          box(
            title = "Locality of the Currency Pairs",
            id = "box4",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            plotOutput("map", height = "750px")
          )
        )
      )
    )
    
    
  )
)

# server function
server <- function(input, output, session) {
  
  # average product quality
  product_quality <- as.numeric(c(survey$`ProductQuality 9pt act`))
  product_score <- 'GBP/USD'
  
  # overall satisfaction
  satisfaction_quality <- as.numeric(c(survey$`QualityOfCurrency 9pt act`))
  satisfaction_score <- 'USD/CHF'
  
  # subset issue codes data as factor
  requests$`Issue Code 1` <- as.factor(requests$`Issue Code 1`)
  
  # KPI boxes
  # average time to close
  output$value1 <- renderValueBox({
    if (input$center == "Combined") {
      time <- requests
    } else {
      time <- requests %>% filter(requests$`Vendor - Site` == input$center)
    }
    time_to_close <- c(time$`Time To Close`)
    avg_time_to_close <- 'EUR/USD'
    valueBox(
      avg_time_to_close
      ,paste('Euro Dollar')
      ,icon = icon("signal",lib='glyphicon')
      ,color = "purple")  
  })
  # average monthly requests 2020 (previous year)
  output$value2 <- renderValueBox({ 
    if (input$center == "Combined") {
      ly_requests <- filter(requests, Year == 2020)
    } else {
      ly_requests <- filter(requests, Year == 2020)
      ly_requests <- ly_requests %>% filter(ly_requests$`Vendor - Site` == input$center)
    }
    avg_monthly_requests <- 'USD/JPY'
    valueBox(
      avg_monthly_requests
      ,'Dollar Yen'
      ,icon = icon("signal",lib='glyphicon')
      ,color = "green")  
  })
  # product quality
  output$value3 <- renderValueBox({
    valueBox(
      product_score
      ,paste('Pound Dollar')
      ,icon = icon("signal")
      ,color = "red")   
  })
  # overall satisfaction
  output$value4 <- renderValueBox({
    valueBox(
      satisfaction_score,
      paste("Dollar Swiss Franc"),
      icon = icon("signal", lib = "glyphicon"),
      color = "yellow"
    )
  })
  
  # customer channel types plot
  output$pie <- renderPlotly({
    if (input$center == "Combined") {
      channel <- requests
    } else {
      channel <- requests %>% filter(requests$`Vendor - Site` == input$center)
    }
    channel <- channel %>% group_by(channel$`Currency Channel`) %>% tally
    channel <- channel %>%
      rename(
        channel = "channel$\`Currency Channel\`"
      )
    channel <- channel[1:3, ]
    
    plot_ly(
      requests, 
      labels = channel$channel,
      values = channel$n, 
      type='pie'
    )
  })
  
  # time series analysis - single chart setup
  output$line2 <- renderPlotly({
    plot_ly(demand, x = demand$`demand$date`, y = demand$n, type = 'scatter', mode = 'lines')
  })
  
  # demand vs speed of closure - multi chart setup - count daily service requests and avg time to close
  output$line <- renderPlotly({
    if (input$center == "Combined") {
      demand <- requests
    } else {
      demand <- requests %>% filter(requests$`Vendor - Site` == input$center)
    }
    
    demand <- data_frame(date = demand$`Ticket Close Date`, id = demand$`Service Request Id`, time = demand$`Time To Close`)
    time_to_close <- aggregate(demand["time"], by=demand["date"], mean)
    demand <- demand %>% group_by(demand$date) %>% tally
    
    plot_ly() %>%
      add_bars(x = demand$`demand$date`, y = demand$n, name = "Price") %>%
      add_lines(x = time_to_close$date, y = time_to_close$time, name = "Trend", yaxis = "y2") %>%
      layout(
        yaxis = list(side = 'left', title = 'Price Movement', showgrid = FALSE, zeroline = FALSE),
        yaxis2 = list(side = 'right', overlaying = "y", title = 'Time', showgrid = FALSE, zeroline = FALSE)
      )
  })
  
  # top issues plot
  output$bar <- renderPlotly({
    if (input$center == "Combined") {
      issues <- requests
    } else {
      issues <- requests %>% filter(requests$`Vendor - Site` == input$center)
    }
    
    # count data using group by
    issues <- issues %>% group_by(issues$`Issue Code 1`) %>% tally
    
    # select top 10
    issues <- issues %>% top_n(10, n)
    
    # rename bar chart column
    issues <- issues %>%
      rename(
        issues = "issues$\`Issue Code 1\`"
      )
    
    # example of creating and sending JSON object to the browser
    #issues_json <- toJSON(issues)
    #session$sendCustomMessage("issues", issues_json)
    
    plot_ly(
      
      # factor() again to drop the levels that are null - 29 to 10
      data = issues,
      y = factor(issues$issues),
      x = issues$n,
      name = "Major Issues",
      type = "bar",
      orientation = "h"
      
    )
  })
  
  # world map plot
  output$map <- renderPlot({
    # create country frequency table, match then output map plot
    if (input$center == "Combined") {
      countries <- requests
    } else {
      countries <- requests %>% filter(requests$`Vendor - Site` == input$center)
    }
    countries <- as.data.frame(table(countries$`Customer Country/Region`))
    colnames(countries) <- c("country", "value")
    matched <- joinCountryData2Map(countries, joinCode = "NAME", nameJoinColumn = "country")
    mapCountryData(matched, nameColumnToPlot = "value", mapTitle = "", 
                   catMethod = "pretty", colourPalette = c("lightblue", "darkblue"))
  })
}

shinyApp(ui, server)
