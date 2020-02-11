######################################################
# CS 424 | Project 1
# Kevin Kowalski - kkowal28
######################################################
#
# This file contains the dashboard plots and figures
# after receiving the cleaned data set from Jupiter
# Notebook.
#
######################################################

library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(dplyr)
library(DT)
library(leaflet)
library(scales)
library(stringr)

# read in cleaned data
data <- read.table(file = "litterati_challenge-65_CLEANED.csv", sep = ",", header = TRUE)

# list of all unique users sorted by litter count
all_users <- data.frame(sort(table(data$username), decreasing = TRUE))
names(all_users)[1] <- "Username"
names(all_users)[2] <- "Count"

# convert all tags to list of individual tags, using regex pattern ,.* for anything following a comma
data$tags <- gsub(",.*", "", data$tags)
tags_list <- data$tags

# list of all unique tags sorted by litter count
all_tags <- data.frame(sort(table(tags_list), decreasing = TRUE))
names(all_tags)[1] <- "Tag"
names(all_tags)[2] <- "Count"

# map styles list (found from links below)
# https://rstudio.github.io/leaflet/basemaps.html and http://leaflet-extras.github.io/leaflet-providers/preview/
styles <- c(providers$OpenStreetMap.Mapnik, providers$CartoDB.Positron, providers$Esri.NatGeoWorldMap)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  
    dashboardHeader(title = "Litterati Challenge 65 Data in Forest Park, Chicago, Illinois"),
    dashboardSidebar(disable = FALSE, collapsed = FALSE,
                     
    sidebarMenu(
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL)),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      
      # user selection
      selectInput(inputId = "users_input",
                  label = "Choose a User:",
                  choices = c("All Users", all_users[1])),
                  #choices = c("All Users", "julieta", "jill-wagner", "maro-", "betivere", "Julianne B", "unknown_user_8", "rerunvonlinus", "serenity", "unknown_user_4", "dimsum22")),
    
      # tag selection
      selectInput(inputId = "tags_input",
                  label = "Choose a Tag:",
                  choices = c("All Tags", all_tags[1])),
                  #choices = c("All Tags", "plastic", "paper", "untagged", "wrapper", "cigarette", "styrofoam", "bag", "bottle", "cup", "recipt")),
      
      # add space to sidebar
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    
      # time selection
      selectInput(inputId = "time_input",
                  label = "Choose a Time:",
                  choices = c("All Times", "Morning", "Afternoon", "Evening", "Night")),
    
      # month selection
      selectInput(inputId = "month_input",
                  label = "Choose a Month:",
                  choices = c("All Months", "January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")),
    
      # map style selection
      selectInput(inputId = "map_input",
                  label = "Choose a Map Style:",
                  choices = c("1", "2", "3")),
    
      # add space to sidebar
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
      menuItem("", tabName = "cheapBlankSpace", icon = NULL),
    
      # about button
      actionButton("about_info", "About", width = 200)
    ),

    dashboardBody(
      
        # col 1
        column(6,
               
               # row 1
               fluidRow(
                 box(title = "Litter Picked Up by Day from April 19, 2018 to January 7, 2020", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("barDays")
                 )
               ),
               
               column(6,
                      
                      # row 2
                      fluidRow(
                        box(title = "Litter GPS Data", solidHeader = TRUE, status = "primary", width = 12,
                            leafletOutput("map", height = 600)
                        )
                      )
               ),
               
               # row 2
               box(title = "Table for Litter Picked Up by Day", solidHeader = TRUE, status = "primary", width = 2,
                   dataTableOutput("tblDays", height = 600)
               ),
               
               column(2,
                
                  fluidRow(
                     # row 2
                     box(title = "Table for Litter Picked Up by Day of Week", solidHeader = TRUE, status = "primary", width = 12,
                         dataTableOutput("tblWeek")
                     )
                  ),
                   
                  fluidRow(
                     # row 2
                     box(title = "Total Litter Count", solidHeader = TRUE, status = "primary", width = 12,
                         span(textOutput("litterCount"), style = "font-size:150%")
                     )
                   )
               ),
               
               # row 2
               box(title = "Table for Litter Picked Up by Hour of Day", solidHeader = TRUE, status = "primary", width = 2,
                   dataTableOutput("tblHour", height = 600)
               )
        ),
        
        # col 2
        column(3,
               
               # row 1
               fluidRow(
                 box(title = "Litter Picked Up by Day of Week from April 19, 2018 to January 7, 2020", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("barWeek")
                 )
               ),
               
               # row 2
               fluidRow(
                 
                 box(title = "Top 10 Users", solidHeader = TRUE, status = "primary", width = 6,
                     dataTableOutput("topUsers")
                 ),
                 
                 box(title = "Top 10 Tags", solidHeader = TRUE, status = "primary", width = 6,
                     dataTableOutput("topTags")
                 )
               )
        ),
        
        # col 3
        column(3,
               
               # row 1
               fluidRow(
                 box(title = "Litter Picked Up by Hour of 24-hr Day from April 19, 2018 to January 7, 2020", solidHeader = TRUE, status = "primary", width = 12,
                     plotOutput("barHour")
                 )
               ),
               
               # row 2
               fluidRow(
                  box(title = "Litter Picked Up by Top 10 Tags from April 19, 2018 to January 7, 2020", solidHeader = TRUE, status = "primary", width = 12,
                      plotOutput("barTags")
                  )
                )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # daily litter table
    dates <- data.frame(table(data$date))
    names(dates)[1] <- "Date"
    names(dates)[2] <- "Count"
    output$tblDays <- DT::renderDataTable({
      DT::datatable(dates) %>%
      DT::formatStyle("Date", fontSize = "125%") %>%
      DT::formatStyle("Count", fontSize = "125%")
    })
    
    # bar plot of daily litter picked up
    output$barDays <- renderPlot({
      
      options(repr.plot.width = 20, repr.plot.height = 4)
      ggplot(data = dates, aes(x = as.Date(Date), y = Count)) + geom_col(color = "dodgerblue4", fill = "dodgerblue4") + 
        scale_x_date(date_breaks = "30 days", date_labels = "%m/%Y") +
        xlab("Day") + ylab("Litter Picked Up") + 
        #xlim("4/19/2019", "1/7/2020") + 
        theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 16), 
              axis.title.x = element_text(size = 18), axis.title.y = element_text(size = 18))
    })
    
    # weekday litter table
    week <- data.frame(table(weekdays(as.Date(data$date, "%Y-%m-%d"))))
    names(week)[1] <- "Day"
    names(week)[2] <- "Count"
    week$Day <- factor(week$Day, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"))
    week <- week[order(week$Day), ]
    output$tblWeek <- DT::renderDataTable({
      DT::datatable(week, options = list(dom = 't'), rownames = FALSE) %>%
      DT::formatStyle("Day", fontSize = "125%") %>%
      DT::formatStyle("Count", fontSize = "125%")
    })
    
    # bar plot of daily litter picked up by day of the week
    output$barWeek <- renderPlot({
      
      options(repr.plot.width = 12, repr.plot.height = 8)
      ggplot(data = week, aes(x = Day, y = Count, fill = Day)) + geom_col() + 
        xlab("Day of the Week") + ylab("Litter Picked Up") + 
        theme(text = element_text(size = 18), legend.position = "none")
    })
    
    # hourly litter table
    # create empty data frame from 0-23 padded to two digits
    hour <- data.frame(formatC(0:23, width = 2, format = "d", flag = "0"), 0)
    names(hour)[1] <- "Hour"
    names(hour)[2] <- "Count"
    
    # extract hour from time column into new data frame
    hour_trim <- data.frame(table(format(strptime(data$time, "%H:%M:%S"), "%H")))
    names(hour_trim)[1] <- "Hour"
    names(hour_trim)[2] <- "Count"
    
    # loop through both dataframes and merge where time matches
    for (i in 1:length(hour$Hour)) {
      for (j in 1:length(hour_trim$Hour)) {
        if (hour$Hour[i] == hour_trim$Hour[j]) {
          hour$Count[i] <- hour_trim$Count[j]
        }
      }
    }
    
    output$tblHour <- DT::renderDataTable({
      DT::datatable(hour, rownames = FALSE) %>%
      DT::formatStyle("Hour", fontSize = "125%") %>%
      DT::formatStyle("Count", fontSize = "125%")
    })
    
    # bar plot of daily litter picked up by hour of day
    output$barHour <- renderPlot({
      
      options(repr.plot.width = 16, repr.plot.height = 8)
      ggplot(data = hour, aes(x = Hour, y = Count)) + geom_col(fill = "dodgerblue4") +
        ylim(0, 1500) + 
        xlab("Hour of the Day (24-hr)") + ylab("Litter Picked Up") + 
        theme(text = element_text(size = 18))
    })
    
    # leaflet map centered on Forest Park
    output$map <- renderLeaflet({
      leaflet(data = data) %>% 
        addTiles(group = "OSM", options = providerTileOptions(minZoom = 10, maxZoom = 19)) %>%
        addProviderTiles(styles[as.integer(input$map_input)]) %>%
        setView(lng = -87.807, lat = 41.865, zoom = 10) %>%
        addMarkers(clusterOptions = markerClusterOptions(), 
                   popup = paste0("<strong>Username: </strong>", data$username, 
                                  "<br><strong>Timestamp: </strong>", data$date, " at ", data$time, 
                                  "<br><strong>Lat/Lon: </strong>", data$lat, ", ", data$lon, 
                                  "<br><strong>Tags: </strong>", data$tags, 
                                  "<br><img src=", data$url, " alt=Litter Image height=400 width=300>"))
    })
    
    # list of all and top 10 users by litter count
    top10 <- top_n(all_users, 10)
    
    # top 10 users table
    output$topUsers <- DT::renderDataTable({
        DT::datatable(top10, options = list(dom = 't'), selection = list(mode = "single", target = "cell", selection = c(1))) %>%
        DT::formatStyle("Username", fontSize = "125%") %>%
        DT::formatStyle("Count", fontSize = "125%")
    })
    
    # top 10 tags table
    top10_tags <- top_n(all_tags, 10)
    
    output$topTags <- DT::renderDataTable({
      DT::datatable(top10_tags, options = list(dom = 't'), selection = list(mode = "single", target = "cell", selection = c(1))) %>%
      DT::formatStyle("Tag", fontSize = "125%") %>%
      DT::formatStyle("Count", fontSize = "125%")
    })
    
    # bar plot of litter picked up by top 10 tags
    output$barTags <- renderPlot({
      
      options(repr.plot.width = 10, repr.plot.height = 6)
      ggplot(data = top10_tags, aes(x = Tag, y = Count, fill = Tag)) + geom_col() + 
        xlab("Tag") + ylab("Litter Picked Up") + 
        theme(text = element_text(size = 18), legend.position = "none")
    })
    
    # count of litter picked up
    output$litterCount <- renderText({paste("A total of ", count(data), " pieces of litter was picked up in Forest Park, Illinois.")})
    
    # about information
    observeEvent(input$about_info, {
        showModal(modalDialog(
        title = "About",
        p("CS 424 - Project 1"),
        p("by Kevin Kowalski"),
        p("The following libraries were used:"),
        p("- ggplot2"),
        p("- dplyr"),
        p("- DT"),
        p("- leaflet"),
        p("- lubridate"),
        p("- scales"),
        p("- shiny"),
        p("- shinydashboard"),
        p("- stringr"),
        p("Data was sourced from: https://www.litterati.org/"),
        p("( Downloaded from: https://www.evl.uic.edu/aej/424/litterati challenge-65.csv )"),
        easyClose = TRUE
      ))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
