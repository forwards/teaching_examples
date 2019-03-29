#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load necessary packages
library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(tsibble)
library(sugrrants)
library(glue)
library(bomrang)
library(viridis)
library(plotly)
library(ggmap)
library(ggthemes)

# Read map data
load("data/brissy_map.rda")

# Read data, and create species list for menu, and create a date
wildlife <- read_csv("data/brissy_ala.csv")
species <- wildlife %>% 
  select(`Vernacular name`) %>%
  distinct() %>%
  arrange(`Vernacular name`)
wildlife <- wildlife %>% 
  mutate(date = ymd(paste0(Year,Month,Day)))

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Exploring wildlife spotted around Brisbane"),
    tabsetPanel(
      # Set up the user interface for the calendar tab
      # to select variables to plot, labels and colouring
      tabPanel("Choose a species",
               # Sidebar choosing variables, labels and colour
               sidebarLayout(
                 sidebarPanel(
                   selectInput("species", "pick one", choices = species, selected = "Australian Brush-turkey")
                 ),

                 # Show the scatterplot, with a fixed height
                 mainPanel(
                   plotlyOutput("map", height="400px")
                 )
               )
    ),
    tabPanel("Choose a time period",
             # Sidebar choosing variables, labels and colour
             sidebarLayout(
               sidebarPanel(
                 selectInput("species2", "pick one", choices = species, selected = "Australian Brush-turkey"),
                 dateInput("from", "From:", value = ymd(min(wildlife$date))),
                 dateInput("to", "To:", value = ymd(max(wildlife$date))) 
               ),
               
               # Show the scatterplot, with a fixed height
               mainPanel(
                 plotlyOutput("time", height="400px")
               )
             )
    )
  )
)

server <- function(input, output) {

  # Make the interactive scatterplot of occurrence on map
  output$map <- renderPlotly({
    # Filter to species of interest and plot locations
    wildlife_sub <- wildlife %>% filter(`Vernacular name` == input$species)
    p <- ggmap(brissy_map) + 
      geom_point(data=wildlife_sub, aes(x=Longitude, y=Latitude, label=date), 
                 alpha=0.5, colour="orange") +
      ggtitle(paste(input$species, ": ", nrow(wildlife_sub))) +
      theme_map()
    ggplotly(p, tooltip = "label")
  })

  # Make the interactive time series plot 
  output$time <- renderPlotly({
    # Filter to species of interest, count number per day and plot over time
    wildlife_sub <- wildlife %>% filter(`Vernacular name` == input$species2)
    wildlife_count <- wildlife_sub %>% count(date) %>%
      filter(date >= input$from, date <= input$to)
    p2 <- ggplot(data=wildlife_count, aes(x=date, y=n, label=date)) + 
      geom_point(colour="black", alpha=0.5) + 
      geom_smooth(colour="orange", se=FALSE) +
      xlab("") + ylab("Number") +
      ggtitle(paste(input$species2, ": ", nrow(wildlife_sub)))
    ggplotly(p2, tooltip = c("label", "y"))
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

