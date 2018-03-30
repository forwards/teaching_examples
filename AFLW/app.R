#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinythemes)
library(tidyverse)
library(plotly)

players <- read_csv("data/players.csv")
teams <- read_csv("data/teams.csv")
realvars <- c("Kicks", "Handballs", "Disp_eff", "Marks", "Frees_Agst", "Goals", "Behinds", "Goal_assists",
              "Time_On_Ground")
catvars <- c("Player", "Club")
clubs <- unique(players$Club)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Exploring the AFLW statistics"),
    tabsetPanel(
      tabPanel("Data",
      # Sidebar with a slider input for number of bins
      sidebarLayout(
        sidebarPanel(
          selectInput('x', "X", realvars, realvars[1]),
          selectInput('y', "Y", realvars, realvars[2]),
          selectInput('label', "Label", catvars),
          radioButtons('clr', "Colour by club:", c("None", clubs))
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("scatterplot")
        )
      )
    ),
    tabPanel("Players",
     sidebarLayout(
       sidebarPanel(
         radioButtons('year', "Year", c("2017", "2018"), "2018"),
         checkboxGroupInput('vars', "Variables to use:", realvars, realvars[1:3])
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("mds")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$scatterplot <- renderPlotly({
     p <- ggplot(players,
            aes_string(x = input$x, y = input$y,
                       label = input$label)) +
       geom_point(alpha = 0.8) +
       facet_wrap(~Year, ncol=2)
     if (input$clr != "None") {
       players$Clubclr <- "no"
       players$Clubclr[players$Club == input$clr] <- "yes"
       p <- p + aes(colour=players$Clubclr) +
         scale_colour_brewer(palette="Dark2") +
         theme(legend.position = "none")
     }
     ggplotly(p)
   })

   output$mds <- renderPlotly({
     players_sub <- players %>%
       filter(Year == input$year) %>%
       select(input$vars)
     players_sub_mat <- as.matrix(players_sub)
     players_mds <- cmdscale(dist(players_sub_mat), k=2)
     players_mds_df <- as_tibble(players_mds)
     players_mds_df$Player <- players$Player[players$Year == input$year]
     p2 <- ggplot(players_mds_df, aes(x=V1, y=V2, label=Player)) + geom_point()
     ggplotly(p2, tooltip=c("label"))
   })

}

# Run the application
shinyApp(ui = ui, server = server)

