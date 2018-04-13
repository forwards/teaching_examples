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
library(forcats)

players <- read_csv("data/players.csv")
realvars <- c("Kicks", "Handballs", "Disp_eff", "Marks", "Frees_Agst", "Goals", "Behinds", "Goal_assists",
              "Time_On_Ground")
catvars <- c("Player", "Club")
clubs <- unique(players$Club)

# Define UI for application
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Exploring the AFLW statistics"),
    tabsetPanel(
      # Set up the user interface for the scatterplots tab
      # to select variables to plot, labels and colouring
      tabPanel("Scatterplots",
               # Sidebar choosing variables, labels and colour
               sidebarLayout(
                 sidebarPanel(
                   selectInput('x', "X", realvars, realvars[1]),
                   selectInput('y', "Y", realvars, realvars[2]),
                   selectInput('label', "Label", catvars),
                   checkboxGroupInput('clr1', "Colour by club:", clubs, NA)
                 ),

                 # Show the scatterplot, with a fixed height
                 mainPanel(
                   plotlyOutput("scatterplot", height="400px")
                 )
               )
      ),
      # Set up the user interface for the dotplots tab
      # to select labels, allow jitter, and colouring
      tabPanel("Dotplots",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput('dotvars', "Variables to plot:", realvars, realvars[1:3]),
                   selectInput('label', "Label", catvars),
                   checkboxInput('jitter', "Add jitter", FALSE),
                   checkboxGroupInput('clr', "Colour by club:", clubs, NA)
                 ),

                 # Show a plot of the generated distribution
                 mainPanel(
                   plotlyOutput("dotplot", height="100%")
                 )
               )
      ),
      # Set up the user interface for the multidimensional scaling tab
      # to select variables to combine, and colouring
      tabPanel("Players",
     sidebarLayout(
       sidebarPanel(
         checkboxGroupInput('vars', "Variables to use:", realvars, realvars[1:3]),
         checkboxGroupInput('clr2', "Colour by club:", clubs, NA)
         ),

        # Show a plot of the generated distribution
        mainPanel(
          plotlyOutput("mds")
        )
      )
    )
  )
)

server <- function(input, output) {

  # Make the interactive scatterplot
  output$scatterplot <- renderPlotly({
     # Set up the basic plot, using the selected variables,
     # facetted by year
     p <- ggplot(players,
            aes_string(x = input$x, y = input$y,
                       label = input$label)) +
       facet_wrap(~Year, ncol=2)
     # Check if Clubs are chosen to colour by. If so add colours to
     # points for the selected clubs, fade everything else out
     if (length(input$clr1) > 0) {
       players_club <- players %>% filter(Club %in% input$clr1)
       p <- p + geom_point(alpha = 0.1, size=1) +
         geom_point(data=players_club, mapping=aes(colour=Club),
                    alpha=1, size=2) +
         scale_colour_brewer(palette="Dark2") +
         theme(legend.position = "bottom")
     }
     else # If no clubs chosen make all points black
       p <- p + geom_point(alpha = 0.4)

     ggplotly(p, tooltip=c("label", "x", "y"))
   })

  # Make the interactive dotplot
  output$dotplot <- renderPlotly({
    # Rearrange data to facilitate plots
    players_sub <- players %>%
      select(Player, Club, Year, input$dotvars)
    players_long <- players_sub %>%
      gather(stat, value, -Player, -Club, -Year)
    # Set up the basic plot, using the selected variables,
    # facetted by year and statistic. Labeller code needed to make
    # facet headings more readable. Remove axis text, for
    # readability, because we can get the values from mouse over
    p <- ggplot(players_long,
                aes_string(x = "1", y = "value",
                           label = input$label)) +
      facet_wrap(~stat+Year, scales="free",
                 ncol=4,
                 labeller = labeller(.multi_line = FALSE,
                   Year = label_value, stat=label_value)) +
      xlab("") + ylab("") +
      theme(axis.text.x = element_blank(),
            strip.text.y = element_blank())
    # If jitter is selected, use geom_jitter to spread points
    if (!input$jitter)
      p <- p + geom_point(alpha = 0.2)
    else
      p <- p + geom_jitter(alpha=0.2, height=0)
    # Colour by Club, if selected
    if (length(input$clr) > 0) {
      players_club <- players_long %>% filter(Club %in% input$clr)
      p <- p +
        geom_point(data=players_club, mapping=aes(colour=Club),
                   alpha=0.5, size=2) +
        scale_colour_brewer(palette="Dark2") +
        theme(legend.position = "bottom")
    }
    # Make it interactive with labels being the Club or Player,
    # and value
    ggplotly(p, tooltip=c("label", "y"))
  })

  # Make the multidimensional scaling plot
  output$mds <- renderPlotly({
     # Needs to be calculated separately by year
     # Subset the data to the year, and chosen variables
     players_sub17 <- players %>%
       filter(Year == "2017") %>%
       select(input$vars)
     players_sub18 <- players %>%
       filter(Year == "2018") %>%
       select(input$vars)
     # MDS requires matrix/numeric input only
     players_sub_mat17 <- as.matrix(players_sub17)
     players_sub_mat18 <- as.matrix(players_sub18)
     # The statistics are all scaled to be centred at 0,
     # with standard deviation 1
     players_sub_mat17 <- apply(players_sub_mat17, 2, scale)
     players_sub_mat18 <- apply(players_sub_mat18, 2, scale)
     # Do the multidimensional scaling with the selected variables
     # Find a 2D layout of points to best match the distance
     # between players using all the chosen statistics
     players_mds17 <- cmdscale(dist(players_sub_mat17), k=2)
     players_mds18 <- cmdscale(dist(players_sub_mat18), k=2)
     # Put the two data sets back together, also adding Player,
     # Year and Club back in
     players_mds_df17 <- as_tibble(players_mds17)
     players_mds_df17$Player <- players$Player[players$Year == "2017"]
     players_mds_df17$Club <- players$Club[players$Year == "2017"]
     players_mds_df17$Year <- "2017"
     players_mds_df18 <- as_tibble(players_mds18)
     players_mds_df18$Player <- players$Player[players$Year == "2018"]
     players_mds_df18$Club <- players$Club[players$Year == "2018"]
     players_mds_df18$Year <- "2018"
     players_mds_df <- bind_rows(players_mds_df17, players_mds_df18)
     # Make the plot
     mds <- ggplot(players_mds_df, aes(x=V1, y=V2, label=Player)) +
       facet_wrap(~Year, scales="free")
     # Colour by Club if chosen
     if (length(input$clr2) > 0) {
       players_mds_df_club <- players_mds_df %>% filter(Club %in% input$clr2)
       mds <- mds + geom_point(alpha=0.1, size=1) +
         geom_point(data=players_mds_df_club,
                    mapping=aes(colour=Club), alpha=0.5, size=2) +
         scale_colour_brewer(palette="Dark2") +
         theme(legend.position = "bottom")
     }
     else
       mds <- mds + geom_point()
     ggplotly(mds, tooltip=c("label"))
   })

}

# Run the application
shinyApp(ui = ui, server = server)

