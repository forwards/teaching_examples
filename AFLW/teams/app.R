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

teams <- read_csv("data/teams.csv")
realvars <- c("Kicks", "Handballs", "Marks", "Clearances", "Frees_For", "Frees_Agst", "Tackles", "Goals", "Behinds", "Disp_eff", "Goal_acc")
clubs <- unique(teams$Club)

# Define UI
ui <- fluidPage(theme = shinytheme("flatly"),
  titlePanel("Exploring the AFLW team statistics"),
    tabsetPanel(
      tabPanel("Data",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput('dotvars', "Variables to plot:", realvars, realvars[1:3]),
                   checkboxGroupInput('clr1', "Colour by club:", clubs, NA)
                 ),

                 mainPanel(
                   plotlyOutput("scatterplot", height="700px")
                 )
               )
      ),
      tabPanel("Teams",
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput('vars', "Variables to use:", realvars, realvars[1:3])
                 ),

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
     teams_long <- teams %>%
       select(Club, Year, input$dotvars) %>%
       gather(stat, value, -Club, -Year)
     stats <- ggplot(teams_long, aes(x=1, y=value, label=Club)) +
       xlab("") + ylab("") +
       facet_wrap(~stat+Year, scales="free_y", ncol=4,
                  labeller = labeller(.multi_line = FALSE,
                     Year = label_value, stat=label_value)) +
       theme(axis.text.x = element_blank(),
             strip.text.y = element_blank())
     if (length(input$clr1) > 0) {
       teams_club <- teams_long %>% filter(Club %in% input$clr1)
       stats <- stats + geom_point(alpha = 0.1, size=1) +
         geom_point(data=teams_club, mapping=aes(x=1, y=value,
                                                 colour=Club, label=Club),
                    alpha=1, size=2) +
         scale_colour_brewer(palette="Dark2") +
         theme(legend.position = "bottom")
     }
     else # If no clubs chosen make all points black
       stats <- stats + geom_point(alpha = 0.4)
     ggplotly(stats, tooltip=c("label", "value"))
   })

   output$mds <- renderPlotly({
     teams_sub17 <- teams %>%
       filter(Year == "2017") %>%
       select(input$vars)
     teams_sub18 <- teams %>%
       filter(Year == "2018") %>%
       select(input$vars)
     teams_sub_mat17 <- as.matrix(teams_sub17)
     teams_sub_mat17 <- apply(teams_sub_mat17, 2, scale)
     teams_sub_mat18 <- as.matrix(teams_sub18)
     teams_sub_mat18 <- apply(teams_sub_mat18, 2, scale)
     teams_mds17 <- cmdscale(dist(teams_sub_mat17), k=2)
     teams_mds18 <- cmdscale(dist(teams_sub_mat18), k=2)
     teams_mds_df17 <- as_tibble(teams_mds17)
     teams_mds_df17$Club <- teams$Club[teams$Year == "2017"]
     teams_mds_df17$Year <- "2017"
     teams_mds_df18 <- as_tibble(teams_mds18)
     teams_mds_df18$Club <- teams$Club[teams$Year == "2018"]
     teams_mds_df18$Year <- "2018"
     teams_mds_df <- bind_rows(teams_mds_df17, teams_mds_df18)
     mds2 <- ggplot(teams_mds_df, aes(x=V1, y=V2, label=Club)) + geom_point() + facet_wrap(~Year, scales="free")
     ggplotly(mds2, tooltip=c("label"))
   })

}

# Run the application
shinyApp(ui = ui, server = server)

