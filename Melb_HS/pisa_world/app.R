#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(tidyverse)
library(gridExtra)
library(plotly)
library(ggthemes)
library(ggmap)
library(forcats)
library(ISOcodes)
data("ISO_3166_1")
pisa_gap <- read_csv("pisa_gap.csv")
pisa_gap <- pisa_gap %>%
  mutate(Name = fct_reorder(Name, wmathgap)) %>%
  mutate(direction=ifelse(wmathgap>0, "boys", "girls"))
world_map <- map_data("world")

pisa_gap <- pisa_gap %>%
  mutate(Name = recode(Name, "Czechia"="Czech Republic",
                       "Korea, Republic of"="South Korea",
                       "Macedonia, Republic of"="Macedonia",
                       "Moldova, Republic of"="Moldova",
                       "Russian Federation"="Russia",
                       "Taiwan, Province of China"="Taiwan",
                       "Trinidad and Tobago"="Trinidad",
                       "United States"="USA",
                       "United Kingdom"="UK",
                       "Viet Nam"="Vietnam"))
world_map$region[world_map$subregion == "Hong Kong"] <- "Hong Kong"
world_map$region[world_map$subregion == "Macao"] <- "Macao"
to_map <- left_join(world_map, pisa_gap, by=c("region"="Name"))
realvars <- c("wmath_m", "wmath_f", "wread_m", "wread_f",
              "wscience_m", "wscience_f",
              "wmathgap", "wreadgap", "wsciegap")

library(shiny)
library(viridis)

# Define UI for application that draws a histogram
ui <- fluidPage(

   # Application title
   titlePanel("PISA Scores across the globe"),

   # Sidebar with a slider input for number of bins
   sidebarLayout(
      sidebarPanel(
        selectInput('y', 'Colour by', realvars)
      ),

      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("map")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

   output$map <- renderPlot({
      # generate bins based on input$bins from ui.R
     ggplot(to_map, aes(map_id = region)) +
       geom_map(aes_string(fill=input$y), map = world_map,
                color="grey70", size=0.1) +
       scale_fill_viridis(na.value="grey99") +
       expand_limits(x = world_map$long, y = world_map$lat) +
       theme_few() +
       theme(legend.position = "bottom",
             legend.key.width=unit(1.5, "cm"),
             axis.ticks = element_blank(),
             axis.title = element_blank(),
             axis.text =  element_blank())
   })
}

# Run the application
shinyApp(ui = ui, server = server)

