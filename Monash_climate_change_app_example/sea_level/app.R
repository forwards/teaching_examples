#
# This is a Shiny web application.
# You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This code loads in the libraries needed to run parts of the app
library(shiny)
library(ggplot2)
library(ggthemes)
library(viridis)
library(ggmap)
library(dplyr)
library(stringr)
library(gridExtra)

# Load the full elevation data for Melbourne only once,
# which makes it faster. We take subsets based on post code
# Also set up the initial views
load("melb_all.rda")
loc <- c(144.9639, -37.81521)
postcode_old <<- "3800"

# Helper function for finding long/lat of postcode
get_lonlat <- function(pc) {
  loc <- geocode(paste0("Vic, ", pc))
  return(loc)
}

# Helper function to check if map needs to be re-drawn
# It is only re-drawn if the postcode changes
check_postcode <- function(old, new) {
  same <- FALSE
  if (old == new)
    same <- TRUE
  return(same)
}

# Define UI for the application
ui <- fluidPage(

   # Application title
   titlePanel("Sea levels in Melbourne"),

   # Sidebar with (1) a text entry for postcode, and
   #  (2) a slider input for number of metres
   sidebarLayout(
      sidebarPanel(
         textInput("postcode",
                   label="Post code:",
                   value="3000"),
         sliderInput("sea_level",
                     "Number of metres:",
                     min = 0,
                     max = 10,
                     value = 0)
      ),

      # Show the plots
      mainPanel(
         plotOutput("map")
      )
   )
)

# This function does the main computations
server <- function(input, output) {
   output$map <- renderPlot({
     # Check that the postcode is 4 digits and
     # has changed. If so, get a new map from google
     if (str_length(input$postcode == 4) &
       !check_postcode(postcode_old, input$postcode)) {
         loc <- get_lonlat(input$postcode)
         m <<- get_map(loc, zoom=14, scale=1)
         postcode_old <<- input$postcode
     }
     # Subset the data based on the map limits
     bb <- attr(m, "bb")
     m_e_sub <- m_e %>%
       filter(lon > bb$ll.lon,
              lon < bb$ur.lon,
              lat > bb$ll.lat,
              lat < bb$ur.lat)
     # Plot the map, and the elevation as tiles. Wait to display
     p <- ggmap(m)
     p <- p + geom_tile(data=m_e_sub,
                  aes(x=lon, y=lat, fill=elev), alpha=0.5) +
              scale_fill_viridis() +
              theme_map() +
              theme(legend.position = c(1, 0))
     # Compute the proportion of the area that would be under water
     under_water <- NULL
     for (i in seq(0, 10, 1)) {
       under_water <- rbind(under_water,
         c(i, nrow(filter(m_e_sub, elev<i))/nrow(m_e_sub)))
     }
     colnames(under_water) <- c("sea_level", "proportion")
     under_water <- data.frame(under_water)

     # Subset the data based on sea level rise
     m_e_sub_lev <- filter(m_e_sub, elev<input$sea_level)

     # Overlay the tiles indicating under water
     p1 <- p + geom_tile(data=m_e_sub_lev,
                   fill="#06548E", alpha=0.8)
     # Separately plot the cumulative distribution of elevation,
     # and mark off the proportion
     p2 <- ggplot(under_water, aes(x=sea_level, y=proportion)) +
       geom_line() + xlab("Sea level rise") +
       geom_vline(xintercept=input$sea_level, colour="red") +
       ylab("Proportion under water") +
       ylim(c(0,1))
     # Layout the two plots in a nice grid, and display them
     grid.arrange(p1, p2, ncol=3, nrow=2,
                  layout_matrix=cbind(c(1, 1), c(1, 1), c(2, NA)))
    })
}

# Run the application
shinyApp(ui = ui, server = server)

