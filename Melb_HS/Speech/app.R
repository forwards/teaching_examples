# credits to @yihui
# https://github.com/yihui/shiny-apps/tree/master/voice
# adapted the code above to ggplotly
library(shiny)
library(plotly)
library(ggplot2)
load("data/pisa_au.rda")

shinyApp(
  ui = fluidPage(
    singleton(tags$head(
      tags$script(src="//cdnjs.cloudflare.com/ajax/libs/annyang/2.6.0/annyang.min.js"),
      includeScript('init.js')
    )),
    div(
      style = 'display: block; margin: auto; width: 100%; max-width: 1000px',
      plotlyOutput("scatterplot", height = "100%"),
      helpText(
        'You are recommended to use Google Chrome to play with this app.',
        'To change the title, say something that starts with "title", e.g.',
        '"title I love the R language", or "title Good Morning".',
        'To change the color of points, say something that starts with "color",',
        'e.g. color "blue", or color "green". When the app is unable to recognize the color,',
        'the points will turn gray.',
        'To add a regression line, say "regression".',
        'To make the points bigger or smaller, say "bigger" or "smaller".'
      )
    )
  ),
  server = function(input, output) {
    output$scatterplot <- renderPlotly({
      col <- input$color
      if (length(col) == 0 || !(col %in% colors())) {
        col <- "grey10"
      }
      p <- ggplot(pisa_au, aes(x = math, y = read)) +
        geom_point(size = input$bigger, alpha = 0.7) +
        facet_grid(~ gender) +
        theme(aspect.ratio = 1)

      if (input$yes != "no") {
        p <- p + geom_smooth(method = "lm", se = TRUE)
      }

      p <- p + ggtitle(input$title)

      ggplotly(p)
    })
  }
)
