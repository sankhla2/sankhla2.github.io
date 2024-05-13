# Importing necessary libraries
library(shiny)
library(plotly)
library(shinythemes)

# Setup linebreaks function
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Data
top_topics <- data.frame(read.csv("~/Downloads/Lab07/Data/wt_topics.csv"))

# UI
ui <- fluidPage(
  navbarPage(
    id = "nbar", 
    title = "B21AI028 DV Lab07", 
    fluid = TRUE,
    tabPanel(
      title = "Scatter Plot",
      titlePanel(title = "Scatter Plot"),
      linebreaks(1),
      sidebarLayout(
        position = "left",
        sidebarPanel(
          br(),
          sliderInput(
            inputId = 'size3', 
            label = 'Marker Size', 
            min = 1, 
            max = 20, 
            value = 10, 
            animate = animationOptions(
              interval = 300, 
              loop = FALSE, 
              playButton = actionButton(
                "play", "Play", icon = icon("play"), 
                width = "100px", 
                style = "margin-top: 10px;"
              ), 
              pauseButton = actionButton(
                "pause", "Pause", icon = icon("pause"), 
                width = "100px", 
                style = "margin-top: 10px;"
              )
            )
          ),
          sliderInput(
            inputId = 'opacity3', 
            label = 'Marker Opacity', 
            min = 0, 
            max = 1, 
            value = 1, 
            animate = animationOptions(
              interval = 300, 
              loop = FALSE, 
              playButton = actionButton(
                "play", "Play", icon = icon("play"), 
                width = "100px", 
                style = "margin-top: 10px;"
              ), 
              pauseButton = actionButton(
                "pause", "Pause", icon = icon("pause"), 
                width = "100px", 
                style = "margin-top: 10px;"
              )
            )
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "scatterPlot", height = "500px")
        )
      )
    )
  ),
  inverse = FALSE,
  theme = shinytheme("slate"),
  tags$head(tags$style('body {font-family: Sans-Serif;}')),
  setBackgroundColor(color = c("#F0F0F0"))
)

# Server
server <- function(input, output) {
  output$scatterPlot <- renderPlotly({
    plot_ly(
      top_topics[200:500,], 
      x = ~VoteWeek, 
      y = ~cnt, 
      type = 'scatter', 
      size = ~cnt, 
      color = ~cnt, 
      mode = 'marker', 
      marker = list(
        size = input$size3, 
        opacity = input$opacity3, 
        colorscale = 'Viridis'
      ), 
      text = ~Title
    ) %>%
      layout(
        margin = list(l = 80, r = 80, t = 60, b = 60),
        title = "Forum Topics",
        xaxis = list(title = "Weeks", type = "date", tickformat = "%b %Y"),
        yaxis = list(title = "Votes")
      ) %>%
      colorbar(title = "Votes", len = 0.9)
  })
}

# Running the Shiny application
shinyApp(ui = ui, server = server)
