# Importing necessary libraries
library(shiny)
library(xts)
library(plotly)
library(shinythemes)
library(shinyWidgets)

# Setup linebreaks function
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Define custom palette
customPalette <- c("#FF9900", "#66CC00", "#0099CC", "#CC0066", "#9933FF", "#FF6600", "#00CC99", "#996633", "#6666CC", "#33CC66")

# Data
forum_votes_data <- data.frame(read.csv("~/Downloads/Lab07/Data/wt_forum_votes.csv"))
world_data <- data.frame(read.csv("~/Downloads/Lab07/Data/world_grand.csv"))
top_topics <- data.frame(read.csv("~/Downloads/Lab07/Data/wt_topics.csv"))

# UI
ui <- fluidPage(
  navbarPage(
    id = "nbar",
    title = "B21AI028 DV Lab07",
    fluid = TRUE,
    tabPanel(
      title = "Line Plot",
      titlePanel(title = "Forum Vote Trend"),
      linebreaks(1),
      sidebarLayout(
        position = "left",
        sidebarPanel(
          br(),
          selectInput("xgrid2", label = "X-Axis Gridlines", choices = c("Yes" = "T", "No" = "F"), selected = "Yes"),
          selectInput("ygrid2", label = "Y-Axis Gridlines", choices = c("Yes" = "T", "No" = "F"), selected = "Yes"),
          pickerInput(
            inputId = "col4",
            label = "Choose Colour",
            choices = customPalette,
            options = list(`style` = "btn-info")
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "linePlot", height = "500px")
        )
      )
    ),
    tabPanel(
      title = "Pie Chart",
      titlePanel(title = "Chart"),
      linebreaks(1),
      sidebarLayout(
        position = "left",
        sidebarPanel(
          br(),
          selectInput(
            "type5",
            label = "Type",
            choices = c("Pie" = "pie", "Donut" = "donut"),
            selected = "Pie"
          )
        ),
        mainPanel(
          plotlyOutput(outputId = "pieChart", height = "500px")
        )
      )
    ),
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
  theme = shinytheme("flatly"),
  tags$head(tags$style('body {font-family: Sans-Serif;}')),
  setBackgroundColor(color = c("#F0F0F0"))
)

# Server
server <- function(input, output) {
  # Render function for the Line Plot
  output$linePlot <- renderPlotly({
    forum_votes <- forum_votes_data[, c(2, 3)]
    qxts <- xts(forum_votes[, -1], order.by = as.POSIXct(forum_votes[, 1]))
    plot_ly(
      x = as.POSIXct(forum_votes[, 1]),
      y = forum_votes[, 2],
      type = "scatter",
      mode = "lines+markers",
      marker = list(color = input$col4),
      line = list(color = input$col4)
    ) %>%
      layout(
        title = "Forum Vote Trend",
        xaxis = list(title = "VoteWeek"),
        yaxis = list(title = "cnt", autorange = TRUE),
        showlegend = FALSE
      )
  })
  
  # Render function for the Pie Chart
  output$pieChart <- renderPlotly({
    if (input$type5 == 'pie') {
      p <- plot_ly(
        world_data,
        labels = ~Country,
        values = ~cnt,
        textposition = 'inside'
      ) %>%
        add_pie(hole = 0.6) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          margin = list(l = 80, r = 80, t = 60, b = 60),
          title = "Kaggle Grand Masters"
        )
    } else if (input$type5 == 'donut') {
      p <- plot_ly(
        world_data,
        labels = ~Country,
        values = ~cnt,
        type = 'pie',
        textposition = 'inside'
      ) %>%
        layout(
          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
          margin = list(l = 80, r = 80, t = 60, b = 60),
          title = "Kaggle Grand Masters"
        )
    }
    p
  })
  
  # Render function for the Scatter Plot
  output$scatterPlot <- renderPlotly({
    plot_ly(
      top_topics[200:500, ],
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
