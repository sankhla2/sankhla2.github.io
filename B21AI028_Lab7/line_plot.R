# Importing necessary libraries
library(shiny)
library(xts)
library(plotly)
library(shinythemes)

# Setup linebreaks function
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Define custom palette
customPalette <- c("#FF9900", "#66CC00", "#0099CC", "#CC0066", "#9933FF", "#FF6600", "#00CC99", "#996633", "#6666CC", "#33CC66")

# Data
forum_votes_data <- data.frame(read.csv("~/Downloads/Lab07/Data/wt_forum_votes.csv"))

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
          colourInput(inputId = "col4", label = "Choose Colour", palette = "limited", value = customPalette[4], allowedCols = customPalette)
        ),
        mainPanel(
          plotlyOutput(outputId = "linePlot", height = "500px")
        )
      )
    )
  ),
  inverse = FALSE,
  theme = shinytheme("slate"),
  tags$head(tags$style('body {font-family: Sans-Serif;}')),
  setBackgroundColor(color = c("#F0F0F0"))
)

# Define the server function
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
}

# Run the Shiny application
shinyApp(ui = ui, server = server)
