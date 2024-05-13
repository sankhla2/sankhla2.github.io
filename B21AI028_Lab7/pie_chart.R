# Importing the necessary libraries
library(shiny)
library(plotly)

# Setup linebreaks function
linebreaks <- function(n) {
  HTML(strrep(br(), n))
}

# Data
world_data <- data.frame(read.csv("~/Downloads/Lab07/Data/world_grand.csv"))

# UI
ui <- fluidPage(
  navbarPage(
    id = "nbar", 
    title = "B21AI028 DV Lab07", 
    fluid = TRUE,
    tabPanel(
      title = "Chart",
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
    )
  ),
  inverse = FALSE,
  theme = shinytheme("slate"),
  tags$head(tags$style('body {font-family: Sans-Serif;}')),
  setBackgroundColor(color = c("#F0F0F0"))
)

# Server
server <- function(input, output) {
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
          margin = list(l=80, r=80, t=60, b=60),
          title = "Kaggle Grand Masters"
        )
    } else if (input$type5 == 'donut') {
      p <- plot_ly(
        grand_world, 
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
}

# Running the Shiny application
shinyApp(ui = ui, server = server)
