library(tidyverse)
library(shiny)

source("funs.R")

# Define UI for application
ui <- fluidPage(

  # Application title
  titlePanel("Number Operations"),

  # Create a tabset
  tabsetPanel(

    # First tab for number squaring
    tabPanel("Square a Number",
             sidebarLayout(
               sidebarPanel(
                 numericInput("number", "Enter a number:", value = 0)
               ),
               mainPanel(
                 textOutput("result")
               )
             )
    ),

    # Second tab for calculator
    tabPanel("Z to T Score",
             sidebarLayout(
               sidebarPanel(
                 width = 3,
                 h4("Z Score"),
                 numericInput("z_score", "Z Score", value = 0, min = -3, max = 3, step = 0.1)
               ),
               mainPanel(
                 textOutput("t_score")
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output) {

  # Convert Z-score to T-score
  output$t_score <- renderText({
    z <- input$z_score
    t <- 50 + 10 * z
    paste("The T Score for Z =", z, "is", t)
  })

  # Square the input number and display the result
  output$result <- renderText({
    result <- input$number ^ 2
    paste("The square of", input$number, "is", result)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
