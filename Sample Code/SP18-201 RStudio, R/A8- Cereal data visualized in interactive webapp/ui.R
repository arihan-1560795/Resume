#ui.R interacts with server.R; it passes along what manufacturer and nutrition the user
#wants to see and plots the resulting graph created by server.R

library(shiny)
library(plotly)
shinyUI(navbarPage('Cereals: Nutrition vs. Popularity',
  sidebarLayout(
    sidebarPanel(

      #The following code creates a panel where the user can select which nutritional value to gauge vs. popularity
      selectInput(inputId = "nutri",
                  label = "Select a nutrition indicator from the drop-down menu:",
                  choices = c('calories', 'protein', 'fat', 'sodium', 'fiber', 'carbo', 'sugars', 'potass', 'vitamins'),
                  selected = "calories"),

      #Creates check-boxes to filter based on cereal manufacturer      
      checkboxGroupInput(
        inputId = 'checkGroup', 
        label = "Filter by cereal brand:", 
        choiceNames =
          list("Nabisco", 
              "American Home Food Products", 
              "General Mills",
              "Kelloggs",
              "Post",
              "Quaker Oats",
              "Ralston Purina"),
        choiceValues =
          list('N', 'A', 'G', 'K', 'P', 'Q', 'R'),
        selected = c('N', 'A', 'G', 'K', 'P', 'Q', 'R')
      ),
        
      hr(),
      fluidRow(column(3, verbatimTextOutput("value")))
        
      ),
    
    mainPanel(
      plotlyOutput("scatter")
    )
  )
))

