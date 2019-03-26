library(dplyr)
library(plotly)
library(shiny)

source("scripts/Department_Wages.R")
source("scripts/Find_Person.R")

wage.df <- read.csv('data/annualsalary.csv', stringsAsFactors = FALSE)

colnames(wage.df)[7] <- "Sal2014"

clean.commas <- function(column) {
  as.numeric(gsub(",","",column))
}

wage.df[5:8] <- lapply(wage.df[5:8], clean.commas)


shinyServer(function(input, output) { 

  output$findPerson <- renderPlotly({
    
    Find_Person(wage.df, input$person)
    
  })
  
  output$dept <- renderPlotly({ 
    
    Department_Wages(wage.df, input$selectDepts)
    
  })
})

