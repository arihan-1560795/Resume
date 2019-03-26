#server.R reads in the cereal dataset and renders a plot based on the nutrition statistic the user
#is interested in seeing compared to the cereal's rating. Users may change the amount of data of
#data plotted on the graph by selecting which manufacturer's cereal they wish to see.

library(plotly)
library(dplyr)
library(shiny)

df <- read.delim(file = 'data/cereal.tsv', sep = '\t', header = TRUE, stringsAsFactors = FALSE)

shinyServer(function(input, output) { 

  #Changes cereal names to be space seperated
  df$name <- gsub("\\_", " ", df$name) 

  #Joins a row containing each manufacturer's complete name  
  mfr <- c('N', 'A', 'G', 'K', 'P', 'Q', 'R')
  cereal.b <- c("Nabisco", "American Home Food Products", "General Mills", "Kelloggs", "Post", "Quaker Oats", "Ralston Purina")  
  dfstuff <- data.frame(mfr, cereal.b)
  df <- full_join(df, dfstuff, by = c('mfr'))
  
  #Filters cereal's based on user-selected manufacturer(s) 
  r_a <- reactive(input$checkGroup)
  r_df <- reactive({df %>% filter(mfr %in% r_a())})
   
  #renders plot       
  output$scatter <- renderPlotly({
    return(plot_ly(
            data = r_df(), 
            x = r_df()[[input$nutri]], 
            y = ~rating, 
            color = ~cereal.b,
            text = ~paste0('Name: ', name, "<br />"), 
            type = 'scatter'
                  ) %>% layout(
            title = "Cereal ratings versus nutrition <br />(Hover for details)", 
            yaxis = list(title = "Rating"), 
            xaxis = list(title = paste0("Nutrition: ", input$nutri)))
          )
  })
})

