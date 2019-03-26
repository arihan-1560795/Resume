library(plotly)
library(stringr)

# Find_Person prints a plotLy scatterplot for the given peron's salary for the 4 years of data that 
# the government has made available. 
# If the person holds multiple positions and earns different salries for each role, the salaries are
# summed up for each year and displayed.
# The job title's held, alongside by which agency are displayed when hovering over the graph.
# Note: assumes that each name is unique and that each person filtered through the given name is the
#       same person. Also assumes that the person has worked under the same agency through the range
#       for which data is available.
#       The given name can have the first and last name in any order, but the function assumes no extra
#       information is given (no initials, middle namem, etc.)

# Split's given name into a vector

Find_Person <- function(wage.df, person){
  name.vec <- unlist(strsplit(toupper(person), split=" "))
  
  # Filters data based if the given name is contained partially or entirely in the name's column
  # Assumes no extra initials are given (only first and last name)
  person.df <- wage.df[sapply(strsplit(wage.df$employee_name, split=", "), function(str) all(name.vec %in% str)), ]
  
  # Creates a dataframe with only the years and their corresponding salary information
  # If more than one row was present, the salaries for that given year are summed
  years <- c('2012', '2013', '2014', '2015')
  salary <- c(sum(person.df$Sal2012), sum(person.df$Sal2013), sum(person.df$Sal2014), sum(person.df$Sal2015))
  ys.df <- data.frame(years, salary)
  
  # Prepares formatted strings for the person's name, the posts they've held, and the agency they've worked under
  agency <- str_to_title(person.df$Agency_Title)
  jobs <- str_to_title(person.df$job_title)
  name <- str_to_title(person)
  
  # Returns a scatterplot with the points connected by lines
  # The x-axis is the years, and y-axis the person's summed salary for that person across all the posts they've held
  # When hovering a point, the person's name, agency, posts they're holding, and salary for that year become visible
  p <- plot_ly(
    data = ys.df, x = ~years, y = ~salary, type = 'scatter', mode = 'lines', hoverinfo = "text", 
    text = paste0(' Name: ', name, 
                  "<br /> Agency:", agency, 
                  "<br /> Job Title(s): ", paste(jobs, collapse=", "),
                  "<br /> Salary for ", ys.df$years, ": $", format(ys.df$salary, big.mark=",", scientific=FALSE))
  )%>% layout(
    title = paste0(name, "'s Salary Over Time"), 
    yaxis = list(title = "Salary in USD"), 
    xaxis = list(title = "Years")
  )
  
  return (p)
}
