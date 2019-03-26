library(shiny)
library(plotly)
library(dplyr)

wage.df <- read.csv('data/annualsalary.csv', stringsAsFactors = FALSE)

vals <- unique(wage.df$Agency_Title)
depts <- seq(1:137)
names(depts) <- vals
depts <- as.list(depts)

shinyUI(navbarPage('Washington Department Wages', theme = "bootstrap.css",
 # Prints out introduction of project on Home tab
 tabPanel('Home',
            mainPanel(
              
              # This prints out information about the intended audience
              h1("Intended Audience", align = "center"),
              p("This is meant for an audience who is interested in the salaries of Washington employees paid by the state. 
                It can be useful to compare the salaries from each department and help point out trends/patterns."),
              br(),
              
              # This prints out information about the dataset
              h1("The Data", align = "center"),
              h2("Who?", style = "color:navy"),
              p("The dataset itself displays over 340,000 salaries for most employees who began working for the state of 
                Washington January 1, 2010 or after and earned more than $300 during the calendar year. This does not include 
                anyone new to the state government of the current calendar year or student employees because it is protected 
                by federal regulation and is not public information. However it does reflect the earnings of those who have 
                since retired or quit so many only show for a portion of a year."),
              h2("What?", style = "color:navy"),
              p("The dataset we used was provided as a public service and can be accessed by anyone and is updated annually. 
                The data represents the total calendar year earning for each worker paid by the state in Washington. The earnings 
                include base pay plus any additional compensations or premiums such as overtime, call-back, standby or assignment 
                pay and are rounded to the nearest $100. The dataset includes the employee's name, job title and total annual 
                earnings. It was gathered by the Office of Financial Management (OFM) and Washington Technology Solutions and 
                higher education institutions and provided by the Legislative Evaluation and Accountability Program. You can find 
                the dataset ", tags$a(href="http://fiscal.wa.gov/salaries.aspx", "here.")),
              h2("When?", style = "color:navy"),
              p("Includes employees who began working starting January 1, 2010 or after."),
              h2("Where?", style = "color:navy"),
              p("Includes workers employed by Washington State."),
              h2("Why?", style = "color:navy"),
              p("This data set could answer questions about Washington State salaries so the viewer can see and compare different 
                departments and the respective total calendar year earnings."),
              br(),
              
              # This prints out the project authors
              h1("Project Authors", align = "center"),
              tags$ul(
                tags$li("Arihan Jalan", align = "center"), 
                tags$li("Vansh Gambhir", align = "center"), 
                tags$li("Madeleine Prak", align = "center")
              ),
              br(),
              
              # This prints out where to find the code on GitHub
              p("You can find our project's code on ", tags$a(href="https://github.com/arihan-1560795/finalWageProject", "Git Hub."))
            )
 ),
 
 tabPanel('WA State Employee Wages',
          sidebarLayout(
            sidebarPanel(
              textInput(inputId = "person", label = h3("View a WA State Employee's Salary"), value = "First and Last Name")
            ),
            mainPanel(
              # Plots the yearly salaries of a specific employee
              plotlyOutput("findPerson"),
              br(),
              p("This graph can be used to search any person's name employed by Washington State and see their yearly salary 
                throughout their working history. One can see that typically, an individual's salary increases the longer 
                they work.", align = "center"),
              br(),
              h4("Insight"),
              p("Mike Leach, the Head Football Coach at Washington State University, has the highest paid salary by the state in 
                Washington of $2,850,000 in 2015. For 2012, 2013, and 2014, the highest paid salary by the state also belongs to a Head 
                Football Coach. This may be explained by how college athletes don't get paid to play. This portrays how big of a role 
                sports plays in our country and is surprising to see in numbers."),
              br(),
              tags$a(href="http://fiscal.wa.gov/WaStEmployeeHistSalary.txt", "Data Source"))
          )
 ),
 
  tabPanel("Department Wages",
           sidebarPanel(
             checkboxGroupInput("selectDepts", 
                                label = h3("Choose the Department(s)"), 
                                choices = depts,
                                selected = 125)
           ),
           
           mainPanel(
             # Plots the department wise average wage comparison over the years
             plotlyOutput("dept"),
             p("This graph can be used to view any specific department's total average wages. You can select multiple departments 
               to compare the wages and to pick out trends of the data. One can see which department has a higher/lower average 
               salary.", align = "center"),
             br(),
             h4("Insight"),
             p("Choosing the University of Washington and Washington State University helps show the contrast between the two average
               salaries from the two schools. The University of Washington clearly has a higher average. Though, there are some key factors
               that could explain the difference. They both are in different locations--UW in the West and WSU in the East--which both have
               different costs of living including different minimum wages. This could influence the difference of the two average salaries."),
             br(),
             tags$a(href="http://fiscal.wa.gov/WaStEmployeeHistSalary.txt", "Data Source")
           )
  )
    
))